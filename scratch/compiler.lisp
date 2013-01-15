
(in-package :cl-user)

(defpackage :lunula-compiler
  (:use :common-lisp))

;; forward reference of lunula package
(defpackage :lunula)
(defmacro lunula::setq (&rest things)
  `(setq ,@things))

(in-package :lunula-compiler)

;;;; UTILITIY FUNCTIONS ;;;;

;;;; EMITTER ;;;;
(defparameter *indent-spaces* 2)
(defparameter *indent-level* 0)
(defparameter *compiler-out* *standard-output*)

(defun emit-indent ()
  (dotimes (n (* *indent-spaces*
                 *indent-level*))
    (princ "    " *compiler-out*)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-indent (&body body)
    `(let ((*indent-level* (1+ *indent-level*)))
       ,@body)))

(defun emit-delimited (delimiter list
                       &key (item-printer (lambda (x)
                                            (princ x *compiler-out*))))
  (do ((list list (cdr list)))
      ((not list))
    (funcall item-printer (car list))
    (when (cdr list)
      (princ delimiter *compiler-out*))))

(defun emit-comment-line (str)
  (emit-indented-line "// " str))

(defun emit-commented (thing)
  (with-input-from-string (in (with-output-to-string (out)
                                (pprint thing out)))
    (do ((n 1 (1+ n))
         (line (read-line in nil nil)(read-line in nil nil)))
        ((not line))
      (cond ((and (= n 1)
                  (string-equal "" line))
             (emit-line ""))
            (t (emit-indented-line "// " line))))))

(defun princ-to-string-delimited (delimeter list)
  (with-output-to-string (*compiler-out*)
    (emit-delimited delimeter list)))

(defun lisp-to-js-name (symbol)
  (let* ((char-map '((#\$ . "dollar")   (#\_ . "under")
                     (#\= . "equal")    (#\! . "bang")
                     (#\@ . "at")       (#\@ . "at")
                     (#\# . "hash")     (#\% . "percent")
                     (#\^ . "carrot")   (#\* . "star")
                     (#\? . "question") (#\: . "colon")
                     (#\; . "semicolon")(#\> . "gt")
                     (#\< . "lt")       (#\. . "dot")
                     (#\, . "comma")    (#\+ . "plus")))
         (firstp t)
         (name (if (symbolp symbol)
                   (symbol-name symbol)
                   symbol)))    
    (apply 'concatenate 'string 
           (map 'list (lambda (c)
                        (prog1
                            (cond ((alpha-char-p c) (princ-to-string c))
                                  ((digit-char-p c) (if firstp
                                                        (concatenate 'string (list #\$)
                                                                     (list c))
                                                        (princ-to-string c)))
                                  ((char= c #\-) "_")
                                  (t (concatenate 'string
                                                  "$"
                                                  (or (cdr (assoc c char-map))
                                                      (princ-to-string
                                                       (char-code c)))
                                                  "$")))
                          (setf firstp nil)))
                name))))

(defun emit (&rest args)
  (dolist (arg args)
    (cond ((listp arg) (apply #'emit arg))
          (t (princ arg *compiler-out*)))))

(defun emit-indented (&rest args)
  (emit-indent)
  (emit args))

(defun emit-line (&rest args)
  (emit args)
  (terpri *compiler-out*))

(defun emit-indented-line (&rest args)
  (emit-indent)
  (emit-line args))

(defun concat (&rest args)
  (with-output-to-string (*compiler-out*)
    (emit args)))

;;;; COMPILER ;;;;


(defparameter *literal-lookups* nil)

;; COMPILE IF

(defun if-p (form)
  (and (listp form)
       (eq 'lunula::if (first form))))

(defun compile-if-statement (form env)
  (let ((pred (second form))
        (then (third form))
        (else (fourth form)))
    ;; compile the predicate
    (emit-indented-line "if (" (compile-expression 'lunula::nil nil)
                        " != " (compile-expression pred env) ") {") 
    ;; compile then form
    (with-indent 
      (compile-statement then env))
    (emit-indented-line "}")    
    ;; compile else form
    (emit-indented-line "else {")    
    (with-indent
      (compile-statement else env))
    (emit-indented-line "}")))

(defun compile-if-expression (form env)
  (let ((pred (second form))
        (then (third form))
        (else (fourth form)))
    ;; compile the predicate
    (emit-indented-line "if (" (compile-expression 'lunula::nil nil)
                        " != "
                        (compile-expression pred env)
                        ") {")   
    ;; compile then form
    (with-indent
        (let ((value (compile-expression then env)))
          (unless (equal "$v" value)          
            (emit-indented-line (value)
                                " = " value ";"))))
    (emit-indented-line "}")
    ;; compile else form
    (emit-indented-line "else {")
    (with-indent
        (let ((value (compile-expression else env)))
          (unless (equal "$v" value)
            (emit-indented-line (value) " = " value ";"))))
    (emit-indented-line "}")
    "$v"))

;; GLOBAL LOOKUPS
(defparameter *interns* nil)
(defparameter *declared-vars* nil)

(defun emit-interns ()
  (let ((packages nil))
    (dolist (intern *interns*)
      (let* ((symbol (first intern))
             (package-name (package-name (symbol-package symbol))))
        (pushnew package-name packages)))
    (emit-indented-line "window.Lunula = window.Lunula || {};")
    (emit-indented-line "Lunula.packages = Lunula.packages || {};")
    ;; make sure packages exist
    (dolist (package-name packages)
      (emit-indented-line "Lunula.packages[\""
                          package-name
                          "\"] = Lunula.packages[\""
                          package-name
                          "\"] || {};")
      (emit-indented-line "Lunula.packages[\""
                          package-name
                          "\"].symbols = Lunula.packages[\""
                          package-name
                          "\"].symbols || {};"))
    ;; intern all of the symbols
    (dolist (intern *interns*)
      (let* ((symbol (first intern))
             (symbol-name (symbol-name symbol))
             (package-name (package-name (symbol-package symbol)))
             (var-name (second intern)))
        (emit-indented-line "Lunula.packages[\""
                            package-name
                            "\"].symbols[\""
                            symbol-name
                            "\"] = Lunula.packages[\""
                            package-name
                            "\"].symbols[\""
                            symbol-name
                            "\"] || " (compile-literal-symbol-expression symbol) ";")
        (emit-indented-line "var " var-name " = Lunula.packages[\""
                            package-name
                            "\"].symbols[\""
                            symbol-name
                            "\"] = Lunula.packages[\""
                            package-name
                            "\"].symbols[\""
                            symbol-name
                            "\"];")))))

(defun global-lookup (symbol)
  (let ((var-name (lisp-to-js-name symbol)))
    (pushnew (list symbol var-name)
             *interns*
             :test 'equal)
    var-name))

(defun global-lookup-value (symbol)
  (unless (boundp symbol)
    (warn "Symbol ~A is not bound" symbol))
  (concat (global-lookup symbol)
          ".value"))

(defun global-lookup-function (symbol)
  (unless (fboundp symbol)
    (warn "Symbol ~A is not bound to a function" symbol))
  (concat (global-lookup symbol)
          ".function"))


;; COMPILE SYMBOL

(defun compile-literal-symbol-expression (symbol)
  (concatenate 'string "{ type: \"symbol\", name: \""
               (symbol-name symbol)
               "\" }"))

(defun compile-symbol-lookup-statement (form env)
  (emit-indented-line "// a symbol statement is dead code") 
  (emit-indented-line "// " (compile-symbol-lookup-expression form env) ";"))

(defun compile-symbol-lookup-expression (form env)
  (let ((lookup (env-lookup env form)))
    (cond (lookup (lisp-to-js-name form))
          (t (global-lookup-value form)))))

;; COMPILE NUMBER

(defun compile-number-statement (form env)
  (emit-indented-line "// a number statement is dead code") 
  (emit-indented-line "// " (compile-number-expression form env) ";"))

(defun compile-number-expression (form env)
  (declare (ignore env))
  form)


;; COMPILE CALL

(defparameter *param-count* 0)

(defun param ()  
  (incf *param-count*)
  (let ((param-name (concatenate 'string
                                 "$p"
                                 (princ-to-string *param-count*))))
    (pushnew param-name *declared-vars*
             :test 'string=)
    param-name))

(defun compile-call-expression (form env)
  (let ((fun-name (first form))
        (params (rest form)))
    ;; handle lambdas in the function position
    (when (lambda-p fun-name)
      (let ((compiled-lambda (compile-lambda-expression fun-name env)))
        (unless (equal "$v" compiled-lambda)
          (emit-indented-line (value) " = " compiled-lambda ";")))
      (setf fun-name (value)))
    ;; compile the parameters
    (let ((v-p/p/values (mapcar (lambda (param)
                                  (let ((compiled-param (compile-expression param env)))
                                    (list (equal "$v" compiled-param)
                                          (if (equal "$v" compiled-param)
                                              (param)
                                              compiled-param)
                                          compiled-param)))
                                params)))
      (dolist (v-p/p/value v-p/p/values)
        (let ((v-p (first v-p/p/value))
              (p (second v-p/p/value))
              (value (third v-p/p/value)))
          (when v-p
            (emit-indented-line p " = " value ";"))))
      (concat (if (equal "$v" fun-name)
                  (value)
                  (global-lookup-function fun-name))
              "("
              (princ-to-string-delimited
               ", "
               (mapcar (lambda (v-p/p/value)
                         (second v-p/p/value))
                       v-p/p/values))
              ")"))))

(defun compile-call-statement (form env)
  (emit-indented-line (compile-call-expression form env)
                      ";"))

;; COMPILE LAMBDA

(defun lambda-p (form)
  (and (listp form)
       (eq 'lunula::lambda (first form))))

(defun value ()
  (pushnew "$v" *declared-vars*
           :test 'string=) 
  "$v")

(defun emit-var-declarations ()
  (when *declared-vars*
    (let ((*declared-vars* (copy-list (sort *declared-vars* 'string<))))
      (emit-indented "var " (first *declared-vars*))
      (dolist (var (rest *declared-vars*))
        (emit ", " var))
      (emit-line ";"))))

(defun compile-lambda-list (lambda-list env)
  ;; http://www.lispworks.com/documentation/lw61/CLHS/Body/03_da.htm
  ;;
  ;; The syntax for ordinary lambda lists is as follows:
  ;;
  ;; lambda-list::= (var* 
  ;;                 [&optional {var | (var [init-form [supplied-p-parameter]])}*] 
  ;;                 [&rest var] 
  ;;                 [&key {var | ({var | (keyword-name var)} [init-form [supplied-p-parameter]])}* [&allow-other-keys]] 
  ;;                 [&aux {var | (var [init-form])}*])   
  (let* ((n 0)
         (arity)
         (arity-op '=)
         (compiled-params (with-output-to-string (*compiler-out*)
                            ;; compile normal arguments up until &optional, &rest, &key or
                            ;; &aux
                            (let ((lambda-list
                                   (do* ((lambda-list lambda-list (rest lambda-list))
                                         (param (car lambda-list) (car lambda-list)))
                                        ((or (not lambda-list)
                                             (member param '(lunula::&optional lunula::&rest lunula::&key lunula::&aux
                                                             cl::&optional cl::&rest cl::&key cl::&aux)))
                                         lambda-list)
                                     (setf env (env-extend env param))
                                     (let ((var-name (lisp-to-js-name param)))
                                       (pushnew var-name *declared-vars*
                                                :test 'string=)
                                       (emit-indented-line var-name " = arguments[" n "];"))
                                     (incf n))))
                              (setf arity n)
                              ;; handle optional parameters
                              (when (member (car lambda-list) '(lunula::&optional cl::&optional))
                                (setf arity-op '>=)
                                (do* ((lambda-list (rest lambda-list) (rest lambda-list))
                                      (param (car lambda-list)
                                             (car lambda-list)))
                                     ((or (not lambda-list)
                                          (member param '(lunula::&rest lunula::&key lunula::&aux
                                                          cl::&rest cl::&key cl::&aux)))
                                      lambda-list)
                                  (let* ((param (if (listp param) (car param) param))
                                         (default (if (listp param) (cadr param) nil))
                                         (var-name (lisp-to-js-name param)))
                                    (setf env (env-extend env param))
                                    (pushnew var-name *declared-vars*
                                             :test 'string=)
                                    (emit-indented-line var-name " = arguments.length > " n " ? arguments[" n "] : " (compile-expression default env) ";"))
                                  (incf n)))
                              (when (member (car lambda-list) '(lunula::&rest
                                                                cl::&rest))
                                (setf arity-op '>=)
                                ;; a rest for sucks up all of the remaining arguments into a
                                ;; list
                                (assert (consp (cdr lambda-list)))
                                (let* ((param (car (cdr lambda-list)))
                                       (var-name (lisp-to-js-name param)))
                                  (pushnew var-name *declared-vars*
                                           :test 'string=)
                                  (setf env (env-extend env param))
                                  (emit-indented-line "var $n = " n ";")
                                  (emit-indented-line var-name " = " (compile-expression 'lunula::nil env) ";")
                                  (emit-indented-line "for($n = arguments.length-1; $n >= " n "; $n--) {")
                                  (with-indent
                                    (emit-indented-line var-name " = { type: \"cons\", car: arguments[$n], cdr: " var-name " };"))
                                  (emit-indented-line "}"))
                                ;; advance the lambda list forward after "&rest args"
                                (setf lambda-list (cddr lambda-list)))
                              ;; we'll handle keyword arguments later
                              #+nil(when (member (car lambda-list) '(lunula::&key
                                                                     cl::&key))
                                     ;; keyword args assume that from this point on the arguments
                                     ;; will be key/value pairs
                                     )))))
    (compile-statement `(lunula::assert (,arity-op (lunula::js-inline () "$v = arguments.length;") arity))
                       env)
    (emit compiled-params)
    env))

(defun compile-lambda-expression (form env)
  (let* ((lambda-list (second form))
         (body (cddr form))
         (body-but-last (butlast body))
         (body-last (car (last body)))
         (*param-count* 0))
    (emit-indented-line (value) " = function () {")
    
    (with-indent
      (let* ((*declared-vars* nil)
             (compiled-body (with-output-to-string (*compiler-out*)
                              (setf env (compile-lambda-list lambda-list env))
                              (dolist (form body-but-last)
                                (let ((*param-count* 0))
                                  (compile-statement form env)))
                              (emit-indented-line "return " (compile-expression body-last env) ";"))))
        (emit-var-declarations)
        (emit compiled-body)))
    (emit-indented-line "};")
    (value)))

(defun compile-lambda-statement (form env)
  (declare (ignore form env))
  (emit-indented-line "// a lambda statement is dead code"))

;; JS-LINES

(defun js-lines-p (form)
  (and (listp form)
       (eq 'lunula::js-lines (first form))))

(defun compile-js-lines-statement (form env)
  (declare (ignore env))
  (let ((lines (rest form)))
    (with-indent
      (dolist (line lines)
        (emit-indented-line line)))))

;; JS-INLINE

(defun js-inline-p (form)
  (and (listp form)
       (eq 'lunula::js-inline (first form))))

(defun compile-js-inline-expression (form env)
  (let ((inputs (second form))
        (lines (cddr form))
        (n 1))
    (emit-indented-line (value) " = " (compile-expression 'lunula::nil env) ";")
    (dolist (input inputs)
      (emit-indented-line (param) " = " (compile-expression input env) ";")
      (incf n))
    (dolist (line lines)
      (emit-indented-line line)))
  (value))

(defun compile-js-inline-statement (form env)
  (declare (ignore form env))
  (error "not yet implemented"))

;; MACROS

(defun macro-p (form)
  (and (listp form)
       (symbolp (first form))
       (macro-function (first form))))

;; EVAL-WHEN

(defun eval-when-p (form)
  (and (listp form)
       (or (eq 'lunula::eval-when (first form))
           (eq 'cl:eval-when (first form)))))

(defun compile-eval-when-statement (form env)
  (let ((when (second form))
        (body (cddr form)))
    (when (member :compile-toplevel when)
      (eval `(progn ,@body)))
    (when (member :load-toplevel when)
      (compile-statement `(lunula::progn ,@body) env))
    (when (member :execute when)
      (error "eval-when :execute not supported yet"))))

(defun compile-eval-when-expression (form env)
  (let ((when (second form))
        (body (cddr form)))
    (when (member :compile-toplevel when)
      (eval `(progn ,@body)))
    (when (member :load-toplevel when)
      (compile-expression `(progn ,@body) env))
    (when (member :execute when)
      (error "eval-when :execute not supported yet"))))


;; SETQ

(defun setq-p (form)
  (and (listp form)
       (eq 'lunula::setq (first form))))

(defun compile-setq-statement (form env)
  (let ((things (cdr form)))
    (labels ((_setq (things)
               (cond ((at-least 2 things)
                      (let* ((var (first things))
                             (value (second things))
                             (lookup (env-lookup env var)))
                        (emit-indented-line 
                         (cond (lookup (lisp-to-js-name lookup))
                               (t (global-lookup-value var)))
                         " = "
                         (compile-expression value env)
                         ";"))
                      (_setq (cddr things)))
                     ((exactly 1 things)
                      (error "setq called with an odd number of parameters.")))))
      (_setq things))))

(defun compile-setq-expression (form env)
  (let ((things (cdr form)))
    (compile-setq-statement form env)
    (compile-expression (car (last things)) env)))

;; PROGN

(defun progn-p (form)
  (and (listp form)
       (eq 'lunula::progn (first form))))

(defun compile-progn-expression (form env)
  (let* ((body (rest form))
         (butlast (butlast body))
         (last (car (last body))))
    (dolist (form butlast)
      (compile-statement form env))
    (compile-expression last env)))

(defun compile-progn-statement (form env)
  (let* ((body (rest form)))
    (dolist (form body)
      (compile-statement form env))))

;; STRINGS

(defun compile-string-statement (form env)
  (emit-indented-line "// a string statement is dead code") 
  (emit-indented-line "// " (compile-string-expression form env) ";"))

(defun compile-string-expression (form env)
  (declare (ignore env))
  (concat "\"" form "\""))

;; QUOTE

(defun quote-p (form)
  (and (listp form)
       (or (eq 'lunula::quote (first form))
           (eq 'cl::quote (first form)))))

(defun compile-quote-expression (form env)
  (labels ((compile-thing (thing)
             (cond ((numberp thing) (compile-number-expression thing env))
                   ((symbolp thing) (global-lookup thing))
                   ((consp thing)
                    (concat "{ type: 'cons', car: "
                            (compile-thing (car thing))
                            ", cdr: "
                            (compile-thing (cdr thing))
                            " }"))
                   (t (error "Invalid thing in quoted expression ~s" thing)))))
    (compile-thing (second form))))

;; DEFPACKAGE

(defun defpackage-p (form)
  (and (listp form)
       (eq 'lunula::defpackage (first form))))

(defun compile-defpackage-statement (form env)
  (declare (ignore env))
  (let ((package-name (second form))
        (options (cddr form)))
    (assert (stringp package-name))

    ;; do the compile-time side-effect
    (eval `(defpackage ,package-name ,@options))

    (when options
      ;; we dont need to handle options yet
      (warn "nothing is being done with options: ~A" options))))

;; IN-PACKAGE

(defun in-package-p (form)
  (and (listp form)
       (eq 'lunula::in-package (first form))))

(defun compile-in-package-statement (form env)
  (declare (ignore env))
  (let ((package-name (second form)))
    (assert (stringp package-name))

    ;; do the compile-time side-effect
    (eval `(in-package ,package-name))

    (emit-indented-line (lisp-to-js-name 'lunula::*package*) ".value = Lunula.packages[\""
                        package-name
                        "\"];")))

;; DEFPARAMETER

(defun defparameter-p (form)
  (and (listp form)
       (eq 'lunula::defparameter (first form))))

(defun compile-defparameter-statement (form env)
  (let* ((symbol (second form))
         (value (third form))
         (documentation (fourth form))
         (var-name (lisp-to-js-name symbol)))
    (assert (not (fifth form)))
    (assert (or (null documentation)
                (stringp documentation)))
    (eval `(defparameter ,symbol ,value ,@(list documentation)))

    (pushnew (list symbol var-name)
             *interns*
             :test 'equal)
    
    (emit-indented-line var-name ".value = "
                        (compile-expression value env)
                        ";")
    (when documentation
      (emit-indented-line var-name ".doc = "
                          documentation
                          ";"))))

;; DEFUN

(defun defun-p (form)
  (and (listp form)
       (eq 'lunula::defun (first form))))

(defun compile-defun-statement (form env)
  (let* ((name (second form))
         (params (third form))
         (body (cdddr form))
         (var-name (lisp-to-js-name name)))
    (eval `(defun ,name (,@params)
             ,@body))
    (pushnew (list name var-name)
             *interns*
             :test 'equal)
    (emit-indented-line var-name ".function = "
                        (compile-lambda-expression `(lambda (,@params)
                                                      ,@body)
                                                   env)
                        ";")))


;; ASSERT

(defun assert-p (form)
  (and (listp form)
       (eq 'lunula::assert (first form))))

(defun compile-assert-statement (form env)
  )

(defun compile-assert-expression (form env)
  (compile-assert-statement form env)
  (compile-expression nil env))

;;;; SETF ;;;;
(defun setf-p (form)
  (and (listp form)
       (eq 'lunula::setf (first form))))

(defun compile-setf-statement (form env)
  (emit-indented-line (compile-setf-expression form env) ";"))

(defun compile-setf-expression (form env)
  (labels ((_setf (forms return-value)
             (cond ((null forms) return-value)
                   ((at-least 2 forms)
                    (let ((place (first forms))
                          (value (second forms)))
                      (_setf (cddr forms)
                             (cond ((symbolp place)
                                    (compile-expression `(lunula::setq ,place ,value) env))
                                   ((listp place)
                                    (break)
                                    (compile-expression `(lunula::funcall (lunula::fdefinition '(lunula::setf ,(first place))) ,value ,@(rest place)) env))))))
                   (t (error "Odd number of arguments to SETF")))))
    (_setf (rest form) "")))

;;;; MAIN COMPILER FUNCTIONS ;;;;

(defparameter *compiler-dispatch*
  '( ;; literals
    (symbolp        (compile-symbol-lookup-statement
                     compile-symbol-lookup-expression))
    (numberp        (compile-number-statement
                     compile-number-expression))
    (stringp        (compile-string-statement
                     compile-string-expression))
    ;; COMMON LISP special operators
    (if-p           (compile-if-statement
                     compile-if-expression))
    (lambda-p       (compile-lambda-statement
                     compile-lambda-expression))
    (eval-when-p    (compile-eval-when-statement
                     compile-eval-when-expression))
    (setq-p         (compile-setq-statement
                     compile-setq-expression))
    (progn-p        (compile-progn-statement
                     compile-progn-expression))
    (quote-p        (compile-quote-statement
                     compile-quote-expression))
    ;; javascript FFI special operators
    (js-lines-p     (compile-js-lines-statement
                     compile-js-lines-expression))
    (js-inline-p    (compile-js-inline-statement
                     compile-js-inline-expression))
    ;; handle macros
    (macro-p        ((lambda (form env)
                       (compile-statement (macroexpand-1 form) env))
                     (lambda (form env)
                       (compile-expression (macroexpand-1 form) env))))
    ;; Convenience special forms that make bootstrapping
    ;; easier. They will eventually become macros and once they
    ;; do, will no longer be handled by the special form.
    (setf-p         (compile-setf-statement
                     compile-setf-expression))
    (assert-p       (compile-assert-statement
                     compile-assert-expression))
    (defpackage-p   (compile-defpackage-statement
                     compile-defpackage-expression))
    (in-package-p   (compile-in-package-statement
                     compile-in-package-expression))
    (defparameter-p (compile-defparameter-statement
                     compile-defparameter-expression))
    (defun-p        (compile-defun-statement
                     compile-defun-expression))
    ;; anything else assumes call
    (listp          (compile-call-statement
                     compile-call-expression))))


(defun compiler-dispatch (type form env)
  (let ((dispatch (find-if (lambda (pred)
                             (funcall pred form))
                           *compiler-dispatch* :key 'first)))
    (if dispatch
        (funcall (funcall (cond ((eq type 'statement)
                                 'first)
                                ((eq type 'expression)
                                 'second)
                                (t (error "invalid compiler dispatch type")))
                          (second dispatch))
                 form env)
        (error "don't know how to compile ~S" form))))

(defun compile-expression (form env)
  (compiler-dispatch 'expression form env))

(defun compile-statement (form env)
  (compiler-dispatch 'statement form env))

(defun compile-stream (in out)
  (let ((*package* *package*)
        (*compiler-out* out)
        (*interns* nil)
        (*literal-lookups* nil)
        (env nil))
    (emit-line "(function () {")          
    ;; Read each toplevel form and compile it
    (with-indent            
      (let ((compiled-forms
             (with-output-to-string (*compiler-out*)
               (do ((form (read in nil nil) (read in nil nil)))
                   ((null form))          
                 ;;(format *error-output* "Compiling form:~%~A~%~%" form)
                 (emit-commented form)
                 (compile-statement form env)))))
        (emit-comment-line "intern all symbols")
        (emit-interns)
        (emit-var-declarations)
        (emit compiled-forms)))          
    (emit-line "})();")))


(defun @compile-file (input-file)
  (with-open-file (in input-file :direction :input)
    (let ((output-file (make-pathname :type "js"
                                      :defaults input-file)))
      (with-open-file (out output-file
                           :direction :output
                           :if-exists :supersede)
        (compile-stream in out))
      output-file)))


;;;; other stuff ;;;;

(defparameter *cl-symbols*
  (let ((cl-symbols nil))
    (do-symbols (sym :common-lisp)
      (push (symbol-name sym) cl-symbols))
    cl-symbols))

(defun cl-symbols-used (form)
  (let ((symbols nil))
    (labels ((symbols-used (form)
               (cond ((symbolp form)
                      (pushnew (symbol-name form) symbols
                               :test 'string=))
                     ((consp form)
                      (symbols-used (car form))
                      (symbols-used (cdr form))))))
      (symbols-used form))
    (intersection symbols *cl-symbols* :test 'string=)))

(defun cl-symbols-used-in-file (file)
  (let ((symbols nil))
    (with-open-file (in file :direction :input)
      (do ((form (read in nil nil) (read in nil nil)))
          ((null form))
        (setf symbols (append symbols
                              (cl-symbols-used form)))))
    (remove-duplicates symbols :test 'string=)))
