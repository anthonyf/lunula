(in-package :lunula)

(defparameter *toplevel-literals* nil)

(defun compile-symbol-expression (form env)
  (let ((lookup (env-lookup env form)))
    (cond (lookup (env-emitted-name lookup))
          (t (global-lookup-value form)))))

;; COMPILE SYMBOL
(defun compile-symbol-statement (form env)
  (emit-indented-line "// a symbol statement is dead code") 
  (emit-indented-line "// " (compile-symbol-expression form env) ";"))

(defstruct (compiler-dispatch-entry (:type list))
  predicate
  compile-statement-function
  compile-expression-function)

;; COMPILE NUMBER
(defun compile-number-statement (form env)
  (emit-indented-line "// a number statement is dead code") 
  (emit-indented-line "// " (compile-number-expression form env) ";"))

(defun compile-number-expression (form env)
  (declare (ignore env))
  form)

;; STRINGS
(defun compile-string-statement (form env)
  (emit-indented-line "// a string statement is dead code") 
  (emit-indented-line "// " (compile-string-expression form env) ";"))

(defun compile-string-expression (form env)
  (declare (ignore env))
  (emit-concat "\"" form "\""))

;; JS-LINES

(defun js-lines-p (form)
  (and (listp form)
       (eq 'js-lines (first form))))

(defun compile-js-lines-statement (form env)
  (declare (ignore env))
  (let ((lines (rest form)))
    (with-indent
      (dolist (line lines)
        (emit-indented-line line)))))

;; JS-INLINE

(defun js-inline-p (form)
  (and (listp form)
       (eq 'js-inline (first form))))

(defun compile-js-inline-expression (form env)
  (let ((inputs (second form))
        (lines (cddr form))
        (n 1))
    (emit-indented-line (value) " = " (compile-expression 'nil env) ";")
    (dolist (input inputs)
      (emit-indented-line (param) " = " (compile-expression input env) ";")
      (incf n))
    (dolist (line lines)
      (emit-indented-line line)))
  (value))

(defun compile-js-inline-statement (form env)
  (declare (ignore form env))
  (error "not yet implemented"))


;; COMPILE LAMBDA
(defun lambda-p (form)
  (and (listp form)
       (eq 'lambda (first form))))

(defun value ()
  (pushnew "$v" *declared-vars*
           :test 'string=) 
  "$v")

(defparameter *declared-vars* nil
  "This variable holds the javascript vars that need to be declared at
  each function scope level.")

(defun emit-var-declarations ()
  "Emit javascript function level vars."
  (when *declared-vars*
    (let ((*declared-vars* (copy-list (cl:sort *declared-vars* #'cl:string<))))
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
         (compiled-params (cl:with-output-to-string (*compiler-out*)
                            ;; compile normal arguments up until &optional, &rest, &key or
                            ;; &aux
                            (let ((lambda-list
                                   (do* ((lambda-list lambda-list (rest lambda-list))
                                         (param (car lambda-list) (car lambda-list)))
                                        ((or (not lambda-list)
                                             (member param '(&optional &rest &key &aux
                                                             cl::&optional cl::&rest cl::&key cl::&aux)))
                                         lambda-list)
                                     (setf env (env-extend env param))
                                     (let ((var-name (env-emitted-name (env-lookup env param))))
                                       (pushnew var-name *declared-vars*
                                                :test 'string=)
                                       (emit-indented-line var-name " = arguments[" n "];"))
                                     (incf n))))
                              (setf arity n)
                              ;; handle optional parameters
                              (when (member (car lambda-list) '(&optional cl::&optional))
                                (setf arity-op '>=)
                                (do* ((lambda-list (rest lambda-list) (rest lambda-list))
                                      (param (car lambda-list)
                                             (car lambda-list)))
                                     ((or (not lambda-list)
                                          (member param '(&rest &key &aux
                                                          cl::&rest cl::&key cl::&aux)))
                                      lambda-list)
                                  (let* ((param (if (listp param) (car param) param))
                                         (default (if (listp param) (cadr param) nil))
                                         (var-name (env-emitted-name (env-lookup env param))))
                                    (setf env (env-extend env param))
                                    (pushnew var-name *declared-vars*
                                             :test 'string=)
                                    (emit-indented-line var-name " = arguments.length > " n " ? arguments[" n "] : " (compile-expression default env) ";"))
                                  (incf n)))
                              (when (member (car lambda-list) '(&rest
                                                                cl::&rest))
                                (setf arity-op '>=)
                                ;; a rest for sucks up all of the remaining arguments into a
                                ;; list
                                (assert (consp (cdr lambda-list)))
                                (let* ((param (car (cdr lambda-list)))
                                       (var-name (env-emitted-name (env-lookup env param))))
                                  (pushnew var-name *declared-vars*
                                           :test 'string=)
                                  (setf env (env-extend env param))
                                  (emit-indented-line "var $n = " n ";")
                                  (emit-indented-line var-name " = " (compile-expression 'nil env) ";")
                                  (emit-indented-line "for($n = arguments.length-1; $n >= " n "; $n--) {")
                                  (with-indent
                                      (emit-indented-line var-name " = { type: \"cons\", car: arguments[$n], cdr: " var-name " };"))
                                  (emit-indented-line "}"))
                                ;; advance the lambda list forward after "&rest args"
                                (setf lambda-list (cddr lambda-list)))
                              ;; we'll handle keyword arguments later
                              #+nil(when (member (car lambda-list) '(&key
                                                                     cl::&key))
                                     ;; keyword args assume that from this point on the arguments
                                     ;; will be key/value pairs
                                     )))))
    (compile-statement `(assert (,arity-op (js-inline () "$v = arguments.length;") ,arity))
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
             (compiled-body (cl:with-output-to-string (*compiler-out*)
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

;; COMPILE CALL

(defparameter *param-count* 0)

(defun param ()  
  (incf *param-count*)
  (let ((param-name (concatenate 'string
                                 "$p"
                                 (cl:princ-to-string *param-count*))))
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
      (emit-concat (if (equal "$v" fun-name)
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

;; COMPILER DISPATCH
(defparameter *compiler-dispatch*
  `((symbolp     compile-symbol-statement    compile-symbol-expression)
    (numberp     compile-number-statement    compile-number-expression)
    (stringp     compile-string-statement    compile-string-expression)
    (lambda-p    compile-lambda-statement    compile-lambda-expression)
    (js-inline-p compile-js-inline-statement compile-js-inline-expression)
    (js-lines-p  compile-js-lines-statement  compile-js-lines-expression)
    ;; anything else assumes call
    (listp   compile-call-statement   compile-call-expression)))

(defun compiler-dispatch (type form env)
  (let ((dispatch (cl:find-if (lambda (pred)
                                (funcall pred form))
                              *compiler-dispatch* :key 'first)))
    (if dispatch
        (funcall (funcall (cond ((eq type 'statement)
                                 'compiler-dispatch-entry-compile-statement-function)
                                ((eq type 'expression)
                                 'compiler-dispatch-entry-compile-expression-function)
                                (t (error "invalid compiler dispatch type")))
                          dispatch)
                 form env)
        (error "don't know how to compile ~S" form))))

(defun compile-expression (form env)
  (compiler-dispatch 'expression form env))

(defun compile-statement (form env)
  (compiler-dispatch 'statement form env))

(defun compile-stream (input-stream output-stream)
  (let ((*package* *package*)
        (*compiler-out* output-stream)
        (*toplevel-interns* nil)
        (*toplevel-literals* nil)
        (*unique-var-name-counter* 0)
        (env nil))
    (emit-line "(function () {")          
    ;; Read each toplevel form and compile it
    (with-indent            
        (let ((compiled-forms
               (cl:with-output-to-string (*compiler-out*)
                 (cl:do ((form (cl:read input-stream nil nil)
                            (cl:read input-stream nil nil)))
                     ((null form))          
                   (cl:format cl:*error-output* "Compiling form:~%~A~%~%" form)
                   ;; TODO (emit-commented form)
                   (compile-statement form env)))))
          (emit-comment-line "intern all symbols")
          ;; TODO (emit-interns)
          ;; TODO (emit-var-declarations)
          (emit compiled-forms)
          ))          
    (emit-line "})();")))

(defun compile-string (str)
  (cl:with-input-from-string (in str)
    (cl:with-output-to-string (out)
      (compile-stream in out))))
