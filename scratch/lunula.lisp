#|
Lunula

Resources:

* Implementing special forms in terms of other special forms --
http://www.pipeline.com/~hbaker1/MetaCircular.html

* Common Lisp special forms
http://www.lispworks.com/documentation/lw50/CLHS/Body/03_ababa.htm

* Implementing Lisp: http://c2.com/cgi/wiki?ImplementingLisp

* The Common Lisp reader algorithm:
http://www.lispworks.com/documentation/lw60/CLHS/Body/02_b.htm

* Portable Common Loops - An implementation of CLOS:
http://free-compilers.sharnoff.org/TOOL/CommonLi-2.html

* Implementing Lisp:
http://www.mlb.co.jp/linux/science/yacas/documentation/LispProgrammingchapter3.html

* Tail call optimization in javascript:  http://w3future.com/weblog/2006/02/#tailCallEliminationInJavascript
|#

(cl:defpackage "LUNULA"
  ;; Import special operators and special symbols which never get set.
  ;; Everything else gets referenced explicitly.
  (:import-from :common-lisp
                #:&optional  #:&key                  #:&body
                #:&rest      #:declare               #:ignore
                #:block      #:let*                  #:return-from      
                #:catch      #:load-time-value       #:setq             
                #:eval-when  #:locally               #:symbol-macrolet  
                #:flet       #:macrolet              #:tagbody          
                #:function   #:multiple-value-call   #:the              
                #:go         #:multiple-value-prog1  #:throw            
                #:if         #:progn                 #:unwind-protect   
                #:labels     #:progv                 #:let
                #:quote))

(cl:in-package "LUNULA")

(eval-when (:compile-toplevel :load-toplevel)
  ;; T, NIL and equality
  (cl:defparameter nil 'cl:nil)
  (cl:defparameter t 'cl:t)

  (cl:defparameter *package* nil)

  (cl:defun eq (a b)
    #-xc(cl:eq a b)
    #+xc(js-inline (a b t nil)
                   "$v = $p1.value === $p2.value ? $p3 : $p4;"))

  (cl:defun null (thing)
    (eq nil thing))

  (cl:defun 2-arg-+ (a b)
    #-xc (cl:+ a b)
    #+xc(js-inline (a b)
                   "$v = $p1 + $p2;"))

  (cl:defun 2-arg-* (a b)
    #-xc (cl:* a b)
    #+xc(js-inline (a b)
                   "$v = $p1 + $p2;"))

  (cl:defun cons (a b)
    #-xc (cl:cons a b)
    #+xc (js-inline (a b)
                    "$v = { type: \"cons\","
                    "       car: $p1,"
                    "       cdr: $p2 };"))

  (cl:defun consp (x)
    #-xc (cl:consp x)
    #+xc(js-inline (x t nil)
                   "$v = (typeof ($p1) === 'object' && $p1.type === 'cons') ? $p2 : $p3;"))

  (cl:defun car (x)
    #-xc (cl:car x)
    #+xc (if (consp x)
             (js-inline (x) "$v = $p1.car;")
             (if (null x)
                 nil
                 (error "Cannot take car of ~A" x))))

  (cl:defun cdr (x)
    #-xc (cl:cdr x)
    #+xc (if (consp x)
             (js-inline (x) "$v = $p1.cdr;")
             (if (null x)
                 nil
                 (error "Cannot take cdr of ~A" x))))

  (cl:defun cddr (x)
    (cdr (cdr x)))

  (cl:defun list (&rest args)
    args)

  (cl:defun list-to-js-array (list)
    (declare (ignore list))
    (cl:error "not yet implemented"))

  (cl:defun list* (&rest args)
    (if (null args)
        nil
        (if (null (cdr args))
            (car args)
            (cons (car args) (apply #'list* (cdr args))))))

  (cl:defun apply (function &rest args)
    #-xc (cl:apply function
                   (cl:apply #'list* args))
    #+xc (js-inline (function (list-to-js-array (apply #'list* args)))
                    "$v = $p1.apply(null, $p2);"))

  (cl:defun funcall (function &rest args)
    (apply function args))

  (cl:defun reduce (f lst &key initial-value)
    (if (null lst)
        initial-value
        (if (null initial-value)
            (reduce f (cdr lst)
                    :initial-value (car lst))
            (reduce f (cdr lst)
                    :initial-value (funcall f initial-value (car lst))))))

  (cl:defun + (&rest args)
    (reduce  '2-arg-+ args :initial-value 0))

  (cl:defun * (&rest args)
    (reduce '2-arg-* args :initial-value 1))

  (cl:defun identity (thing)
    thing)

  (cl:defun eql (a b)
    ;; TODO: This is probably not implemented correctly
    (eq a b))

  (cl:defun member (item list &key (key #'identity) (test #'eql))
    (if (null list)
        nil
        (if (funcall test item (funcall key (car list)))
            list
            (member item (cdr list) :key key :test test))))

  (cl:defun first (list)
    (car list ))

  (cl:defun second (list)
    (car (cdr list)))

  (cl:defun rest (list)
    (cdr list))

  (cl:defun listp (thing)
    (cl:or (null thing)
           (consp thing)))

  (cl:defun mapcar (function list)
    (if (null list)
        nil
        (cons (funcall function (car list))
              (mapcar function (cdr list)))))

  (cl:defun macro-function (symbol &optional env)
    #-xc (cl:macro-function symbol env)
    #+xc (js-inline (symbol)
                    "$v = $p1.macroFunction;"))

  (cl:defun (cl:setf macro-function) (value symbol &optional env)
    #-xc (cl:setf (cl:macro-function symbol env) value)
    #+xc (js-inline (symbol value)
                    "$p1.macroFunction = value;"
                    "$v = $v2;")))

(cl:defmacro defmacro (name lambda-list &rest body)
  (labels ((destructure-lambda-list (lambda-list)
             (if (null lambda-list)
                 `(progn (if (null args)
                             nil
                             (error "args are null"))
                         ,@body)
                 (if (member (first lambda-list) '(&key &optional &rest &body))
                     `(apply #'(cl:lambda ,(mapcar (cl:lambda (s)
                                                     (if (eq s '&body)
                                                         '&rest
                                                         s))
                                                   lambda-list)
                                 ,@body)
                             args)
                     (if (listp (first lambda-list))
                         `(apply #'(cl:lambda (,@(first lambda-list))
                                     (let ((args (rest args)))
                                       ,(destructure-lambda-list (rest lambda-list))))
                                 (first args))
                         `(apply #'(cl:lambda (,(first lambda-list))
                                     (let ((args (rest args)))
                                       ,(destructure-lambda-list (rest lambda-list))))
                                 (list (first args))))))))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (cl:setf (macro-function ',name)
                #'(cl:lambda (form &optional env)
                    (declare (ignore env))
                    (let ((args (rest form)))
                      ,(destructure-lambda-list lambda-list)))))))

(eval-when (:compile-toplevel :load-toplevel)
  (cl:defun 1- (n)
    #-xc (cl:1- n)
    #+xc (js-inline (n)
                    "$v = $p1 - 1"))

  ;; TODO: recursive method
  (cl:defun nthcdr (n list)
    (if (null list)
        nil
        (if (eq n 0)
            list
            (nthcdr (1- n) (cdr list)))))
  (cl:defun at-least (n list)
    (consp (nthcdr (1- n) list)))
  (cl:defun not (thing)
    (if thing nil t))

  (cl:defun exactly (n list)
    (let ((nthcdr (nthcdr (1- n) list)))
      (cl:and (consp nthcdr)
              (not (consp (cdr nthcdr))))))

  (cl:defun error (datum &rest args)
    #-xc (funcall #'cl:error datum args)
    #+xc (js-inline (nil datum (list-to-js-array args))
                    throw $p2           ;
                    "$v = $p1;"))

  (cl:defun fdefinition (function-name)
    #-xc (cl:fdefinition function-name)
    #+xc (js-inline (function-name)
                    "$v = $p1.function;"))

  (cl:defun stringp (thing)
    #-xc (cl:stringp thing)
    #+xc (js-inline (thing t nil)
                    "$v = (typeof ($p1) === 'string' || (typeof ($p1) === 'object' && $p1.type === 'string')) ? $p2 : $p3;"))

  (cl:defun minusp (thing)
    #-xc (cl:minusp thing)
    #+xc (js-inline (thing t nil)
                    "$v = $p1 < 0;"))

  (cl:defun integerp (thing)
    #-xc (cl:integerp thing)
    #+xc (js-inline (thing t nil)
                    "$v = (typeof $p1 === 'number' && $p1 % 1 == 0) ? $p2 : $p3;"))

  (cl:defun make-symbol (string)
    #-xc (cl:make-symbol string)
    #+xc (js-inline (string)
                    "$v = { type: \"symbol\", name: $p1 };"))

  (cl:defun concatenate-strings (&rest strings)
    #-xc (apply #'cl:concatenate 'cl:string strings)
    #+xc (js-inline ((list-to-js-array strings))
                    "$v = $p1.join('');"))

  (cl:defun to-string (thing)
    #-xc (cl:write-to-string thing)
    #+xc (js-inline (thing)
                    "$v = $p1.toString();"))

  (let ((gensym-prefix "G")
        (gensym-count 1))
    (cl:defun gensym (&optional (x nil s))
      (if s
          (if (stringp x)
              (setq gensym-prefix x)
              (if (integerp x)
                  (if (minusp x)
                      (error "~S: index ~S is negative" 'gensym x)
                      (setq gensym-count x))
                  (error "~S: argument ~S of wrong type" 'gensym x))))
      (let ((symbol (make-symbol
                     (concatenate-strings gensym-prefix
                                          (to-string gensym-count))))) 
        (cl:setf gensym-count (+ 1 gensym-count))
        symbol)))

  (defmacro or (&rest forms)
    (if forms
        (let ((first-form (gensym)))
          `(let ((,first-form ,(first forms)))
             (if ,first-form
                 ,first-form
                 (or ,@(rest forms)))))
        nil))

  (defmacro and (&rest forms)
    (if (null forms)
        t
        (if (null (cdr forms))
            (car forms)
            `(if ,(car forms)
                 (and ,@(cdr forms))))))

  (cl:defun (cl:setf fdefinition) (value function-name)
    (if (or (symbolp function-name)
            (and (listp function-name)
                 (eq (first function-name) 'cl:setf)))
        nil
        (error "Invalid function name ~A" function-name))
    #-xc (cl:setf (cl:fdefinition function-name) value)
    #+xc (js-inline (function-name value)
                    "$p1.function = value;"
                    "$v = $v2;"))

  (cl:defun symbolp (thing)
    #-xc (cl:symbolp thing)
    #+xc (js-inline (thing t nil)
                    "$v = (typeof ($p1) === 'object' && $p1.type === 'symbol') ? $p2 : $p3;")))
  
(defmacro setf (&rest args)
  (cl:labels ((_setf (forms)
                (if (null forms)
                    nil
                    (if (at-least 2 forms)
                        (let ((place (first forms))
                              (value (second forms)))
                          (if (listp place)
                              `((funcall (fdefinition '(cl:setf ,(first place)))
                                         ,value ,@(rest place)))
                              `((cl:setq ,place ,value)
                                ,@(_setf (cddr forms)))))
                        (error "Odd number of arguments to SETF")))))
    `(progn ,@(_setf args))))

(defmacro defun (name (&rest params) &body body)
  (let ((doc-string)
        (declaration))
    ;; TODO: handle more than one declare statement
    (if (and (listp (first body))
             (eq 'declare (first (first body))))

        (progn (setf declaration (first body))
               (setf body (rest body))))
    (if (stringp (first body))
        (progn (setf doc-string (first body))
               (setf body (rest body))))
    `(progn
       ;; forward declaration (to shush warnings
       (eval-when (:compile-toplevel)
         (setf (fdefinition ',name)
               (cl:lambda (&rest args)
                 (declare (ignore args)))))
       ;; the actual function declaration
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (fdefinition ',name)
               (cl:lambda (,@params)
                 ,@body))))))

(defun (cl:setf car) (value place)
  (cl:assert (consp place))
  #-xc (cl:setf (cl:car place) value)
  #+xc (js-inline (value place)
                  "$v = $p2.car = $p1;"))

(defun (cl:setf cdr) (value place)
  (cl:assert (consp place))
  #-xc (cl:setf (cl:cdr place) value)
  #+xc (js-inline (value place)
                  "$v = $p2.cdr = $p1;"))

(defmacro when (predicate &body body)
  `(if ,predicate
       (progn ,@body)))

(defmacro unless (predicate &body body)
  `(if ,predicate
       nil
       (progn ,@body)))

(defmacro cond (&rest clauses)
  (when clauses
    (let ((test1 (car (car clauses)))
          (forms1 (cdr (car clauses))))
      (if forms1
          `(if ,test1
               (progn ,@forms1)
               (cond ,@(cdr clauses)))
          (let ((tmp (gensym)))
            `(let ((,tmp ,test1))
               (if ,tmp
                   ,tmp
                   (cond ,@(cdr clauses)))))))))

(defun numberp (thing)
  #-xc (cl:numberp thing)
  #+xc (js-inline (thing)
                  "$v = 'number' === typeof($p1);"))

(defun zerop (number)
  (if (numberp number)
      nil
      (error "Non-number passed to ZEROP"))
  #-xc (cl:zerop number)
  #+xc (js-inline (number)
                  "$v = $p1 === 0;"))

(cl:defun 2-arg-- (a b)
  #-xc (cl:- a b)
  #+xc(js-inline (a b)
                 "$v = $p1 - $p2;"))

(defun - (&rest args)
  (reduce  '2-arg-- args :initial-value 0))

;; TODO: recursive method
(defun nth (n list)
  (if (zerop n)
      (car list)
      (nth (- n 1) (cdr list))))

#+nil(defmacro while (test do &body body)
  "Execute body while the test is true."
  (unless (eq do 'do)
    (error "malformed WHILE"))
  `(do () ((not ,test) nil) ,@body))

#+nil(defun find (item sequence
                  &key from-end (test #'eql) test-not (start 0) end key)
  "Return the first element in SEQUENCE satisfying TEST or TEST-NOT."
  (when test-not (setq test (complement test-not)))
  (find-if #'(cl:lambda (arg) (funcall test item arg))
           sequence :from-end from-end :start start :end end :key key))

;;;; COMPILER ;;;;;


;; ENVIRONMENT ;;

(defun env-extend (env var)
  (cons var env))

#+nil(defun env-lookup (env var)
  (find var env))

#+nil(defmacro assert ()
  ())
#+nil (defmacro defparameter ())
#+nil (defmacro defvar ())

;; look here for implementation examples:
;; http://homepage1.nifty.com/bmonkey/lisp/sacla/html/lisp/data-and-control.lisp.html
;; http://homepage1.nifty.com/bmonkey/lisp/sacla/html/lisp/do.lisp.htlm