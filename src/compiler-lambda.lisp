(in-package :lunula)


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
