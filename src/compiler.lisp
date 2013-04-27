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
