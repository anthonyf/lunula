(in-package :lunula)

(defparameter *toplevel-literals* nil)

(defstruct (compiler-dispatch-entry (:type list))
  predicate
  compile-statement-function
  compile-expression-function)

(defun compile-symbol-expression (form env)
  (let ((lookup (env-lookup env form)))
    (cond (lookup (env-emitted-name lookup))
          (t (global-lookup-value form)))))

(defun compile-symbol-statement (form env)
  (emit-indented-line "// a symbol statement is dead code") 
  (emit-indented-line "// " (compile-symbol-expression form env) ";"))

(defparameter *compiler-dispatch*
  `((symbolp compile-symbol-statement compile-symbol-expression)
    (numberp compile-number-statement compile-number-expression)))

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
