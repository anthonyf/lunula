(in-package :lunula)

;; Finally we can define DEFMACRO in terms of everything we defined
;; before this point.  This definition of DEFMACRO relies on LAMBDA
;; for handling lambda lists.  A better version would not use LAMBDA
;; but instead do the destructuring programmatically.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun expand-macro (args-name lambda-list body)
    (if (null lambda-list)
        `(progn (cl:when ,args-name
                  (error "to many arguments passed to macro"))
                ,@body)
        ;; if the first item in the lambda list is a list, then we
        ;; treat it as its own lambda list and let lambda handle it
        (if (listp (first lambda-list))
            `(apply (lambda (,args-name ,@(first lambda-list))
                      ,(expand-macro args-name (rest lambda-list) body))
                    (rest ,args-name) (first ,args-name))
            ;; if the lambda list starts with &rest or &optional, &key or
            ;; &body just treat the rest of the lambda list as usual
            (if (member (first lambda-list) '(&rest &optional &key &body))
                ;; lambda doesnt know what &body is, so convert it to &rest
                (let ((body-pos (position '&body lambda-list)))
                  (cl:when body-pos
                    (setf (nth body-pos lambda-list) '&rest))
                  `(apply (lambda (,@lambda-list)
                            ,@body)
                          ,args-name))
                `(apply (lambda (,args-name ,(first lambda-list))
                          ,(expand-macro args-name (rest lambda-list) body))
                        (rest ,args-name) (first ,args-name) nil)))))

  (setf (macro-function 'defmacro)
        (lambda (whole env)
          (declare (ignore env))
          (let ((macro-name (cadr whole))
                (lambda-list (caddr whole))
                (body (cdddr whole))
                (args (gensym "ARGS")))
            `(eval-when (:compile-toplevel :load-toplevel :execute)
               (setf (macro-function ',macro-name)
                     (lambda (whole env)
                       (declare (ignore env))
                       (let ((,args (rest whole)))
                         ,(expand-macro args
                                        lambda-list
                                        body)))))))))

(defun macroexpand-1 (form &optional env)
  (if (macro-function (car form))
      (values (funcall (macro-function (car form)) form env)
              t)
      (values form nil)))
