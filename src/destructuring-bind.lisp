(in-package :lunula)

;; stolen from CLTL2
(defmacro destructuring-bind (lambda-list list &body body)
  "Bind the variables in lambda-list to the result list and execute body."
  ;; This implementation does not do the defmacro extensions,
  ;; Except that it does handle a trailing dot: (x y . z)
  (cond ((null lambda-list)
         `(progn ,@body))
        ((not (symbolp list))
         (let ((var (gensym)))
           `(let ((,var ,list))
              (destructuring-bind ,lambda-list ,var ,@body))))
        ((symbolp lambda-list)
         `(let ((,lambda-list ,list)) ,@body))
        ((atom lambda-list)
         (error "Can't bind ~A to a value." lambda-list))
        ((member (first lambda-list) '(&rest &optional &key &aux))
         `(apply #'(lambda ,lambda-list ,@body) ,list))
        (t `(destructuring-bind ,(first lambda-list) (first ,list)
              (destructuring-bind ,(rest lambda-list) (rest ,list)
                ,@body)))))
