(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun expand-macro (lambda-list body)
    (cond ((null lambda-list)
           `(progn (unless (null args)
                     (error "to many arguments passed to macro"))
                   ,body))
          ;; if the first item in the lambda list is a list, then we
          ;; treat it as its own lambda list and let lambda handle it
          ((listp (first lambda-list))
           `(apply (lambda (args ,@(first lambda-list))
                     ,(expand-macro (rest lambda-list) body))
                   (rest args) (first args)))
          ;; if the lambda list starts with &rest or &optional, &key or
          ;; &body just treat the rest of the lambda list as usual
          ((member (first lambda-list) '(&rest &optional &key &body))
           ;; lambda doesnt know what &body is, so convert it to &rest
           (let ((&body-pos (position '&body lambda-list)))
             (when &body-pos
               (setf (nth &body-pos lambda-list) '&rest))
             `(apply (lambda (,@lambda-list)
                       ,body)
                     args)))
          (t 
           `(apply (lambda (args ,(first lambda-list))             
                     ,(expand-macro (rest lambda-list) body))
                   (rest args) (first args) nil))))

  (setf (macro-function '@defmacro)
        (lambda (whole env)
          (declare (ignore env))
          (let ((macro-name (cadr whole))
                (lambda-list (caddr whole))
                (body (cadddr whole)))
            `(setf (macro-function ',macro-name)
                   (lambda (whole env)
                     (declare (ignore env))
                     (let ((args (rest whole)))
                       ,(expand-macro lambda-list
                                      body))))))))



(@defmacro test ((a b c) g (d e f))
  `(+ ,a ,b ,c ,d ,e ,f ,g))


(@defmacro @if (test-form then &optional else)
  `(if ,test-form
       ,then
       ,else))
