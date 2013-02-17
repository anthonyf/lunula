(in-package :lunula)

(defmacro multiple-value-bind ((&rest vars) values-form &rest body)
  ;; TODO: handle declare form
  (cond ((null vars)
         `(progn
            ,values-form
            ,@body))
        (t (let ((rest (gensym)))
             `(multiple-value-call #'(lambda (&optional ,@vars &rest ,rest)
                                       (declare (ignore ,rest))
                                       ,@body)
                ,values-form)))))

(defmacro multiple-value-list (form)
  `(multiple-value-call #'list ,form))

(defun values-list (list)
  (apply #'values list))
