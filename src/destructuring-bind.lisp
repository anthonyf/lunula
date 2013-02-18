(in-package :lunula)

(defmacro destructuring-bind (lambda-list expression &body body)
  `(apply (lambda ,lambda-list
            ,@body)
          ,expression))
