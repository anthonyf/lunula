(in-package :lunula)

(defstruct (env (:type list))
  symbol
  emitted-name)

(defvar *unique-var-name-counter* 0)

(defun find-unique-emitted-name (env var)
  (declare (ignore env var))
  (concatenate 'string "l" (integer-to-string *unique-var-name-counter*)))

(defmacro env-extend (env var)
  (let ((var-name (gensym)))
    `(let ((,var-name ,var))
       (push (make-env :symbol ,var-name
                       :emitted-name (find-unique-emitted-name ,env ,var-name))
             ,env))))

(defun env-lookup (env var)
  (cl:find var env :key #'env-symbol))

(defmacro with-env (env &body body)
  `(let ((,env ,env))
     ,@body
     nil))

