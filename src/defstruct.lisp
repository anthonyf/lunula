(in-package :lunula)

(defmacro defstruct (name-and-options &rest slot-descriptions)
  (let ((name (cond ((symbolp name-and-options)
                     name-and-options)
                    ((listp name-and-options)
                     (first name-and-options))
                    (t (error "")))))))
