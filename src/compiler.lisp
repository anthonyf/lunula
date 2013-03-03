(in-package :lunula)

(defun compile-stream (input-stream output-stream)
  (let ((*package* *package*))
    ))

(defun compile-string (str)
  (with-input-from-string (in str)
    (cl:with-output-to-string (out)
      (compile-stream in out))))
