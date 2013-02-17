(in-package :lunula)

(defun throw-eof-error ()
  ;; TODO: replace this with a real EOF error at some point
  (error "end of file!"))

(defun throw-reader-error ()
  ;; TODO: replace this with a real reader error
  (error "reader error"))
