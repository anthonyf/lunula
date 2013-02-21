(in-package :lunula)

(defun single-quote-reader (stream char)
  (declare (ignore char))
  (list 'quote (read stream t nil t)))

(set-macro-character #\' #'single-quote-reader)

(defun semicolon-reader (stream char)
  (declare (ignore char))
  (loop
     (when (char= (read-char stream nil #\Newline t)
                  #\Newline)
       (return)))
  ;; Return zero values.
  (values))

(set-macro-character #\; #'semicolon-reader)
