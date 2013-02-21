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

(defun eat-whitespace (stream)
  (loop
     (let ((char (peek-char nil stream nil nil)))
       (cond ((null char)
              (values))
             ((whitespacep char)
              (read-char stream))
             (t (values))))))

(defun list-reader (stream char)
  (declare (ignore char))
  (let ((list nil))
    (loop
       (eat-whitespace stream)
       (let ((char (peek-char nil stream nil nil)))
         (cond ((null char)
                (error "EOF read before closing paren"))
               ((char= #\) char)  (return list))
               ((char= #\. char)
                (error "DOT not supported yet"))
               (t (push (read stream) list)))))))

(set-macro-character #\( #'list-reader)
