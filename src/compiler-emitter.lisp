(in-package :lunula)

(defparameter *compiler-out* *standard-output*)
(defparameter *indent-spaces* 2)
(defparameter *indent-level* 0)

(defun emit-indent ()
  (dotimes (n (* *indent-spaces*
                 *indent-level*))
    (cl:princ #\space *compiler-out*)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-indent (&body body)
    `(let ((*indent-level* (1+ *indent-level*)))
       ,@body)))

(defun emit-delimited (delimiter list
                       &key (item-printer (lambda (x)
                                            (cl:princ x *compiler-out*))))
  (do ((list list (cdr list)))
      ((not list))
    (funcall item-printer (car list))
    (when (cdr list)
      (cl:princ delimiter *compiler-out*))))

(defun emit-comment-line (str)
  (emit-indented-line "// " str))

(defun princ-to-string-delimited (delimeter list)
  (cl:with-output-to-string (*compiler-out*)
    (emit-delimited delimeter list)))

(defun emit (&rest args)
  (dolist (arg args)
    (cond ((listp arg) (apply #'emit arg))
          (t (cl:princ arg *compiler-out*)))))

(defun emit-indented (&rest args)
  (emit-indent)
  (emit args))

(defun emit-line (&rest args)
  (emit args)
  (cl:terpri *compiler-out*))

(defun emit-indented-line (&rest args)
  (emit-indent)
  (emit-line args))

(defun emit-commented (thing)
  (with-input-from-string (in (cl:with-output-to-string (out)
                                (cl:pprint thing out)))
    (do ((n 1 (1+ n))
         (line (cl:read-line in nil nil)
               (cl:read-line in nil nil)))
        ((not line))
      (cond ((and (= n 1)
                  (string-equal "" line))
             (emit-line ""))
            (t (emit-indented-line "// " line))))))

(defun emit-concat (&rest args)
  (cl:with-output-to-string (*compiler-out*)
    (emit args)))
