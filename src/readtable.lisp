(in-package :lunula)

(defun make-default-read-table ()
  ;; TODO: this is fragile.  Use a closure or something instead of a
  ;; list to represent the readtable object.
  (list
   nil     ;; macro characters
   :upcase ;; readtable-case
   ))

(defun readtable-case (read-table)
  (cadr read-table))

(defvar *read-table* (make-default-read-table))

(defun set-macro-character (char function &optional non-terminating-p (read-table *read-table*))
  (let ((loc (member char (first read-table) :key #'car)))
    (cond (loc
           (setf (cdr (car loc)) (list function
                                       non-terminating-p)))
          (t
           (setf (car read-table)
                 (cons (cons char (list function
                                        non-terminating-p))
                       (car read-table)))))))

(defun get-macro-character (char &optional (read-table *read-table*))
  (let ((loc (member char (first read-table) :key #'car)))
    (cond (loc
           (values (first (cdr (car loc)))
                   (second (cdr (car loc)))))
          (t
           (values nil nil)))))
