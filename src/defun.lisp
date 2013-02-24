(in-package :lunula)

;; Here we define DEFUN.  We need this early so we can build up the
;; functions we need to implement macros.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (macro-function 'defun)
        (lambda (form env)
          (declare (ignore env))
          (cl:let ((body (cdr (cdr (cdr form))))
                   (doc-string nil)
                   (declare nil))
            ;; do we have a doc string?
            (cl:when (stringp (car body))
              (setq doc-string (car body))
              (setq body (cdr body)))
            ;; do we have a declare statement?
            (cl:when (and (consp (car body))
                          (eq 'declare (car (car body))))
              (setq declare (car body))
              (setq body (cdr body)))
            (cl:let ((body body)
                     ;; block-name needs to be be a symbol, so deal with a
                     ;; '(setf ...) appropriately
                     (block-name (if (symbolp (car (cdr form)))
                                     (car (cdr form))
                                     ;; was a '(setf ...) form
                                     (car (cdr (car (cdr form))))))
                     (doc-string doc-string)
                     (declare declare))
              ;; TODO: need to handle doc strings for defun
              (declare (ignore doc-string))
              `(eval-when (:compile-toplevel :load-toplevel :execute)

                 (funcall (fdefinition '(setf fdefinition))
                          (lambda ,(car (cdr (cdr form)))
                            ,@(cl:when declare
                                       (cons declare nil))
                            (block ,block-name
                              ,@body))
                          ',(car (cdr form)))))))))
