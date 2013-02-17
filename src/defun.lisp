(in-package :lunula)

;; Here we define DEFUN.  We need this early so we can build up the
;; functions we need to implement macros.  This definition of defun is
;; really ugly mostly because it only uses imported primitives from
;; the host implementation.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (funcall (fdefinition '(setf macro-function))
           (lambda (form env)
             (declare (ignore env))
             ((lambda (body doc-string declare)
                ;; do we have a doc string?
                (if (stringp (car body))
                    (progn (setq doc-string (car body))
                           (setq body (cdr body))))
                ;; do we have a declare statement?
                (if (and (consp (car body))
                         (eq 'declare (car (car body))))
                    (progn (setq declare (car body))
                           (setq body (cdr body))))
                ((lambda (body block-name doc-string declare)
                   ;; TODO: need to handle doc strings for defun
                   (declare (ignore doc-string))
                   `(eval-when (:compile-toplevel :load-toplevel :execute)

                      (funcall (fdefinition '(setf fdefinition))
                               (lambda ,(car (cdr (cdr form)))
                                 ,@(if declare
                                       (cons declare nil))
                                 (block ,block-name
                                   ,@body))
                               ',(car (cdr form)))))
                 body
                 ;; block-name needs to be be a symbol, so deal with a
                 ;; '(setf ...) appropriately
                 (if (symbolp (car (cdr form)))
                     (car (cdr form))
                     (car (cdr (car (cdr form)))) ;; was a '(setf ...) form
                     )
                 doc-string
                 declare))
              ;; body
              (cdr (cdr (cdr form)))
              nil
              nil))
           'defun))
