(in-package :lunula)

;; FLET is a special form but we can define it in terms of MACROLET.
;; This code was taken from
;; http://home.pipeline.com/~hbaker1/MetaCircular.html
(defmacro flet (fns &body forms)
  (let* ((fnames (mapcar #'car fns))
         (nfnames (mapcar #'(lambda (ignore)
                              (declare (ignore ignore))
                              (gensym))
                          fnames))
         (nfbodies (mapcar #'(lambda (f) `#'(lambda ,@(cdr f))) fns)))
    `(let ,(mapcar #'(lambda (nfn nfb) `(,nfn ,nfb))
                   nfnames nfbodies)
       (macrolet
           ,(mapcar #'(lambda (f nf) `(,f (&rest a) `(apply ,',nf ,@a nil)))
                    fnames nfnames)
         ,@forms))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun itoa-list (n &optional (m 0))
    (if (zerop n) nil `(,m ,@(itoa-list (1- n) (1+ m))))))

;; LABELS is a special form but can be defined in terms of
(defmacro labels (fns &body forms)
  (let* ((fnames (mapcar #'car fns))
         (fnvec (gensym))
         (findicies (itoa-list (length fns)))
         (fbodies (mapcar #'(lambda (f i)
                              `(,f (&rest a) (apply (svref ,fnvec ,i) ,fnvec a)))
                          fnames findicies))
         ;;(fdecls `(declare (inline ,@fnames)))
         (nfbodies (mapcar #'(lambda (f)
                               `#'(lambda (,fnvec ,@(cadr f))
                                    (flet ,fbodies ;;,fdecls
                                      ,@(cddr f))))
                           fns)))
    `(let ((,fnvec (vector ,@nfbodies)))
       (flet ,fbodies ;;,fdecls
             ,@forms))))
