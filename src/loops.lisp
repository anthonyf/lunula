(in-package :lunula)

(defun group-pairs (pairs)
  "this is a helper for psetq"
  (let ((new-list))
    (loop
       (unless pairs
         (return (reverse new-list)))
       (setq new-list (cons (list (car pairs)
                                  (cadr pairs))
                            new-list))
       (setq pairs (cddr pairs)))))

(defmacro psetq (&rest pairs)
  (let ((pairs (mapcar (lambda (pair)
                         (list (gensym) (car pair) (cadr pair)))
                       (group-pairs pairs))))
    `(let ,(mapcar (lambda (pair)
                     (list (car pair)
                           (caddr pair)))
                   pairs)
       ,@(mapcar (lambda (pairs)
                   `(setq ,(cadr pairs) ,(car pairs)))
                 pairs))))

(defun nreverse (sequence)
  (let ((i 0)
        (j (1- (length sequence)))
        (temp nil))
    (loop
       (when (>= i j)
         (return sequence))
       (setf temp (elt sequence i))
       (setf (elt sequence i) (elt sequence j))
       (setf (elt sequence j) temp)
       (setf i (1+ i))
       (setf j (1- j)))))

(defun nconc (&rest lists)
  (let ((new-list (car lists)))
    (setq lists (cdr lists))
    (loop (unless lists
            (return new-list))
       (rplacd (last new-list)
               (car lists))
       (setq lists (cdr lists)))))

(defun nreconc (x y)
  (nconc (nreverse x)
         y))

(defun expand-do (varlist endlist parallel-p body)
  (let ((set-type (if parallel-p 'psetq 'setq))
        (end-test-form (car endlist))
        (result-forms (cdr endlist)))
    `(let (,@(mapcar (lambda (var-form)
                       (cond ((listp var-form)
                              `(,(first var-form) ,(second var-form)))
                             (t
                              `(,var-form))))
                     varlist))
       (loop
          (,set-type ,@(apply #'append (mapcar (lambda (var-form)
                                                 (cond ((and (listp var-form)
                                                             (third var-form))
                                                        `(,(first var-form) ,(third var-form)))
                                                       (t nil)))
                                               varlist)))
          (when ,end-test-form
            (return (values ,@result-forms)))
          ,@body))))

(defmacro do (varlist endlist &body body)
  (expand-do varlist endlist t body))

(defmacro do* (varlist endlist &body body)
  (expand-do varlist endlist nil body))

(defun copy-list (list)
  (let ((new-list))
    (loop
       (when (null list)
         (return (reverse new-list)))
       (push (car list) new-list)
       (pop list))))

(defun list* (arg &rest others)
  (cond ((null others) arg)
        ((null (cdr others)) (cons arg (car others)))
        (t (let ((others (copy-list others)))
             (do ((x others (cdr x)))
                 ((null (cddr x)) (rplacd x (cadr x))))
             (cons arg others)))))

(defmacro dotimes ((var count &optional result) &body body)
  (let ((gcount (gensym)))
    `(let ((,gcount ,count)
           (,var 0))
       (loop
          (when (>= ,var ,gcount)
            (return ,result))
          ,@body
          (incf ,var)))))

(defmacro dolist ((var list &optional result) &body body)
  (let ((glist (gensym)))
    `(let ((,glist ,list)
           (,var nil))
       (loop
          (unless ,glist
            (return ,result))
          (setq ,var (pop ,glist))
          ,@body))))
