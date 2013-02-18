(in-package :lunula)

(defmacro when (test &body then)
  `(if ,test (progn ,@then)))

(defmacro unless (test &body then)
  `(if (not ,test) (progn ,@then)))

(defmacro cond (&rest clauses)
  (if clauses
      `(if ,(car (car clauses))
           (progn ,@ (cdr (car clauses)))
           (cond ,@(cdr clauses)))))

(defmacro case (keyform &body cases)
  (let ((last-case (car (last cases)))
        (keyform-var (gensym)))
    (when (or (eq 'otherwise (car last-case))
              (eq 't (car last-case)))
      (setq cases (butlast cases)))
    `(let ((,keyform-var ,keyform))
       (cond ,@(mapcar (lambda (case)
                         (cond ((listp (car case))
                                `((or ,@(mapcar (lambda (case)
                                                  `(eql ',case ,keyform-var))
                                                (car case)))
                                  ,@(cdr case)))
                               (t `((eql ',(car case) ,keyform-var)
                                    ,@(cdr case)))))
                       cases)
             ,@(when last-case
                     `((t ,@(cdr last-case))))))))
