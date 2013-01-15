#| EMULATE MULTIPLE VALUES

The emulation of multiple values can be insightful, because it can highlight a possible optimization which avoids the execution of those forms whose values are not wanted. In other words, the values function should be a special form which evaluates only those arguments necessary to fulfill the requested number of values.[8]

|#

(in-package :cl-user)

(defparameter *mv-nbr-expected* 1) ; Usually 1 value expected.

(defparameter *mv-vals* (make-array  10 #+nil multiple-values-limit))

(defmacro @multiple-value-list (form)
  (let ((val1 (gensym)))
    `(progv '(*mv-nbr-expected*) (list multiple-values-limit)
       (let ((,val1 ,form)) ; Receive the first value here.
         (if (= (symbol-value '*mv-nbr-expected*) multiple-values-limit)
           (list ,val1)
           (coerce (subseq *mv-vals* 0 (symbol-value '*mv-nbr-expected*))
                   'list))))))

(defun @values (&rest args)
  (dotimes (i (setf (symbol-value '*mv-nbr-expected*)
                    (min (symbol-value '*mv-nbr-expected*) (length args)))
              (car args))
    (setf (aref *mv-vals* i) (elt args i))))

(defmacro @multiple-value-prog1 (exp &rest forms)
  (let ((valn (gensym)))
    `(let ((,valn (@multiple-value-list ,exp)))
       (progn ,@forms (apply #'@values ,valn)))))

(defmacro @multiple-value-call (fn &body forms)
  `(apply ,fn
     (append ,@(mapcar #'(lambda (fm) `(@multiple-value-list ,fm)) forms))))

(defmacro mvprogn (&body forms)
  (if forms `(progn (progv '(*mv-nbr-expected*) '(0)
                      (progn ,@(butlast forms)))
                   ,@(last forms))
    ''nil))

(defmacro mvif (be te &optional (ee ''nil))
  `(if (progv '(*mv-nbr-expected*) '(1) ,be) ,te ,ee))


;; test

(defun test ()
  (@values 1 2 3))

(defun test2 ()
  (test))

(@multiple-value-call '+ (test2))

