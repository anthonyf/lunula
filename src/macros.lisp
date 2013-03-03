(in-package :lunula)

(defmacro prog1 (result &body body)
  (cl:let ((gresult (gensym)))
    `(let ((,gresult ,result))
       ,@body
       ,gresult)))

(defmacro push (obj place)
  `(setf ,place (cons ,obj ,place)))

(defmacro pop (place)
  `(prog1
       (car ,place)
     (setf ,place (cdr ,place))))

(defmacro incf (place &optional (delta 1))
  `(setf ,place (+ ,delta ,place)))

(defmacro decf (place &optional (delta 1))
  `(setf ,place (- ,delta ,place)))
