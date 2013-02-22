(in-package :lunula)

;;; If you don't have CLOS (the Common Lisp Object System) installed,
;;; then this defines a simple version of DEFMETHOD which only
;;; dispatches on the first argument, and works for structures (and
;;; some other types) but not classes.  Note that you can still do
;;; (single) inheritance with structures using the :include option.
;;; To properly inform DEFMETHOD of the inheritance tree, you should
;;; use DEFSTRUCTURE rather than DEFSTRUCT.  This has the added
;;; benefit of allowing you to write PRINT-STRUCTURE methods rather
;;; than :print-function functions, if you like (they will be
;;; inherited properly, and they don't have the silly DEPTH argument).

(defun supertype (type)
  "Find the most specific supertype of this type."
  (cond ((eq type t) nil)
        ((get type :supertype))
        (t 'atom)))

(defun call-method-for (name type var args)
  "Find the method for this type, following :supertype links if needed."
  (let ((m (get name type)))
    (cond (m (apply m var args))
          ((eq type nil) (error "Can't find method ~A for ~A." name var))
          (t (call-method-for name (supertype type) var args)))))

(defun ensure-generic-function (name)
  "Define NAME to be a generic function."
  (unless (eq (symbol-function name) (get name :generic))
    (setf (symbol-function name)
          #'(lambda (var &rest args)
              (labels ((call-next-method ()
                         (call-method-for name (supertype (type-of var))
                                          var args)))
                (call-method-for name (type-of var) var args))))))

(defmacro defmethod (name ((var class) &rest other-args) &rest body)
  "This version of DEFMETHOD is like the CLOS version, except it only
  dispatches on the first argument, and it only handles structures and
  some built-in types, not classes."
  `(setf (get ',name :generic) (ensure-generic-function ',name)
         (get ',name ',class) #'(lambda (,var . ,other-args) . ,body)))


;; Construct a small part of the built-in type hierarchy
(mapc
 #'(lambda (pair) (setf (get (first pair) :supertype) (second pair)))
 '((null list) (cons list) (list t) (atom t) (keyword symbol) (null symbol)
   (fixnum integer) (bignum integer) (integer rational) (ratio rational)
   (rational real) (float real) (real number) (complex number)
   (string vector) (bit-vector vector) (vector array) (error condition)))

