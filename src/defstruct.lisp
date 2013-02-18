(in-package :lunula)

(defun defstruct-name-and-options (name-and-options)
  (multiple-value-bind (name type)
      (cond ((symbolp name-and-options)
             (list name-and-options nil))
            ((listp name-and-options)
             (destructuring-bind (name &rest options)
                 name-and-options
               (let ((type-option (member :type options :key #'car)))
                 (if type-option
                     (values name (cadar type-option))
                     (values name nil)))))
            (t (error "invalid struct name")))
    (let ((constructor-name (intern (concatenate 'string "MAKE-" (symbol-name name)))))
      (values name type constructor-name))))

(defun defstruct-slot-description (slot-description)
  (cond ((symbolp slot-description)
         slot-description)
        ((listp slot-description)
         (car slot-description))
        (t (error "invalid slot description"))))

(defmacro defstruct (name-and-options &rest slot-descriptions)
  (multiple-value-bind (name type constructor-name)
      (defstruct-name-and-options name-and-options)
    (let* ((slot-indices (itoa-list (length slot-descriptions)))
           (slot-names (mapcar (lambda (slot-description)
                                 (multiple-value-bind (slot-name)
                                     (defstruct-slot-description slot-description)
                                   slot-name))
                               slot-descriptions)))
      `(progn
         (defun ,constructor-name (&key ,@slot-names)
           ,(case type
                  (vector `(vector ,@slot-names))
                  (list `(list ,@slot-names))
                  (otherwise (error "Only structs of type list or vector are supported"))))
         ,@(mapcar (lambda (slot-description slot-index)
                     (multiple-value-bind (slot-name)
                         (defstruct-slot-description slot-description)
                       (let ((accessor-name (intern (concatenate 'string
                                                                 (symbol-name name) "-"
                                                                 (symbol-name slot-name)))))
                         `(progn (defun ,accessor-name (instance)
                                   (elt instance ,slot-index))
                                 (defun (setf ,accessor-name) (value instance)
                                   (setf (elt instance ,slot-index) value))))))
                   slot-descriptions slot-indices)
         ',name))))

