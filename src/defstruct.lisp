(in-package :lunula)

(defmacro defstruct (name-and-options &rest slot-descriptions)
  (destructuring-bind (name type)
      (cond ((symbolp name-and-options)
             (list name-and-options nil))
            ((listp name-and-options)
             (destructuring-bind (name &rest options)
                 name-and-options
               (let ((type-option (member :type options :key #'car)))
                 (if type-option
                     (list name (cadar type-option))
                     (list name nil)))))
            (t (error "invalid struct name")))
    (let* ((constructor-name (intern (concatenate 'string "MAKE-" (symbol-name name))))
           (slot-names (mapcar (lambda (slot-description)
                                 (cond ((symbolp slot-description)
                                        slot-description)
                                       ((listp slot-description)
                                        (car slot-description))
                                       (t (error "invalid slot description"))))
                               slot-descriptions))
           (slot-indices (itoa-list (length slot-descriptions))))
      `(progn
         (defun ,constructor-name (&key ,@slot-names)
           ,(case type
                  (vector `(vector ,@slot-names))
                  (list `(list ,@slot-names))
                  (otherwise (error "Only structs of type list or vector are supported"))))
         ,@(mapcar (lambda (slot-name slot-index)
                     (let ((accessor-name (intern (concatenate 'string
                                                               (symbol-name name) "-"
                                                               (symbol-name slot-name)))))
                       `(progn (defun ,accessor-name (instance)
                                 (elt instance ,slot-index))
                               (defun (setf ,accessor-name) (value instance)
                                 (setf (elt instance ,slot-index) value)))))
                   slot-names slot-indices)
         ',name))))

