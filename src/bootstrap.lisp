(in-package :lunula)

;; Now that DEFUN is defined we can start building up the library of
;; functions that we need in order to implement DEFMACRO.

(defun not (thing) (if thing nil t))
(defun null (thing) (eq nil thing))

(defun list (&rest args) args)

(defun eql (a b)
  (or (and (numberp a)
           (numberp b)
           (eql (cl:type-of a) (cl:type-of b))
           (= a b))
      (and (characterp a)
           (characterp b)
           (char= a b))
      (eq a b)))

(defun caar (thing) (car (car thing)))
(defun cadr (thing) (car (cdr thing)))
(defun cdar (thing) (cdr (car thing)))
(defun cddr (thing) (cdr (cdr thing)))
(defun caadr (thing) (car (cadr thing)))
(defun cadar (thing) (car (cdar thing)))
(defun caddr (thing) (car (cddr thing)))
(defun cddar (thing) (cdr (cdar thing)))
(defun cdddr (thing) (cdr (cddr thing)))
(defun caadar (thing) (car (cadar thing)))
(defun cadddr (thing) (car (cdddr thing)))

(defun listp (thing)
  (or (consp thing)
      (null thing)))

(defun atom (thing)
  (not (consp thing)))

(defun first (list) (car list))
(defun second (list) (cadr list))
(defun third (list) (caddr list))
(defun rest (list) (cdr list))

(defun last (list)
  (loop
     (if (not (cadr list))
         (return list))
     (setq list (cdr list))))

(defun butlast (list)
  (cl:let ((new-list nil))
    (loop
       (if (not (cadr list))
           (return (reverse new-list)))
       (setq new-list (cons (car list) new-list))
       (setq list (cdr list)))))

(defun complement (function)
  (lambda (&rest args)
    (not (apply function args))))

(defun identity (a) a)

(defun member (item list &key (key #'identity) (test #'eql))
  (loop
     (cl:when (null list)
       (return nil))
     (cl:when (funcall test item (funcall key (car list)))
       (return list))
     (setq list (cdr list))))

(defun 1- (n) (- n 1))
(defun 1+ (n) (+ n 1))

(defun list-length (list)
  (cl:let ((n 0))
    (loop
       (cl:when (not list)
         (return n))
       (setq n (1+ n)
             list (cdr list)))))

(defun length (sequence)
  (if (listp sequence)
      (list-length sequence)
      (if (arrayp sequence)
          (array-dimension sequence 0)
          (error "unsupported sequence type"))))

(defun position (item sequence &key from-end
                                 (test #'eql) test-not
                                 (start 0) end
                                 (key #'identity))
  ;; TODO: this is really a dumbed down version of POSITION.  Needs to
  ;; be fleshed out.
  (cl:when (not (listp sequence))
    (error "only list sequences are supported"))
  (cl:when (/= 0 start)
    (error "start parameter not supported yet"))
  (cl:when end
    (error "end parameter not supported yet"))
  (cl:when from-end
    (error "from-end parameter not supported yet"))
  (cl:when test-not
    (setq test (complement test-not)))
  (cl:let ((n 0))
    (loop
       (cl:when (null sequence)
         (return nil))
       (cl:when (funcall test item (funcall key (car sequence)))
         (return n))
       (setq n (1+ n))
       (setq sequence (cdr sequence)))))

(defun nth (n list)
  (cl:when (< n 0)
    (error "cannot get nth position of a negative number"))
  (loop
     (cl:when (= n 0)
       (return (car list)))
     (setq n (1- n))
     (setq list (cdr list))))

(defun (setf nth) (value n list)
  (cl:when (< n 0)
    (error "cannot set nth position of a negative number"))
  (loop
     (cl:when (= n 0)
       (rplaca list value)
       (return value))
     (setq n (1- n))
     (setq list (cdr list))))

(defun (setf elt) (value sequence index)
  (if (listp sequence)
      (funcall (fdefinition '(setf nth)) value index sequence)
      (if (arrayp sequence)
          (funcall (fdefinition '(setf aref)) value sequence index)
          (error "unsupported sequence type"))))

(defun elt (sequence index)
  (if (listp sequence)
      (nth index sequence)
      (if (arrayp sequence)
          (aref sequence index)
          (error "unsupported sequence type"))))

(defun make-list (length &key initial-element)
  (cl:let ((new-list nil))
    (loop
       (cl:when (= 0 length)
         (return new-list))
       (setq new-list (cons initial-element new-list))
       (setq length (1- length)))))

(defun make-sequence (type length &key initial-element)
  (if (eq type 'list)
      (make-list length :initial-element initial-element)
      (if (eq type 'string)
          (make-string length :initial-element (or initial-element #\nul))
          (if (eq type 'vector)
              (make-array (list length) :initial-element initial-element)
              (error "unsupported sequence type")))))

(defun reverse (sequence)
  (cl:when (not (listp sequence))
    (error "non-list sequences not supported yet"))
  (cl:let ((new-list nil))
    (loop (cl:when (not sequence)
            (return new-list))
       (setq new-list (cons (car sequence) new-list))
       (setq sequence (cdr sequence)))))

(defun mapc (function list &rest more-lists)
  (cl:let ((lists (cons list more-lists)))
    (loop
       (if (cl:let ((lists lists))
             (loop
                (cl:when (not lists)
                  (return nil))
                (cl:when (not (car lists))
                  (return t))
                (setq lists (cdr lists))))
           (return))
       (apply function
              (cl:let ((lists lists)
                       (args nil))
                (loop
                   (cl:when (not lists)
                     (return (reverse args)))
                   (setq args (cons (caar lists)
                                    args))
                   (setq lists (cdr lists)))))
       (setq lists
             (cl:let ((lists lists)
                      (new-lists nil))
               (loop
                  (cl:when (not lists)
                    (return (reverse new-lists)))
                  (setq new-lists (cons (cdar lists)
                                        new-lists))
                  (setq lists (cdr lists)))))))
  list)

(defun mapcar (function list &rest more-lists)
  (cl:let ((new-list nil))
    (apply #'mapc (lambda (&rest args)
                    (setq new-list (cons (apply function args)
                                         new-list)))
           list more-lists)
    (reverse new-list)))

(defun min (number &rest more-numbers)
  (cl:let ((min number))
    (loop
       (cl:when (null more-numbers)
         (return min))
       (cl:when (< (car more-numbers) min)
         (setq min (car more-numbers)))
       (setq more-numbers (cdr more-numbers)))))

(defun map-min-length (sequences)
  (cl:let ((sequences (cdr sequences))
           (length (length (first sequences))))
    (loop
       (cl:when (not sequences)
         (return length))
       (cl:when (< (length (car sequences))
                   length)
         (setq length (length (car sequences))))
       (setq sequences (cdr sequences)))))

(defun map-get-args (sequences index)
  (cl:let ((args nil))
    (loop
       (cl:when (not sequences)
         (return (reverse args)))
       (setq args (cons (elt (car sequences) index)
                        args))
       (setq sequences (cdr sequences)))))

(defun map (result-type function first-sequence &rest more-sequences)
  ;; TODO: MAP will be slow when sequences or result sequence are
  ;; lists due to the use of ELT.
  (cl:let* ((sequences (cons first-sequence
                             more-sequences))
            (length (map-min-length (cons first-sequence
                                          more-sequences)))
            (new-sequence (cl:when result-type
                            (make-sequence result-type length)))
            (index 0))
    (loop
       (cl:when (= index length)
         (return))
       (cl:let ((value (apply function
                              (map-get-args sequences index))))
         (cl:when new-sequence
           (setf (elt new-sequence index) value)))
       (setq index (1+ index)))
    new-sequence))

(defun reduce (function sequence &key (key #'identity) (initial-value 0))
  (map nil #'(lambda (item)
               (setf initial-value (funcall function (funcall key item) initial-value)))
       sequence)
  initial-value)

(defun concatenate (output-type-spec &rest sequences)
  (cl:let ((total-length (reduce #'+ (mapcar #'length sequences)))
           (new-sequence nil)
           (index 0))
    (setq new-sequence (make-sequence output-type-spec total-length))
    (mapc #'(lambda (sequence)
              (map nil #'(lambda (element)
                           (setf (elt new-sequence index) element)
                           (setq index (1+ index)))
                   sequence))
          sequences)
    new-sequence))

(defun append (&rest lists)
  ;; TODO: this is a very lazy and inefficient implementation of
  ;; append
  (apply #'concatenate 'list lists))

(defun vector-equal (a b)
  (and (= (length a)
          (length b))
       (cl:let ((length (length a))
                (index 0))
         (loop (cl:when (= index length)
                 (return t))
            (cl:when (/= (aref a index) (aref b index))
              (return nil))
            (setq index (1+ index))))))

(defun equal (a b)
  (or (and (consp a)
           (consp b)
           (equal (car a) (car b))
           (equal (cdr a) (cdr b)))
      (and (stringp a)
           (stringp b)
           (string= a b))
      (eql a b)
      ;; TODO: handle arrays
      ;; TODO: handle pathnames
      ))

(defun equalp (a b)
  (or (and (characterp a)
           (characterp b)
           (char-equal a b))
      (and (numberp a)
           (numberp b)
           (= a b))
      (and (stringp a)
           (stringp b)
           (string-equal a b))
      (and (consp a)
           (consp b)
           (equalp (car a) (car b))
           (equalp (cdr a) (cdr b)))
      (and (and (vectorp a) (not (stringp a)))
           (vectorp b)
           (vector-equal a b))
      (eq a b)))

(defun integer-to-string (integer)
  (map 'string
       #'(lambda (char-code)
           (nth char-code '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)))
       (cl:let ((digits nil))
         (loop
            (setq digits (cons (mod integer 10)
                               digits))
            (setq integer (truncate (/ integer 10)))
            (cl:when (<= integer 0)
              (return)))
         digits)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *gensym* 0)
  (defun gensym (&optional (prefix "G"))
    (cl:let ((sym (make-symbol (concatenate 'string prefix (integer-to-string *gensym*)))))
      (setq *gensym* (1+ *gensym*))
      sym)))
