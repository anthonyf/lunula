(in-package :lunula)

;; Now that DEFUN is defined we can start building up the library of
;; functions that we need in order to implement DEFMACRO.

(defun not (thing) (if thing nil t))
(defun null (thing) (eq nil thing))

(defun list (&rest args) args)

(defun eql (a b)
  (or (eq a b)
      (and (numberp a)
           (numberp b)
           (eq a b))
      (and (characterp a)
           (characterp b)
           (eq a b))))

(defun cadr (thing) (car (cdr thing)))
(defun caar (thing) (car (car thing)))
(defun cdar (thing) (cdr (car thing)))
(defun cddr (thing) (cdr (cdr thing)))
(defun caadr (thing) (car (cadr thing)))
(defun caddr (thing) (car (cddr thing)))
(defun cdddr (thing) (cdr (cddr thing)))
(defun cadar (thing) (car (cdar thing)))
(defun cddar (thing) (cdr (cdar thing)))
(defun cadddr (thing) (car (cdddr thing)))
(defun caadar (thing) (car (cadar thing)))

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
  ((lambda (new-list)
     (loop
        (if (not (cadr list))
            (return (reverse new-list)))
        (setq new-list (cons (car list) new-list))
        (setq list (cdr list))))
   nil))

(defun complement (function)
  (lambda (&rest args)
    (not (apply function args))))

(defun identity (a) a)

(defun member (item list &key (key #'identity) (test #'eql))
  (loop
     (if (null list)
         (return nil))
     (if (funcall test item (funcall key (car list)))
         (return list))
     (setq list (cdr list))))

(defun 1- (n) (- n 1))
(defun 1+ (n) (+ n 1))

(defun list-length (list)
  ((lambda (n)
     (loop
        (if (not list)
            (return n))
        (setq n (1+ n)
              list (cdr list))))
   0))

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
  (if (not (listp sequence))
      (error "only list sequences are supported"))
  (if (/= 0 start)
      (error "start parameter not supported yet"))
  (if end
      (error "end parameter not supported yet"))
  (if from-end
      (error "from-end parameter not supported yet"))
  (if test-not
      (setq test (complement test-not)))
  ((lambda (n)
     (loop
        (if (null sequence)
            (return nil))
        (if (funcall test item (funcall key (car sequence)))
            (return n))
        (setq n (1+ n))
        (setq sequence (cdr sequence))))
   0))

(defun nth (n list)
  (if (< n 0) (error "cannot get nth position of a negative number"))
  (loop
     (if (= n 0)
         (return (car list)))
     (setq n (1- n))
     (setq list (cdr list))))

(defun (setf nth) (value n list)
  (if (< n 0) (error "cannot set nth position of a negative number"))
  (loop
     (if (= n 0)
         (progn
           (rplaca list value)
           (return value)))
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
  ((lambda (new-list)
     (loop
        (if (= 0 length) (return new-list))
        (setq new-list (cons initial-element new-list))
        (setq length (1- length))))
   nil))

(defun make-sequence (type length &key initial-element)
  (if (eq type 'list)
      (make-list length :initial-element initial-element)
      (if (eq type 'string)
          (make-string length :initial-element (or initial-element #\nul))
          (if (eq type 'vector)
              (make-array (list length) :initial-element initial-element)
              (error "unsupported sequence type")))))

(defun reverse (sequence)
  (if (not (listp sequence))
      (error "non-list sequences not supported yet"))
  ((lambda (new-list)
     (loop (if (not sequence)
               (return new-list))
        (setq new-list (cons (car sequence) new-list))
        (setq sequence (cdr sequence))))
   nil))

(defun mapc (function list &rest more-lists)
  ((lambda (lists)
     (loop
        (if ((lambda (lists)
               (loop
                  (if (not lists)
                      (return nil))
                  (if (not (car lists))
                      (return t))
                  (setq lists (cdr lists))))
             lists)
            (return))
        (apply function
               ((lambda (lists args)
                  (loop
                     (if (not lists)
                         (return (reverse args)))
                       (setq args (cons (caar lists)
                                        args))
                       (setq lists (cdr lists))))
                lists nil))
        (setq lists
              ((lambda (lists new-lists)
                 (loop
                    (if (not lists)
                        (return (reverse new-lists)))
                    (setq new-lists (cons (cdar lists)
                                          new-lists))
                    (setq lists (cdr lists))))
               lists nil))))
   (cons list more-lists))
  list)

(defun mapcar (function list &rest more-lists)
  ((lambda (new-list)
     (apply #'mapc (lambda (&rest args)
                     (setq new-list (cons (apply function args)
                                          new-list)))
            list more-lists)
     (reverse new-list))
   nil))

(defun min (number &rest more-numbers)
  ((lambda (min)
     (loop
        (if (null more-numbers)
            (return min))
        (if (< (car more-numbers) min)
            (setq min (car more-numbers)))
        (setq more-numbers (cdr more-numbers))))
   number))

(defun map-min-length (sequences)
  ((lambda (sequences length)
     (loop
        (if (not sequences)
            (return length))
        (if (< (length (car sequences))
               length)
            (setq length (length (car sequences))))
        (setq sequences (cdr sequences))))
   (cdr sequences)
   (length (first sequences))))

(defun map-get-args (sequences index)
  ((lambda (args)
     (loop
        (if (not sequences)
            (return (reverse args)))
        (setq args (cons (elt (car sequences) index)
                         args))
        (setq sequences (cdr sequences))))
   nil))

(defun map (result-type function first-sequence &rest more-sequences)
  ;; TODO: MAP will be slow when sequences or result sequence are
  ;; lists due to the use of ELT.
  ((lambda (sequences length)
     ((lambda (new-sequence index)
        (loop
           (if (= index length)
               (return))
           ((lambda (value)
              (if new-sequence
                  (setf (elt new-sequence index) value)))
            (apply function
                   (map-get-args sequences index)))
           (setq index (1+ index)))
        new-sequence)
      (if result-type
          (make-sequence result-type length))
      0))
   (cons first-sequence
         more-sequences)
   (map-min-length (cons first-sequence
                         more-sequences))))

(defun reduce (function sequence &key (key #'identity) (initial-value 0))
  (map nil #'(lambda (item)
               (setf initial-value (funcall function (funcall key item) initial-value)))
       sequence)
  initial-value)

(defun concatenate (output-type-spec &rest sequences)
  ((lambda (total-length new-sequence index)
     (setq new-sequence (make-sequence output-type-spec total-length))
     (mapc #'(lambda (sequence)
               (map nil #'(lambda (element)
                            (setf (elt new-sequence index) element)
                            (setq index (1+ index)))
                    sequence))
           sequences)
     new-sequence)
   (reduce #'+ (mapcar #'length sequences))
   nil
   0))

(defun append (&rest lists)
  ;; TODO: this is a very lazy and inefficient implementation of
  ;; append
  (apply #'concatenate 'list lists))

(defun vector-equal (a b)
  (and (= (length a)
          (length b))
       ((lambda (length index)
          (loop (if (= index length)
                    (return t))
             (if (/= (aref a index) (aref b index))
                 (return nil))
             (setq index (1+ index))))
        (length a)
        0)))

(defun equal (a b)
  (or (eql a b)
      (and (consp a)
           (consp b)
           (equal (car a) (car b))
           (equal (cdr a) (cdr b)))
      (and (stringp a)
           (stringp b)
           (string= a b))
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
       ((lambda (digits)
          (loop
             (setq digits (cons (mod integer 10)
                                digits))
             (setq integer (truncate (/ integer 10)))
             (if (<= integer 0)
                 (return)))
          digits)
        nil)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *gensym* 0)
  (defun gensym (&optional (prefix "G"))
    ((lambda (sym)
       (setq *gensym* (1+ *gensym*))
       sym)
     (make-symbol (concatenate 'string prefix (integer-to-string *gensym*))))))
