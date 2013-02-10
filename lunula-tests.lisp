(cl:in-package :cl-user)

;;;; It is useful to run these tests against both Lunula and CL to
;;;; make sure they produce the same result.  This helps with CL
;;;; compliance testing.  Call SWITCH-TESTS to change tests to run
;;;; against CL or LUNULA

(defun lunula-user::switch-tests (use-cl)
  (if use-cl
      (pushnew :lunula-test-use-cl *features*)
      (setf *features* (remove :lunula-test-use-cl *features*))))

(cl:defpackage
    #-lunula-test-use-cl :lunula-tests
    #+lunula-test-use-cl :lunula-tests-cl
    (:use  #-lunula-test-use-cl :lunula
           #+lunula-test-use-cl :common-lisp))

(cl:in-package #-lunula-test-use-cl :lunula-tests
               #+lunula-test-use-cl :lunula-tests-cl)

(defun assert-eq (a b)
  (assert (eq a b)))

(defun assert-eql (a b)
  (assert (eql a b)))

(defun assert-equal (a b)
  (assert (equal a b)))

(defun assert-equalp (a b)
  (assert (equalp a b)))

(defun assert-true (a)
  (assert a))

(defun assert-false (a)
  (assert (not a)))

;;;; ATOM ;;;;
(assert-true (atom 'sss))
(assert-false (atom (cons 1 2)))
(assert-true (atom nil))
(assert-true (atom '()))
(assert-true (atom 3))

;;;; LENGTH ;;;;
(assert-eq 3 (length '(1 2 3)))
(assert-eq 0 (length nil))

;;;; MEMBER ;;;;
(assert-equal '(2 3 4) (member 2 (list 1 2 3 4)))
(assert-equal nil (member 5 (list 1 2 3 4)))

;;;; POSITION ;;;;
(assert-eq 3 (position 10 '(1 2 3 10 3 4)))
(assert-eq nil (position nil '(1 2 3 10 3 4)))
(assert-eq nil (position 11 '(1 2 3 10 3 4)))

;;;; IF ;;;;
(assert-eq 2 (if 1 2 3))
(assert-eq 3 (if nil 2 3))

;;;; WHEN ;;;;
(assert-eq 3 (when t
               2 3))
(assert-eq nil (when (eq 1 2) 3))

;;;; COND ;;;;
(assert-eq 1 (cond ((eq 1 1) 1)
                   ((eq 2 3) 2)
                   (t 3)))
(assert-eq 2 (cond ((eq 1 4) 1)
                   ((eq 2 2) 2)
                   (t 3)))
(assert-eq 3 (cond ((eq 1 2) 1)
                   ((eq 2 1) 2)
                   (t 3)))

;;;; EQUAL ;;;;
(assert (not (equal 'a 'b)))
(assert (equal 'a 'a))
(assert (equal 3 3))
(assert (not (equal 3 3.0)))
(assert (equal 3.0 3.0))
;; TODO: support complex numbers
#+nil (assert (equal #c (3 -4)
               #c (3 -4)))

#+nil (assert (not (equal #c (3 -4.0)
                    #c (3 -4))))

(assert (not (equal (cons 'a 'b)
                    (cons 'a 'c))))

(assert (equal (cons 'a 'b)
               (cons 'a 'b)))
(assert (equal #\A #\A))

(assert (not (equal #\A #\a)))
(assert (equal "Foo" "Foo"))

;; TODO: impelement copy-seq
#+nil(assert (equal "Foo" (copy-seq "Foo")))

(assert (not (equal "FOO" "foo")))
(assert (equal "This-string" "This-string"))
(assert (not (equal "This-string" "this-string")))

;;;; EQUALP ;;;;
(assert-false (equalp 'a 'b))
(assert-true (equalp 'a 'a))
(assert-true (equalp 3 3))
(assert-true (equalp 3 3.0))
(assert-true (equalp 3.0 3.0))
;; TODO: support complex numbers
;;(assert-true (equalp #c(3 -4) #c(3 -4)))
;;(assert-true (equalp #c(3 -4.0) #c(3 -4)))
(assert-false (equalp (cons 'a 'b) (cons 'a 'c)))
(assert-true (equalp (cons 'a 'b) (cons 'a 'b)))
(assert-true (equalp #\A #\A))
(assert-true (equalp #\A #\a))
(assert-true (equalp "Foo" "Foo"))
;; TODO: impelement copy-seq
;;(assert-true (equalp "Foo" (copy-seq "Foo")))
(assert-true (equalp "FOO" "foo"))

;;;; NTH ;;;;
(let ((list (list 1 2 3)))
  (assert-eq 4 (setf (nth 2 list) 4))
  (assert-equal list '(1 2 4)))

;;;; REVERSE ;;;;
(assert-equal '(4 3 2 1) (reverse (list 1 2 3 4)))

;;;; MAP MAPCAR MAPC ;;;;
(assert-equalp #(-1 -2 -3 -4 -5) (map 'vector #'- '(1 2 3 4 5)))
(assert-equal '(-1 -2 -3 -4 -5) (map 'list #'- #(1 2 3 4 5)))
(assert-equal nil (map nil #'- #(1 2 3 4 5)))

(assert-equal '(2 3 4 5) (mapcar #'(lambda (a)
                                     (1+ a))
                                 (list 1 2 3 4)))

(assert-equal '(1 2 3) (mapc #'+ (list 1 2 3)))


;;;; REDUCE ;;;;
(assert-eq 15 (reduce #'+ '(1 2 3 4 5)))

;;;; LET ;;;;
(assert-equal (let ((c 4))
                (let ((a 1)
                      (b 2)
                      (c 3)
                      (d (+ c 10)))
                  (list a b c d)))
              '(1 2 3 14))

(let ())
(let (a b))

;;;; LET* ;;;;
(assert-equal (let ((c 4))
                (let* ((a 1)
                       (b 2)
                       (c (+ c 3))
                       (d (+ c 10)))
                  (list a b c d)))
              '(1 2 7 17))

(let* ())
(let* (a b))

;;;; COMPLIMENT ;;;;
(assert-true (funcall (complement #'zerop)
                      1))

(assert-false (funcall (complement #'characterp)
                       #\A))

(assert-false (funcall (complement #'member)
                       'a '(a b c)))

(assert-true (funcall (complement #'member)
                      'd '(a b c)))

;;;; CONCATENATE ;;;;
(assert-equal "1 2 3 4 5 6" (concatenate 'string "1 2 3" " 4 5 6"))
(assert-equal '(#\1 #\2 #\3 #\4 #\5 #\6) (concatenate 'list "123" "456"))

;;;; CASE ;;;;
(assert-equal 'hello (case 10
                       ((1 2 3) 'blah)
                       (10 'hello)
                       (otherwise 'there)))
(assert-equal 'there (case 10
                       ((1 2 3) 'blah)
                       (11 'hello)
                       (otherwise 'there)))

;;;; DO ;;;;
(assert-equal 4 (do ((temp-one 1 (1+ temp-one))
                     (temp-two 0 (1- temp-two)))
                    ((> (- temp-one temp-two) 5) temp-one)))

(assert-equal 3 (do ((temp-one 1 (1+ temp-one))
                     (temp-two 0 (1+ temp-one)))     
                    ((= 3 temp-two) temp-one)))

(assert-equal 2 (do* ((temp-one 1 (1+ temp-one))
                      (temp-two 0 (1+ temp-one)))
                     ((= 3 temp-two) temp-one)))

;;;; NRECONC ;;;;

(assert-equal '(3 2 1 A B C)
              (let ((list-1 (list 1 2 3))
                    (list-2 (list 'a 'b 'c)))
                (nreconc list-1 list-2)))

;;;; EVERY, SOME, NOTEVERY, NOTANY ;;;;
(assert-true (every #'characterp "abc"))
(assert-true (some #'= '(1 2 3 4 5) '(5 4 3 2 1)))
(assert-false (notevery #'< '(1 2 3 4) '(5 6 7 8) '(9 10 11 12)))
(assert-true (notany #'> '(1 2 3 4) '(5 6 7 8) '(9 10 11 12)))

;;;; FLET ;;;;
(assert-eq 30 (flet ((foo (a b)
                       (+ a b)))
                (foo 10 20)))

(assert-eq 60 (flet ((foo (a b)
                       (+ a b))) 
                (flet ((foo (a b)
                         (foo (+ a b) (+ a b))))
                  (foo 10 20))))

;;;; LABELS ;;;;
(assert-eq 55 (labels ((fib (n)
                         (cond ((= n 0) 0)
                               ((= n 1) 1)
                               (t (+ (fib (- n 1))
                                     (fib (- n 2)))))))
                (fib 10)))

;;;; MULTIPLE-VALUE-BIND ;;;;

(assert-equal '(1 2 3)
              (multiple-value-bind (a b c)
                  (values 1 2 3)
                (list a b c)))

(assert-false (multiple-value-bind ()
                  (values-list (list 1 2 3))
                nil))

;;;; ASSOC ;;;;
(let ((values '((x . 100) (y . 200) (z . 50))))
  (assert-eql '(y . 200) (assoc 'y values))
  (assert-eq nil (assoc 'a values))
  (assert-eql '(x . 100) (assoc 'x values))
  (assert-eql '(z . 50) (assoc 'z values)))

;;;; READER ;;;;

(assert-eq 101 (read-from-string "101"))
(assert-eq 101 (read-from-string "101."))

(assert-eq 201 (let ((*read-base* 16)) 
                 (read-from-string "201.")))

(assert-eq 513 (let ((*read-base* 16)) 
                 (read-from-string "201")))

(assert-eq 2561 (let ((*read-base* 16)) 
                  (read-from-string "A01")))

;; TODO
;;(assert-eq 1.0 (read-from-string "1.0"))
