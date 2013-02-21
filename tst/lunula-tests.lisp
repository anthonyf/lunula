(in-package :cl-user)

;;;; It is useful to run these tests against both Lunula and CL to
;;;; make sure they produce the same result.  This helps with CL
;;;; compliance testing.  

(cl:defpackage :lunula-tests
    (:use  #+lunula-test :lunula
           #-lunula-test :common-lisp))

(cl:in-package #-lunula-test-use-cl :lunula-tests
               #+lunula-test-use-cl :lunula-tests-cl)

(defmacro assert-eq (a b)
  `(assert (eq ,a ,b)))

(defmacro assert-eql (a b)
  `(assert (eql ,a ,b)))

(defmacro assert-equal (a b)
  `(assert (equal ,a ,b)))

(defmacro assert-equalp (a b)
  `(assert (equalp ,a ,b)))

(defmacro assert-true (a)
  `(assert ,a))

(defmacro assert-false (a)
  `(assert (not ,a)))

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

(assert-eq 2 (let ((type 'list))
               (case type
                 (vector 1)
                 (list 2)
                 (otherwise 3))))
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
  (assert-equal '(y . 200) (assoc 'y values))
  (assert-eq nil (assoc 'a values))
  (assert-equal '(x . 100) (assoc 'x values))
  (assert-equal '(z . 50) (assoc 'z values)))

(let ((alist '((1 . "one")(2 . "two")(3 . "three"))))
  (assert-equal '(2 . "two") (assoc 2 alist))
  (setq alist '(("one" . 1)("two" . 2)))
  (assert-equalp '("one" . 1) (assoc "one" alist :test #'equalp))
  (assert-equalp '("two" . 2) (assoc #\o alist :key #'(lambda(x)(char x 2)))))

;;;; DESTRUCTURING-BIND ;;;;

(assert-eq 6 (destructuring-bind (a b c) 
                 (list 1 2 3)
               (+ a b c)))
(assert-eq 150 (destructuring-bind (a &optional (b 20) &key (c 30) (d 40) (e 50))
                   (list 10)
                 (+ a b c d e)))

;;;; DEFSTRUCT ;;;;

(defstruct (foo (:type list)) a b c)
(let ((foo (make-foo :a 10 :b 20 :c 30)))
  (assert-eq 10 (foo-a foo))
  (assert-eq 20 (foo-b foo))
  (assert-eq 30 (foo-c foo)))

(defstruct (foo2 (:type vector)) a b c)
(let ((foo (make-foo2 :a 10 :b 20 :c 30)))
  (assert-eq 10 (foo2-a foo))
  (assert-eq 20 (foo2-b foo))
  (assert-eq 30 (foo2-c foo)))

(defstruct (foo3 (:type vector)) (a 10) (b 20) (c 30))
(let ((foo (make-foo3 :a 5)))
  (assert-eq 5 (foo3-a foo))
  (assert-eq 20 (foo3-b foo))
  (assert-eq 30 (foo3-c foo)))

;;;; READER ;;;;

;; integer tests
(assert-eq 101 (read-from-string "101"))
(assert-eq 101 (read-from-string "101."))

(assert-eq 201 (let ((*read-base* 16)) 
                 (read-from-string "201.")))

(assert-eq 513 (let ((*read-base* 16)) 
                 (read-from-string "201")))

(assert-eq 2561 (let ((*read-base* 16)) 
                  (read-from-string "A01")))

;; float tests
;; NOTE: these could fail due to precision errors.  Should probably
;; just comment these out.
(assert-eql 1.0 (read-from-string "1.0"))
(assert-eql -.0 (read-from-string "-.0"))
(assert-eql -.1 (read-from-string "-.1"))
(assert-eql 10.0 (read-from-string "10.0"))
(assert-eql 10.01 (read-from-string "10.01"))
(assert-eql -10.01 (read-from-string "-10.01"))
(assert-eql .001 (read-from-string ".001"))
(assert-eql 1.23e-8 (read-from-string "123.0e-10"))
(assert-eql 1200000.0 (read-from-string "12e5"))
;;(assert-eql 1200000.0 (read-from-string "12s5"))
;;(assert-eql 1200000.0 (read-from-string "12D5"))

;; char case tests
(assert-true (upper-case-p #\A))
(assert-false (upper-case-p #\a))
(assert-true (both-case-p #\a))
(assert-false (both-case-p #\5))
(assert-false (lower-case-p #\5))
(assert-false (upper-case-p #\5))

;; symbol tests
(assert-eq :blah (read-from-string ":BLAH"))
(assert-eq :BLAH (read-from-string ":blah"))
(assert-eq :|blah| (read-from-string ":|blah|"))
(assert-eq 'a|baz|a (read-from-string "a|baz|a"))
(defpackage "FOO")
(assert-eq 'FOO::BAR (read-from-string "FOO::BAR"))
(assert-eq 'FOO::|baz| (read-from-string "FOO::|baz|"))
(assert-eq '|FOOabc| (read-from-string "|FOOabc|"))

;; quote reader
(assert-equal ''A (read-from-string "'a"))

;; comment reader
(assert-eql 2 (read-from-string ";test comment 3
2"))
