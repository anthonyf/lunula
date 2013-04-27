(in-package :cl-user)

(defpackage :com.purevirtual.lunula-test-system 
  (:use :asdf :cl))

(in-package :com.purevirtual.lunula-test-system)

(defsystem lunula-test
  :name "lunula-test"
  :author "Anthony Fairchild"
  :version "1.0"
  :maintainer "Anthony Fairchild <fairchild.anthony@gmail.com>"
  :licence "MIT"
  :description "Unit tests for Lunula"
  :long-description ""    
  :depends-on (:lunula)
  :serial T
  :components
  ((:file "tst/lunula-tests")))


(pushnew :lunula-test *features*)
