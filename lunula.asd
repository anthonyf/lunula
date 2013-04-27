(in-package :cl-user)

(defpackage :com.purevirtual.lunula-system 
  (:use :asdf :cl))

(in-package :com.purevirtual.lunula-system)

(defsystem lunula
  :name "lunula"
  :author "Anthony Fairchild"
  :version "1.0"
  :maintainer "Anthony Fairchild <fairchild.anthony@gmail.com>"
  :licence "MIT"
  :description "a lisp to javascript compiler"
  :long-description ""    
  :depends-on ()
  :serial T
  :components
  ((:file "src/packages")
   (:file "src/defun")
   (:file "src/bootstrap")
   (:file "src/defmacro")
   (:file "src/macros")
   (:file "src/let")
   (:file "src/flet-and-labels")
   (:file "src/conditionals")
   (:file "src/destructuring-bind")
   (:file "src/loops")
   (:file "src/values")
   (:file "src/sequence")
   (:file "src/math")
   (:file "src/defstruct")
   ;;(:file "src/mini-clos")
   (:file "src/error")
   (:file "src/streams")
   (:file "src/readtable")
   (:file "src/reader")
   (:file "src/reader-macros")
   (:file "src/backquote")
   (:file "src/printer")
   (:file "src/lexical-environment")
   (:file "src/global-environment")
   (:file "src/compiler-emitter")
   (:file "src/compiler")))

