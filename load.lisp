(in-package :cl-user)

(load "src/package.lisp")
(load "src/defun.lisp")
(load "src/bootstrap.lisp")
(load "src/defmacro.lisp")
(load "src/destructuring-bind.lisp")
(load "src/macros.lisp")
(load "src/let.lisp")
(load "src/flet-and-labels.lisp")
(load "src/conditionals.lisp")
(load "src/loops.lisp")
(load "src/values.lisp")
(load "src/sequence.lisp")
(load "src/math.lisp")
(load "src/defstruct.lisp")
(load "src/error.lisp")
(load "src/streams.lisp")
(load "src/readtable.lisp")
(load "src/reader.lisp")
(load "src/reader-macros.lisp")
(load "src/backquote.lisp")

(pushnew :lunula-test *features*)
(load "tst/lunula-tests.lisp")
