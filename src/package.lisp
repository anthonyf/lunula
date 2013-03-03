(defpackage :lunula
  ;; Do not import the entire COMMON-LISP package.  Only import
  ;; primitive things that will not be redifened in the LUNULA
  ;; package.  We want to be explicit when we use anything else from
  ;; COMMON-LISP so we can easlily identify it later and implement it
  ;; in the compiler.
  (:import-from :common-lisp
                #:defpackage #:in-package #:*package* #:find-package #:export #:package-name
                #:nil #:t
                #:quote
                #:otherwise
                #:eq #:char-equal #:char= #:string-equal #:string=
                #:&rest #:&body #:&key #:&optional #:&whole #:&aux
                #:eval-when #:progn #:if
                #:lambda #:declare #:ignore #:inline
                #:fdefinition #:macro-function
                #:block #:return
                #:macrolet
                #:throw #:catch
                #:apply #:funcall
                #:and #:or
                #:cons #:car #:cdr
                #:rplaca #:rplacd
                #:= #:< #:> #:<= #:>= #:/=
                #:+ #:- #:* #:/ #:mod
                #:zerop
                #:consp #:numberp #:characterp #:symbolp #:stringp #:arrayp #:vectorp
                #:type-of
                #:aref #:char #:svref
                #:assert #:error
                #:setq #:setf
                #:loop ;; only simple loop supported for now
                #:make-symbol #:intern #:symbol-value #:symbol-name #:symbol-function #:symbol-package #:get #:boundp
                #:string #:vector
                #:defvar #:defparameter
                #:array-dimension
                #:make-string
                #:make-array
                #:code-char #:char-code
                #:truncate
                #:values
                #:multiple-value-call
                )
  (:export #:defpackage #:in-package #:*package* #:find-package #:export
           #:nil #:t
           #:quote
           #:eq #:eql #:equal #:equalp #:char-equal #:char= #:string=
           #:not #:null
           #:&rest #:&body #:&key #:&optional #:&whole #:&aux
           #:eval-when
           #:progn #:prog1
           #:if #:cond #:when #:unless #:case #:otherwise
           #:lambda #:declare #:ignore
           #:flet #:labels
           #:fdefinition #:macro-function
           #:block #:return
           #:throw #:catch
           #:apply #:funcall
           #:and #:or
           #:cons #:car #:cdr
           #:atom #:listp
           #:rplaca #:rplacd
           #:list #:vector
           #:list*
           #:= #:< #:> #:<= #:>= #:/=
           #:+ #:- #:* #:/
           #:1+ #:1-
           #:zerop
           #:consp #:numberp #:characterp #:symbolp #:stringp #:arrayp #:vectorp
           #:type-of
           #:aref #:char #:svref
           #:assert #:error
           #:defun #:defmacro
           #:nth #:elt
           #:let #:let*
           #:setq #:setf #:psetq
           #:loop #:do #:do*
           #:length
           #:member
           #:position
           #:complement
           #:make-symbol #:intern #:symbol-value #:symbol-name #:symbol-function #:symbol-package #:get #:boundp
           #:string
           #:concatenate
           #:princ-to-string
           #:defvar
           #:reverse #:nreverse
           #:map #:mapcar #:mapc
           #:reduce
           #:array-dimension
           #:make-string
           #:make-array
           #:code-char #:char-code
           #:truncate
           #:read-char #:peek-char #:write-char #:unread-char
           #:*standard-input* #:*standard-output*
           #:*read-table* #:*read-base* #:readtable-case
           #:set-macro-character #:get-macro-character
           #:first #:rest #:second #:third #:last #:butlast
           #:append #:nconc #:nreconc
           #:copy-list
           #:subseq
           #:some #:every #:notevery #:notany
           #:digit-char #:digit-char-p
           #:read #:read-from-string #:with-input-from-string
           #:macroexpand-1
           #:values #:values-list
           #:multiple-value-call #:multiple-value-bind #:multiple-value-list
           #:assoc
           #:char-upcase #:char-downcase
           #:abs #:expt
           #:plusp #:minusp
           #:upper-case-p #:lower-case-p #:both-case-p
           #:destructuring-bind
           #:defstruct
           #:alpha-char-p
           #:push #:pushnew #:pop
           #:incf #:decf
           #:defvar #:defparameter
           ))

(defpackage :lunula-user
  (:use :lunula))

;;;; common lisp special forms
;; block      let*                  return-from
;; catch      load-time-value       setq
;; eval-when  locally               symbol-macrolet
;; flet       macrolet              tagbody
;; function   multiple-value-call   the
;; go         multiple-value-prog1  throw
;; if         progn                 unwind-protect
;; labels     progv
;; let        quote                
