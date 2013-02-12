(defpackage :lunula
  ;; Do not import the entire COMMON-LISP package.  Only import
  ;; primitive things that will not be redifened in the LUNULA
  ;; package.  We want to be explicit when we use anything else from
  ;; COMMON-LISP so we can easlily identify it later and implement it
  ;; in the compiler.
  (:import-from :common-lisp
                #:defpackage #:in-package
                #:nil #:t
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
                #:aref #:char #:svref
                #:assert #:error
                #:setq #:setf
                #:loop ;; only simple loop supported for now
                #:make-symbol
                #:string #:vector
                #:defvar #:defparameter
                #:array-dimension
                #:make-string
                #:make-array
                #:truncate
                #:values
                #:multiple-value-call
                )
  (:export #:nil #:t
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
           #:consp #:numberp #:characterp
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
           #:make-symbol
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
           #:read-char #:peek-char #:write-char
           #:*standard-input* #:*standard-output*
           #:*read-table* #:*read-base*
           #:set-macro-character #:get-macro-character
           #:first #:rest #:second #:third #:last #:butlast
           #:append #:nconc #:nreconc
           #:copy-list
           #:subseq
           #:some #:every #:notevery #:notany
           #:digit-char #:digit-char-p
           #:read #:read-from-string
           #:macroexpand-1
           #:values #:values-list
           #:multiple-value-call #:multiple-value-bind #:multiple-value-list
           #:assoc
           #:char-upcase #:char-downcase
           #:abs #:expt
           #:plusp #:minusp))

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


(in-package :lunula)

;;;;;;;;;;;;;;;
;;;; DEFUN ;;;;
;;;;;;;;;;;;;;;

;; Here we define DEFUN.  We need this early so we can build up the
;; functions we need to implement macros.  This definition of defun is
;; really ugly mostly because it only uses imported primitives from
;; the host implementation.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (funcall (fdefinition '(setf macro-function))
           (lambda (form env)
             (declare (ignore env))
             ((lambda (body doc-string declare)
                ;; do we have a doc string?
                (if (stringp (car body))
                    (progn (setq doc-string (car body))
                           (setq body (cdr body))))
                ;; do we have a declare statement?
                (if (and (consp (car body))
                         (eq 'declare (car (car body))))
                    (progn (setq declare (car body))
                           (setq body (cdr body))))
                ((lambda (body block-name doc-string declare)
                   ;; TODO: need to handle doc strings for defun
                   (declare (ignore doc-string))
                   `(eval-when (:compile-toplevel :load-toplevel :execute)

                      (funcall (fdefinition '(setf fdefinition))
                               (lambda ,(car (cdr (cdr form)))
                                 ,@(if declare
                                       (cons declare nil))
                                 (block ,block-name
                                   ,@body))
                               ',(car (cdr form)))))
                 body
                 ;; block-name needs to be be a symbol, so deal with a
                 ;; '(setf ...) appropriately
                 (if (symbolp (car (cdr form)))
                     (car (cdr form))
                     (car (cdr (car (cdr form)))) ;; was a '(setf ...) form
                     )
                 doc-string
                 declare))
              ;; body
              (cdr (cdr (cdr form)))
              nil
              nil))
           'defun))

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


;; Finally we can define DEFMACRO in terms of everything we defined
;; before this point.  This definition of DEFMACRO relies on LAMBDA
;; for handling lambda lists.  A better version would not use LAMBDA
;; but instead do the destructuring programmatically.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun expand-macro (args-name lambda-list body)
    (if (null lambda-list)
        `(progn (if ,args-name
                    (error "to many arguments passed to macro"))
                ,@body)
        ;; if the first item in the lambda list is a list, then we
        ;; treat it as its own lambda list and let lambda handle it
        (if (listp (first lambda-list))
            `(apply (lambda (,args-name ,@(first lambda-list))
                      ,(expand-macro args-name (rest lambda-list) body))
                    (rest ,args-name) (first ,args-name))
            ;; if the lambda list starts with &rest or &optional, &key or
            ;; &body just treat the rest of the lambda list as usual
            (if (member (first lambda-list) '(&rest &optional &key &body))
                ;; lambda doesnt know what &body is, so convert it to &rest
                ((lambda (body-pos)
                   (if body-pos
                       (setf (nth body-pos lambda-list) '&rest))
                   `(apply (lambda (,@lambda-list)
                             ,@body)
                           ,args-name))
                 (position '&body lambda-list))
                `(apply (lambda (,args-name ,(first lambda-list))
                          ,(expand-macro args-name (rest lambda-list) body))
                        (rest ,args-name) (first ,args-name) nil)))))

  (setf (macro-function 'defmacro)
        (lambda (whole env)
          (declare (ignore env))
          ((lambda (macro-name lambda-list body args)
             `(eval-when (:compile-toplevel :load-toplevel :execute)
                (setf (macro-function ',macro-name)
                      (lambda (whole env)
                        (declare (ignore env))
                        ((lambda (,args)
                           ,(expand-macro args
                                          lambda-list
                                          body))
                         (rest whole))))))
           (cadr whole)
           (caddr whole)
           (cdddr whole)
           (gensym "ARGS")))))

(defun macroexpand-1 (form &optional env)
  (if (macro-function (car form))
      (values (funcall (macro-function (car form)) form env)
              t)
      (values form nil)))

(defmacro prog1 (result &body body)
  ((lambda (gresult)
     `((lambda (,gresult)
         ,@body
         ,gresult)
       ,result))
   (gensym)))

(defmacro push (obj place)
  `(setf ,place (cons ,obj ,place)))

(defmacro pop (place)
  `(prog1
       (car ,place)
     (setf ,place (cdr ,place))))

(defmacro incf (place &optional (delta 1))
  `(setf ,place (+ ,delta ,place)))

(defmacro decf (place &optional (delta 1))
  `(setf ,place (- ,delta ,place)))

(defmacro when (test &body then)
  `(if ,test (progn ,@then)))

(defmacro unless (test &body then)
  `(if (not ,test) (progn ,@then)))

(defmacro cond (&rest clauses)
  (if clauses
      `(if ,(car (car clauses))
           (progn ,@ (cdr (car clauses)))
           (cond ,@(cdr clauses)))))

(defmacro let (vars &body forms)
  `(funcall #'(lambda ,(mapcar #'(lambda (a)
                                   (if (consp a)
                                       (car a)
                                       a))
                               vars) ,@forms)
            ,@(mapcar #'(lambda (a)
                          (if (consp a)
                              (cadr a)
                              nil))
                      vars)))

(defmacro let* (vars &body forms)
  (if vars `(let (,(car vars)) (let* ,(cdr vars) ,@forms))
      `(let () ,@forms)))

;; FLET is a special form but we can define it in terms of MACROLET.
;; This code was taken from
;; http://home.pipeline.com/~hbaker1/MetaCircular.html
(defmacro flet (fns &body forms)
  (let* ((fnames (mapcar #'car fns))
         (nfnames (mapcar #'(lambda (ignore)
                              (declare (ignore ignore))
                              (gensym))
                          fnames))
         (nfbodies (mapcar #'(lambda (f) `#'(lambda ,@(cdr f))) fns)))
    `(let ,(mapcar #'(lambda (nfn nfb) `(,nfn ,nfb))
                   nfnames nfbodies)
       (macrolet
           ,(mapcar #'(lambda (f nf) `(,f (&rest a) `(apply ,',nf ,@a nil)))
                    fnames nfnames)
         ,@forms))))

(eval-when (:compile-toplevel)
  (defun itoa-list (n &optional (m 0))
    (if (zerop n) nil `(,m ,@(itoa-list (1- n) (1+ m))))))

;; LABELS is a special form but can be defined in terms of
(defmacro labels (fns &body forms)
  (let* ((fnames (mapcar #'car fns))
         (fnvec (gensym))
         (findicies (itoa-list (length fns)))
         (fbodies (mapcar #'(lambda (f i)
                              `(,f (&rest a) (apply (svref ,fnvec ,i) ,fnvec a)))
                          fnames findicies))
         ;;(fdecls `(declare (inline ,@fnames)))
         (nfbodies (mapcar #'(lambda (f)
                               `#'(lambda (,fnvec ,@(cadr f))
                                    (flet ,fbodies ;;,fdecls
                                      ,@(cddr f))))
                           fns)))
    `(let ((,fnvec (vector ,@nfbodies)))
       (flet ,fbodies ;;,fdecls
             ,@forms))))

(defun last (list)
  (loop
     (when (not (cadr list))
       (return list))
     (setq list (cdr list))))

(defun butlast (list)
  (let ((new-list))
    (loop
       (when (not (cadr list))
         (return (reverse new-list)))
       (setq new-list (cons (car list) new-list))
       (setq list (cdr list)))))

(defmacro case (keyform &body cases)
  (let ((last-case (car (last cases)))
        (keyform-var (gensym)))
    (when (or (eq 'otherwise (car last-case))
              (eq 't (car last-case)))
      (setq cases (butlast cases)))
    `(let ((,keyform-var ,keyform))
       (cond ,@(mapcar (lambda (case)
                         (cond ((listp (car case))
                                `((or ,@(mapcar (lambda (case)
                                                  `(eql ,case ,keyform-var))
                                                (car case)))
                                  ,@(cdr case)))
                               (t `((eql ,(car case) ,keyform-var)
                                    ,@(cdr case)))))
                       cases)
             ,@(when last-case
                     `((t ,@(cdr last-case))))))))


(defun group-pairs (pairs)
  "this is a helper for psetq"
  (let ((new-list))
    (loop
       (unless pairs
         (return (reverse new-list)))
       (setq new-list (cons (list (car pairs)
                                  (cadr pairs))
                            new-list))
       (setq pairs (cddr pairs)))))

(defmacro psetq (&rest pairs)
  (let ((pairs (mapcar (lambda (pair)
                         (list (gensym) (car pair) (cadr pair)))
                       (group-pairs pairs))))
    `(let ,(mapcar (lambda (pair)
                     (list (car pair)
                           (caddr pair)))
                   pairs)
       ,@(mapcar (lambda (pairs)
                   `(setq ,(cadr pairs) ,(car pairs)))
                 pairs))))

(defun append (&rest lists)
  ;; TODO: this is a very lazy and inefficient implementation of
  ;; append
  (apply #'concatenate 'list lists))

(defun nreverse (sequence)
  (let ((i 0)
        (j (1- (length sequence)))
        (temp nil))
    (loop
       (when (>= i j)
         (return sequence))
       (setf temp (elt sequence i))
       (setf (elt sequence i) (elt sequence j))
       (setf (elt sequence j) temp)
       (setf i (1+ i))
       (setf j (1- j)))))

(defun nconc (&rest lists)
  (let ((new-list (car lists)))
    (setq lists (cdr lists))
    (loop (unless lists
            (return new-list))
       (rplacd (last new-list)
               (car lists))
       (setq lists (cdr lists)))))

(defun nreconc (x y)
  (nconc (nreverse x)
         y))

(defun expand-do (varlist endlist parallel-p body)
  (let ((set-type (if parallel-p 'psetq 'setq))
        (end-test-form (car endlist))
        (result-forms (cdr endlist)))
    `(let (,@(mapcar (lambda (var-form)
                       (cond ((listp var-form)
                              `(,(first var-form) ,(second var-form)))
                             (t
                              `(,var-form))))
                     varlist))
       (loop
          (,set-type ,@(apply #'append (mapcar (lambda (var-form)
                                                 (cond ((and (listp var-form)
                                                             (third var-form))
                                                        `(,(first var-form) ,(third var-form)))
                                                       (t nil)))
                                               varlist)))
          (when ,end-test-form
            (return (values ,@result-forms)))
          ,@body))))

(defmacro do (varlist endlist &body body)
  (expand-do varlist endlist t body))

(defmacro do* (varlist endlist &body body)
  (expand-do varlist endlist nil body))

(defun copy-list (list)
  (let ((new-list))
    (loop
       (when (null list)
         (return (reverse new-list)))
       (push (car list) new-list)
       (pop list))))

(defun list* (arg &rest others)
  (cond ((null others) arg)
        ((null (cdr others)) (cons arg (car others)))
        (t (let ((others (copy-list others)))
             (do ((x others (cdr x)))
                 ((null (cddr x)) (rplacd x (cadr x))))
             (cons arg others)))))

(defmacro dotimes ((var count &optional result) &body body)
  (let ((gcount (gensym)))
    `(let ((,gcount ,count)
           (,var 0))
       (loop
          (when (>= ,var ,gcount)
            (return ,result))
          ,@body
          (incf ,var)))))

(defmacro dolist ((var list &optional result) &body body)
  (let ((glist (gensym)))
    `(let ((,glist ,list)
           (,var nil))
       (loop
          (unless ,glist
            (return ,result))
          (setq ,var (pop ,glist))
          ,@body))))

(defmacro multiple-value-bind ((&rest vars) values-form &rest body)
  ;; TODO: handle declare form
  (cond ((null vars)
         `(progn
            ,values-form
            ,@body))
        (t (let ((rest (gensym)))
             `(multiple-value-call #'(lambda (&optional ,@vars &rest ,rest)
                                       (declare (ignore ,rest))
                                       ,@body)
                ,values-form)))))

(defmacro multiple-value-list (form)
  `(multiple-value-call #'list ,form))

(defun values-list (list)
  (apply #'values list))

(defun some (pred first-seq &rest more-seqs)
  (let ((length (length first-seq))
        (seqs (cons first-seq
                    more-seqs)))
    (dolist (seq more-seqs)
      (setq length (min length (length seq))))
    (dotimes (i length)
      (when (apply pred (mapcar (lambda (seq)
                                  (elt seq i))
                                seqs))
        (return t)))))

(defun notany (pred first-seq &rest more-seqs)
  (not (apply #'some pred first-seq more-seqs)))

(defun every (pred first-seq &rest more-seqs)
  (let ((length (length first-seq))
        (seqs (cons first-seq
                    more-seqs)))
    (dolist (seq more-seqs)
      (setq length (min length (length seq))))
    (dotimes (i length t)
      (unless (apply pred (mapcar (lambda (seq)
                                    (elt seq i))
                                  seqs))
        (return nil)))))

(defun notevery (pred first-seq &rest more-seqs)
  (not (apply #'every pred first-seq more-seqs)))

(defun subseq (sequence start &optional end)
  (let* ((end (or end (length sequence)))
         (new-seq (make-sequence (cond ((listp sequence) 'list)
                                       ((stringp sequence) 'string)
                                       ((vectorp sequence) 'vector)
                                       (t (error "not a valid sequence")))
                                 (- end start)))
         (index 0))
    (loop
       (when (>= index end)
         (return new-seq))
       (setf (elt new-seq index)
             (elt sequence (+ index start)))
       (incf index))))

(defun abs (num)
  (if (< num 0)
      (* num -1)
      num))

(defun plusp (num)
  (>= num 0))

(defun minusp (num)
  (< num 0))

(defun expt (base power)
  ;; TODO: power can only be an integer with the code below
  (let ((num 1)
        (op (if (< power 0)  #'/ #'*)))
    (dotimes (i (abs power))
      (setq num (funcall op num base)))
    num))

(defun assoc (item alist &key (key #'identity) (test #'eql) test-not)
  (when test-not
    (setq test (complement test-not)))
  (loop
     (cond ((null alist)
            (return nil))
           ((funcall test
                     (funcall key (caar alist))
                     item)
            (return (car alist)))
           (t (pop alist)))))

(defun throw-eof-error ()
  ;; TODO: replace this with a real EOF error at some point
  (error "end of file!"))

(defun throw-reader-error ()
  ;; TODO: replace this with a real reader error
  (error "reader error"))

(defvar *standard-input* nil)
(defvar *standard-output* (make-string-output-stream))

(defun read-char (&optional (stream *standard-input*) (eof-error-p t) eof-value recursive-p)
  (funcall stream :read-char stream eof-error-p eof-value recursive-p))

(defun peek-char (&optional peek-type (stream *standard-input*) (eof-error-p t) eof-value recursive-p)
  (funcall stream :peek-char peek-type stream eof-error-p eof-value recursive-p))

(defun write-char (character &optional (stream *standard-output*))
  (funcall stream :write-char character stream))

(defun eofp (stream)
  (let ((eof (gensym)))
    (eq eof (peek-char nil stream nil eof))))

(defun make-string-input-stream (string &optional (start 0) end)
  (let ((string (subseq string start end))
        (length (length string))
        (index 0))
    (lambda (op &rest args)
      (case op
        (:read-char (apply (lambda (stream &optional (eof-error-p t) eof-value recursive-p)
                             (declare (ignore stream recursive-p))
                             (cond ((>= index length)
                                    (if eof-error-p
                                        (throw-eof-error)
                                        eof-value))
                                   (t (prog1
                                          (char string index)
                                        (incf index)))))
                           args))
        (:peek-char (apply (lambda (peek-type stream &optional (eof-error-p t) eof-value recursive-p)
                             (declare (ignore stream recursive-p))
                             (when peek-type
                               ;; TODO: support peek type
                               (error "peek-type of char or t is not supported yet"))
                             (cond ((>= index length)
                                    (if eof-error-p
                                        (throw-eof-error)
                                        eof-value))
                                   (t (char string index))))
                           args))
        (otherwise (error "unsupported stream operation"))))))

(defun make-string-output-stream (&key (element-type 'character))
  (unless (eq element-type 'character)
    (error "element-type only supports CHARACTER type"))
  (let ((chars nil))
    (lambda (op &rest args)
      (case op
        (:write-char (apply (lambda (character stream)
                              (declare (ignore stream))
                              (push character chars))
                            args))
        (:get-output-stream-string (apply (lambda ()
                                            (prog1 (map 'string
                                                        #'identity
                                                        (reverse chars))
                                              (setq chars nil)))
                                          args))
        (otherwise (error "unsupported stream operation"))))))

(defun get-output-stream-string (string-output-stream)
  (funcall string-output-stream :get-output-stream-string))

(defun make-default-read-table ()
  ;; TODO: this is fragile.  Use a closure or something instead of a
  ;; list to represent the readtable object.
  (list
   nil ;; macro characters
   ))

(defvar *read-table* (make-default-read-table))

(defun set-macro-character (char function &optional non-terminating-p (read-table *read-table*))
  (let ((loc (member char (first read-table) :key #'car)))
    (cond (loc
           (setf (cdr (car loc)) (list function
                                       non-terminating-p)))
          (t
           (setf (car read-table)
                 (cons (cons char (list function
                                        non-terminating-p))
                       (car read-table)))))))

(defun get-macro-character (char &optional (read-table *read-table*))
  (let ((loc (member char (first read-table) :key #'car)))
    (cond (loc
           (values (first (cdr (car loc)))
                   (second (cdr (car loc)))))
          (t
           (values nil nil)))))

(defun single-quote-reader (stream char)
  (declare (ignore char))
  (list 'quote (cl:read stream t nil t)))
(set-macro-character #\' #'single-quote-reader)

(defun semicolon-reader (stream char)
  (declare (ignore char))
  (loop
     (when (char= (read-char stream nil #\Newline t)
                  #\Newline)
       (return)))
  ;; Return zero values.
  (values))
(set-macro-character #\; #'semicolon-reader)

;;;;;;;;;;;;;;;;;;;
;;;; BACKQUOTE ;;;;
;;;;;;;;;;;;;;;;;;;

;;; Common Lisp backquote implementation, written in Common Lisp.
;;; Author: Guy L. Steele Jr.     Date: 27 December 1985
;;; Tested under Symbolics Common Lisp and Lucid Common Lisp.
;;; This software is in the public domain.

;;; $ is pseudo-backquote and % is pseudo-comma.  This makes it
;;; possible to test this code without interfering with normal
;;; Common Lisp syntax.

;;; The following are unique tokens used during processing.
;;; They need not be symbols; they need not even be atoms.

(defvar *comma* (make-symbol "COMMA"))
(defvar *comma-atsign* (make-symbol "COMMA-ATSIGN"))
(defvar *comma-dot* (make-symbol "COMMA-DOT"))
(defvar *bq-list* (make-symbol "BQ-LIST"))
(defvar *bq-append* (make-symbol "BQ-APPEND"))
(defvar *bq-list** (make-symbol "BQ-LIST*"))
(defvar *bq-nconc* (make-symbol "BQ-NCONC"))
(defvar *bq-clobberable* (make-symbol "BQ-CLOBBERABLE"))
(defvar *bq-quote* (make-symbol "BQ-QUOTE"))
(defvar *bq-quote-nil* (list *bq-quote* nil))

;;; Reader macro characters:
;;;    $foo is read in as (BACKQUOTE foo)
;;;    %foo is read in as (#:COMMA foo)
;;;    %@foo is read in as (#:COMMA-ATSIGN foo)
;;;    %.foo is read in as (#:COMMA-DOT foo)
;;; where #:COMMA is the value of the variable *COMMA*, etc.

;;; BACKQUOTE is an ordinary macro (not a read-macro) that
;;; processes the expression foo, looking for occurrences of
;;; #:COMMA, #:COMMA-ATSIGN, and #:COMMA-DOT.  It constructs code
;;; in strict accordance with the rules on pages 349-350 of
;;; the first edition (pages 528-529 of this second edition).
;;; It then optionally applies a code simplifier.

(set-macro-character #\`
                     #'(lambda (stream char)
                         (declare (ignore char))
                         (list 'backquote (cl:read stream t nil t))))

(set-macro-character #\,
                     #'(lambda (stream char)
                         (declare (ignore char))
                         (case (peek-char nil stream t nil t)
                           (#\@ (read-char stream t nil t)
                                (list *comma-atsign* (cl:read stream t nil t)))
                           (#\. (read-char stream t nil t)
                                (list *comma-dot* (cl:read stream t nil t)))
                           (otherwise (list *comma* (cl:read stream t nil t))))))


;;; If the value of *BQ-SIMPLIFY* is non-NIL, then BACKQUOTE
;;; processing applies the code simplifier.  If the value is NIL,
;;; then the code resulting from BACKQUOTE is exactly that
;;; specified by the official rules.

(defparameter *bq-simplify* t)


(defmacro backquote (x)
  (bq-completely-process x))

;;; Backquote processing proceeds in three stages:
;;;
;;; (1) BQ-PROCESS applies the rules to remove occurrences of
;;; #:COMMA, #:COMMA-ATSIGN, and #:COMMA-DOT corresponding to
;;; this level of BACKQUOTE.  (It also causes embedded calls to
;;; BACKQUOTE to be expanded so that nesting is properly handled.)
;;; Code is produced that is expressed in terms of functions
;;; #:BQ-LIST, #:BQ-APPEND, and #:BQ-CLOBBERABLE.  This is done
;;; so that the simplifier will simplify only list construction
;;; functions actually generated by BACKQUOTE and will not involve
;;; any user code in the simplification.  #:BQ-LIST means LIST,
;;; #:BQ-APPEND means APPEND, and #:BQ-CLOBBERABLE means IDENTITY
;;; but indicates places where "%." was used and where NCONC may
;;; therefore be introduced by the simplifier for efficiency.
;;;
;;; (2) BQ-SIMPLIFY, if used, rewrites the code produced by
;;; BQ-PROCESS to produce equivalent but faster code.  The
;;; additional functions #:BQ-LIST* and #:BQ-NCONC may be
;;; introduced into the code.
;;;
;;; (3) BQ-REMOVE-TOKENS goes through the code and replaces
;;; #:BQ-LIST with LIST, #:BQ-APPEND with APPEND, and so on.
;;; #:BQ-CLOBBERABLE is simply eliminated (a call to it being
;;; replaced by its argument).  #:BQ-LIST* is replaced by either
;;; LIST* or CONS (the latter is used in the two-argument case,
;;; purely to make the resulting code a tad more readable).

(defun bq-completely-process (x)
  (let ((raw-result (bq-process x)))
    (bq-remove-tokens (if *bq-simplify*
                          (bq-simplify raw-result)
                          raw-result))))

(defun bq-process (x)
  (cond ((atom x)
         (list *bq-quote* x))
        ((eq (car x)
             'backquote)
         (bq-process (bq-completely-process (cadr x))))
        ((eq (car x)
             *comma*)
         (cadr x))
        ((eq (car x)
             *comma-atsign*)
         (error ",@~S after `" (cadr x)))
        ((eq (car x)
             *comma-dot*)
         (error ",.~S after `" (cadr x)))
        (t (do ((p x (cdr p))
                (q '()
                   (cons (bracket (car p))
                         q)))
               ((atom p)
                (cons *bq-append*
                      (nreconc q (list (list *bq-quote* p)))))
             (when (eq (car p)
                       *comma*)
               (unless (null (cddr p))
                 (error "Malformed ,~S" p))
               (return (cons *bq-append*
                             (nreconc q (list (cadr p))))))
             (when (eq (car p)
                       *comma-atsign*)
               (error "Dotted ,@~S" p))
             (when (eq (car p)
                       *comma-dot*)
               (error "Dotted ,.~S" p))))))

;;; This implements the bracket operator of the formal rules.
(defun bracket (x)
  (cond ((atom x)
         (list *bq-list* (bq-process x)))
        ((eq (car x)
             *comma*)
         (list *bq-list* (cadr x)))
        ((eq (car x)
             *comma-atsign*)
         (cadr x))
        ((eq (car x)
             *comma-dot*)
         (list *bq-clobberable* (cadr x)))
        (t (list *bq-list* (bq-process x)))))

;;; This auxiliary function is like MAPCAR but has two extra
;;; purposes: (1) it handles dotted lists; (2) it tries to make
;;; the result share with the argument x as much as possible.
(defun maptree (fn x)
  (if (atom x)
      (funcall fn x)
      (let ((a (funcall fn (car x)))
            (d (maptree fn (cdr x))))
        (if (and (eql a (car x))
                 (eql d (cdr x)))
            x
            (cons a d)))))

;;; This predicate is true of a form that when read looked
;;; like %@foo or %.foo.
(defun bq-splicing-frob (x)
  (and (consp x)
       (or (eq (car x)
               *comma-atsign*)
           (eq (car x)
               *comma-dot*))))

;;; This predicate is true of a form that when read
;;; looked like %@foo or %.foo or just plain %foo.
(defun bq-frob (x)
  (and (consp x)
       (or (eq (car x)
               *comma*)
           (eq (car x)
               *comma-atsign*)
           (eq (car x)
               *comma-dot*))))

;;; The simplifier essentially looks for calls to #:BQ-APPEND and
;;; tries to simplify them.  The arguments to #:BQ-APPEND are
;;; processed from right to left, building up a replacement form.
;;; At each step a number of special cases are handled that,
;;; loosely speaking, look like this:
;;;
;;;  (APPEND (LIST a b c) foo) => (LIST* a b c foo)
;;;       provided a, b, c are not splicing frobs
;;;  (APPEND (LIST* a b c) foo) => (LIST* a b (APPEND c foo))
;;;       provided a, b, c are not splicing frobs
;;;  (APPEND (QUOTE (x)) foo) => (LIST* (QUOTE x) foo)
;;;  (APPEND (CLOBBERABLE x) foo) => (NCONC x foo)
(defun bq-simplify (x)
  (if (atom x)
      x
      (let ((x (if (eq (car x)
                       *bq-quote*)
                   x
                   (maptree #'bq-simplify x))))
        (if (not (eq (car x)
                     *bq-append*))
            x
            (bq-simplify-args x)))))

(defun bq-simplify-args (x)
  (do ((args (reverse (cdr x))
             (cdr args))
       (result
        nil
        (cond ((atom (car args))
               (bq-attach-append *bq-append* (car args)
                                 result))
              ((and (eq (caar args)
                        *bq-list*)
                    (notany #'bq-splicing-frob (cdar args)))
               (bq-attach-conses (cdar args)
                                 result))
              ((and (eq (caar args)
                        *bq-list**)
                    (notany #'bq-splicing-frob (cdar args)))
               (bq-attach-conses
                (reverse (cdr (reverse (cdar args))))
                (bq-attach-append *bq-append*
                                  (car (last (car args)))
                                  result)))
              ((and (eq (caar args)
                        *bq-quote*)
                    (consp (cadar args))
                    (not (bq-frob (cadar args)))
                    (null (cddar args)))
               (bq-attach-conses (list (list *bq-quote*
                                             (caadar args)))
                                 result))
              ((eq (caar args)
                   *bq-clobberable*)
               (bq-attach-append *bq-nconc* (cadar args)
                                 result))
              (t (bq-attach-append *bq-append*
                                   (car args)
                                   result)))))
      ((null args)
       result)))

(defun null-or-quoted (x)
  (or (null x)
      (and (consp x)
           (eq (car x)
               *bq-quote*))))


;;; When BQ-ATTACH-APPEND is called, the OP should be #:BQ-APPEND
;;; or #:BQ-NCONC.  This produces a form (op item result) but
;;; some simplifications are done on the fly:
;;;
;;;  (op '(a b c) '(d e f g)) => '(a b c d e f g)
;;;  (op item 'nil) => item, provided item is not a splicable frob
;;;  (op item 'nil) => (op item), if item is a splicable frob
;;;  (op item (op a b c)) => (op item a b c)
(defun bq-attach-append (op item result)
  (cond ((and (null-or-quoted item)
              (null-or-quoted result))
         (list *bq-quote* (append (cadr item)
                                  (cadr result))))
        ((or (null result)
             (equal result *bq-quote-nil*))
         (if (bq-splicing-frob item)
             (list op item)
             item))
        ((and (consp result)
              (eq (car result)
                  op))
         (list* (car result)
                item (cdr result)))
        (t (list op item result))))

;;; The effect of BQ-ATTACH-CONSES is to produce a form as if by
;;; `(LIST* ,@items ,result) but some simplifications are done
;;; on the fly.
;;;
;;;  (LIST* 'a 'b 'c 'd) => '(a b c . d)
;;;  (LIST* a b c 'nil) => (LIST a b c)
;;;  (LIST* a b c (LIST* d e f g)) => (LIST* a b c d e f g)
;;;  (LIST* a b c (LIST d e f g)) => (LIST a b c d e f g)

(defun bq-attach-conses (items result)
  (cond ((and (every #'null-or-quoted items)
              (null-or-quoted result))
         (list *bq-quote*
               (append (mapcar #'cadr items)
                       (cadr result))))
        ((or (null result)
             (equal result *bq-quote-nil*))
         (cons *bq-list* items))
        ((and (consp result)
              (or (eq (car result)
                      *bq-list*)
                  (eq (car result)
                      *bq-list**)))
         (cons (car result)
               (append items (cdr result))))
        (t (cons *bq-list** (append items (list result))))))

;;; Removes funny tokens and changes (#:BQ-LIST* a b) into
;;; (CONS a b) instead of (LIST* a b), purely for readability.
(defun bq-remove-tokens (x)
  (cond ((eq x *bq-list*)
         'list)
        ((eq x *bq-append*)
         'append)
        ((eq x *bq-nconc*)
         'nconc)
        ((eq x *bq-list**)
         'list*)
        ((eq x *bq-quote*)
         'quote)
        ((atom x)
         x)
        ((eq (car x)
             *bq-clobberable*)
         (bq-remove-tokens (cadr x)))
        ((and (eq (car x)
                  *bq-list**)
              (consp (cddr x))
              (null (cdddr x)))
         (cons 'cons (maptree #'bq-remove-tokens (cdr x))))
        (t (maptree #'bq-remove-tokens x))))
;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END OF BACKQUOTE ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; constituent traits ;;;;
(defvar *constituent-traits* '(alphabetic digit package-marker plus-sign minus-sign dot decimal-point ratio-marker exponent-marker invalid))

(defvar *syntax-types* '((#\Backspace  constituent)               (#\Tab        whitespace)
                         (#\Newline    whitespace)                (#\Linefeed   whitespace)
                         (#\Page       whitespace)                (#\Return     whitespace)
                         (#\Space      whitespace)                (#\!          constituent)
                         (#\"          terminating-macro-char)    (#\#          non-terminating-macro-char)
                         (#\$          constituent)               (#\%          constituent)
                         (#\&          constituent)               (#\'          terminating-macro-char)
                         (#\(          terminating-macro-char)    (#\)          terminating-macro-char)
                         (#\*          constituent)               (#\+          constituent)
                         (#\,          terminating-macro-char)    (#\-          constituent)
                         (#\.          constituent)               (#\/          constituent)
                         (#\0          constituent)               (#\1          constituent)
                         (#\2          constituent)               (#\3          constituent)
                         (#\4          constituent)               (#\5          constituent)
                         (#\6          constituent)               (#\7          constituent)
                         (#\8          constituent)               (#\9          constituent)
                         (#\:          constituent)               (#\;          terminating-macro-char)
                         (#\<          constituent)               (#\=          constituent)
                         (#\>          constituent)               (#\?          constituent)
                         (#\@          constituent)               (#\A          constituent)
                         (#\B          constituent)               (#\C          constituent)
                         (#\D          constituent)               (#\E          constituent)
                         (#\F          constituent)               (#\G          constituent)
                         (#\H          constituent)               (#\I          constituent)
                         (#\J          constituent)               (#\K          constituent)
                         (#\L          constituent)               (#\M          constituent)
                         (#\N          constituent)               (#\O          constituent)
                         (#\P          constituent)               (#\Q          constituent)
                         (#\R          constituent)               (#\S          constituent)
                         (#\T          constituent)               (#\U          constituent)
                         (#\V          constituent)               (#\W          constituent)
                         (#\X          constituent)               (#\Y          constituent)
                         (#\Z          constituent)               (#\[          constituent)
                         (#\\          single-escape)             (#\]          constituent)
                         (#\^          constituent)               (#\_          constituent)
                         (#\`          terminating-macro-char)    (#\a          constituent)
                         (#\b          constituent)               (#\c          constituent)
                         (#\d          constituent)               (#\e          constituent)
                         (#\f          constituent)               (#\g          constituent)
                         (#\h          constituent)               (#\i          constituent)
                         (#\j          constituent)               (#\k          constituent)
                         (#\l          constituent)               (#\m          constituent)
                         (#\n          constituent)               (#\o          constituent)
                         (#\p          constituent)               (#\q          constituent)
                         (#\r          constituent)               (#\s          constituent)
                         (#\t          constituent)               (#\u          constituent)
                         (#\v          constituent)               (#\w          constituent)
                         (#\x          constituent)               (#\y          constituent)
                         (#\z          constituent)               (#\{          constituent)
                         (#\|          multiple-escape)           (#\}          constituent)
                         (#\~          constituent)               (#\Rubout     constituent)))

(defvar *char-traits* '((#\Backspace . (invalid))                                 (#\Tab      . (invalid))
                        (#\Newline   . (invalid))                                 (#\Linefeed . (invalid))
                        (#\Page      . (invalid))                                 (#\Return   . (invalid))
                        (#\Space     . (invalid))                                 (#\!        . (alphabetic))
                        (#\"         . (alphabetic))                              (#\#        . (alphabetic))
                        (#\$         . (alphabetic))                              (#\%        . (alphabetic))
                        (#\&         . (alphabetic))                              (#\'        . (alphabetic))
                        (#\(         . (alphabetic))                              (#\)        . (alphabetic))
                        (#\*         . (alphabetic))                              (#\,        . (alphabetic))
                        (#\0         . (alphadigit))                              (#\1        . (alphadigit))
                        (#\2         . (alphadigit))                              (#\3        . (alphadigit))
                        (#\4         . (alphadigit))                              (#\5        . (alphadigit))
                        (#\6         . (alphadigit))                              (#\7        . (alphadigit))
                        (#\8         . (alphadigit))                              (#\9        . (alphadigit))
                        (#\:         . (package-marker))                          (#\;        . (alphabetic))
                        (#\<         . (alphabetic))                              (#\=        . (alphabetic))
                        (#\>         . (alphabetic))                              (#\?        . (alphabetic))
                        (#\@         . (alphabetic))                              (#\[        . (alphabetic))
                        (#\\         . (alphabetic))                              (#\]        . (alphabetic))
                        (#\^         . (alphabetic))                              (#\_        . (alphabetic))
                        (#\`         . (alphabetic))                              (#\|        . (alphabetic))
                        (#\~         . (alphabetic))                              (#\{        . (alphabetic))
                        (#\}         . (alphabetic))                              (#\+        . (alphabetic plus-sign))
                        (#\-         . (alphabetic minus-sign))                   (#\.        . (alphabetic dot decimal-point))
                        (#\/         . (alphabetic ratio-marker))                 (#\A        . (alphadigit))
                        (#\B         . (alphadigit))                              (#\C        . (alphadigit))
                        (#\D         . (alphadigit double-float exponent-marker)) (#\E        . (alphadigit float exponent-marker))
                        (#\F         . (alphadigit single-float exponent-marker)) (#\G        . (alphadigit))
                        (#\H         . (alphadigit))                              (#\I        . (alphadigit))
                        (#\J         . (alphadigit))                              (#\K        . (alphadigit))
                        (#\L         . (alphadigit long-float exponent-marker))   (#\M        . (alphadigit))
                        (#\N         . (alphadigit))                              (#\O        . (alphadigit))
                        (#\P         . (alphadigit))                              (#\Q        . (alphadigit))
                        (#\R         . (alphadigit))                              (#\S        . (alphadigit short-float exponent-marker))
                        (#\T         . (alphadigit))                              (#\U        . (alphadigit))
                        (#\V         . (alphadigit))                              (#\W        . (alphadigit))
                        (#\X         . (alphadigit))                              (#\Y        . (alphadigit))
                        (#\z         . (alphadigit))                              (#\a        . (alphadigit))
                        (#\b         . (alphadigit))                              (#\c        . (alphadigit))
                        (#\d         . (alphadigit double-float exponent-marker)) (#\e        . (alphadigit float exponent-marker))
                        (#\f         . (alphadigit single-float exponent-marker)) (#\g        . (alphadigit))
                        (#\h         . (alphadigit))                              (#\i        . (alphadigit))
                        (#\j         . (alphadigit))                              (#\k        . (alphadigit))
                        (#\l         . (alphadigit long-float exponent-marker))   (#\m        . (alphadigit))
                        (#\n         . (alphadigit))                              (#\o        . (alphadigit))
                        (#\p         . (alphadigit))                              (#\q        . (alphadigit))
                        (#\r         . (alphadigit))                              (#\s        . (alphadigit short-float exponent-marker))
                        (#\t         . (alphadigit))                              (#\u        . (alphadigit))
                        (#\v         . (alphadigit))                              (#\w        . (alphadigit))
                        (#\x         . (alphadigit))                              (#\y        . (alphadigit))
                        (#\z         . (alphadigit))                              (#\Rubout   . (invalid))))

(defun char-traits (char)
  (cdr (assoc char *char-traits*)))

(defun whitespacep (char)
  (equal 'whitespace (cadr (assoc char *syntax-types*))))

(defun constituentp (char)
  (equal 'constituent (cadr (assoc char *syntax-types*))))

(defun single-escape-p (char)
  (equal 'single-escape (cadr (assoc char *syntax-types*))))

(defun multiple-escape-p (char)
  (equal 'multiple-escape (cadr (assoc char *syntax-types*))))

(defun invalidp (char)
  (member 'invalid (cdr (assoc char *char-traits*))))

(defun plus-sign-p (char)
  (member 'plus-sign (cdr (assoc char *char-traits*))))

(defun minus-sign-p (char)
  (member 'minus-sign (cdr (assoc char *char-traits*))))

(defun signp (char)
  (or (plus-sign-p char)
      (minus-sign-p char)))

(defun decimal-dot-p (char)
  (member 'decimal-point (cdr (assoc char *char-traits*))))

(defun non-terminating-macro-char-p (char)
  (equal 'non-terminating-macro-char (cadr (assoc char *syntax-types*))))

(defun terminating-macro-char-p (char)
  (equal 'terminating-macro-char (cadr (assoc char *syntax-types*))))

(defun maybe-change-case (char)
  ;; TODO: implement readcase here
  char)

(defvar *read-base* 10)

(defun digits-for-radix (radix)
  "returns the valid char digits for the given radix.  Radix can be
  between 2 and 36 and the char values are 0-9, A-Z (case
  insensitive)."
  (unless (and (>= radix 2) (<= radix 36))
    (error "invalid radix"))
  (let ((all-digits '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
                      #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J
                      #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T
                      #\U #\V #\W #\X #\Y #\Z)))
    (subseq all-digits 0 radix)))

(defun digit-char (weight &optional (radix 10))
  (nth weight (digits-for-radix radix)))

(defun char-upcase (char)
  (unless (characterp char)
    ;;TODO: this should throw a type-error
    (error "char is not of type character"))
  (let ((cons (assoc char '((#\a . #\A) (#\b . #\B) (#\c . #\C) (#\d . #\D) (#\e . #\E)
                            (#\f . #\F) (#\g . #\G) (#\h . #\H) (#\i . #\I) (#\j . #\J)
                            (#\k . #\K) (#\l . #\L) (#\m . #\M) (#\n . #\N) (#\o . #\O)
                            (#\p . #\P) (#\q . #\Q) (#\r . #\R) (#\s . #\S) (#\t . #\T)
                            (#\u . #\U) (#\v . #\V) (#\w . #\W) (#\x . #\X) (#\y . #\Y)
                            (#\z . #\Z)))))
    (if cons
        (cdr cons)
        char)))

(defun char-downcase (char)
  (unless (characterp char)
    ;;TODO: this should throw a type-error
    (error "char is not of type character"))
  (let ((cons (assoc char '((#\A . #\a) (#\B . #\b) (#\C . #\c) (#\D . #\d) (#\E . #\e)
                            (#\F . #\f) (#\G . #\g) (#\H . #\h) (#\I . #\i) (#\J . #\j)
                            (#\K . #\k) (#\L . #\l) (#\M . #\m) (#\N . #\n) (#\O . #\o)
                            (#\P . #\p) (#\Q . #\q) (#\R . #\r) (#\S . #\s) (#\T . #\t)
                            (#\U . #\u) (#\V . #\v) (#\W . #\w) (#\X . #\x) (#\Y . #\y)
                            (#\Z . #\z)))))
    (if cons
        (cdr cons)
        char)))

(defun digit-char-p (char &optional (radix 10))
  (position (char-upcase char) (digits-for-radix radix)))

(defun make-integer (sign digits radix)
  (let ((int 0))
    (loop
       (unless digits
         (return (if (minus-sign-p sign)
                     (- int)
                     int)))
       (setq int (+ (* radix int)
                    (digit-char-p (car digits) radix)))
       (pop digits))))

(defun make-float (sign major-digits minor-digits exponent-sign exponent-digits)
  (let* ((major (make-integer #\+ major-digits 10))
         (minor 0.0)
         (minor-digits (reverse minor-digits))
         (exp (if exponent-digits
                  (make-integer exponent-sign exponent-digits 10)
                  0)))
    (loop
       (unless minor-digits
         (return (let ((num (* (+ major (* .1 minor))
                               (expt 10 exp))))
                   (if (minus-sign-p sign)
                       (- num)
                       num))))
       (setq minor (+ (* minor .1)
                      (digit-char-p (car minor-digits) 10)))
       (pop minor-digits))))

(defun maybe-eat-sign (token)
  (cond ((signp (car token))
         (values (car token)
                 (cdr token)))
        (t (values #\+
                   token))))

(defun eat-dot (token)
  (cond ((decimal-dot-p (car token))
         (values (car token)
                 (cdr token)))
        (t (values nil token))))

(defun eat-one-digit (token radix)
  (if (digit-char-p (car token) radix)
      (values (car token)
              (cdr token))
      (values nil token)))

(defun eat-zero-or-more-digits (token radix)
  (let ((digits nil))
    (loop
       (cond ((not token)
              (return (values (reverse digits) token)))
             ((digit-char-p (car token) radix)
              (push (car token) digits)
              (pop token))
             (t
              (return (values (reverse digits) token)))))))

(defun eat-one-or-more-digits (token radix)
  (multiple-value-bind (digit token)
      (eat-one-digit token radix)
    (if digit
        (multiple-value-bind (digits token)
            (eat-zero-or-more-digits token radix)
          (values (cons digit digits)
                  token))
        (values nil token))))

(defun integer-token-p (token)
  (or (multiple-value-bind (sign token)
          (maybe-eat-sign token)
        (multiple-value-bind (digits token)
            (eat-one-or-more-digits token 10)
          (when digits
            (multiple-value-bind (dot token)
                (eat-dot token)
              (when (and dot (not token))
                (make-integer sign digits 10))))))
      (multiple-value-bind (sign token)
          (maybe-eat-sign token)
        (multiple-value-bind (digits token)
            (eat-one-or-more-digits token *read-base*)
          (when (and digits (null token))
            (make-integer sign digits *read-base*))))))

(defun eat-exponent (token)
  (let ((orig-token token))
    (cond ((null token)
           (values nil nil orig-token))
          ((or (char-equal #\e (car token))
               (char-equal #\E (car token)))
           (let ((token (cdr token)))
             (multiple-value-bind (sign token)
                 (maybe-eat-sign token)
               (multiple-value-bind (digits token)
                   (eat-one-or-more-digits token 10)
                 (when (and digits (null token))
                   (values sign digits token))))))
          (t (values nil nil orig-token)))))

(defun maybe-eat-dot-and-optional-decimal-digits (token)
  (multiple-value-bind (dot token)
      (eat-dot token)
    (if dot
        (multiple-value-bind (digits token)
            (eat-zero-or-more-digits token 10)
          (values digits token))
        (values nil token))))

(defun float-token-method-1-p (token)
  (multiple-value-bind (sign token)
      (maybe-eat-sign token)
    (multiple-value-bind (major-digits token)
        (eat-zero-or-more-digits token 10)
      (multiple-value-bind (dot token)
          (eat-dot token)
        (when dot
          (multiple-value-bind (minor-digits token)
              (eat-one-or-more-digits token 10)
            (when minor-digits
              (multiple-value-bind (expt-sign expt-digits token)
                  (eat-exponent token)
                (when (null token)
                  (make-float sign
                              major-digits
                              minor-digits
                              expt-sign expt-digits))))))))))

(defun float-token-method-2-p (token)
  (multiple-value-bind (sign token)
      (maybe-eat-sign token)
    (multiple-value-bind (major-digits token)
        (eat-one-or-more-digits token 10)
      (when major-digits
        (multiple-value-bind (minor-digits token)
            (maybe-eat-dot-and-optional-decimal-digits token)
          (multiple-value-bind (expt-sign expt-digits token)
              (eat-exponent token)
            (when (and expt-sign expt-digits (null token))
              (make-float sign
                          major-digits
                          minor-digits
                          expt-sign expt-digits))))))))

(defun float-token-p (token)
  ;; http://www.lispworks.com/documentation/HyperSpec/Body/02_cbb.htm
  (or (float-token-method-1-p token)
      (float-token-method-2-p token)))

(defun ratio-token-p (token)
  ;; TODO implement ratios
  (error "implement me!"))

(defun numeric-token-p (token)
  ;; http://www.lispworks.com/documentation/HyperSpec/Body/02_ca.htm
  (or (integer-token-p token)
      (float-token-p token)
      #+nil(ratio-token-p token)))

(defun symbol-token-p (token)
  ;; http://www.lispworks.com/documentation/HyperSpec/Body/02_cd.htm
  (make-symbol (concatenate 'string token)))

(defun token-to-object (token)
  ;; valid patterns for tokens
  ;; http://www.lispworks.com/documentation/HyperSpec/Body/02_ce.htm
  (or (numeric-token-p token)
      (symbol-token-p token)))

(defun read (&optional (stream *standard-input*) (eof-error-p t) eof-value recursive-p)
  ;; the reader algorithm is defined here:
  ;; http://www.lispworks.com/documentation/HyperSpec/Body/02_b.htm
  ;;
  ;; Another useful link:
  ;; http://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node190.html
  (let ((state 1)
        (x nil)
        (token-chars nil))
    (loop
       (case state
         ;; 1. If at end of file, end-of-file processing is performed as
         ;; specified in read. Otherwise, one character, x, is read from
         ;; the input stream, and dispatched according to the syntax
         ;; type of x to one of steps 2 to 7.
         (1 (cond ((eofp stream)
                   (if eof-error-p
                       (throw-eof-error)
                       (return eof-value)))
                  (t (setf x (read-char stream nil nil))
                     (setq state 2))))
         ;; 2. If x is an invalid character, an error of type
         ;; reader-error is signaled.
         (2 (cond ((and (not (whitespacep x))
                        (invalidp x))
                   (throw-reader-error))
                  (t (setq state 3))))
         ;; 3. If x is a whitespace[2] character, then it is discarded
         ;; and step 1 is re-entered.
         (3 (cond ((whitespacep x)
                   (setq state 1))
                  (t (setq state 4))))
         ;; 4. If x is a terminating or non-terminating macro
         ;; character then its associated reader macro function is
         ;; called with two arguments, the input stream and x.
         (4 (let ((macro-character-function (get-macro-character x)))
              (cond (macro-character-function
                     ;; The reader macro function may return zero
                     ;; values or one value. If one value is returned,
                     ;; then that value is returned as the result of
                     ;; the read operation; the algorithm is done. If
                     ;; zero values are returned, then step 1 is
                     ;; re-entered.
                     (let ((values (multiple-value-list (funcall macro-character-function stream x))))
                       (cond ((null values)
                              (setq state 1))
                             (t ;; at least one value was returned
                              (return (car values))))))
                    (t (setq state 5)))))
         ;; 5. If x is a single escape character then the next
         ;; character, y, is read, or an error of type end-of-file is
         ;; signaled if at the end of file. y is treated as if it is a
         ;; constituent whose only constituent trait is
         ;; alphabetic[2]. y is used to begin a token, and step 8 is
         ;; entered.
         (5 (cond ((single-escape-p x)
                   (let ((y (read-char stream nil nil)))
                     ;; TODO throw a real end-of-file error
                     (unless y (throw-eof-error))
                     ;; TODO do more stuff
                     (push y token-chars)
                     (setq state 8)))
                  (t (setq state 6))))
         ;; 6. If x is a multiple escape character then a token
         ;; (initially containing no characters) is begun and step 9
         ;; is entered.
         (6 (cond ((multiple-escape-p x)
                   ;; TODO: implement multiple escape characters
                   (error "implement me!"))
                  (t (setq state 7))))
         ;; 7. If x is a constituent character, then it begins a
         ;; token. After the token is read in, it will be interpreted
         ;; either as a Lisp object or as being of invalid syntax. If
         ;; the token represents an object, that object is returned as
         ;; the result of the read operation. If the token is of
         ;; invalid syntax, an error is signaled. If x is a character
         ;; with case, it might be replaced with the corresponding
         ;; character of the opposite case, depending on the readtable
         ;; case of the current readtable, as outlined in Section
         ;; 23.1.2 (Effect of Readtable Case on the Lisp Reader). X is
         ;; used to begin a token, and step 8 is entered.
         (7 (cond ((constituentp x)
                   (push (maybe-change-case x) token-chars)
                   (setq state 8))
                  (t (error "how did we get here?"))))
         ;; 8. At this point a token is being accumulated, and an even
         ;; number of multiple escape characters have been
         ;; encountered. If at end of file, step 10 is
         ;; entered. Otherwise, a character, y, is read, and one of
         ;; the following actions is performed according to its syntax
         ;; type:
         (8 (let ((y (read-char stream nil nil)))
              (cond
                ((not y)
                 (setq state 10))
                ;; If y is a constituent or non-terminating macro
                ;; character:
                ((or (constituentp y)
                     (non-terminating-macro-char-p y))
                 ;; -- If y is a character with case, it might be
                 ;; replaced with the corresponding character of the
                 ;; opposite case, depending on the readtable case of
                 ;; the current readtable, as outlined in Section
                 ;; 23.1.2 (Effect of Readtable Case on the Lisp
                 ;; Reader).
                 ;; -- Y is appended to the token being built.
                 (push (maybe-change-case y) token-chars)
                 ;; -- Step 8 is repeated. 
                 (setq state 8))
                ;; If y is a single escape character, then the next
                ;; character, z, is read, or an error of type
                ;; end-of-file is signaled if at end of file. Z is
                ;; treated as if it is a constituent whose only
                ;; constituent trait is alphabetic[2]. Z is appended
                ;; to the token being built, and step 8 is repeated.
                ((single-escape-p y)
                 (let ((z (read-char stream nil nil)))
                   (unless z (throw-eof-error))
                   (push z token-chars)
                   (setq state 8)))
                ;; If y is a multiple escape character, then step 9 is
                ;; entered.
                ((multiple-escape-p y)
                 (setq state 9))
                ;; If y is an invalid character, an error of type
                ;; reader-error is signaled.
                ((invalidp y)
                 (throw-reader-error))
                ;; If y is a terminating macro character, then it
                ;; terminates the token. First the character y is
                ;; unread (see unread-char), and then step 10 is
                ;; entered.
                ((terminating-macro-char-p y)
                 (unread-char y stream)
                 (setq state 10))
                ;;  If y is a whitespace[2] character, then it
                ;;  terminates the token. First the character y is
                ;;  unread if appropriate (see
                ;;  read-preserving-whitespace), and then step 10 is
                ;;  entered.
                ((whitespacep y)
                 ;; TODO: maybe preserve whitespace here.
                 (unread-char y stream)
                 (setq state 10))
                (t (error "this shouldnt happen")))))
         ;; 9. At this point a token is being accumulated, and an odd
         ;; number of multiple escape characters have been
         ;; encountered. If at end of file, an error of type
         ;; end-of-file is signaled. Otherwise, a character, y, is
         ;; read, and one of the following actions is performed
         ;; according to its syntax type:
         (9 (let ((y (read-char stream nil nil)))
              (cond ((not y)
                     (throw-eof-error))
                    ;; If y is a constituent, macro, or whitespace[2]
                    ;; character, y is treated as a constituent whose
                    ;; only constituent trait is alphabetic[2]. Y is
                    ;; appended to the token being built, and step 9
                    ;; is repeated.
                    ((or (constituentp y)
                         (get-macro-character y)
                         (whitespacep y))
                     (push y token-chars)
                     (setq state 9))
                    ;; If y is a single escape character, then the
                    ;; next character, z, is read, or an error of type
                    ;; end-of-file is signaled if at end of file. Z is
                    ;; treated as a constituent whose only constituent
                    ;; trait is alphabetic[2]. Z is appended to the
                    ;; token being built, and step 9 is repeated.
                    ((single-escape-p y)
                     (let ((z (read-char stream nil nil)))
                       (unless z (throw-eof-error))
                       (push z token-chars)
                       (setq state 9)))
                    ;; If y is a multiple escape character, then step
                    ;; 8 is entered.
                    ((multiple-escape-p y)
                     (setq state 8))
                    ;; If y is an invalid character, an error of type
                    ;; reader-error is signaled.
                    ((invalidp y)
                     (throw-reader-error))
                    (t (error "this shouldnt happen")))))
         ;; 10. An entire token has been accumulated. The object
         ;; represented by the token is returned as the result of the
         ;; read operation, or an error of type reader-error is
         ;; signaled if the token is not of valid syntax.
         (10 (return (token-to-object (reverse token-chars))))))))

(defun read-from-string (string &optional (eof-error-p t) eof-value &key (start 0) end preserve-whitespace)
  (declare (ignore preserve-whitespace))
  (let ((stream (make-string-input-stream string start end)))
    (read stream eof-error-p eof-value)))
