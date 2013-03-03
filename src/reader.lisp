(in-package :lunula)

;;;; constituent traits ;;;;
(defvar *constituent-traits* '(alphabetic digit package-marker plus-sign minus-sign dot decimal-point ratio-marker exponent-marker invalid))

(defvar *syntax-types* '((#\Backspace  . constituent)               (#\Tab        . whitespace)
                         (#\Newline    . whitespace)                (#\Linefeed   . whitespace)
                         (#\Page       . whitespace)                (#\Return     . whitespace)
                         (#\Space      . whitespace)                (#\!          . constituent)
                         (#\"          . terminating-macro-char)    (#\#          . non-terminating-macro-char)
                         (#\$          . constituent)               (#\%          . constituent)
                         (#\&          . constituent)               (#\'          . terminating-macro-char)
                         (#\(          . terminating-macro-char)    (#\)          . terminating-macro-char)
                         (#\*          . constituent)               (#\+          . constituent)
                         (#\,          . terminating-macro-char)    (#\-          . constituent)
                         (#\.          . constituent)               (#\/          . constituent)
                         (#\0          . constituent)               (#\1          . constituent)
                         (#\2          . constituent)               (#\3          . constituent)
                         (#\4          . constituent)               (#\5          . constituent)
                         (#\6          . constituent)               (#\7          . constituent)
                         (#\8          . constituent)               (#\9          . constituent)
                         (#\:          . constituent)               (#\;          . terminating-macro-char)
                         (#\<          . constituent)               (#\=          . constituent)
                         (#\>          . constituent)               (#\?          . constituent)
                         (#\@          . constituent)               (#\A          . constituent)
                         (#\B          . constituent)               (#\C          . constituent)
                         (#\D          . constituent)               (#\E          . constituent)
                         (#\F          . constituent)               (#\G          . constituent)
                         (#\H          . constituent)               (#\I          . constituent)
                         (#\J          . constituent)               (#\K          . constituent)
                         (#\L          . constituent)               (#\M          . constituent)
                         (#\N          . constituent)               (#\O          . constituent)
                         (#\P          . constituent)               (#\Q          . constituent)
                         (#\R          . constituent)               (#\S          . constituent)
                         (#\T          . constituent)               (#\U          . constituent)
                         (#\V          . constituent)               (#\W          . constituent)
                         (#\X          . constituent)               (#\Y          . constituent)
                         (#\Z          . constituent)               (#\[          . constituent)
                         (#\\          . single-escape)             (#\]          . constituent)
                         (#\^          . constituent)               (#\_          . constituent)
                         (#\`          . terminating-macro-char)    (#\a          . constituent)
                         (#\b          . constituent)               (#\c          . constituent)
                         (#\d          . constituent)               (#\e          . constituent)
                         (#\f          . constituent)               (#\g          . constituent)
                         (#\h          . constituent)               (#\i          . constituent)
                         (#\j          . constituent)               (#\k          . constituent)
                         (#\l          . constituent)               (#\m          . constituent)
                         (#\n          . constituent)               (#\o          . constituent)
                         (#\p          . constituent)               (#\q          . constituent)
                         (#\r          . constituent)               (#\s          . constituent)
                         (#\t          . constituent)               (#\u          . constituent)
                         (#\v          . constituent)               (#\w          . constituent)
                         (#\x          . constituent)               (#\y          . constituent)
                         (#\z          . constituent)               (#\{          . constituent)
                         (#\|          . multiple-escape)           (#\}          . constituent)
                         (#\~          . constituent)               (#\Rubout     . constituent)))

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

(defun syntax-type (char)
  (cdr (assoc char *syntax-types*)))

(defun whitespacep (char)
  (eq 'whitespace (syntax-type char)))

(defun constituentp (char)
  (eq 'constituent (syntax-type char)))

(defun single-escape-p (char)
  (eq 'single-escape (syntax-type char)))

(defun multiple-escape-p (char)
  (eq 'multiple-escape (syntax-type char)))

(defun package-marker-p (char)
  (member 'package-marker (char-traits char)))

(defun package-marker ()
  (caar (member 'package-marker *char-traits*
                :key #'cdr
                :test #'member)))

(defun exponent-marker-p (char)
  (member 'exponent-marker (char-traits char)))

(defun ratio-marker-p (char)
  (member 'ratio-marker (char-traits char)))

(defun invalidp (char)
  (member 'invalid (char-traits char)))

(defun plus-sign-p (char)
  (member 'plus-sign (char-traits char)))

(defun minus-sign-p (char)
  (member 'minus-sign (char-traits char)))

(defun signp (char)
  (or (plus-sign-p char)
      (minus-sign-p char)))

(defun decimal-dot-p (char)
  (member 'decimal-point (char-traits char)))

(defun non-terminating-macro-char-p (char)
  (eq 'non-terminating-macro-char (syntax-type char)))

(defun terminating-macro-char-p (char)
  (eq 'terminating-macro-char (syntax-type char)))

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

(defun upper-case-p (char)
  (let ((upcase (char-upcase char))
        (downcase (char-downcase char)))
    (and (not (char= upcase downcase))
       (char= char upcase))))

(defun lower-case-p (char)
  (let ((upcase (char-upcase char))
        (downcase (char-downcase char)))
    (and (not (char= upcase downcase))
       (char= char downcase))))

(defun both-case-p (char)
  (let ((upcase (char-upcase char))
        (downcase (char-downcase char)))
    (not (char= upcase downcase))))

(defun alpha-char-p (char)
  (when (or (upper-case-p char)
            (lower-case-p char))
    t))

(defun maybe-change-case (char)
  (case (readtable-case *read-table*)
    (:upcase (char-upcase char))
    (:downcase (char-downcase char))
    (:preserve char)
    (:invert (cond ((upper-case-p char)
                    (char-downcase char))
                   ((lower-case-p char)
                    (char-upcase char))
                   (t char)))
    (t (error "invalid read case"))))

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

(defun make-float (sign major-digits minor-digits exponent-char exponent-sign exponent-digits)
  (declare (ignore exponent-char))
  ;; exponent-char determines the size of the float. For now we just
  ;; make it one size.
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

(defun make-ratio (sign numerator-digits denominator-digits)
  (let ((numerator (make-integer sign numerator-digits *read-base*))
        (denominator (make-integer #\+ denominator-digits *read-base*)))
    ;; For now we do not support ratios.  We just convert them into
    ;; floats.
    (/ (* numerator 1.0)
       (* denominator 1.0))))

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
           (values nil nil nil orig-token))
          ((exponent-marker-p (car token))
           (let ((expt-char (car token))
                 (token (cdr token)))
             (multiple-value-bind (sign token)
                 (maybe-eat-sign token)
               (multiple-value-bind (digits token)
                   (eat-one-or-more-digits token 10)
                 (when (and digits (null token))
                   (values expt-char sign digits token))))))
          (t (values nil nil nil orig-token)))))

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
              (multiple-value-bind (expt-char expt-sign expt-digits token)
                  (eat-exponent token)
                (when (null token)
                  (make-float sign
                              major-digits
                              minor-digits
                              expt-char expt-sign expt-digits))))))))))

(defun float-token-method-2-p (token)
  (multiple-value-bind (sign token)
      (maybe-eat-sign token)
    (multiple-value-bind (major-digits token)
        (eat-one-or-more-digits token 10)
      (when major-digits
        (multiple-value-bind (minor-digits token)
            (maybe-eat-dot-and-optional-decimal-digits token)
          (multiple-value-bind (expt-char expt-sign expt-digits token)
              (eat-exponent token)
            (when (and expt-sign expt-digits (null token))
              (make-float sign
                          major-digits
                          minor-digits
                          expt-char expt-sign expt-digits))))))))

(defun float-token-p (token)
  ;; http://www.lispworks.com/documentation/HyperSpec/Body/02_cbb.htm
  (or (float-token-method-1-p token)
      (float-token-method-2-p token)))

(defun ratio-token-p (token)
  (multiple-value-bind (sign token)
      (maybe-eat-sign token)
    (multiple-value-bind (numerator-digits token)
        (eat-one-or-more-digits token *read-base*)
      (when numerator-digits
        (when (ratio-marker-p (car token))
          (let ((token (cdr token)))
            (multiple-value-bind (denominator-digits token)
                (eat-one-or-more-digits token *read-base*)
              (when (and denominator-digits (null token))
                (make-ratio sign numerator-digits denominator-digits)))))))))

(defun numeric-token-p (token)
  ;; http://www.lispworks.com/documentation/HyperSpec/Body/02_ca.htm
  (or (integer-token-p token)
      (float-token-p token)
      (ratio-token-p token)))

(defun symbol-token-p (token)
  ;; http://www.lispworks.com/documentation/HyperSpec/Body/02_cd.htm
  (let* ((split (split-list token (package-marker)))
         (length-of-split (length split)))
    (case length-of-split
      (1 (intern (concatenate 'string (car split))
                 *package*))
      (2 (multiple-value-bind (package name)
             (values-list split)
           (cond ((and name (null package))
                  (intern (concatenate 'string name)
                          (find-package "KEYWORD")))
                 ((and name package)
                  (let ((symbol-name (concatenate 'string name))
                        (package-name (concatenate 'string package)))
                    (multiple-value-bind (symbol status)
                        (cl:find-symbol symbol-name package-name)
                      (cond ((eq status :external)
                             symbol)
                            (t (error "symbol ~s is not external to package ~s"
                                      symbol-name package-name))))))
                 (t (error "invalid package designator")))))
      (3 (multiple-value-bind (package dummy name)
             (values-list split)
           (cond (dummy
                  (error "invalid symbol"))
                 ((and (null package) name)
                  (error "invalid symbol in the form of ::aaaa"))
                 ((and (null name) package)
                  (error "invalid symbol in the form of aaaa::"))
                 ((and package name)
                  (intern (concatenate 'string name)
                          (find-package (concatenate 'string package))))
                 (t (error "invalid symbol")))))
      (otherwise (error "invalid symbol")))))

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
                   (setq state 9))
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

(defmacro with-input-from-string ((var string &key index (start 0) end) &body body)
  `(let ((,var (make-string-input-stream ,string ,start ,end)))
     ,@body))
