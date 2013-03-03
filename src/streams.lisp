(in-package :lunula)

(defun read-char (&optional (stream *standard-input*) (eof-error-p t) eof-value recursive-p)
  (funcall stream :read-char stream eof-error-p eof-value recursive-p))

(defun unread-char (character &optional (stream *standard-input*))
  (funcall stream :unread-char character stream))

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
        (:unread-char (apply (lambda (character stream)
                               (declare (ignore character stream))
                               (decf index)
                               nil)
                             args))
        (:read-char (apply (lambda (stream eof-error-p eof-value recursive-p)
                             (declare (ignore stream recursive-p))
                             (cond ((>= index length)
                                    (if eof-error-p
                                        (throw-eof-error)
                                        eof-value))
                                   (t (prog1
                                          (char string index)
                                        (incf index)))))
                           args))
        (:peek-char (apply (lambda (peek-type stream eof-error-p eof-value recursive-p)
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

(defvar *standard-input* nil)
(defvar *standard-output* (make-string-output-stream))
