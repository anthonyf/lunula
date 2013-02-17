(in-package :lunula)

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

(defun split-list (list token &optional (key #'identity) (test #'eql))
  (let ((splits nil)
        (current nil))
    (dolist (item list)
      (cond ((funcall test
                      (funcall key item)
                      token)
             (push (reverse current) splits)
             (setq current nil))
            (t (push item current))))
    (push (reverse current) splits)
    (reverse splits)))

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
