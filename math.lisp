(in-package :lunula)

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
