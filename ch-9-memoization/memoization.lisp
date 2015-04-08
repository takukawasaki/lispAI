(defun fib (n)
  (cond ((= n 0) 1)
	((= n 1) 1)
	(t (+ (fib (- n 1))
	      (fib (- n 2))))))


(defun memo (fn)
  "Return a memo-function of fn."
  (let ((table (make-hash-table)))
    #'(lambda (x)
	(multiple-value-bind (val found-p)
	    (gethash x table)
	  (if found-p
	      val
	      (setf (gethash x table) (funcall fn x)))))))

(defun memoize (fn-name)
  (setf (symbol-function fn-name) (memo (symbol-function fn-name))))
