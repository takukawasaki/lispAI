 
(defun qsort (L)
  (let* ((L0 (first L))
	 (L1 (rest L))
	 (fn (lambda (a) (< a L0))))
    (if (null L)
	nil
	(append (qsort (remove-if-not #'fn L1))
		(list L0)
		(qsort (remove-if #'fn L1))))))



