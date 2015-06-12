(load "~/Dropbox/lisp-AI-book/ch-9-memoization/memoization2.lisp")

(defun win (n)
  (or (<= n 3)
      (loss (- n 1))
      (loss (- n 2))
      (loss (- n 3))))

(defun loss (n)
  (not (win n)))

(defun nim (n)
  (cond ((<= n 3) n)
	((loss (- n 3)) 3)
	((loss (- n 2)) 2)
	((loss (- n 1)) 1)
	(t 1)))


(defun moves (s)
  (remove-duplicates
   (loop for n in s append (make-moves n s))
   :test #'equal))

(defun make-moves (n s)
  (when (>= n 2)
    (let ((s/n (remove n s :count 1)))
      (loop for i from 1 to (- (ceiling n 2) 1)
	 collect (sort* (list* i (- n 1) s/n)
			#'>)))))

(defun sort* (seq pred &key key)
  (sort (copy-seq seq) pred :key key))

(defun loss1 (s)
  (let ((choices (moves s)))
    (or (null choices)
	(every #'win choices))))

(defun win (s) (not (loss s)))

(defun grundy (s)
  (let ((choices (moves s)))
    (or (find-if #'loss1 choices)
	(first choices))))


