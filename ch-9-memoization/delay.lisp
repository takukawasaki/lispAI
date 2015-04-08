(load "~/Dropbox/lisp-AI-book/ch-8-make-it-easy/simplifier.lisp")


(defstruct delay
  (value    nil)
  (function nil))

(defmacro delay (&rest body)
  `(make-delay :function #'(lambda () . ,body)))

(defun force (x)
  (if (not (delay-p x))
      x
      (progn
	(when (delay-function x)
	  (setf (delay-value x)
		(funcall (delay-function x)))
	  (setf (delay-function x) nil))
	(delay-value x))))

(defmacro make-pipe (head tail)
  `(cons ,head #'(lambda () ,tail)))

(defconstant empty-pipe nil)

(defun head (pipe)
  (first pipe))

(defun tail (pipe)
  (if (functionp (rest pipe))
      (setf (rest pipe) (funcall (rest pipe)))
      (rest pipe)))


(defun pipe-elt (pipe i)
  (if (= i 0)
      (head pipe)
      (pipe-elt (tail pipe) (- i 1))))

(defun integers (&optional (start 0) end)
  (if (or (null end) (<= start end))
      (make-pipe start (integers (+ start 1) end))
      nil))

(defun enumerate (pipe &key count key (result pipe))
  (if (or (eq pipe empty-pipe) (eql count 0))
      result
      (progn
	(unless (null key) (funcall key (head pipe)))
	(enumerate (tail pipe) :count (if count (- count 1))
		   :key key :result result))))

(defun filter (pred pipe)
  (if (funcall pred (head pipe))
      (make-pipe (head pipe)
		 (filter pred (tail pipe)))
      (filter pred (tail pipe))))

(defun sieve (pipe)
  (make-pipe (head pipe)
	     (filter #'(lambda (x) (/= (mod x (head pipe)) 0))
		     (sieve (tail pipe)))))

(defvar *primes* (sieve (integers 2)))

(defun map-pipe (fn pipe)
  (if (eq pipe empty-pipe)
      empty-pipe
      (make-pipe (funcall fn (head pipe))
		 (map-pipe fn (tail pipe)))))

(defun append-pipes (x y)
  (if (eq x empty-pipe)
      y
      (make-pipe (head x)
		 (append-pipes (tail x) y))))

(defun mappend-pipe (fn pipe)
  (if (eq pipe empty-pipe)
      empty-pipe
      (let ((x (funcall fn (head pipe))))
	(make-pipe (head x)
		   (append-pipes (tail x)
				 (mappend-pipe
				  fn (tail pipe)))))))

(defun generate-all (phrase)
  (if (listp phrase)
      (if (null phrase)
	  (list nil)
	  (combine-all-pipes
	   (generate-all (first phrase))
	   (generate-all (rest phrase))))
      (let ((choices  (rule-rhs (assoc phrase *grammar*))))
	(if choices
	    (mappend-pipe #'generate-all choices)
	    (list (list phrase))))))

(defun combine-all-pipes (xpipe ypipe)
  (mappend-pipe
   #'(lambda (y)
       (map-pipe #'(lambda (x) (append-pipes x y))
		 xpipe))
   ypipe))




