(defparameter *f* ((lambda (x)
                        ((lambda (y)
	                        (+ x y))
                           (* x x)))
                        5))

(defvar list1 '(1 2 3 4))
(let ((result (first list1)))
  (setf list1 (rest list1))
  result)


(defun length1 (list)
  (let ((len 0))
    (dolist (element list)
      (incf len))
    len))
(defun length1.1 (list)
  (let ((len 0))
    (dolist (element list len))
    (incf len)))
(defun length2 (list)
  (let ((len 0))
    (mapc #'(lambda (element)
	      (incf len))
	  list)
    len))

(defun length3 (list)
  (do ((len 0 (+ len 1))
       (l list (rest l)))
      ((null l) len)))

(defun length4 (list)
  (loop for elem in list
       count t))

(defun length5 (list)
  (loop for elem in list
       summing 1))


(defun length7 (list)
  (count-if #'true list))

(defun true (x) t)

(defun length8 (list)
  (if (null list)
      0
      (+ 1 (position-if #'true list :from-end t))))

(defun length9 (list)
  (if (null list)
      0
      (+ 1 (length9 (rest list)))))

(defun length10 (list)
  (length10-aux list 0))

(defun length10-aux (sublist len-so-far)
  (if (null sublist)
      len-so-far
      (length10-aux (rest sublist) (+ 1 len-so-far))))

(defun length11 (list &optional (len-so-far 0))
  (if (null list)
      len-so-far
      (length11 (rest list) (+ 1 len-so-far))))

(defun length12 (the-list)
  (labels
      ((length13 (list len-so-far)
	 (if (null list)
	     len-so-far
	     (length13 (rest list) (+ 1 len-so-far)))))
    (length13 the-list 0)))

(defvar i 0)
(defmacro while0.1o (test &rest body)
  "repeat body while test is true"
  (list* 'loop
	 (list 'unless test '(return nil))
	 body))

(defmacro while1.1 (test &rest body)
  "repeat body while test is true"
  (let ((code '(loop (unless test (return nil)) . body)))
    (subst test 'test (subst body 'body code))))


(defmacro while (test &rest body)
  "repeat body while test is true"
  `(loop (unless ,test (return nil))
      ,@body))

;;solution 3.4
(defun dprint (x)
  "print an expression in dotted pair notation."
  (cond ((atom x) (princ x))
	(t (princ "(")
	   (dprint (first x))
	   (pr-rest2 (rest x))
	   (princ ")")
	   x)))

(defun pr-rest (x)
  (princ " . ")
  (dprint x))

(defun pr-rest2 (x)
  (cond ((null x))
	((atom x) (princ " . "))
	(t (princ " " ) (dprint (first x)) (pr-rest2 (rest x)))))

;;solution 3.5

(defstruct node
  name
  (yes nil)
  (no nil))

(defvar *db*
  (make-node :name 'animal
	     :yes (make-node :name 'mamal)
	     :no (make-node
		  :name 'vegetable
		  :no (make-node :name 'mineral))))

(defun questions (&optional (node *db*))
  (format t "~&Is it s ~a?" (node-name node))
  (case (read)
    ((y yes) (if (not (null (node-yes node)))
		 (questions (node-yes node))
		 (setf (node-yes node)(give-up ))))
    ((n no) (if (not (null (node-no node)))
		(questions (node-no node))
		(setf (node-no node) (give-up))))
    (it 'aha!)
    (t (format t "Reply with YES ,NO, or It if I have guessed it.")
       (questions node))))

(defun give-up ()
  (format t "~&I give up - what is it?" )
  (make-node :name (read)))


;;debug

(defun average (numbers)
  (if (null numbers)
      (error "average of empy list is undefined")
      (/ (reduce #'+ numbers) (length numbers))))

(defun average2 (numbers)
  (if (null numbers)
      (progn
	(cerror "Use 0 as the average"
		"Average of the empty list is udefined")
	0)
      (/ (reduce #'+ numbers )
	 (length numbers))))

(defun sqr (x)
  "mmultiply x by itself"
  (check-type x number)
  (* x x))

(defun sqr2 (x )
  (assert (numberp x))
  (* x x))

(defun sqr3 (x)
  (assert (numberp x) (x)
	  )
  (* x x))

(defun test-ex ()
  "test the program EX on a series  of example"
  (init-ex);初期化
  (assert (equal (ex 3 4) 5))
  (assert (equal (ex 5 0) 0))
  (assert (equal (ex 'x 0) 0 )))

(defun bank-account (balance)
  "open a bank account starting with the given balance"
  #'(lambda (action amount)
      (case action
	(deposit (setf balance (+ balance amount)))
	(withdraw (setf balance (- balance amount))))))


(defun show-both (x)
  (multiple-value-bind (int rem)
      (round x)
    (format t "~f = ~d + ~f" x int rem)))

(defun math-quiz (&optional (op '+) (range 100) (n 10) )
  "ask the users a series of math problems."
  (dotimes (i n)
    (problem (random range) op (random range))))

(defun problem (x op y)
  "ask a math problem, read a reply and say if it is correcy"
  (format t "~&How much is ~d ~a ~d? " x op y)
  (if (eql (read) (funcall op x y))
      (princ "Correct!!")
      (princ "sorry that's not right.")))

(defun find-all (item sequence &rest keyword-args
		 &key  (test #'eql) test-not &allow-other-keys)
  "find all those elements of sequence that match item ,
   according to the keywords. Doesnt alter sequence."
  (if test-not
      (apply #'remove item sequence
	     :test-not (complement test-not) keyword-args)
      (apply #'remove item sequence
	     :test (complement test) keyword-args)))

(setf (symbol-function 'find-all-if) #'remove-if-not)

;;tutor 3.9

(defun length-r (list)
  (reduce #'(lambda (x y) (+ x 1)) list
	  :initial-value 0))

(defun length-r2 (list)
  (reduce #'+ (mapcar #'(lambda (x) 1) list)))
