(defparameter *p* '(john q public))
(defparameter *names* '((malcom x) (john r pussy) (admiral halold hg) (mis maining sun) (mrs jj kerney hern) (wikii beech)))
(defparameter *titles* '(mr mrs ms sir madam dr admiral major general mis))

(defun last-name (name)
  (first (last name)))

(defun first-name (name)
  (if (member (first name) *titles*)
      (first-name (rest name))
      (first name)))


(defun mappend (fn the-list)
  (apply #'append (mapcar fn the-list)))


(defun self-and-double(x)
  (list x (+ x x)))

(defun number-and-negations(input)
  (mappend #'number-and-negation input))

(defun number-and-negation (x)
  (if (numberp x)
      (list x (- x))
      nil))

(defun last-name2 (name)
   (last (set-difference name *titles*)))

 
(defun pow (x n)
  (cond ((= n 0) 1)
	((evenp n) (expt (pow x (/ n 2)) 2))
	(t (* x ( pow x (- n 1) )))))

(defun count-atom (exp)
  (cond ((null exp) 0)
	((atom exp) 1)
	(t (+ (count-atom (first exp))
	      (count-atom (rest exp))))))

(defun count-all-atom (exp &optional (if-null 1))
  (cond ((null exp) if-null)
	((atom exp ) 1)
	(t (+ (count-all-atom (first exp) 1)
	      (count-all-atom (rest exp) 0)))))
      
(defun count-anywhere (item tree)
  (cond ((eql item tree ) 1)
	((atom tree) 0)
	(t ( + (count-anywhere item (first tree))
	       (count-anywhere item (rest tree))))))
      

(defun dot-product (a b)
  (apply #'+ (mapcar #'* a b)))







