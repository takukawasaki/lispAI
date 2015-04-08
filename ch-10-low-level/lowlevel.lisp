(defun sum-squares00 (seq)
  (let ((sum 0))
    (dotimes (i (length seq))
      (incf sum (square (elt seq i))))
    sum))

(defun square (x) (* x x))

(defun sum-squares (seq)
  (declare (type the list  seq)
	   (inline square) (optimize speed (safety 0)))
  (let ((sum 0))
    (declare (fixnum sum))
    (dotimes (i (length seq))
      (declare (fixnum i))
      (incf sum (the fixnum (square (elt seq i)))))
    sum))

  
(defun f (x y)
  (declare (fixnum x y) (optimize (safety 0) (speed 3)))
  (the fixnum (+ x y)))

(defun g (x y)
  (+ x y))


(defun random-elt (s) (elt s (random (length s))))

(defun random-mem (l) (nth (random (length (the list l))) l))

(defun reg (a b c d) (list a b c d))

(defun rst (a b c &rest d) (list* a b c d))

(defun opt (&optional a b (c 1) (d 4)) (list a b c d))

(defun key (&key a b (c 1) (d (sqrt a))) (list a b c d))


(let* ((vars (make-array 10 :fill-pointer 0 :adjustable t))
       (vals (make-array 10 :fill-pointer 0 :adjustable t))
       (success (cons vars vals))))

(defun efficient-pat-match00 (pattern input)
  (setf (fill-pointer vars) 0)
  (setf (fill-pointer vals) 0)
  (pat-match-1 pattern input))

(defun pat-match-01 (pattern input)
  (cond ((variable-p pattern) (match-var pattern input))
	((eql pattern input) success)
	((and (consp pattern) (consp input))
	 (and (pat-match-1 (first pattern) (first input))
	      (pat-match-1 (rest pattern) (rest input))))
	(t fail)))

(defun match-var (var input)
  (let ((i (position var vars)))
    (cond ((null i)
	   (when (= current-size max-size)
	     (setf max-size (* 2 max-size)
		   vars (replace (make-array max-size) vars)
		   vals (replace (make-array max-size) vals)
		   success (cons vars vals)))
	   (setf (aref vars current-size) var)
	   (setf (aref vals current-size) input)
	   success)
	  ((equal input  (aref vals i)) success)
	  (t fail))))

(proclaim '(inline reuse-cons))

(defun reuse-cons (x y x-y)
  (if (and (eql x (car x-y)) (eql y (cdr x-y)))
      x-y
      (cons x y)))

(defun remq (item list)
  (cond ((null list) nil)
	((eq item (first list)) (remq item (rest list)))
	(t (reuse-cons (first list)
		       (remq item (rest list))
		       list))))

(defvar *unique-cons-table* (make-hash-table :test #'eq))

(defun ucons (x y)
  (let ((car-table (or (gethash x *unique-cons-table*)
		       (setf (gethash x *unique-cons-table*)
			     (make-hash-table :test #'eq)))))
    (or (gethash y car-table)
	(setf (gethash y car-table) (cons x y)))))

(defvar *unique-atom-table* (make-hash-table :test #'equal))

(defun unique (exp)
  (typecase exp
    (symbol exp)
    (fixnum exp)
    (atom (or (gethash exp *unique-atom-table*)
	      (setf (gethash exp *unique-atom-table*) exp)))
    (cons (unique-cons (car exp) (cdr exp)))))

(defun unique-cons (x y)
  (let ((ux) (uy))
    (let ((car-table
	   (or (gethash x *unique-cons-table*)
	       (gethash (setf ux (unique x)) *unique-cons-table*)
	       (setf (gethash ux *unique-cons-table*)
		     (make-hash-table :test #'eq)))))
      (or (gethash y car-table)
	  (gethash (setf uy (unique y)) car-table)
	  (setf (gethash uy car-table)
		(cons ux uy))))))
(defun ulist (&rest args)
  (unique args))

(defun uapend (x y)
  (if (null x)
      (unique y)
      (ucons (first x) (uappend (rest x) y))))

(defstruct point x y z)

(defun scale-point (k pt)
  (make-point :x (* k (point-x pt))
	      :y (* k (point-y pt))
	      :z (* k (point-z pt))))

(defmacro defresource (name &key constructor (initial-copies 0)
			      (size (max initial-copies 10)))
  (let ((resource (symbol name '-resource))
	(deallocate (symbol 'deallocate- name))
	(allocate (symbol 'allocate- name)))
    `(let ((,resource (make-array ,size :fill-pointer 0)))
       (defun ,allocate ()
	 (if (= (fill-pointer ,resource) 0)
	     ,constructor
	     (vector-pop ,resource)))
       (defun ,deallocate (,name)
	 (vector-push-extend ,name ,resource))
       ,(if (> initial-copies 0)
	    `(mapc #',deallocate (loop repeat ,initial-copies
				    collect (,allocate))))
       ',name)))



(defstruct trie (value nil) (arcs nil))
(defconstant trie-deleted "deleted")

(defun put-trie (key trie value)
  (setf (trie-value (find-trie key t trie)) value))

(defun get-trie (key trie)
  (let* ((key-trie (find-trie key nil trie))
	 (val (if key-trie (trie-value key-trie))))
    (if (or (null key-trie) (eq val trie-deleted))
	(values nil nil)
	(values val t))))

(defun delete-trie (key trie)
  (put-trie key trie trie-deleted))

(defun find-ttrie (key extend? trie)
  (cond ((null trie) nil)
	((atom key)
	 (follow-arc key extend? trie))
	(t (find-trie
	    (cdr key) extend?
	    (find-trie
	     (car key) extend?
	     (find-trie
	      "." extend? trie))))))


(defun follow-arc (component extend? trie)
  (let ((arc (assoc component (trie-arcs trie))))
    (cond ((not (null arc)) (cdr arc))
	  ((not extend?) nil)
	  (t (let ((new-trie (make-trie)))
	       (push (cons component new-trie)
		     (trie-arcs trie))
	     new-trie)))))
