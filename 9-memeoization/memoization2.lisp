(defparameter *simple-grammar*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Noun))
    (verb-phrase -> (Verb noun-phrase))
    (Article -> the a)
    (Noun -> man ball woman table )
    (Verb -> hit took saw liked)))

(defvar *grammar* *simple-grammar*
  )



(defun memo (fn name key test)
  "Return a memo-function of fn"
  (let ((table (make-hash-table :test test)))
    (setf (get name 'memo) table)
    #'(lambda (&rest args)
	(let ((k (funcall key args)))
	  (multiple-value-bind (val found-p)
	      (gethash k table)
	    (if found-p val
		(setf (gethash k table) (apply fn args))))))))

(defun memoize (fn-name &key (key #'first) (test #'eql))
  (setf (symbol-function fn-name)
	(memo (symbol-function fn-name) fn-name key test)))


(defun clear-memoize (fn-name)
  (let ((table (get fn-name 'memo)))
    (when table (clrhash table))))

(defun rule-lhs (rule)
  (first rule))

(defun rule-rhs (rule)
  (rest (rest rule)))

(defun one-of (set)
  (list (random-elt set)))

(defun random-elt (choice)
  (elt choice (random (length choice))))

(defun compile-rule (rule)
  (let ((rhs (rule-rhs rule)))
    `(defun ,(rule-lhs rule) ()
       ,(cond ((every #'atom rhs) `(one-of ',rhs))
	      ((length=1 rhs) (build-code (first rhs)))
	      (t `(case (random ,(length rhs))
		    ,@(build-cases 0 rhs)))))))

(defun build-cases (number choices)
  (when choices
    (cons (list number (build-code (first choices)))
	  (build-cades (+ number 1) (rest choices)))))

(defun build-code (choice)
  (cond ((null choice) nil)
	((atom choice) (list choice))
	((length=1 choice) choice)
	(t `(append ,@(mapcar #'build-code choice)))))

(defun length=1 (x)
  (and (consp x) (null (rest x))))

(defmacro defrule (&rest rule)
  "define a grammar rule"
  (compile-rule rule))

(defrule Sentence -> (NP VP))
(defrule NP -> (Art Noun))
(defrule VP -> (Verb NP))
(defrule Art -> the a)
(defrule Noun -> man ball woman table)
(defrule Verb -> hit took saw liked)


