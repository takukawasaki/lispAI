(load "/Users/kawasakitaku/Dropbox/lisp-AI-book/ch-1-tutorial/ch-1-tutorial.lisp")

(defun sentence () (append (noun-phrase) (verb-phrase)))

(defun noun-phrase () (append (Article) (Adj*) (Noun) (PP*)))

(defun PP () (append (Prep) (noun-phrase)))

(defun Adj () (one-of '(big little blue green adiabatic)))

(defun Prep () (one-of '(to in by with on)))

(defun verb-phrase () (append (Verb) (noun-phrase)))

(defun Article () (one-of '(the a)))

(defun Noun () (one-of '(man ball woman table)))

(defun Verb () (one-of '(hit took saw liked))) 

(defun one-of (set) 
  (list (random-elt set)))

(defun random-elt (choices)
"elt func get a element from lit,example (elt ,LIST ,INDEXNUM)"
  (elt choices (random (length choices))))

(defun Adj* ()
  (if (= (random 2 ) 0)
      nil
      (append (Adj) (Adj*))))

(defun PP* ()
  (if (random-elt '(t nil))
      (append (PP) (PP*))
      nil))

(defparameter *simple-grammar* 
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Noun))
    (verb-phrase -> (Verb noun-phrase))
    (Article -> the a)
    (Noun -> man ball woman table )
    (Verb -> hit took saw liked)))

(defvar *grammar* *simple-grammar*
  )

(defun rule-lhs (rule)
  (first rule))

(defun rule-rhs (rule)
  (rest (rest rule)))

(defun rewrites (category)
  (rule-rhs (assoc category *grammar*)))

(defun generate1 (phrase)
  (cond ((listp phrase)
	 (mappend #'generate1 phrase))
	((  rewrites phrase) 
	 (generate1 (random-elt (rewrites phrase))))
	(t (list phrase))))

(defun generate2 (phrase)
  (if (listp phrase)
      (mappend #'generate2 phrase)
      (let ((choices (rewrites phrase)))
	(if (null choices)
	    (list phrase)
	    (generate2 (random-elt choices))))))

(defun generate3 (phrase)
  (let ((choices nil))
    (cond ((listp phrase) 
	   (mappend #'generate3 phrase))
	  ((setf choices (rewrites phrase))
	   (generate3 (random-elt choices)))
	  (t (list phrase)))))

(defun generate4 (phrase)
  (cond ((listp phrase) 
	 (mappend #'generate4 phrase))
	((non-terminal-p  phrase)
	 (generate4 (random-elt (rewrites phrase))))
	(t (list phrase))))

(defun non-terminal-p (category)
  (not (null (rewrites category))))

(defparameter *bigger-grammar* 
  '(( sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Adj* Noun PP*) (Name) (Pronoun))
    (verb-phrase -> (Verb noun-phrase PP*))
    (PP* -> () (PP PP*))
    (Adj* -> () (Adj Adj*))
    (PP -> (Prep noun-phrase))
    (Prep -> to in by with on)
    (Adj -> big little blue green adiabastic)
    (Article -> the a)
    (Name -> Pat Kim Lee Terry Robin)
    (Noun -> man ball woman table)
    (Verb -> hit took saw liked)
    (Pronoun -> he she it these those that)))

(setf *grammar* *bigger-grammar*)

(defun generate-tree (phrase)
  (cond ((listp phrase)
	 (mapcar #'generate-tree phrase))
	((rewrites phrase) 
	 (cons phrase 
	       (generate-tree (random-elt (rewrites phrase)))))
	(t (list phrase))))

(setf *grammar* *simple-grammar*)


(defun generate-all (phrase)
  (cond ((null phrase) (list nil))
	((listp phrase ) 
	 (combine-all (generate-all (first phrase))
		      (generate-all (rest phrase))))
	((rewrites phrase) 
	 (mappend #'generate-all (rewrites phrase)))
	(t (list (list phrase)))))

(defun combine-all (xlist ylist)
  (mappend #'(lambda (y) 
	       (mapcar #'(lambda (x) (append x y) ) xlist))
	   ylist))
