(load "~/Documents/lisp-AI-book/ch-4-early-AI/gps-four.lisp")

(defconstant fail nil "Indicate pat-match failure")
(defparameter no-bindings '((t . t)))


(defun simple-equal (x y)
  "Are you and y equal? (Don't check inside strings.)"
  (if (or (atom x) (atom y))
      (eql x y)
      (and (simple-equal (first x) (first y))
	   (simple-equal (rest x) (rest y)))))

(defun pat-match (pattern input &optional (bindings no-bindings))
  "Does pattern match input? Any variable can match anything."
  (cond ((eq bindings fail) fail)
	((variable-p pattern)
	 (match-variable pattern input bindings))
	((eql pattern input) bindings)
	((segment-pattern-p pattern)
	 (segment-match pattern input bindings))
	((and (consp pattern) (consp input))
	 (pat-match (rest pattern) (rest input)
		    (pat-match (first pattern) (first input)
			       bindings)))
	(t fail)))

(defun segment-pattern-p (pattern)
  "Is this a segmrnt matching pattern: ((?* var)) . pat"
  (and (consp pattern)
       (starts-with (first pattern) '?*)))

(defun segment-match (pattern input bindings &optional (start 0))
  "Match the segment pattern ((?* var) . pat) against input"
  (let ((var (second (first pattern)))
	(pat (rest pattern)))
    (if (null pat)
	(match-variable var input bindings)
	(let ((pos (position (first pat) input
			     :start start :test #'equal)))
	  (if (null pos)
	      fail
	      (let ((b2 (pat-match pat (subseq input pos) bindings)))
		(if (eq b2 fail)
		    (segment-match pattern input bindings (+ pos 1))
		    (match-variable var (subseq input 0 pos) b2))))))))


(defun match-variable (var input bindings)
  "Does Var match input ? Uses (or updates) and returns bindings"
  (let ((binding (get-binding var bindings)))
    (cond ((not binding) (extend-bindings var input bindings))
	  ((equal input (binding-val binding)) bindings)
	  (t  fail))))



(defun variable-p (x)
  "Is x a variable (a symbol beginning with `?')?"
  (and (symbolp x) (equal (char (symbol-name x) 0) #\?)))


(defun get-binding (var binding)
  "Find a (variable . value) pair in bindings list"
  (assoc var binding))

(defun binding-val (binding)
  "Get the value part of a single bindings"
  (cdr binding))

(defun lookup (var bindings)
  "Get the value part of a single bindings"
  (binding-val (get-binding var bindings)))

(defun extend-bindings (var val bindings)
  "Add a (var . value) pair to a binding list"
  (cons (cons var val)
	(if (eq bindings no-bindings)
	    nil
	    bindings)))



