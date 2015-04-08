(load "~/Dropbox/lisp-AI-book/software-tools/interpreter_required.lisp")

(defun pat-match (pattern input &optional (bindings no-bindings))
  "Match pattern against input in the context of the bindings"
  (cond ((eq bindings fail) fail)
	((variable-p pattern)
	 (match-variable pattern input bindings))
	((eql pattern input) bindings)
	((segment-pattern-p pattern)
	 (segment-matcher pattern input bindings))
	((single-pattern-p pattern)                 ; ***
	 (single-matcher pattern input bindings))   ; ***
	((and (consp pattern) (consp input))
	 (pat-match (rest pattern) (rest input)
		    (pat-match (first pattern) (first input)
			       bindings)))
	(t fail)))


(defun match-variable (var input bindings)
  "Does Var match input ? Uses (or updates) and returns bindings"
  (let ((binding (get-binding var bindings)))
    (cond ((not binding) (extend-bindings var input bindings))
	  ((equal input (binding-val binding)) bindings)
	  (t  fail))))

(setf (get '?is  'single-match) 'match-is)
(setf (get '?or  'single-match) 'match-or)
(setf (get '?and 'single-match) 'match-and)
(setf (get '?not 'single-match) 'match-not)

(setf (get '?*  'segment-match) 'segment-match)
(setf (get '?+  'segment-match) 'segment-match+)
(setf (get '??  'segment-match) 'segment-match?)
(setf (get '?if 'segment-match) 'match-if)

(defun segment-pattern-p (pattern)
  "Is this a segment-matching pattern like ((?* var) . pat)?"
  (and (consp pattern) (consp (first pattern))
       (symbolp (first (first pattern)))
       (segment-match-fn (first (first pattern)))))

(defun single-pattern-p (pattern)
    "Is this a single-matching pattern?
  E.g. (?is x predicate) (?and . patterns) (?or . patterns)."
    (and (consp pattern)
	 (single-match-fn (first pattern))))

(defun segment-matcher (pattern input bindings)
  "Call the right function for this kind of segment pattern."
  (funcall (segment-match-fn (first (first pattern)))
	   pattern input bindings))

(defun single-matcher (pattern input bindings)
  "Call the right function for this kind of single pattern."
  (funcall (single-match-fn (first pattern))
	   (rest pattern) input bindings))

(defun segment-match-fn (x)
    "Get the segment-match function for x, 
  if it is a symbol that has one."
    (when (symbolp x) (get x 'segment-match)))

(defun single-match-fn (x)
    "Get the single-match function for x, 
  if it is a symbol that has one."
    (when (symbolp x) (get x 'single-match)))

(defun match-is (var-and-pred input bindings)
    "Succeed and bind var if the input satisfies pred,
  where var-and-pred is the list (var pred)."
    (let* ((var (first var-and-pred))
	   (pred (second var-and-pred))
	   (new-bindings (pat-match var input bindings)))
      (if (or (eq new-bindings fail)
	      (not (funcall pred input)))
	  fail
	  new-bindings)))

(defun match-and (patterns input bindings)
  "Succeed if all the patterns match the input."
  (cond ((eq bindings fail) fail)
	((null patterns) bindings)
	(t (match-and (rest patterns) input
		      (pat-match (first patterns) input
				 bindings)))))

(defun match-or (patterns input bindings)
  "Succeed if any one of the patterns match the input."
  (if (null patterns)
      fail
      (let ((new-bindings (pat-match (first patterns)
				     input bindings)))
	(if (eq new-bindings fail)
	    (match-or (rest patterns) input bindings)
	    new-bindings))))

(defun match-not (patterns input bindings)
    "Succeed if none of the patterns match the input.
  This will never bind any variables."
    (if (match-or patterns input bindings)
	fail
	bindings))

(defun segment-match (pattern input bindings &optional (start 0))
  "Match the segment pattern ((?* var) . pat) against input."
  (let ((var (second (first pattern)))
	(pat (rest pattern)))
    (if (null pat)
	(match-variable var input bindings)
	(let ((pos (first-match-pos (first pat) input start)))
	  (if (null pos)
	      fail
	      (let ((b2 (pat-match
			 pat (subseq input pos)
			 (match-variable var (subseq input 0 pos)
					 bindings))))
		;; If this match failed, try another longer one
		(if (eq b2 fail)
		    (segment-match pattern input bindings (+ pos 1))
		    b2)))))))

(defun first-match-pos (pat1 input start)
    "Find the first position that pat1 could possibly match input,
  starting at position start.  If pat1 is non-constant, then just
  return start."
    (cond ((and (atom pat1) (not (variable-p pat1)))
	   (position pat1 input :start start :test #'equal))
	  ((<= start (length input)) start) ;*** fix, rjf 10/1/92 (was <)
	  (t nil)))

(defun segment-match+ (pattern input bindings)
  "Match one or more elements of input."
  (segment-match pattern input bindings 1))

(defun segment-match? (pattern input bindings)
  "Match zero or one element of input."
  (let ((var (second (first pattern)))
	(pat (rest pattern)))
    (or (pat-match (cons var pat) input bindings)
	(pat-match pat input bindings))))

(defun match-if (pattern input bindings)
    "Test an arbitrary expression involving variables.
  The pattern looks like ((?if code) . rest)."
    ;; *** fix, rjf 10/1/92 (used to eval binding values)
    (and (progv (mapcar #'car bindings)
	     (mapcar #'cdr bindings)
	   (eval (second (first pattern))))
	 (pat-match (rest pattern) input bindings)))

(defun pat-match-abbrev (symbol expansion)
  "Define symbol as a macro standing for a pat-match pattern."
  (setf (get symbol 'expand-pat-match-abbrev)
	(expand-pat-match-abbrev expansion)))

(defun expand-pat-match-abbrev (pat)
  "Expand out all pattern matching abbreviations in pat."
  (cond ((and (symbolp pat) (get pat 'expand-pat-match-abbrev)))
	((atom pat) pat)
	(t (cons (expand-pat-match-abbrev (first pat))
		 (expand-pat-match-abbrev (rest pat))))))

(defun rule-based-translator
    (input rules &key (matcher 'pat-match)
		   (rule-if #'first) (rule-then #'rest) (action #'sublis))
    "Find the first rule in rules that matches input,
  and apply the action to that rule."
    (some
     #'(lambda (rule)
	 (let ((result (funcall matcher (funcall rule-if rule)
				input)))
	   (if (not (eq result fail))
	       (funcall action result (funcall rule-then rule)))))
     rules))

(defun eliza ()
  "Respond to user input using pattern matching rules."
  (loop
     (print 'eliza>)
     (print (flatten (use-eliza-rules (read))))))

(defun use-eliza-rules (input)
  "Find some rule with which to transform the input."
  (rule-based-translator input *eliza-rules*
			 :action #'(lambda (bindings responses)
				     (sublis (switch-viewpoint bindings)
					     (random-elt responses)))))

(defun lisp-demo ()
  (loop
     (print '>)
     (print (eval (read)))))

(defun interactive-interpreter (prompt transformer)
  "Read an expression , transform it , and print the result."
  (loop
     (print prompt)
     (print (funcall transformer (read)))))

(defun interactive-interpreter2 (prompt transformer)
  (loop
     (handler-case
	 (progn
	   (if (stringp prompt)
	       (print prompt)
	       (funcall prompt))
	   (print (funcall transformer (read))))
       (error (condition)
	 (format t "~&;; Error ~a ignored, back to top level."
		 condition)))))
(defun interactive-interpreter3
    (&key (read #'read) (eval #'eval) (print #'print)
       (prompt "> ") (input t) (output t))
  (loop
     (fresh-line output)
     (princ prompt output)
     (funcall print (funcall eval (funcall read input))
	      output)))




(defun prompt-generator (&optional (num 0) (ctr-string "[~d] "))
  #'(lambda () (format t ctr-string (incf num))))

(defun compose (&rest functions)
  #'(lambda (x)
      (reduce #'funcall functions :from-end t :initial-value x)))

(defun compose-fast (&rest functions)
  (case (length functions)
    (0 #'identity)
    (1 (first functions))
    (2 (let ((f (first functions))
	     (g (second functions)))
	 #'(lambda (x) (funcall f (funcall g x)))))
    (t #'(lambda (x)
	   (reduce #'funcall functions :from-end t
		   :initial-value x)))))





(defun lisp ()
  (interactive-interpreter '> #'eval))

(defun eliza2 ()
  (interactive-interpreter 'eliza>
			   #'(lambda (x) (flatten (use-eliza-rules x)))))




