(load "~/Dropbox/lisp-AI-book/ELIZA/eliza-four.lisp")

(defun unify (x y &optional (bindings no-bindings))
  "See if x and y match with given bindings"
  (cond ((eq bindings fail) fail)
	((eql x y) bindings)
	((variable-p x) (unify-variable x y bindings))
	((variable-p y) (unify-variable y x bindings))
	((and (consp x) (consp y))
	 (unify (rest x) (rest y)
		(unify (first x) (first y) bindings)))
	(t fail)))

(defun unify-variable (var x bindings)
  "Unify var with x, using (and maybe extending) bindings"
  ;;BUGの出るバージョン
  (cond  ((get-binding var bindings)
	  (unify (lookup var bindings) x bindings))
	 ((and (variable-p x) (get-binding x bindings))
	  (unify var (lookup x bindings) bindings))
	 ((and *occurs-check* (occurs-check var x bindings))
	  fail)
	 (t (extend-bindings var x bindings))))

(defun occurs-check (var x bindings)
  (cond ((eq var x) t)
	((and (variable-p x) (get-binding x bindings))
	 (occurs-check var (lookup x bindings) bindings))
	((consp x) (or (occurs-check var (first x) bindings)
		       (occurs-check var (rest x) bindings)))
	(t nil)))

(defun reuse-cons ( x y x-y)
  (if (and (eql x (car x-y)) (eql y (cdr x-y)))
      x-y
      (cons x y)))

(defun subst-bindings (bindings x)
  (cond ((eq bindings fail) fail)
	((eq bindings no-bindings) x)
	((and (variable-p x) (get-binding x bindings))
	 (subst-bindings bindings (lookup x bindings)))
	((atom x) x)
	(t (reuse-cons (subst-bindings bindings (car x))
		       (subst-bindings bindings (cdr x))
		       x))))



(defun unifier (x y)
  "Return something that unifies with both x and y (or fail)."
   (subst-bindings (unify x y) x))

;;pat-match
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

(defparameter *occurs-check* t)

(defun lisp-member (item list)
  (and (consp list)
       (or (eql item (first list))
	   (lisp-member item (rest list)))))

(defun clause-head (clause)
  (first clause))

(defun clause-body (clause)
  (rest clause))

(defun get-clauses (pred)
  (get pred 'clauses))

(defun predicate (relation)
  (first relation))

(defvar *db-predicates* nil)

(defmacro <- (&rest clause)
  `(add-clause ',clause))

(defun add-clause(clause)
  (let ((pred (predicate (clause-head clause))))
    (assert (and (symbolp pred) (not (variable-p pred))))
    (pushnew pred *db-predicates*)
    (setf (get pred 'clauses)
	  (nconc (get-clauses pred) (list clause)))
    pred))


(defun clear-db ()
  (mapc #'clear-predicate *db-predicates*))

(defun clear-predicate (predicate)
  (setf (get predicate 'clauses) nil))


(defun prove-last (goal bindings)
  (mapcan #'(lambda (clause)
	      (let ((new-clause (rename-variables clause)))
		(prove-all (clause-body new-clause)
			   (unify goal (clause-head new-clause)
				  bindings))))
	  (get-clauses (predicate goal))))


(defun prove-all-last (goals bindings)
  (cond ((eq bindings fail) fail)
	((null goals) (list bindings))
	(t (mapcan #'(lambda (goal1-solution)
		       (prove-all-last (rest goals) goal1-solution))
		   (prove-last (first goals) bindings)))))


(defun rename-variables (x)
  (sublis (mapcar #'(lambda (var) (cons var (gensym (string var))))
		  (variables-in x))
	  x))


(defun variables-in (exp)
  (unique-find-anywhere-if #'variable-p exp))

(defun unique-find-anywhere-if (predicate tree
				&optional found-so-far)
  (if (atom tree)
      (if (funcall predicate tree)
	  (adjoin tree found-so-far)
	  found-so-far)
      (unique-find-anywhere-if
       predicate
       (first tree)
       (unique-find-anywhere-if predicate (rest tree)
				found-so-far))))



(<- (likes Kim Robin))  
(<- (likes Sandy Lee))  
(<- (likes Sandy Kim))  
(<- (likes Robin cats))  
(<- (likes Sandy ?x) (likes ?x cats))  
(<- (likes Kim ?x) (likes ?x Lee) (likes ?x Kim))  
(<- (likes ?x ?x))


(defmacro ?- (&rest goals) `(top-level-prove ',goals))

(defun top-level-prove-last (goals)
  (show-prolog-solutions
   (variables-in goals)
   (prove-all-last goals no-bindings)))

(defun show-prolog-solutions (vars solutions)
  (if (null solutions)
      (format t "~&No.")
      (mapc #'(lambda (solution)
		(show-prolog-vars vars solution))
	    solutions))
  (values))

(defun show-prolog-vars-last (vars bindings)
  (if (null vars)
      (format t "~&Yes")
      (dolist (var vars)
	(format t "~&~a = ~a" var
		(subst-bindings bindings var))))
  (princ ";"))

(defun prove-all (goals bindings)
  (cond ((eq bindings fail) fail)
	((null goals) bindings)
	(t (prove (first goals) bindings (rest goals)))))



(defun prove (goal bindings other-goals)
  (let ((clauses (get-clauses (predicate goal))))
    (if (listp clauses)
	(some
	 #'(lambda(clause)
	     (let ((new-clause (rename-variables clause)))
	       (prove-all
		(append (clause-body new-clause) other-goals)
		(unify goal (clause-head new-clause) bindings))))
	 clauses)
	(funcall clauses (rest goal) bindings
		 other-goals))))


(defun top-level-prove (goals)
  (prove-all `(,@goals (show-prolog-vars ,@(variables-in goals)))
		  no-bindings)
  (format t "~&No.")
  (values))

(defun show-prolog-vars (vars bindings other-goals)
  (if (null vars)
      (format t "~&Yes")
      (dolist (var vars)
	(format t "~&~a = ~a" var
		(subst-bindings bindings var))))
  (if (continue-p)
      fail
      (prove-all other-goals bindings)))

(setf (get 'show-prolog-vars 'clauses) 'show-prolog-vars)


(defun continue-p ()
  (case (read-char)
    (#\; t)
    (#\. nil)
    (#\newline (continue-p))
    (otherwise
     (format t "Type ; to see more or . to stop")
     (continue-p))))


(<- (length0 () 0))
(<- (length0 (?x . ?y) (1+ ?n)) (length0 ?y ?n))


(defmacro <- (&rest clause)
  `(add-clause ',(replace-?-vars clause)))

(defmacro ?- (&rest goals)
  `(top-level-prove ',(replace-?-vars goals)))

(defun replace-?-vars (exp)
  (cond ((eq exp '?) (gensym "?"))
	((atom exp) exp)
	(t (reuse-cons (replace-?-vars (first exp))
		       (replace-?-vars (rest exp))
		       exp))))

(<- (member ?item (?item . ?rest)))
(<- (member ?item (?x . ?rest)) (member ?item ?rest))

(<- (nextto ?x ?y ?list) (iright ?x ?y ?list))
(<- (nextto ?x ?y ?list) (iright ?y ?x ?list))

(<- (iright ?left ?right (?left ?right . ?rest)))
(<- (iright ?left ?right (?x . ?rest))
    (iright ?left ?right ?rest))

(<- (= ?x ?x))

(<- (zebra ?h ?w ?z)
    (= ?h ((house norwegian ? ? ? ?)
	   ?
	   (house ? ? ? milk ?) ? ?))
    (member (house englishman ? ? ? red) ?h)
    (member (house spaniard dog ? ? ?) ?h)
    (member (house ? ? ? coffee green) ?h)
    (member (house ukrainian ? ? tea ?) ?h)
    (iright (house ? ? ? ? ivory)
	    (house ? ? ? ? green) ?h)
    (member (house ? snails winston ? ?) ?h)
    (member (house ? ? kools ? yellow) ?h)
    (nextto (house ? ? chesterfield ? ?)
	    (house ? fox ? ? ?) ?h)
    (nextto (house ? ? kools ? ?)
	    (house ? horse ? ? ?) ?h)
    (member (house ? ? luckystrike orange-juice ?) ?h)
    (member (house japanese ? parliaments ? ?) ?h)
    (nextto (house norwegian ? ? ? ?)
	    (house ? ? ? ? blue) ?h)
    (member (house ?w ? ? water ?) ?h)
    (member (house ?z zebra ? ? ?) ?h))

(defconstant unbound "Unbound")

(defstruct var name (binding unbound))

(defun bound-p (var)
  (not (eq (var-binding var) unbound)))

(defmacro deref (exp)
  `(progn (loop while (and (var-p ,exp) (bound-p ,exp))
	     do (setf ,exp (var-binding , exp)))
	  ,exp))


(defun unify? (x y)
  (cond ((eql (deref x) (deref y)) t)
	((var-p x) (set-binding! x Y))
	((var-p y) (set-binding! y x))
	((and (consp x) (consp y))
	 (and (unify! (first x) (first y))
	      (unify! (rest x) (rest y))))
	(t nil)))

(defun set-binding! (var value)
  (setf (var-binding var) value)
  t)

(defstruct (var (:print-function print-var))
  name (binding unbound))

(defun print-var (var stream depth)
  (if (or (and (numberp *print-level*))
	  (>= depth *print-level*))
      (var-p (deref var)))
  (format stream "?~a" (var-name var))
  (write var :stream stream))

(defvar *trail* (make-array 200 :fill-pointer 0 :adjustable t))

(defun set-binding? (vat value)
  (unless (eq var value)
    (vector-push-extend var *trail*)
    (setf (var-binding var) value))
  t)

(defun undo-bindings! (old-trail)
  (loop until (= (fill-pointer *trail*) old-trail)
     do (setf (var-binding (vactor-pop *trail*)) unbound)))

(defvar *var-counter* 0)

(defstruct (var (:constructor ? ())
		(:print-function print-var))
  (name (incf *var-counter*))
  (binding unbound))

