(load "~/Dropbox/lisp-AI-book/ELIZA/eliza-four.lisp")
(load "~/Dropbox/lisp-AI-book/ch-9-memoization/delay.lisp")
(load "~/Dropbox/lisp-AI-book/paip/auxfns.lisp")
(load "~/Dropbox/lisp-AI-book/ch-8-make-it-easy/simplifier.lisp")
(load "~/Dropbox/lisp-AI-book/ch-9-memoization/memoization2.lisp")

(defun simplify-exp (exp)
  (cond ((simplify-by-fn exp))
	((rule-based-translator exp (rules-for (exp-op exp))
				:rule-if #'exp-lhs :rule-then #'exp-rhs
				:action #'(lambda (bindings response)
					    (simplify (sublis bindings response)))))
	((evaluable exp) (eval exp))
	(t exp)))

(defvar *rules-for* (make-hash-table :test #'eq))

(defun main-op (rule)
  (exp-op (exp-lhs rule)))

(defun index-rules (rules)
  (clrhash *rules-for*)
  (dolist (rule rules)
    (setf (gethash (main-op rule) *rules-for*)
	  (nconc (gethash (main-op rule) *rules-for*)
		 (list rule)))))

(defun rules-for (op) (gethash op *rules-for*))

(index-rules *simplification-rules*)


(defvar *bindings* nil)


(defun compile-rule00 (rule)
  (let ((*bindings* nil))
    `(lambda (x)
       ,(compile-exp 'x (exp-lhs rule)
		     (delay (build-exp (exp-rhs rule)
				       *bindings*))))))

(defun compile-exp (var pattern consequent)
  (cond ((get-binding pattern *bindings*)
	  `(if (equal ,var ,(lookup pattern *bindings*))
	       ,(force consequent)))
	 ((variable-p pattern)
	  (push (cons pattern var) *bindings*)
	  (force consequent))
	 ((atom pattern)
	  `(if (eql ,var ',pattern)
	       ,(force consequent)))
	 ((starts-with pattern '?is)
	  (push (cons (second pattern) var) *bindings*)
	  `(if (,(third pattern) ,var)
	       ,(force consequent)))
	 (t
	  `(if (op? ,var ',(exp-op pattern))
	       ,(compile-args var pattern consequent)))))

(defun compile-args (var pattern consequent)
  (let ((L (symbol var 'L))
	(R (symbol var 'R)))
    (if (exp-rhs pattern)
	`(let ((,L (exp-lhs ,var))
	       (,R (exp-rhs ,var)))
	  ,(compile-exp L (exp-lhs pattern)
			(delay
			 (compile-exp R (exp-rhs pattern)
				      consequent))))
	`(let ((,L (exp-lhs ,var)))
	   ,(compile-exp L (exp-lhs pattern) consequent)))))

(defun build-exp (exp bindings)
  (cond ((assoc exp bindings) (rest (assoc exp bindings)))
	((variable-p exp)
	 (error "Variable ~a ccurred on right-hand side,~
                   but not left." exp))
	((atom exp) `',exp)
	(t (let ((new-exp (mapcar #'(lambda (x)
				      (build-exp x bindings))
				  exp)))
	     `(simplify-exp (list .,new-exp))))))

(defun op? (exp op)
  (and (exp-p exp) (eq (exp-op exp) op)))

(defun symbol (&rest args)
  (intern (format nil "~{~a~}" args)))

(defun new-symbol (&rest args)
  (make-symbol (format nil "~{~a~}" args)))


(defun compile-rule-set (op)
  (set-simp-fn op
	       (compile nil
			`(lambda (x)
			   ,(reduce #'combine-rules
				    (mapcar #'compile-indexed-rule
					    (rules-for op)))))))

(defun compile-indexed-rule (rule)
  (let ((*bindings* nil))
    (compile-args
     'x (exp-lhs rule)
     (delay (build-exp (exp-rhs rule) *bindings*)))))

(defun combine-rules (a b)
  (cond ((and (listp a) (listp b)
	      (= (length a) (length b) 3)
	      (equal (first a) (first b))
	      (equal (second a) (second b)))
	 (list (first a) (second a)
	       (combine-rules (third a) (third b))))
	((matching-ifs a b)
	 `(if ,(second a)
	      ,(combine-rules (third a) (third b))
	      ,(combine-rules (fourth a) (fourth b))))
	((starts-with a 'or)
	 (if (matching-ifs (last1 a) b)
	     (appehd (butlast a)
		     (list (combine-rules (last1 a) b)))
	     (append a (list b))))
	(t `(or ,a ,b))))

(defun matching-ifs (a b)
  (and (starts-with a 'if) (starts-with b 'if)
       (equal (second a) (second b))))

(defun last1 (list)
  (first (last list)))

(defun compile-all-rules-indexed (rules)
  (index-rules rules)
  (let ((all-ops (delete-duplicates (mapcar #'main-op rules))))
    (mapc #'compile-rule-set all-ops)))

(defun simplify-exp0 (exp)
  (cond ((simplify-by-fn exp))
	((evaliable exp) (eval exp))
	(t exp)))


