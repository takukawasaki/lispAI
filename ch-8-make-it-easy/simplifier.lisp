(load "~/Dropbox/lisp-AI-book/student/student.lisp")

(defun infix->prefix (exp)
  (cond ((atom exp) exp)
	((= (length exp) 1) (infix->prefix (first exp)))
	((rule-based-translator exp *infix->prefix-rules*
				:rule-if #'rule-pattern :rule-then
				#'rule-response
				:action
				#'(lambda (bindings response)
				    (sublis (mapcar
					     #'(lambda (pair)
						 (cons (first pair)
						       (infix->prefix
							(rest pair))))
					     bindings)
					    response))))
	((symbolp (first exp))
	 (list (first exp) (infix->prefix (rest exp))))
	(t (error "illegal exp"))))

(defun variable-p (exp)
  (member exp '(x y z m n o p q r s t u v w)))


(pat-match-abbrev 'x+ '(?+ x))
(pat-match-abbrev 'y+ '(?+ y))

(defun rule-pattern (rule) (first rule))
(defun rule-response (rule) (second rule))

(defparameter *infix->prefix-rules*
  (mapcar #'expand-pat-match-abbrev
	  '(((x+ = y+) (= x y))
	    ((- x+)    (-x))
	    ((+ x+)    (+ x))
	    ((x+ + y+) (+ x y))
	    ((x+ - y+) (- x y))
	    ((x+ * y+) (* x y))
	    ((x+ / y+) (/ x y))
	    ((x+ ^ y+) (^ x y)))))


(defparameter *simplification-rules*
  (mapcar #'infix->prefix
	  '((x + 0  = x)
	    (0 + x  = x)
	    (x + x  = 2 * x)
	    (x - 0  = x)
	    (0 - x  = - x)
	    (x - x  = 0)
	    (- - x  = x)
	    (x * 1  = x)
	    (1 * x  = x)
	    (x * 0  = 0)
	    (0 * x  = 0)
	    (x * x  = x ^ 2)
	    (x / 0  = undefined)
	    (0 / x  = 0)
	    (x / 1  = x)
	    (x / x  = 1)
	    (0 ^ 0  = undefined)
	    (x ^ 0  = 1)
	    (0 ^ x  = 0)
	    (1 ^ x  = 1)
	    (x ^ 1  = x)
	    (x ^ -1 = 1 / x)
	    (x * (y / x) = y)
	    ((y / x) * x = y)
	    ((y * x) / x = y)
	    ((x * y) / x = y)
	    (x + - x = 0)
	    ((- x) + x = 0)
	    (x + y - x = y))))

(defun ^ (x y) (expt x y))

(defun simplifier ()
  (loop
     (print 'simplifier>)
     (print (simp (read)))))

(defun simp (inf) (prefix->infix (simplify (infix->prefix inf))))

(defun simplify (exp)
  (if (atom exp) exp
      (simplify-exp (mapcar #'simplify exp))))

(defun simplify-exp (exp)
  (cond ((rule-based-translator exp *simplification-rules*
				:rule-if #'exp-lhs :rule-then #'exp-rhs
				:action #'(lambda (bindings response)
					    (simplify (sublis bindings response)))))
	((evaluable exp) (eval exp))
	(t exp)))

(defun evaluable (exp)
  (and (every #'numberp (exp-args exp))
       (or (member (exp-op exp) '(+ - * /))
	   (and (eq (exp-op exp) '^)
		(integerp (second (exp-args exp)))))))


(pat-match-abbrev 'n '(?is n numberp))
(pat-match-abbrev 'm '(?is m numberp))
(pat-match-abbrev 's '(?is s not-numberp))

(defun not-numberp (x) (not (numberp x)))

(defun simp-rule (rule)
  (let ((exp (infix->prefix rule)))
    (mkexp (expand-pat-match-abbrev (exp-lhs exp))
	   (exp-op exp) (exp-rhs exp))))

(setf *simplification-rules*
      (append *simplification-rules*
	      (mapcar #'simp-rule
		      '((s * n = n * s)
			(n * (m * x) = (n * m) * x)
			(x * (n * y) = n * (x * y))
			((n * x) * y = n * (x * y))
			(n + s = s + n)
			((x + m) + n = x + n + m)
			(x + (y + n) = (x + y) + n)
			((x + n) + y = (x + y) + n)))))



(setf *simplification-rules*
      (append *simplification-rules*
	      (mapcar #'simp-rule
		      '((log 1         = 0)
			(log 0         = undefined)
			(log e         = 1)
			(sin 0         = 0)
			(sin pi        = 0)
			(cos 0         = 1)
			(cos pi        = -1)
			(sin(pi / 2)   = 1)
			(cos(pi / 2)   = 0)
			(log (e ^ x)   = x)
			(e ^ (log x)   = x)
			((x ^ y) * (x ^ z) = x ^ (y + z))
			((x ^ y) / (x ^ z) = x ^ (y - z))
			(log x + log y = log(x * y))
			(log x - log y = log(x / y))
			((sin x) ^ 2 + (cos x) ^ 2 = 1)
			))))

(defparameter *infix->prefix-rules*
  (mapcar #'expand-pat-match-abbrev
	  '(((x+ = y+) (= x y))
	    ((- x+)    (- x))
	    ((+ x+)    (+ x))
	    ((x+ + y+) (+ x y))
	    ((x+ - y+) (- x y))
	    ((d y+ / d x) (d y x))        ;*** New rule
	    ((Int y+ d x) (int y x))      ;*** New rule
	    ((x+ * y+) (* x y))
	    ((x+ / y+) (/ x y))
	    ((x+ ^ y+) (^ x y)))))

(setf *simplification-rules*
      (append *simplification-rules*
	      (mapcar #'simp-rule
		      '((d x / d x       = 1)
			(d (u + v) / d x = (d u / d x) + (d v / d x))
			(d (u - v) / d x = (d u / d x) - (d v / d x))
			(d (- u) / d x   = - (d u / d x))
			(d (u * v) / d x = u * (d v / d x) + v * (d u / d x))
			(d (u / v) / d x = (v * (d u / d x) - u * (d v / d x))
			 / v ^ 2) ; [This corrects an error in the first printing]
			(d (u ^ n) / d x = n * u ^ (n - 1) * (d u / d x))
			(d (u ^ v) / d x = v * u ^ (v - 1) * (d u / d x)
			 + u ^ v * (log u) * (d v / d x))
			(d (log u) / d x = (d u / d x) / u)
			(d (sin u) / d x = (cos u) * (d u / d x))
			(d (cos u) / d x = - (sin u) * (d u / d x))
			(d (e ^ u) / d x = (e ^ u) * (d u / d x))
			(d u / d x       = 0)))))


(defun simp-fn (op) (get op 'simp-fn))

(defun set-simp-fn (op fn) (setf (get op 'simp-fn) fn))


(defun simplify-exp (exp)
  "Simplify using a rule, or by doing arithmetic,
  or by using the simp function supplied for this operator."
  (cond ((simplify-by-fn exp))                             ;***
	((rule-based-translator
	  exp *simplification-rules*
	  :rule-if #'exp-lhs :rule-then #'exp-rhs
	  :action #'(lambda (bindings response)
		      (simplify (sublis bindings response)))))
	((evaluable exp) (eval exp))
	(t exp)))

(defun simplify-by-fn (exp)
  "If there is a simplification fn for this exp,
  and if applying it gives a non-null result,
  then simplify the result and return that."
  (let* ((fn (simp-fn (exp-op exp)))
	 (result (if fn (funcall fn exp))))
    (if (null result)
	nil
	(simplify result))))


(defun factorize (exp)
  "Return a list of the factors of exp^n,
  where each factor is of the form (^ y n)."
  (let ((factors nil)
	(constant 1))
    (labels
	((fac (x n)
	   (cond
	     ((numberp x)
	      (setf constant (* constant (expt x n))))
	     ((starts-with x '*)
	      (fac (exp-lhs x) n)
	      (fac (exp-rhs x) n))
	     ((starts-with x '/)
	      (fac (exp-lhs x) n)
	      (fac (exp-rhs x) (- n)))
	     ((and (starts-with x '-) (length=1 (exp-args x)))
	      (setf constant (- constant))
	      (fac (exp-lhs x) n))
	     ((and (starts-with x '^) (numberp (exp-rhs x)))
	      (fac (exp-lhs x) (* n (exp-rhs x))))
	     (t (let ((factor (find x factors :key #'exp-lhs
				    :test #'equal)))
		  (if factor
		      (incf (exp-rhs factor) n)
		      (push `(^ ,x ,n) factors)))))))
      ;; Body of factorize:
      (fac exp 1)
      (case constant
	(0 '((^ 0 1)))
	(1 factors)
	(t `((^ ,constant 1) .,factors))))))


(defun unfactorize (factors)
  "Convert a list of factors back into prefix form."
  (cond ((null factors) 1)
	((length=1 factors) (first factors))
	(t `(* ,(first factors) ,(unfactorize (rest factors))))))


(defun divide-factors (numer denom)
  "Divide a list of factors by another, producing a third."
  (let ((result (mapcar #'copy-list numer)))
    (dolist (d denom)
      (let ((factor (find (exp-lhs d) result :key #'exp-lhs
			  :test #'equal)))
	(if factor
	    (decf (exp-rhs factor) (exp-rhs d))
	    (push `(^ ,(exp-lhs d) ,(- (exp-rhs d))) result))))
    (delete 0 result :key #'exp-rhs)))

(defun free-of (exp var)
  "True if expression has no occurrence of var."
  (not (find-anywhere var exp)))

(defun find-anywhere (item tree)
  "Does item occur anywhere in tree?  If so, return it."
  (cond ((eql item tree) tree)
	((atom tree) nil)
	((find-anywhere item (first tree)))
	((find-anywhere item (rest tree)))))


(defun length=1 (x)
  (and (consp x) (null (rest x))))


(defun integrate (exp x)
  ;; First try some trivial cases
  (cond
    ((free-of exp x) `(* ,exp x))          ; Int c dx = c*x
    ((starts-with exp '+)                  ; Int f + g  =
     `(+ ,(integrate (exp-lhs exp) x)      ;   Int f + Int g
	 ,(integrate (exp-rhs exp) x)))
    ((starts-with exp '-)
     (ecase (length (exp-args exp))
       (1 (integrate (exp-lhs exp) x))     ; Int - f = - Int f
       (2 `(- ,(integrate (exp-lhs exp) x) ; Int f - g  =
	      ,(integrate (exp-rhs exp) x)))))  ; Int f - Int g
    ;; Now move the constant factors to the left of the integral
    ((multiple-value-bind (const-factors x-factors)
	 (partition-if #'(lambda (factor) (free-of factor x))
		       (factorize exp))
       (identity ;simplify
	`(* ,(unfactorize const-factors)
	    ;; And try to integrate:
	    ,(cond ((null x-factors) x)
		   ((some #'(lambda (factor)
			      (deriv-divides factor x-factors x))
			  x-factors))
		   ;; <other methods here>
		   (t `(int? ,(unfactorize x-factors) ,x)))))))))

(defun partition-if (pred list)
  "Return 2 values: elements of list that satisfy pred,
  and elements that don't."
  (let ((yes-list nil)
	(no-list nil))
    (dolist (item list)
      (if (funcall pred item)
	  (push item yes-list)
	  (push item no-list)))
    (values (nreverse yes-list) (nreverse no-list))))

(defun deriv-divides (factor factors x)
  (assert (starts-with factor '^))
  (let* ((u (exp-lhs factor))              ; factor = u^n
	 (n (exp-rhs factor))
	 (k (divide-factors
	     factors (factorize `(* ,factor ,(deriv u x))))))
    (cond ((free-of k x)
	   ;; Int k*u^n*du/dx dx = k*Int u^n du
	   ;;                    = k*u^(n+1)/(n+1) for n/=1
	   ;;                    = k*log(u) for n=1
	   (if (= n -1)
	       `(* ,(unfactorize k) (log ,u))
	       `(/ (* ,(unfactorize k) (^ ,u ,(+ n 1)))
		   ,(+ n 1))))
	  ((and (= n 1) (in-integral-table? u))
	   ;; Int y'*f(y) dx = Int f(y) dy
	   (let ((k2 (divide-factors
		      factors
		      (factorize `(* ,u ,(deriv (exp-lhs u) x))))))
	     (if (free-of k2 x)
		 `(* ,(integrate-from-table (exp-op u) (exp-lhs u))
		     ,(unfactorize k2))))))))


(defun deriv (y x) (simplify `(d ,y ,x)))

(defun integration-table (rules)
  (dolist (i-rule rules)
    ;; changed infix->prefix to simp-rule - norvig Jun 11 1996
    (let ((rule (simp-rule i-rule)))
      (setf (get (exp-op (exp-lhs (exp-lhs rule))) 'int)
	    rule))))

(defun in-integral-table? (exp)
  (and (exp-p exp) (get (exp-op exp) 'int)))

(defun integrate-from-table (op arg)
  (let ((rule (get op 'int)))
    (subst arg (exp-lhs (exp-lhs (exp-lhs rule))) (exp-rhs rule))))


(integration-table
 '((Int log(x) d x = x * log(x) - x)
   (Int exp(x) d x = exp(x))
   (Int sin(x) d x = - cos(x))
   (Int cos(x) d x = sin(x))
   (Int tan(x) d x = - log(cos(x)))
   (Int sinh(x) d x = cosh(x))
   (Int cosh(x) d x = sinh(x))
   (Int tanh(x) d x = log(cosh(x)))
   ))

(set-simp-fn 'Int #'(lambda (exp)
		      (unfactorize
		       (factorize
			(integrate (exp-lhs exp) (exp-rhs exp))))))

(defun starts-with (list x)
  "Is this a list whole first element is x"
   (and (consp list) (eql (first list) x)))
