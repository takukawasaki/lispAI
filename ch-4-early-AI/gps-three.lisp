(load "~/Documents/lisp-AI-book/ch-3-tutorial/ch-3-tutorial.lisp")

(defvar *ops* nil "A list of available operators")


(defun GPS (state goals  &optional (*ops* *ops*))
  "general Problem solver : from state ,achive goals using *ops*"
 (remove-if #'atom (achive-all (cons '(start) state) goals nil)))

(defvar *dbg-ids* nil "Identifiers used by dbg")


(defun dbg (id format-string &rest args)
  "Print debugging into if (Degbug ID) has been specified"
  (when (member id *dbg-ids*)
    (fresh-line *debug-io*)
    (apply #'format *debug-io* format-string args)))

(defun undebug (&rest ids)
  "Stop dbg on the ids With no ids , stop dbg altogether."
  (setf *dbg-ids* (if (null ids)
		      nil
		      (set-difference *dbg-ids* ids))))

(defun mydebug (&rest ids)
  "Start dbg output on the given ids"
  (setf *dbg-ids* (union ids *dbg-ids*)))


(defun dbg-indent (id indent format-string &rest args)
  "Print indented debugging into if (DEBUG ID) has been specified."
  (when  (member id *dbg-ids*)
    (fresh-line *debug-io*)
    (dotimes (i indent) (princ " " *debug-io*))
    (apply #'format *debug-io* format-string args)))

(defstruct op "An operation"
	   (action nil)
	   (preconds nil)
	   (add-list nil)
	   (del-list))

(defparameter *school-ops*
  (list
   (make-op :action 'drive-son-to-school
	    :preconds '(son-at-home car-works)
	    :add-list '(son-at-school)
	    :del-list '(son-at-home))
   (make-op :action 'shop-installs-battery
	    :preconds '(car-needs-battery shop-knows-problem shop-has-money)
	    :add-list '(car-works))
   (make-op :action 'tell-shop-problem
	    :preconds '(in-communication-with-shop)
	    :add-list '(shop-knows-problem))
   (make-op :action 'telephone-shop
	    :preconds '(know-phone-number)
	    :add-list '(in-communication-with-shop))
   (make-op :action 'look-up-number
	    :preconds '(have-phone-book)
	    :add-list '(know-phone-number))
   (make-op :action 'give-shop-money
	    :preconds '(have-money)
	    :add-list '(shop-has-money)
	    :del-list '(have-money))))
	    	   


(defun executing-p (x)
  "Is X of the form : (executing ....)"
 (starts-with x 'executing))

(defun starts-with (list x)
 "Is this a list whole first element is x"
 (and (consp list) (eql (first list) x)))

(defun convert-op (op)
  "Make op confirm to the (Executing op) convention"
  (unless (some #'executing-p (op-add-list op))
	(push (list 'executing (op-action op))(op-add-list op)))
  op)

(defun op (action &key preconds add-list del-list)
  "Make a new operator that obeys the (EXECUTING op) convention"
  (convert-op 
   (make-op :action action
	    :preconds preconds
	    :add-list add-list
	    :del-list del-list)))

(mapc #'convert-op *school-ops*)

(defun achive-all (state goals goal-stack)
  (some #'(lambda (goals) (achive-each state goals goal-stack))
	(orderings goals)))


(defun achive-each (state goals goal-stack)
  "A goal is achived if it already holds"
  (let ((current-state state))
    (if (and (every #'(lambda (g)
			(setf current-state
			      (achive current-state g goal-stack)))
		    goals)
	     (subsetp goals current-state :test #'equal))
	current-state)))

(defun achive (state goal goal-stack)
  "A goal is achived if it is already holds
      or if there is an appropriate op for it that is applicable"
  (dbg-indent :gps (length goal-stack) "Goal: ~a" goal)
  (cond ((member-equal goal state) state)
	((member-equal goal goal-stack) nil)
	(t (some #'(lambda (op) (apply-op state goal op goal-stack))
		 (find-all goal *ops* :test #'appropriate-p)))))

(defun orderings (l)
  (if (> (length l) 1)
      (list l (reverse l))
      (list l)))

(defun member-equal (item list)
  (member item list :test #'equal))

(defun apply-op (state goal op goal-stack)
  "Return a new transformed state if op is applicable"
  (dbg-indent :gps (length goal-stack) "Consider: ~a" (op-action op))
  (let ((state2 (achive-all state (op-preconds op)
			    (cons goal goal-stack))))
    (unless (null state2)
      (dbg-indent :gps (length goal-stack) "Action: ~a" (op-action op))
      (append (remove-if #'(lambda (x)
			     (member-equal x (op-del-list op)))
			 state2)
	      (op-add-list op)))))

(defun appropriate-p (goal op)
  "an op is appropriaate to a goal if it is in its add-list."
  (member-equal goal (op-add-list op)))


(defun use (oplist)
  "Use oplist as the default listof operators."
  (length (setf *ops* oplist)))

;;monkey-and-bananas
(defparameter *banana-ops*
  (list
   (op 'climb-on-chair
       :preconds '(chair-at-middle-room at-middle-room on-floor)
       :add-list '(at-bananas on-chair)
       :del-list '(at-middle-room on-the-floor))
   (op 'push-chair-from-door-to-middle-room
       :preconds '(chair-at-door at-door)
       :add-list '(chair-at-middle-room at-middle-room)
       :del-list '(chair-at-door at-door))
   (op 'walk-from-door-to-middle-room
       :preconds '(at-door on-floor)
       :add-list '(at-middle-room)
       :del-list '(at-door))
   (op 'grasp-bananas
       :preconds '(at-bananas empty-handed)
       :add-list '(has-bananas )
       :del-list '(empty-handed))
   (op 'drop-ball
       :preconds '(has-ball)
       :add-list '(empty-handed)
       :del-list '(has-ball))
   (op 'eat-bananas
       :preconds '(has-bananas)
       :add-list '(empty-handed not-hungry)
       :del-list '(has-bananas hungry))))

;;maze,a labyrinth

(defun make-maze-ops (pair)
  "Make maze ops in both  direction"
  (list (make-maze-op (first pair) (second pair))
	(make-maze-op (second pair) (first pair))))

(defun make-maze-op (here there)
  "Mzke an operator to move between two places"
  (op `(move from,here to  ,there)
      :preconds `((at ,here))
      :add-list `((at ,there))
      :del-list `((at ,here))))

(defun mappend (fn the-list)
    (apply #'append (mapcar fn the-list)))


(defparameter *maze-ops*
  (mappend #'make-maze-ops
	   '((1 2) (2 3) (3 4) (4 9) (9 14) (9 8) (8 7) (7 12) (12 13)
	     (12 11) (11 6) (11 16) (16 17) (17 22) (21 22) (22 23)
	     (23 18) (23 24) (24 19) (19 20) (20 15) (15 10) (10 5) (20 25))))



(defun GPS2 (state goals &optional (*ops* *ops*))
  (find-all-if #'action-p
	       (achive-all (cons '(start) state) goals nil)))

(defun action-p (x)
  (or (equal x '(start)) (executing-p x)))

(defun find-path (start end)
  (let ((result (GPS2 `((at ,start)) `((at ,end)))))
    (unless (null result)
      (cons start (mapcar #'destination
			  (remove '(start) result
				  :test #'equal))))))

(defun destination (action)
  (fifth (second action)))

(defun make-block-ops (blocks)
  (let ((ops nil))
    (dolist (a  blocks)
      (dolist (b blocks)
	(unless (equal a b)
	  (dolist (c blocks)
	    (unless (or (equal c a) (equal c b))
	      (push (move-op a b c) ops)))
	  (push (move-op a 'table b) ops)
	  (push (move-op a b 'table ) ops))))
    ops))

(defun move-op (a b c)
  (op `(move ,a from ,b to ,c)
      :preconds `((space on ,a) (space on ,c) (,a on ,b))
      :add-list (move-ons a b c)
      :del-list (move-ons a c b)))

(defun move-ons (a b c)
  (if (eq b 'table )
      `((,a on ,c))
      `((,a on ,c) (space on ,b))))

(setf (symbol-function 'find-all-if) #'remove-if-not )

