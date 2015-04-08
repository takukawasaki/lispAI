(load "~/Dropbox/lisp-AI-book/ch-3-tutorial/ch-3-tutorial.lisp")

(defvar *state* nil "The current state : a list of condition")
(defvar *ops* nil "A list of available operaters.")

(defstruct op "An operation"
	   (action nil)
	   (preconds nil)
	   (add-list nil)
	   (del-list))

(defun GPS (*state* goals *ops*)
  "General Problems Solver : achive all goals using *ops*"
  (if (every #'achive goals) 'solved))

(defun achive (goal)
  "A goal is achived if it already holds ,
or if there is an appropriate op for it that is applicable"
  (or (member goal *state*)
      (some #'apply-op
	    (find-all goal *ops* :test #'appropriate-p))))

(defun appropriate-p (goal op)
  "An op is appropriate to a goal if it is in its add list"
  (member goal (op-add-list op)))

(defun apply-op (op)
  "print a message and update *state* if op is appracable"
  (when (every #'achive (op-preconds op))
    (print (list 'executing (op-action op)))
    (setf *state* (set-difference *state* (op-del-list op)))
    (setf *state* (union *state* (op-add-list op)))
    t))


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

(defun achive-all (goals)
  "Try to achive each goals, then make sure they still holds."
  (and (every #'achive goals) (subsetp goals *state*)))


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


