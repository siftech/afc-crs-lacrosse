;;; -------------------------------------------------------------------------
;;; skill-class.lisp
;;; - Skills are capabilities agents have in the domain.
;;; - Currently this includes skills for defeating a threat, or
;;;   achieving a goal.
;;; - $Revision: 1.5 $
;;; -------------------------------------------------------------------------
;;; COPYRIGHT START
;;; COPYRIGHT END
;;; -------------------------------------------------------------------------

(cl:in-package :fuzzbomb)

;;; -------------------------------------------------------------------------
;;; All skills share these slots.
;;; More specific info available in specializations.
;;;


(defclass skill ()
  ((phases :initarg :phases :initform nil :accessor phases
           :documentation "List of mission-phases that in which this skill is available.
                           If the list contains the symbol :all, this skill is available in any phase.")
   (torm :initarg :torm :initform nil :accessor torm
         :documentation "Transition or machine responding to this threat.")
   (quality :initarg :quality :initform nil :accessor quality
            :documentation "Measure of how skillful the agent is at dealing with this threat or goal.
                            A number, zero or greater.  This value will be used by compute-bid to
                            calculate the agent's bid for a goal or threat task.")))

;;;-------------------------------------------------------------------------
;;; Skills that achieve a goal.

(defclass goal-skill (skill)
  ((goal :initarg :goal :initform nil :accessor goal)
   ))

(defun goal-skill-p (x)
  (typep x 'goal-skill))

(defmethod threat-or-goal ((skill goal-skill))
  (goal skill))

;;;-------------------------------------------------------------------------
;;; Skills that defeat a threat.

(defclass threat-skill (skill)
  ((threat :initarg :threat :initform nil :accessor threat)
   ))

(defun threat-skill-p (x)
  (typep x 'threat-skill))

(defmethod threat-or-goal ((skill threat-skill))
  (threat skill))

;;;----------------------------------------------------------------------
;;; merge skills

(defgeneric merge-skills (skill-1 skill-2)
  (:documentation "Attempt to merge the two skill objects.
   If they are incompatible, error.  If skill-1 is missing
   data that can be found in skill-2, put it in skill-1.
   Returns skill-1."))

(defmethod merge-skills ((skill-1 threat-skill) (skill-2 threat-skill))
  (setf (threat skill-1) (merge-if-compatible (threat skill-1) (threat skill-2)))
  (call-next-method)
  )

(defmethod merge-skills ((skill-1 goal-skill) (skill-2 goal-skill))
  (setf (goal skill-1) (merge-if-compatible (goal skill-1) (goal skill-2)))
  (call-next-method)
  )

(defmethod merge-skills ((skill-1 skill) (skill-2 skill))
  "Update skill-1 with any information it's missing, but does exist in skill-2.
   Return skill-1.  Will signal error, if merges are not compatible."
  (setf (phases skill-1) (merge-if-compatible (phases skill-1) (phases skill-2)
                                              :test #'atom-or-set-eq))
  (setf (torm skill-1) (merge-if-compatible (torm skill-1) (torm skill-2)
                                            :test #'atom-or-set-eq))
  (setf (quality skill-1) (merge-if-compatible (quality skill-1) (quality skill-2)
                                               :test #'=))
  skill-1)

(defun merge-if-compatible (v1 v2 &key (test #'eq))
  (cond ((null v1) v2)
        ((null v2) v1)
        ((not (funcall test v1 v2))
         (error "Attempt to merge incompatible values: ~a and ~a." v1 v2))
        (t v1)))

(defun atom-or-set-eq (aos-1 aos-2)
  (cond
   ((and (listp aos-1) (listp aos-2)
         (set-equal aos-1 aos-2))
    t)
   ((eq aos-1 aos-2)
    t)
   (t
    nil)))

(defmethod same-threat-or-goal ((s1 skill) (s2 skill))
  (eq (threat-or-goal s1) (threat-or-goal s2)))
  
;;;-------------------------------------------------------------------------

(defmethod print-object ((obj skill) stream)
  (format stream "#<~A>" (type-of obj)))

(defmethod print-object ((obj threat-skill) stream)
  (format stream "#<~A threat: ~a>" (type-of obj) (threat obj)))

(defmethod print-object ((obj goal-skill) stream)
  (format stream "#<~A goal: ~a>" (type-of obj) (goal obj)))

;;;-------------------------------------------------------------------------
;;; Used for more detailed printout of objects;
;;; Do this later.

;; lp = "long print"
(defmethod lp ((obj skill))
  (format t "#<~A >" (type-of obj) ))
;;(describe-object obj T))

