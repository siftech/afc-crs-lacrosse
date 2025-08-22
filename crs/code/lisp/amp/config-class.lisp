;;; -------------------------------------------------------------------------
;;; config-class.lisp
;;; - class defns for problem configurations (config) classes in AMP
;;; - $Revision: 1.18 $
;;; -------------------------------------------------------------------------
;;; COPYRIGHT START
;;; COPYRIGHT END
;;; -------------------------------------------------------------------------

(cl:in-package :fuzzbomb)

                                        ;(declaim (optimize (speed 3) (safety 1)))

;;;------------------------------------------------------------------------
;;; Configs are problem configurations. A given phase can have multiple
;;; configs associated with it. Each config has a plan object hanging off
;;; of its plan slot, and the "stored plan" slot of this plan object will
;;; eventually be filled in when the SSP/CSM is invoked to do the planning.
;;; It is possible the config is insoluble, however, in which case we'll
;;; have some way of marking that the config plan attempt failed.
;;;
;;; The prob-config is generated first (as a list of API calls) for a given
;;; phase, this plan object is created, and the new config is placed on the
;;; "pending-configs" list for the phase. At some later point, the SSP/CSM
;;; generates a plan (called a "stored-plan" by the SSP), and a pointer to
;;; this structure is stored in the stored-plan slot of the plan object.

;;; The config object is then moved from pending-configs to the planned-configs
;;; list. Later comparison (sorting, etc.) can be done on the planned-configs
;;; list to decide which plan to run when execution is warranted.
;;;
;;;


(defclass config ()
  ((mission-phase :initform nil :initarg :mission-phase :accessor mission-phase
                  :documentation "The phase this config is associated with.")
   (api-list :initform nil :initarg :api-list :accessor api-list
             :documentation "The quoted api list of calls that represents the problem description for CSM/SSP purposes.")
   (plan :initform nil :initarg :plan :accessor plan
         :documentation "The amp plan object.")
   (contracts :initform nil :initarg :contracts :accessor contracts
              :documentation "Contracts that affected this config.")
   (task :initform nil :initarg :task :accessor task
         :documentation "AMP task for CSM-planning this config.")
   ))

;;; Virtual slots

(defmethod amp ((obj config))
  *self*)

;;;(defmethod amp ((obj config))
;;;  "Get the amp that this config is part of.  Returns nil if
;;;   not currently associated with an amp."
;;;  (when (mission-phase obj)
;;;    (amp (mission-phase obj))))


;;; The quality of the plan for this config.
;;; This will be zero if no SSP plan created yet.
(defmethod quality ((config config))
  (quality (plan config)))

;;; The stored (SSP) plan for this config.
(defmethod stored-plan ((config config))
  (stored-plan (plan config)))


;; Computing utilities.

(defun config-p (x)
  (typep x 'config))

(defmethod lp ((obj config))
  (format T "#<~A:~%" (type-of obj))
  (format T "   Phase: ~S~%"    (mission-phase obj))
  (format T "   Plan: ~S~%"     (plan obj))
  (format T "   API List: ~S~%" (api-list obj))
  (format T "   Quality: ~S~%"  (quality obj))
  (format t ">~%"))

(defmethod print-object ((obj config) stream)
  (format stream "#<~A for ~A: T:~A G:~A>"
          (type-of obj) (name (mission-phase obj))
          (length (remove-if-not #'threat-contract-p (contracts obj)))
          (length (remove-if-not #'goal-contract-p (contracts obj)))))

;;; Actions for threats hack
;;;------------------------------------------------------------------------
;;; - an interesting question is how the actions are given to the CSM:
;;;     are they all presented in here, then glomped into prob prob-configs
;;;     already tied to the machines they address  (ie, radar-threat machine
;;;     also includes actions to defeat radar threat).
;;; - or does AMP somehow figure out which to include for a partic threat/goal?
;;; - the latter provides greater flexibility for AMP to reconfigure set
;;;     of available actions independent of the threat set.
;;;     - also, can be used at bid time to determine if can bid, and how much
;;; - the former is easier.... add-threat in contract carries all we need
;;;     to do to update prob-config and re-run.
;;; - the current development code is a first hack at the latter, in a can;
;;;     - canned actions-for-threats in amp object gives appropriate actions
;;;     for a given threat machine; could be auto-generated, but is not now.
;;;------------------------------------------------------------------------

;;;  
;;; Pull these from amp level objects.

(defun actions-for-threat (threat)
  "Get list of actions corresponding to threat from amp object."
  (let ((skill (find threat (threat-skills *self*) :key #'threat)))
    (when skill
      (torm skill))))

(defun actions-for-goal (goal)
  "Get list of actions corresponding to goal from amp object."
  (let ((skill (find goal (goal-skills *self*) :key #'goal)))
    (when skill
      (torm skill))))

;;; Methods

;;; Mutators for configs.
;;; Return the mutated object.

(defmethod copy-config ((config config))
  (make-instance 'config
    :mission-phase (mission-phase config)
    :api-list (api-list config)
    :plan (plan config)
    :contracts (contracts config)))

;;; Add a new threat.
;;; When agent agrees to a contract to handle a threat, it calls this
;;; to add related stuff (trans, machine, actions) to the appropriate
;;; config.
;(defmethod add-threat ((config config) threat)
;  (add-trans-or-machine config threat)
;  (add-actions-for-threat config threat))
;
;;;; Add a new goal
;(defmethod add-goal ((config config) goal)
;  (add-config-item config :goals goal)
;  (dolist (a (actions-for-goal goal))
;    (add-trans-or-machine config a)))

;;;------------------------------------------------------------------------
;;; adds the appropriate actions to handle a threat to the config
;;; - eventually the AMP should be able to make ongoing tradeoff
;;;     decisions about which actions to try to use, in cases where it
;;;     has some alternative versions of actions useful in similar situations.

;(defmethod add-actions-for-threat ((config config) threat)
;  (dolist (a (actions-for-threat threat))
;    (add-trans-or-machine config a)))

;;; - note this adds the name of the action/machine, not the object, to the
;;;     def-problem API call in phase's prob-config.
;;; - note this prefers machines to transitions, in event they have same name.
;;;     - generally this makes sense b/c machines tend to subsume transitions.

;(defmethod add-trans-or-machine ((config config) TorM)
;  ;; the NIL arg below means "dont throw error if dont find it"
;  (cond ((find-instance-by-name 'machine torm nil)
;         (add-config-item config :machines torm))
;        ;; important to use find-trans to get fix-dashes effect
;        ((find-trans-by-name torm nil)
;         (add-config-item config :transitions torm))
;        (T (error "Couldnt find action/machine matching ~A" torm))))


;;; Deletors

;;; Delete a threat.
(defmethod delete-threat ((config config) threat)
  (declare (ignore threat))
  nil)

;;; Delete a goal.
(defmethod delete-goal ((config config) goal)
  (declare (ignore goal))
  nil)


;;; Delete a machine or transition
(defmethod delete-machine-or-transition ((config config) TorM)
  (declare (ignore torm))
  nil)


;;;------------------------------------------------------------------------

;;;------------------------------------------------------------------------
;;; given a config obj and a new (generic) item, integrate the item into the
;;; config's api-list.

;;; Note eventually this would update the potential reward for a mission
;;; phase, based on what the goal object specifies as value.

;;; Note: This destructively modifies the api-list slot in place.
;;; If you now re-run the CSM planner, it will construct a new plan.

;;; We can no longer have this be destructive, since we want

(defmethod add-config-item ((config config) key item)
  (setf (api-list config)
    (setkeyval key (append (list item) (getkeyval key (api-list config)))
               (api-list config))))

(defmethod set-config-item ((config config) key item)
  (setf (api-list config)
    (setkeyval key (list item) (api-list config))))


;;;------------------------------------------------------------------------
;;; recursively traverse keylist until find the key, then return the next
;;; element.  Note we dont assume keylist is actually an even [key value]
;;; list, but can be odd, w/ non-keys interspersed.  Handles quoted API forms.

(defun getkeyval (key keylist)
  (cond ((null keylist) nil)
        ((eq key (first keylist)) (second keylist))
        (T (getkeyval key (rest keylist)))))

(defun setkeyval (key newval keylist)
  ;; if get to null keylist, means didnt find key; add it.
  (cond ((null keylist) (list key newval))
        ((eq key (first keylist)) (cons key (cons newval (rest (rest keylist)))))
        (T (cons (first keylist) (setkeyval key newval (rest keylist))))))


