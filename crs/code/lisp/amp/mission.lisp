;;; -------------------------------------------------------------------------
;;; mission.lisp
;;; - class defns etc for missions, phases in AMP
;;; - $Revision: 1.18 $
;;; -------------------------------------------------------------------------
;;; COPYRIGHT START
;;; COPYRIGHT END
;;; -------------------------------------------------------------------------

(cl:in-package :fuzzbomb)

                                        ;(declaim (optimize (speed 3) (safety 1)))


;;;------------------------------------------------------------------------
;;; Missions arrive with just a set of top-level goals. [NOT YET]
;;; Then the "canned" mission planner reduces those to a set of baseline.
;;; phases. Each phase has multiple problem configs (each of which has
;;; one or zero plans associated with it), so to start with, a default
;;; config is pushed onto each phase's pending-configs list. From then
;;; on, the AMP continues to pick pending configs off of phases, solve
;;; them via invocations of the CSM/SSP, move them to the planned-configs
;;; list, and put better (harder or simpler) configs on the pending list.
;;; When it becomes time for the plan to be executed, the best one is
;;; chosen off the planned-configs list using some TBD criteria.
;;;
;;; One thing to start working on now (7/1/01) is to select a problem
;;; config off of the pending-configs list based on some measure of which
;;; is most worth it, rather than just solving the default one.
;;;
;;; Stuff still true
;;; - The baseline phases are also enhanced to more successfully achieve
;;; the mission goals by assigning optional commitments (goals and
;;; threat handling) to different agents via bidding.
;;; - for now, this says nothing about how the incoming mission is specified;
;;; this is just a structure for the canned mission planner to fill out.
;;; - as the Master AMP contracts out for the various optional commitments,
;;;     they are removed from the goals/threats lists in this object.
;;;     - for now, only the master AMP uses those lists; others just have
;;;     phases that get modified as they take on contracts.
;;;
;;; Note: Each amp has its own mission object.
;;;       Currently, only the master uses the goals and threats slots.
;;;
;;;


(defclass mission ()
  ((name :initform nil :initarg :name :accessor name)
   (amp :initform nil :initarg :amp :accessor amp
        :documentation "My amp")
   (phases :initform nil :initarg :phases :accessor phases
           :documentation "List of mission-phase objects comprising this mission")
   (goals :initform nil :initarg :goals :accessor goals
          ;; opportunity-probability is "Probability of opportunity arising to get this goal."
          ;; See goal-contract definition in contract-class.lisp.
          :documentation "List of (phase-name goal opportunity-probability) triples for this mission")
   (threats :initform nil :initarg :threats :accessor threats
            ;; lethality is [0 1] "probability that you'll die if dont handle this threat *in this phase*".
            ;; See threat-contract definition in contract-class.lisp.
            :documentation "List of (phase-name threat lethality) triples for this mission")
   (goal-skills :initform nil :initarg :goal-skills :accessor goal-skills
                :documentation "List of (goal trans-or-machine quality) triples that apply to all phases.")
   (threat-skills :initform nil :initarg :threat-skills :accessor threat-skills
                  :documentation "List of (threat trans-or-machine quality) triples that apply to all phases.")
   ))

(defun mission-p (x)
  (typep x 'mission))

(defmethod lp ((obj mission))
  (format T "#<~A ~A~%" (type-of obj) (name obj))
  (format T "   Phases: ~S~%" (phases obj))
  (format T "   Goals: ~S~%" (goals obj))
  (format T "   Threats: ~S~%" (Threats obj))
  (format t ">~%"))

(defmethod print-object ((obj mission) stream)
  (format stream "#<~A ~A>" (type-of obj) (name obj)))

;;;------------------------------------------------------------------------
;;; Find phase of mission by name (symbol)
;;; - if you pass in a phase obj, it just returns it, which is
;;;     useful for conditioning args to other functions.

(defun find-phase (name &optional (m (mission *self*)))
  ;;   Either it is a phase instance, or we
  ;; should just look for it with find. If we restrict ourselves to
  ;; symbolp, we cannot easily auto-gen unique names for phases in a
  ;; mission with a moving horizon.
  (cond ((phase-p name) name)
        (;; (symbolp name)
         t
         (dbug :amp "Mission: ~A" m)
         (find name (phases m) :test #'string-equal :key #'name))))

;;;------------------------------------------------------------------------

;;; These methods supplied for convenient mission description at start-up.
;;; In general, skills are managed at the phase level.

;;;  
;;; These methods create the skills objects and add them to the amp.
;;; Slightly complicated by desire to maintain backward-compatibility with existing
;;; domains, such as they are.
;;; First we create a single skill object.  Then we populate various lists at amp,
;;; mission, and phase levels, checking consistency with any existing objects in those lists.
;;; Old way created different objects for different lists, but I don't see need for that.
;;;   Possibly should remove all but the amp-level slots for skills.
;;; In fact, I'd like to squash down to a single skills slot in the amp object, which doesn't
;;; distinguish between threats and goals.
(defmethod init-threat-skill ((m mission) phase-name-or-all
                              threat trans-or-machine quality)
  "Takes five args.  First is a mission object.  Second is an atom or list, identifying
   a phase or set of phases (:all or 'all are special symbols indicating all phases).
   Third is symbol identifying a threat (used for lookup during negotiation and configuration.
   Fourth is a single symbol or list of symbols naming transitions.  Fifth is a number."
  (let ((skill (make-instance 'threat-skill
                 :threat threat
                 :torm (ensure-list-if-atom trans-or-machine)
                 :phases phase-name-or-all
                 :quality quality))
        ;;   amp slot of mission doesn't always get set, so use *self* instead.
        (places-to-add-skill (list m *self*))) ;; modified below
    ;; build up places-to-add-skill, which may (for now) include mission, amp, phases.
    (cond 
     ;; Handle :all.  We're insisting that :all be passed in as an atom (not as part of a list).
     ((and (atom phase-name-or-all)
           (member phase-name-or-all '(all :all)))
      (setf places-to-add-skill (union (phases m) places-to-add-skill)))
     ;; handle phase list
     (t (let ((phase-list (ensure-list-if-atom phase-name-or-all)))
          (dolist (phase-name phase-list)
            (let ((phase (find-phase phase-name m)))
              (when (not phase)
                (error "Could not find phase named ~s in mission ~s." phase-name m))
              (push phase places-to-add-skill))))))
    ;; add skill wherever desired
    (dbug :test "Adding skills to ~s." places-to-add-skill)
    (dolist (obj places-to-add-skill)
      (setf (threat-skills obj) (add-skill-checking-compatibility skill (threat-skills obj))))
    skill))

(defmethod init-goal-skill ((m mission) phase-name-or-all
                              goal trans-or-machine quality)
  "Takes five args.  First is a mission object.  Second is an atom or list, identifying
   a phase or set of phases (:all or 'all are special symbols indicating all phases).
   Third is symbol identifying a goal (used for lookup during negotiation and configuration.
   Fourth is a single symbol or list of symbols naming transitions.  Fifth is a number."
  (let ((skill (make-instance 'goal-skill
                 :goal goal
                 :torm (ensure-list-if-atom trans-or-machine)
                 :phases phase-name-or-all
                 :quality quality))
        ;;   amp slot of mission doesn't always get set, so use *self* instead.
        (places-to-add-skill (list m *self*))) ;; modified below
    ;; build up places-to-add-skill, which may (for now) include mission, amp, phases.
    (cond 
     ;; Handle :all.  We're insisting that :all be passed in as an atom (not as part of a list).
     ((and (atom phase-name-or-all)
           (member phase-name-or-all '(all :all)))
      (setf places-to-add-skill (union (phases m) places-to-add-skill)))
     ;; handle phase list
     (t (let ((phase-list (ensure-list-if-atom phase-name-or-all)))
          (dolist (phase-name phase-list)
            (let ((phase (find-phase phase-name m)))
              (when (not phase)
                (error "Could not find phase named ~s in mission ~s." phase-name m))
              (push phase places-to-add-skill))))))
    ;; add skill wherever desired
    (dbug :test "Adding skills to ~s." places-to-add-skill)
    (dolist (obj places-to-add-skill)
      (setf (goal-skills obj) (add-skill-checking-compatibility skill (goal-skills obj))))
    skill))

(defun add-skill-checking-compatibility (skill skill-list)
  (let ((matching-skill (find skill skill-list :test #'same-threat-or-goal)))
    (cond 
     ;; If there's already a matching skill, update it with merge and leave the list alone.
     (matching-skill
      (merge-skills matching-skill skill)
      skill-list)
     ;; Otherwise, push skill onto skill-list.
     (t
      (push skill skill-list)
      skill-list))))

(defmethod clear-skills ((mission mission))
  (setf (threat-skills mission) nil)
  (setf (goal-skills mission) nil)
  (dolist (phase (phases mission))
    (clear-skills phase)))

(defun make-threat-skill (phase-name-or-all threat trans-or-machines quality)
  "Helper to simply converting specs to objs, ala (apply #'make-threat-skill spec)."
  (make-instance 'threat-skill
                 :phases phase-name-or-all
                 :threat threat
                 :torm (ensure-list-if-atom trans-or-machines)
                 :quality quality))

(defun make-goal-skill (phase-name-or-all goal trans-or-machines quality)
  "Helper to simply converting specs to objs, ala (apply #'make-goal-skill spec)."
  (make-instance 'goal-skill
                 :phases phase-name-or-all
                 :goal goal
                 :torm (ensure-list-if-atom trans-or-machines)
                 :quality quality))

;;;------------------------------------------------------------------------
;;; Handling of RTS schedule slots:
;;; - when you first download to a slot
;;;------------------------------------------------------------------------
;;; Note we had to name this class "mission-phase" b/c "phase" caused
;;; name conflict w/ Lisp.

;;; There is currently no template-config.
;;; (There is now one "template-config" per phase, which constitutes
;;; the most basic requirements for a valid plan to handle this phase.
;;; This template-config is not necessarily associated with any of the
;;; generated plans in the current-plan or inferior-plans slots. Each of
;;; those plans has its own slot holding a pointer to its associated plan.
;;; The template-config has its own slot for historical and tradeoff
;;; reasoning purposes only.)

;;; Handled configs are those with a plan in the plan slot.
;;; Pending configs are those with a partial plan, or no plan.

;;; Currently, only a distinction between current-plan and inferior-plans.
;;; Default plans not used yet.

(defclass mission-phase ()
  ((name :initform nil :initarg :name :accessor name)
   (mission :initform nil :initarg :mission :accessor mission
            :documentation "Back-ptr to parent mission object.")
   (duration :initarg :duration :accessor duration
             :documentation "Estimated duration in quanta.")
   (start-time :initform 0 :initarg :start-time :accessor start-time
               :documentation "Computed start time of phase in quanta.")
   (threat-skills :initform nil :initarg :threat-skills :accessor threat-skills
                  :documentation "List of threat-skill objects.")
   (goal-skills :initform nil :initarg :goal-skills :accessor goal-skills
                :documentation "List of goal-skill objects.")
   (planned-configs :initform nil :initarg :planned-configs
                    :accessor planned-configs
                    :documentation "Problem config objects with a complete plan attached.")
   (pending-configs :initform nil :initarg :pending-configs
                    :accessor pending-configs
                    :documentation "Problem config objects  with a partial or non-existent plan attached.")
   (rts-current-index :initform (decf *rts-current-index-number* 2)
                      :initarg :rts-current-index
                      :accessor rts-current-index
                      :documentation "The current index into the rts tap schedule buffer points to the current valid runnable schedule.")
   (potential-reward
    :initform 0 :initarg :potential-reward :accessor potential-reward
    :documentation "Reward potentially obtainable in this phase.")
   ))

(defmethod amp ((obj mission-phase))
  "Get the amp that this mission-phase is part of.  Returns nil if
   not currently associated with an amp."
  (when (mission obj)
    (amp (mission obj))))

(defmethod clear-skills ((phase mission-phase))
  (setf (threat-skills phase) nil)
  (setf (goal-skills phase) nil))

;; protection for the slot accessor above against nil args
(defmethod rts-current-index ((n null))
  nil)

(defun phase-p (x)
  (typep x 'mission-phase))

(defun mission-phase-p (x)
  (typep x 'mission-phase))

(defmethod lp ((obj mission-phase))
  (format T "#<~A ~A>~%" (type-of obj) (name obj)))

(defmethod print-object ((obj mission-phase) stream)
  (format stream "#<~A ~A>" (type-of obj) (name obj)))

;;; Compute which phase follows this one.
(defmethod next-phase-name ((p mission-phase))
  (second (member (name p) *phasename-order*)))

(defmethod next-phase ((p mission-phase))
  (if (null (next-phase-name p))
      nil
    (find-phase (next-phase-name p) (mission p))))

;;; Return T if no next phase.
(defmethod final-phase-p ((p mission-phase))
  (null (next-phase p)))

;;; returns the last quantum this phase will run during.  one-minus
;;; because of zero based counting.
(defmethod end-time ((p mission-phase))
  (1- (+ (start-time p) (duration p))))

;;; Note: These functions are non-destructive.
;;; Next index means where you'd put a new tap schedule.
;;; Next is always guaranteed to return a valid index.
(defmethod rts-next-index ((p mission-phase))
  (declare (special *always-use-same-rts-sched-index-for-phase*))
  (if *always-use-same-rts-sched-index-for-phase* 
      (abs (rts-current-index p))
      (toggle-rts-index (rts-current-index p))))

;;; Given a current index, compute the next available place to store
;;; a new tap schedule.
(defun toggle-rts-index (i)
  (cond ((minusp i) (abs i))
        ((oddp i) (1+ i))
        (t (1- i))))

;;; -------------------------------------------------------------------------
;;; which mission phase object would/has downloaded to given RTS schedule slot?

(defun find-phase-for-slot (slot)
  (dolist (ph (phases (mission *self*)))
    (when (or (eq slot (rts-current-index ph))
              (eq slot (rts-next-index ph)))
      (return-from find-phase-for-slot ph)))
  nil
  )

;;;------------------------------------------------------------------------

;;; Methods
;;;------------------------------------------------------------------------

;;; Skills are kept in the phase(s) in which they a;pply.
;;; Can add skills at the mission or phase level.
;;; TorM = Transition or Machine.
;;; Note that these are methods on a mission-phase.
;;; Both the mission and mission-phase classes are defined in this file.

(defmethod add-threat-skill ((p mission-phase) (spec list))
  (destructuring-bind (threat TorM quality) spec
    (let ((new-threat-skill
           (make-instance 'threat-skill
             :phases (list p)
             :threat threat
             :torm TorM
             :quality quality)))
      (push new-threat-skill (threat-skills p)))))

(defmethod add-threat-skill ((p mission-phase) (ts threat-skill))
  "Add an existing skill object to a mission phase."
  ;; sanity check - This phase must be a member of this skill's phases list.
  (assert (or (eq :all (phases ts)) (and (listp (phases ts)) (member p (phases ts)))))
  (push ts (threat-skills p)))

(defmethod add-goal-skill ((p mission-phase) (spec list))
  (destructuring-bind (goal TorM quality) spec
    (let ((new-goal-skill
           (make-instance 'goal-skill
             :phases (list p)
             :goal goal
             :torm TorM
             :quality quality)))
      (push new-goal-skill (goal-skills p)))))
  
;;;(defmethod add-goal-skill ((p mission-phase) goal TorM quality)
;;;  (let ((new-goal-skill
;;;         (make-instance 'goal-skill
;;;           :phases (list p)
;;;           :goal goal
;;;           :torm TorM
;;;           :quality quality)))
;;;    (push new-goal-skill (goal-skills p))))

(defmethod add-goal-skill ((p mission-phase) (gs goal-skill))
  "Add an existing skill object to a mission phase."
  ;; sanity check - This phase must be a member of this skill's phases list.
  (assert (or (eq :all (phases gs)) (and (listp (phases gs)) (member p (phases gs)))))
  (push gs (goal-skills p)))

;;;------------------------------------------------------------------------
;;; Look up a skill by the threat or goal expression it defeats/achieves.

(defmethod find-threat-skill ((p mission-phase) threat)
  (find threat (threat-skills p) :key #'threat :test #'equal))

(defmethod find-goal-skill ((p mission-phase) goal)
  (find goal (goal-skills p) :key #'goal :test #'equal))

;;; Find threat or goal skill.
;;; Assumes goals and threats have unique names.
;;; Note that at the mission-phase level, goals and threats still
;;; stored separately.
(defmethod find-TorG-skill ((p mission-phase) torg)
  (or  (find torg (goal-skills p) :key #'goal :test #'equal)
       (find torg (threat-skills p) :key #'goal :test #'equal)))


;;;------------------------------------------------------------------------
;;; Just a temp hack to determine the current config
;;; This means current config to solve (not to execute).
;;; It is used to identify the most current config to
;;; munge by adding a new goal, threat, whatever.

(defmethod default-config ((p mission-phase))
  (cond ((pending-configs p) (car (pending-configs p)))
        ((planned-configs p) (car (planned-configs p)))
        (T (error "No default config for phase ~A~%" p))))


;;; This replaces default configs, and is used to select next
;;; task to work on.
(defmethod all-configs ((p mission-phase))
  (append (pending-configs p) (planned-configs p)))

