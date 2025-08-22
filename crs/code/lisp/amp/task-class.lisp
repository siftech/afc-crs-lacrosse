;;; -------------------------------------------------------------------------
;;; task-class.lisp
;;; - task objects that AMP works on...
;;; -------------------------------------------------------------------------
;;; COPYRIGHT START
;;; COPYRIGHT END
;;; -------------------------------------------------------------------------

(cl:in-package :fuzzbomb)

;(declaim (optimize (speed 3) (safety 1)))


;;; Set to select which utility function to use.
(define-restricted-var *delib-mode*
 ( :discounted-utility :utility
        :marginal-utility :shortest :earliest :earliest-shortest :threats-plus-goals)
  "Valid values:
        :discounted-utility - maximizes time-discounted incremental utility
        :utility - maximizes incremental utility
        :marginal-utility - max. marginal utility of task, ignoring task duration
        :shortest - chooses shortest task
        :earliest - chooses task with earliest phase
        :earliest-shortest - chooses shortest task with earliest phase
        :threats-plus-goals - runs hardest first....not a very good idea,
        but reqd for hacking in oep, for now   4/02
  ")

;;;------------------------------------------------------------------------
;;; TASK
;;;------------------------------------------------------------------------

(defclass task (named-object)
  ((status :initform :new :initarg :status :accessor status )
   ))

;; Define utilities, etc.

;; Defaults for non-plan-config-tasks and other ones not specialized
(defmethod scoring-fn ((task task)) 1)

(defun task-p (x)
  (typep x 'task))

(defmethod lp ((obj task))
  (format T "#<~A ~A>~%" (type-of obj) (name obj)))

(defmethod print-object ((obj task) stream)
  (format stream "#<~A ~A>" (type-of obj) (name obj)))

;;;   In lax finals land, the challenge-tasks (lax-ctask) have
;;; deadlines which can be checked.  Internal tasks, like deliberation-task and
;;; process-delib-result-task, don't have deadlines.
(defmethod deadline-exceeded-p ((task task))
  (declare (ignorable task))
  nil)

;;;------------------------------------------------------------------------

(defclass process-msgs-task (task)
 ()
)


(defmethod execute-task ((task process-msgs-task))
  (declare (special *sockets* *self*))
  (process-msgs)
  (dolist (s *sockets*)
        (when (listen s)
  		(push task (tasks *self*))
		(return-from execute-task)))
)

;;;------------------------------------------------------------------------
(defclass is-this-best-task (task)
  (;; List of revisions to check.
   (svc-revs :initform nil :initarg :svc-revs :accessor svc-revs)
   ;; Revision this is currently checking.
   (svc-rev :initform nil :initarg :svc-rev :accessor svc-rev)
   ;; alist of svc-rev to list of test results it is waiting for.
   (pending-tests :initform nil :initarg :pending-tests :accessor pending-tests)))

;; we only ever want one of these.  if it is on the (tasks *self*), then we should start some new tests.
(defvar *is-this-best-task*
      (make-instance 'is-this-best-task))

;;;------------------------------------------------------------------------
;;; Initial (non-contracted) config task have "Init" name and no contracts.
;;; Other plan-config tasks have 1 or more contracts in the contracts slot.

(defclass plan-config-task (task)
  ((config :initform nil :initarg :config :accessor config)
   (contracts :initform nil :initarg :contracts :accessor contracts)
   ))

(defun plan-config-task-p (x)
  (typep x 'plan-config-task))

(defmethod print-object ((obj plan-config-task) stream)
  (format stream "#<~A ~A Phase: ~A T:~A G:~A>"
          (type-of obj) (name obj) (phasename obj)
          (length (remove-if-not #'threat-contract-p (contracts obj)))
          (length (remove-if-not #'goal-contract-p (contracts obj)))))

(defmethod mission-phase ((plan-config-task plan-config-task))
  (mission-phase (config plan-config-task)))

(defmethod phasename  ((plan-config-task plan-config-task))
  (name (mission-phase plan-config-task)))

(defmethod end-time ((task plan-config-task))
  (end-time (mission-phase task)))

(defmethod start-time ((task plan-config-task))
  (start-time (mission-phase task)))

;;; Various task scoring functions.

(defun cur-phase ()
  (setf *cur-phase* (cur-phase-no-rts)))  ;else compute where the

;;;------------------------------------------------------------------------
;;; This function computes the current phase based on the
;;; value of *cur-quantum* and the expected start times of
;;; the phases. This is useful for debugging when *run-rts*
;;; is nil, and the RTS doesn't update *cur-phase*
;;; Note that phases must be ordered for this to work.
;;;  : old ver would error out when we get past
;;; end of mission...also was wrong for last phase.
;;; - now returns :EOM distinguished phase token... possibly we should
;;; create a real end-of-mission phase object.

(defun cur-phase-no-rts ()
    (declare (special *self*))
    (dolist (p (phases (mission *self*)))
      (when (and (>= *cur-quantum* (start-time p))
                 (< *cur-quantum* (end-time p)))
          (return-from cur-phase-no-rts p)))
  :EOM)

;;;------------------------------------------------------------------------
;;; returns q from now until end of the arg.  if in past, 0

(defun quanta-from-now-through (&optional (p (cur-phase)))
  "Return time in quanta from now until the end of the phase p,
   or 0, if p has already ended."
  (max 0 (- (end-time p) *cur-quantum*)))

;;;------------------------------------------------------------------------
;;; returns q from now until start of the arg; if already in progress, 0

(defun quanta-from-now-until (&optional (p (cur-phase)))
  "Return time in quanta from now until the start of the phase p,
   or 0, if p has already started."
  (max 0 (- (start-time p) *cur-quantum*)))

;;;------------------------------------------------------------------------
;;;
;;; Scoring functions.

;;;------------------------------------------------------------------------
;;; This scoring function dispatches to scores a task based on the value
;;; of *delib-mode*.
;;; Note that the higher the result, the more favorable, so costs
;;; must be (or divided).
;;; - if you want to prune a task, return a negative.

(defmethod scoring-fn ((task plan-config-task))
  (ecase *delib-mode*
    (:shortest (score-for-shortest task))
    (:earliest (score-for-earliest task))
    (:earliest-shortest (score-for-earliest-shortest task))
    (:marginal-utility (marginal-expected-utility task))
    (:utility (incr-utility task))
    (:discounted-utility (discounted-incr-utility task))
    (:threats-plus-goals (score-for-threats-plus-goals task))
    ))


;;;------------------------------------------------------------------------
;;; Score for shortest.  Invert b/c we're always maximizing score.
;;; - if task will finish after the phase it helps, ferget it.

(defmethod score-for-shortest ((task plan-config-task))
  (let* ((cost (quanta-cost task))
         (finishtime (+ *cur-quantum* cost)))
  (if (>= finishtime (end-time task))
        -1
        (/ 1 cost))))

;;;------------------------------------------------------------------------
;;; - inverted so more of thse is worse... that means when plan
;;; the longer ones later, will replace shorter/easier.
(defmethod score-for-threats-plus-goals ((task plan-config-task))
   (/ 1 (max 1 (length (contracts task)))))

;;;------------------------------------------------------------------------
;;; Earliest
;;; The earlier the smaller number of quanta from now until
;;; end of task's phase.  Use end to value in-progress phases >0.
;;; Invert b/c we're always maximizing score.
;;; - this should go negative if task is for past phase, eliminating it...

(defmethod score-for-earliest ((task plan-config-task))
  (let* ((end-time (end-time task)))
  (if (>= *cur-quantum* end-time)
        -1
        (/ 1 (- end-time *cur-quantum*)))))


;;; - Add on quanta-cost to also account for runtime...
;;; - first sort by earlyphase, then runtime.
(defmethod score-for-earliest-shortest ((task plan-config-task))
  (let* ((end-time (end-time task))
         (cost (quanta-cost task)))
  (if (>= *cur-quantum* (- end-time cost))
        -1
        (/ 1 (- (+ end-time cost) *cur-quantum*)))))

;;;------------------------------------------------------------------------
;; Discounted Utility

(defmethod discounted-incr-utility ((task plan-config-task))
  (let ((discounted-incr-utility (* (expt *greedy-discount-factor* (quanta-from-now-until task))
                                    (incr-utility task))))
    (dbug :delib-trace "discounted-incr-utility: ~d~%" discounted-incr-utility)
    discounted-incr-utility))

;;;------------------------------------------------------------------------
;;; Note that a plan-config-task can have 0, 1, or multiple
;;; contracts associated with it now. This function has to be
;;; rewritten.

(defmethod incr-utility ((task plan-config-task))
  (let ((ben (marginal-expected-utility task))
        (cost (quanta-cost task)))
    (if (or (zerop ben) (zerop cost))
        (progn
          (dbug :delib-trace "incr-utility will return 0.0: ben: ~A cost ~A~%" ben cost)
          0.0)
        (progn ;else
          (dbug :delib-trace "incr-utility will return ~A: task: ~A ben: ~A cost: ~A~%"
                (float (/ (float ben) (float cost))) task ben cost)
          (float (/ (float ben) (float cost))))
        )))

;;;------------------------------------------------------------------------
;;; in converting seconds from perf prof to quanta, we use ceiling so
;;; that fractional quanta are counted as wholes.
;;; - ie, if task takes 2.5 sec and sec-per-q is 1, this is
;;; going to take 3 q to finish, not two.
;;; - NOTE a problem here is this assumes that the baseline
;;; goals/threats dont count in perf-prof lookup, and only counts the
;;; ones that are added via contracting..

(defmethod quanta-cost ((task plan-config-task))
  (let ((num-threats
                (length (remove-if-not #'threat-contract-p (contracts task))))
        (num-goals
                (length (remove-if-not #'goal-contract-p (contracts task)))))
  (ceiling (perf-prof :goals num-goals :threats num-threats)
        *secs-per-quantum*)))

;;;;------------------------------------------------------------------------
;(defmethod benefit ((task plan-config-task))
;  (benefit (contracts task)))
;
;;;;------------------------------------------------------------------------
;
;(defmethod benefit ((contract-list cons))
;  (loop for c in contract-list
;       summing (benefit c)))

;;; Return zero cost on null arg.
(defmethod quanta-cost ((nothing null)) 0)

;;(defmethod benefit ((nothing null)) 0)

;;;------------------------------------------------------------------------
;;; To find the expected utility of an action, we need to figure out how
;;; probable we are to get its good-outcome, multiply that times the
;;; expected-future-payoff and add the remaining
;;; probability times the stay-same payoff
;;; - the marginal utility is that minus just the stay-same payoff
;;;     from now on, instead of doing the planning and running new plan
;;; exputil of task is pT + (1-p)Noop = pT + Noop -pNoop
;;; so marginal is above -Noop = pT-pNoop = p(T-Noop)
(defmethod marginal-expected-utility ((task plan-config-task))
  (let* ((plan-prob (perf-prof-prob task))
         (delib-quantum *cur-quantum*)
         (efp-task (expected-future-payoff task delib-quantum))
         (efp-noop (expected-future-payoff nil delib-quantum))
         (meu (* plan-prob (- efp-task efp-noop)))
         )

    (dbug :delib-trace "meu for ~A: ~A"
          task meu)
    (dbug :delib-trace "based on efp-task: ~A efp-noop: ~A plan-prob: ~A"
          efp-task efp-noop plan-prob)
    meu
    ))

;;;------------------------------------------------------------------------
;;; 11/15/01   the best config is, for now, just the
;;; most recently planned... this is potentially inaccurate b/c of
;;; dependencies between plans for diff phases due to survival probability.
;;; For ex, if you first plan for high survival in phase A and then
;;; plan for high goal effort but less survival in phase A and
;;; laster plan for a subseq phase B to get lots of goal juju, then the
;;; earlier plan for A may be preferable to the later plan for A.
;;; - however, we cannot afford to think about all the possible combinations
;;; of diff plans/configs for all diff phases, during utility calculations
;;; or during delib sched in general.

(defun best-config-for-phase (p)
  (first (planned-configs (find-phase p))))

(defun best-plan-for-phase (p)
  (plan (first (planned-configs (find-phase p)))))

;;;------------------------------------------------------------------------
;;; if you solve the task, you expect to get this much payoff by
;;; living the world w/ the current best plans for each phase plus the
;;; new one from solving the contract.
;;;   11/20/01: if you are gonna finish the plan after the relevant
;;; phase has started, only get partial credit for the goal juju.

(defmethod expected-future-payoff (task delib-quantum)  ;; task could be nil.
  (declare (special *self*))
  (dbug :delib-trace "Computing expected future payoff for plan-config-task: ~A at quantum ~A" task delib-quantum)
  (let ((cum-surv-prob 1)
        (cum-expected-payoff 0)
       ; (task-phase (when task (find-phase (phasename task))))
	)

    (dolist (phase (this-and-future-phases (mission *self*)))
      (let* ((phase-duration (duration phase))
             (time-living-in-newplan (time-living-in-newplan task phase delib-quantum))
             (time-ratio (min 1 (/ time-living-in-newplan phase-duration)))
             (phase-surv-prob (survival-probability-of-task-in-phase task phase))
             (phase-reward (* time-ratio
                              (reward-of-task-in-phase task phase))))

;        (when (and task
;                   (eq phase task-phase))
;          (dbug :delib-trace "  unhandled-contracts: ~A"
;                (unhandled-contracts (config task))))

        (*= cum-surv-prob phase-surv-prob)
        (incf cum-expected-payoff
              (* cum-surv-prob phase-reward))
        (when task
          (dbug :delib-trace "   Phase ~A, cum-surv-prob is now ~A, phase-reward is ~A, time-ratio is ~A, cum-expected-payoff is ~A"
                phase cum-surv-prob phase-reward time-ratio cum-expected-payoff)
          )
        ))
    cum-expected-payoff
    ))

(defmethod time-living-in-newplan ((task null) (phase mission-phase) (delib-quantum t))
  (min (duration phase) (- (end-time phase) delib-quantum)))

(defmethod time-living-in-newplan ((task plan-config-task) (phase mission-phase) (delib-quantum t))
  (let ((task-phase (when task (find-phase (phasename task))))
        (duration (duration phase)))
    (cond ((eql phase task-phase)
           (let* ((cost (quanta-cost task))
                  (finishtime (+ delib-quantum cost)))
             (min duration (- (end-time task-phase) finishtime))))
          (t
           (time-living-in-newplan nil phase delib-quantum)))))

(defmethod survival-probability-of-task-in-phase ((task plan-config-task) (phase mission-phase))
  (if (eq (find-phase (phasename task)) phase)
      (survival-probability task)
    (survival-probability phase)))

(defmethod survival-probability-of-task-in-phase ((task null) (phase mission-phase))
  (survival-probability phase))

(defmethod reward-of-task-in-phase ((task plan-config-task) (phase mission-phase))
  (if (eq (find-phase (phasename task)) phase)
      (reward task)
    (reward phase)))

(defmethod reward-of-task-in-phase ((task null) (phase mission-phase))
  (reward phase))

;;;------------------------------------------------------------------------
;; [sfriedman:20121116.0959CST] Specializing this on mission type, to
;;   allow dynamic phase specialization for certain mission types.

;;(defun this-and-future-phases ()
;;  (member (cur-phase) (phases (mission *self*))))

(defmethod this-and-future-phases ((mission mission))
  (member (cur-phase) (phases mission)))

;;;------------------------------------------------------------------------
;;; the reward for a phase is the sum of the values of goals achieved
;;; by the best plan.

(defmethod reward ((task plan-config-task))
  (reward (config task)))

(defmethod reward ((p mission-phase))
  (reward (best-config-for-phase p)))

;;; for a config, we look at the API list so we count all the goals
;;; it will address, not just the contract goals, b/c INIT goals are
;;; not included as contracts.
; (defmethod reward ((c config))
;   (let ((sum 0)
;         (counted-goals nil))
;   (dolist (c (contracts c))
;         (when (goal-contract-p c)
;                 (incf sum (reward c))
;                 (push (goal c) counted-goals)))
;   (dolist (g (getkeyval :goals (api-list c)))
;         (when (not (member g counted-goals))
;                 (incf sum (reward g))))
;   sum))

;;; reward for a goal contract is the probability of opportunity to get it
;;; times the absolute reward of the goal itself.
;(defmethod reward ((c goal-contract))
;  (* (opp-prob c) (reward (find-goal-by-name (goal c)))))

;(defmethod reward ((g symbol))
;  (reward (find-goal-by-name g)))

(defmethod reward ((c threat-contract)) 0)

(defmethod reward ((nothing null)) 0)

;;; sums rewards over any list
(defmethod reward ((l cons))
  (loop for i in l summing (reward i)))

;;;------------------------------------------------------------------------
;;; these are used to get the max potential reward, independent of
;;; probability of goal opportunity....if all the goals show up, what could
;;; you actually get.


(defmethod max-reward ((c threat-contract)) 0)
;(defmethod max-reward ((c goal-contract))
;  (reward (find-goal-by-name (goal c))))

;;; -------------------------------------------------------------------------
;;; for now, since we dont have adjustable meters and are not
;;; doing multi-agents (delib demo), the max reward is found by summing
;;; reward for all the contracts for this phase... however,  in general this
;;; is not right approach b/c agents negot and change contracts.
;;; - ack, this ignores the init goals.
;;; - OK, only b/c this is called just once at start, when the single
;;; pending configs have only the init goals, I'll make it look there too.

(defmethod max-reward ((p mission-phase))
  (+
    (loop for c in (contracts-for-phase p)
        summing (max-reward c))
    (reward (first (pending-configs p)))))

;;;------------------------------------------------------------------------
;;; for a given mission phase, the surv prob is product of
;;; surv-probs of the threats you dont handle.

(defmethod survival-probability ((task plan-config-task))
  (survival-probability (config task)))

;;; if you have a best config for a phase, use it, else
;;; you have no plan and handle none of the threat contracts.
(defmethod survival-probability ((p mission-phase))
  (let ((best-config (best-config-for-phase p)))

  (if best-config
        (survival-probability (best-config-for-phase p))
        (survival-probability (contracts-for-phase p)))))

;;; surv prob of a config is those contracts you dont handle!
; (defmethod survival-probability ((c config))
;   (survival-probability (unhandled-contracts c)))

(defmethod survival-probability ((c threat-contract))
  (- 1 (lethality c)))

(defmethod survival-probability ((c goal-contract)) 1)

;; this happens if there are no contracts for phase... you're safe!
(defmethod survival-probability ((nothing null)) 1)

;;; multiplies survival-probabilities over any list
(defmethod survival-probability ((l cons))
  (reduce #'* (mapcar #'survival-probability l)))

;;;------------------------------------------------------------------------
;;; Define a task to add a phase to the AMP's mission.

;;; When changing the slots for this class, be sure to update the
;;; broadcast-message call in execute-task and the
;;; make-add-phase-task-from-msg function.
(defclass add-phase (task)
  ((phase-name :initarg :phase-name :accessor phase-name)
   (initial-states :initarg :initial-states :accessor initial-states)
   (goals :initform '() :initarg :goals :accessor goals)
   (goal-skills :initform nil :initarg :goal-skills :accessor goal-skills)
   (threats :initform '() :initarg :threats :accessor threats)
   (threat-skills :initform nil :initarg :threat-skills :accessor threat-skills)
   (duration :initarg :duration :accessor duration)
   ))

;;; It is always worth a lot to add a phase.
(defmethod scoring-fn ((task add-phase))
  1000)

;;;-------------------------------------------------------------------------
(defun make-add-phase-task-from-msg (msg)
  (make-instance 'add-phase
    :phase-name (getassoc :phase-name msg)
    :initial-states (getassoc :initial-states msg)
    :goals (getassoc :goals msg)
    :goal-skills (getassoc :goal-skills msg)
    :threats (getassoc :threats msg)
    :threat-skills (getassoc :threat-skills msg)
    :duration (getassoc :duration msg)))

;;;-------------------------------------------------------------------------
;;; Generates unique phase name (string).

(defun generate-phase-name (base-phase-name)
  (format nil "~A.~A" base-phase-name (incf *phase-number*)))

;;;------------------------------------------------------------------------
;;; Define a special task to stop the amp when it is done working. Test
;;; domains can create an instance of this in their -amp.lisp file in
;;; their *default-mission-planner-fn*.
;;;
;;; (push (make-instance 'stop-amp-task) (tasks *self*))
;;;

(defclass stop-amp-task (task)
  ()
  (:documentation "When executed, this TASK stops the AMP by setting *HALT* to T. This task has an expected utility of -1 as long as there is CSM task executing and 1 otherwise. Thus, it should be the minimally-useful task and should be chosen after all tasks of value execute."))

(defmethod scoring-fn ((task stop-amp-task))
  "This task scores LEAST-POSITIVE-DOUBLE-FLOAT, so that it will be chosen if there is nothing else to do."
    least-positive-double-float)

(defmethod execute-task ((task stop-amp-task))
  "Stop the AMP from running anymore by setting *HALT* to T."
  (setf *halt* 'stop-amp-task))


;;; ------------------------------------------------------------
;;; Task for code to know when there are no other tasks ready to
;;; execute. This was added to support tests pertaining to the
;;; moving-horizon implementation.

(defclass ready-task (task)
  ((ready :initform nil :accessor ready)))

(defmethod scoring-fn ((task ready-task))
  "This task scores LEAST-POSITIVE-DOUBLE-FLOAT, so that it will be chosen if there is nothing else to do."
    least-positive-double-float)

(defmethod execute-task ((task ready-task))
  "Signal that the next thing can proceed be setting the ready flag."
  (setf (ready task) t))

(defun wait-for-ready-task (ready-task)
  (dbug :test-deep "Waiting for ready-task to execute.")
  (musliner:while (not (ready ready-task))
    (sleep .1)))

;;; ------------------------------------------------------------
;;; CGC tasks.



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; WARNING!  This is probably not the docker-task you're looking for.
;;; See "docker-task-new" in docker-task.lisp
;;; This docker-task is only inherited by pcap-task and capture-pcap-task
;;; which don't seem to be used by any of current tests.
;;;  
;;; 
;;;docker task - has a special suspend method to try to halt the process inside the docker happily.
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass docker-task (deliberation-task)
  ((nickname :initform nil :initarg :nickname :accessor nickname)))
;;
;;(defmethod suspend-delib-task ((task docker-task))
;;  (stop-docker (nickname task))
;;  (call-next-method)
;;)


;(defmethod prepare-to-suspend ((task docker-task))
;  (stop-docker (nickname task))
;  (call-next-method)
;)

(defmethod kill-task ((task docker-task))
  ;;(stop-docker (nickname task))
  (call-next-method)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; afl-task
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *use-afl-blind-thread* 1 "The number of AFL blind threads to spawn.")

(defvar *no-afl-plusplus* nil "If non-nil, driller and use-afl invocations will use AFLabiondo instead of AFL++")

(defvar *use-afl-via-driller* nil "If non-nil, a Driller task will be created, but no driller threads will run, only AFL will run within it via shellphuzz wrapper.")

(defvar *afl-qemu-mode* nil)

(defvar *use-afl-hb* nil
  "If non-nil, a afl-hb-task will be created for each binary found that has had whatclib results on it.")

(defparameter *afl-hb-more-args-afl-fuzz* nil
  "String containing more args to pass to afl-fuzz for afl-hb.")

(defparameter *afl-hb-more-args-verify-trace* nil
  "String containing more args to pass to the script that calls afl-qemu-trace for afl-hb.")

(defvar *use-driller* nil "If non-nil, hijack all AFL tasks and use Driller instead")

;;; FIXME PROFUZZER add *use-profuzzer*

(defvar *use-fuzzball* nil "If non-nil, hijack all AFL tasks and use Fuzzball instead")

(defvar *use-pereti-unpack* nil "If non-nil, applies pereti unpack step to any compressed directory")

(defvar *use-pereti-disasm* nil "If non-nil, applies pereti dissassemble to any executable (that comes from pereti-unpack), NOTE: Currently only works with outputs from the pereti-unpack-task")

(defvar *use-canned-povs* nil "If non-nil, find and use pov manifest for executables given in a tarball or
somesuch to pereti")

(defvar *use-isabel* nil "If non-nil, run the ISABEL BF analysis when a fault is detected")

(defvar *use-tfuzz* nil "If non-nil, hijack all AFL tasks and use tfuzz instead ")

(defvar *use-grassmarlin* nil "If non-nil (and *use-driller* nil), run the GrassMarlin analysis.")

(defvar *use-canned-peach-pit* nil "If non-nil, must be a string specifying a path to a peach pit to use for peach fuzzing.")

(defvar *successful-afl-tasks* 0 "Cheezy hack counter for easy testing... how many targets has AFL pwned?")

(defvar *use-gui* nil "If non-nil, send driller coverage info on to the GUI")

(defvar *use-whatclib* nil
  "If non-nil, a whatclib task will be created for each binary found
  during triage-target.")

(defvar *heapbuster-mode* nil
  "If non-nil and *use-whatclib* not nil, run in heapbuster mode... get whatclib results, look up vulnerabilities in heaphopper database, and feed binary + potential vulns to fuzzball")
(defvar *heapbuster-more-args* ""
  "String containing additional args to pass to the get-vuln-table.py script run to fetch heap vulnerabilities of a particular c library discovered by heaphopper")

(defvar *use-fuzzball-with-pereti-paths* nil
  "If non-nil, will use paths in warnings from Pereti to help Fuzzball")

(defvar *driller-timeout* 700
  "Driller timeout in seconds.")
(defvar *driller-workers* 1
  "Number of driller threads to spawn.  When 0, only AFL will run.")
(defvar *driller-force-interval* 5
  "Force greaser/fuzzer assistance at a regular interval (in seconds).")
(defvar *driller-more-args* ""
  "String containing additional args to pass to driller.  Use cautiously, since we do not check for conflicting args.")
(defvar *successful-driller-tasks* 0 "Cheezy hack counter for easy testing... how many targets has driller pwned?")

(defvar *fuzzball-more-args* ""
  "String containing additional args to pass to fuzzball.")

(defvar *successful-fuzzball-tasks* 0 "Cheezy hack counter for easy testing... how many targets has fuzzball pwned?")

(defvar *pereti-csonar* nil
  "String containing the address and port of codesonar, e.g. 10.7.13.110:7340, or nil to disable csonar")

(defvar *pereti-more-args* ""
  "String containing additional args to pass to pereti when it calls fw_analyze in the pereti container.")

(defvar *pereti-workspace-dir* ""
  "String containing directory where pereti container's /workspace directory is mounted.")

(defvar *tfuzz-fuzz-timeout* 600 "How long in seconds tfuzz should be allowed to fuzz the binary")
(defvar *tfuzz-target-options* "" "Target options for tfuzz")
(defvar *tfuzz-code-branch* "ca_base64" "Which branch do you want to tfuzz code to be on?")

(defun run-tfuzz-on-bin (task &aux ret )
  (dbug :amp "Called run-tfuzz-on-bin.")
  (dbug :top "input-pathname ~A" (input-pathname task))
  (dbug :top "output-pathname ~A" (output-pathname task))

  (dbug :top "I think I'm a distributed-on-cluster job, I'll make sure the tfuzz container is running")
  (setf ret (run-shell-command "../tools/ensure-image tfuzz -d"))	;; cant use /neo-fuzz here in case are in local acl
  (when (not (= 0 ret))
    (dbug :top "ERROR: ensure-image failed, return code ~a!" ret))
  (dbug :top "ensure-image finished")

  (let ((cmd (format nil "/realuser.sh /bin/bash -c 'cd /root/release && git reset --hard && git checkout ~A && cd ~~ && source /root/.virtualenvs/tfuzz_test/bin/activate && /neo-fuzz/code/T-Fuzz/utils/run.sh ~A \"~A\" ~A'"
		     *tfuzz-code-branch*
		     (namestring (path (bin task)))
		     *tfuzz-target-options*
		     *tfuzz-fuzz-timeout*)))
    (dbug :top "Running command: ~A" cmd)
    (with-docker-exec-stream (io-stream (strcat (uiop:getenv "CONTAINER_PREFIX") "-tfuzz") cmd)
      (dolines (line io-stream)
	(dbug :top "got line [~A]" line)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; proto-task
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *successful-proto-tasks* 0 "Cheezy hack counter for easy testing... how many targets has AFL pwned?")

(defun node-pair-for-edge (edge-hash node-hash-list)
  (let ((from (gethash :from edge-hash))
        (to (gethash :to edge-hash)))
    (dbug :top "from: ~s  to: ~s~%" to from)
    (list (node-for-id from node-hash-list) (node-for-id to node-hash-list))))

(defun node-for-id (id node-hash-list)
  (find id node-hash-list :key #'(lambda (node-hash) (gethash :id node-hash))))

(defun gm-node-key-most-confident-value-pairs (node-xmls)
  (let ((groups-xmls (xmls:xmlrep-find-child-tags "group" node-xmls)))
    (loop :for group-xmls in groups-xmls
       :collect (list (intern (gm-group-key group-xmls) :fuzzbomb)
                      (gm-group-most-confident-value group-xmls)))))

(defun gm-node-group-keys (node-xmls)
  (let ((groups-xmls (xmls:xmlrep-find-child-tags "group" node-xmls)))
    (loop :for group-xmls :in groups-xmls
       :collect (gm-group-key group-xmls))))

(defun gm-group-key (group-xmls)
  (xmls:xmlrep-attrib-value "key" group-xmls))

(defun gm-group-most-confident-value (group-xmls)
  "Returns any of the values in the group w/ lowest confidence.
   (In grassmarlin, lowest 1 is highest confidence, 5 is lowest.)"
  (let* ((values-xmls (xmls:xmlrep-find-child-tags "v" group-xmls))
         ;; ((<confidence> <value>)*)
         (value-pairs (loop :for value-xmls :in values-xmls
                         :collect (list (read-from-string (xmls:xmlrep-attrib-value "confidence" value-xmls))
                                        (xmls:xmlrep-string-child value-xmls))))

         (best-confidence nil)
         (best-pair nil))
    (dolist (value-pair value-pairs)
      (when (or (null best-pair) (< (first value-pair) best-confidence))
        (setf best-pair value-pair)
        (setf best-confidence (first value-pair))))
    (second best-pair)))

;;; ------------------------------------------------------------
;;;   FIXME This is one of two classes that inherit from old
;;; "docker-task".  Let's delete them and docker-task, too!
(defclass capture-pcap-task (docker-task)
  ((interface :initform nil :initarg :interface :accessor interface)
   (packet-count :initform nil :initarg :packet-count :accessor packet-count)
   (task-pcap-file :initform nil :initarg :task-pcap-file :accessor task-pcap-file)
   (src-host :initform nil :initarg :src-host :accessor src-host)
   (init-cmd :initform nil :initarg :init-cmd :accessor init-cmd)
   (ready-cmd :initform nil :initarg :ready-cmd :accessor ready-cmd)
   (run-cmd :initform nil :initarg :run-cmd :accessor run-cmd)
   (shutdown-cmd :initform nil :initarg :shutdown-cmd :accessor shutdown-cmd)
  ))

(defun modify-pit-ip (orig-pit-path-string new-pit-path-string new-ip-string)
  ;; use peach container which should have up-to-date python3 install
  (docker-exec (strcat (uiop:getenv "CONTAINER_PREFIX") "-peach")
               (format nil "bash -c \"cd /neo-fuzz/code/tools/protocol-learning && /realuser.sh python3 modify-pit-ip.py ~A ~A ~A\""
                       orig-pit-path-string
                       new-pit-path-string
                       new-ip-string)))

