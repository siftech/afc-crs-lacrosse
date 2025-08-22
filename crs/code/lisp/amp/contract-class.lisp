;;; -------------------------------------------------------------------------
;;; contract-class.lisp
;;; - Contracts exchanged by AMPs to deal w/ diff goals and threats.
;;; - $Revision: 1.26 $
;;; -------------------------------------------------------------------------
;;; COPYRIGHT START
;;; COPYRIGHT END
;;; -------------------------------------------------------------------------

(cl:in-package :fuzzbomb)

;;; -------------------------------------------------------------------------
;;; Contracts
;;;
;;; We now support different kinds of contracts, to support different kinds
;;; of negotiation protocols. Single-agent contracts mean that a contract
;;; is worked (awarded or claimed) by a single agent, as in bid/award/lock
;;; protocols (and other protocols TBD). Multi-agent contracts can be
;;; worked concurrently by any number of contractors. We may need to
;;; specialize further, but this seems to be a basic distinction.
;;;
;;;
;;; -------------------------------------------------------------------------

;;; Store the fake benefit numbers in an a-list for now.

;(setf *torg-benefits*
;      '(
;       (engine-failure 8)
;       (radar-threat 7)
;       (radar-threat2 7)
;       (ir-threat 6)
;       (fly-normally 4)
;       (get-to-ingress-phase 4)
;       (get-to-attack-phase 4)
;       (get-to-egress-phase 4)
;       (get-to-landing-phase 4)
;       (destroy-target 5)
;       ))

;;; -------------------------------------------------------------------------
;;; Note a contract should only have a single name, no matter what agent it
;;; is on... the first guy to make it new does the auto-allocation of a
;;; name using (generate-contract-name), and after that even when it is
;;; sent to another agent to perform work on it, it is always known by that
;;; identifier.  So all msgs talking about contracts use the contract-name.

;;; A basic contract, that we can customize for different bidding protocols.
;;; Note that basic contracts are phase-specific.
(defclass contract ()
        ;; the unique id
  ((name :initarg :name :initform (generate-contract-name) :accessor name)
   ;; the name of the phase this contract involves
   (phasename :initarg :phasename :initform nil :accessor phasename)
   ;; the announcing and managing agent
   (manager :initarg :manager :initform nil :accessor manager)
   ;; the current (keyword) status
   (status :initform :new :accessor status)
   ;; which configs am I included in?
   (configs :initarg :configs :initform nil :accessor configs)
))

(defmethod mission-phase ((c contract))
  (find-phase (phasename c)))

;; Defaults for all non-plan-config-tasks
;; Contracts sometimes treated as tasks.
;; Processing contracts is higher utility than plan-config-tasks
;; or other kinds of tasks.
(defmethod scoring-fn ((contract contract)) 2000)


;;; ---------------------------------------------------------------------
;;; returns all the contracts applying to the arg phase.

(defmethod contract-applies-to-phase-p ((contract t) (phase mission-phase))
  (eq (phasename contract) (name phase)))

;; [sfriedman:20121116.1002CST] Calls a method predicate that we can
;; specialize for differnet types of missions and phases.

(defun contracts-for-phase (phase)
  ;; fix in case is a phase name not object.
  (setf phase (find-phase phase))
  (remove-if-not #'(lambda (c) (contract-applies-to-phase-p c phase))
                 (contracts *self*)))

;;; ---------------------------------------------------------------------
;;; Multi-agent contracts
;;;
;;; Allow multiple agents to work on the same contract simultaneously.
;;; Supports non-locking protocol.
;;; Make separate classes for threats and goals for compatibility
;;; with single-agent contracts.
;;; ---------------------------------------------------------------------

;;; A no-locking contract, in which multi agents can be
;;; contractors concurrently.
;(defclass multi-agent-contract (contract)
;   ;; multi-contractors for single contract
;   ((contractors :initform nil :initarg :contractor :accessor contractor)
;))
;
;
;;;; ---------------------------------------------------------------------
;;;; Multi-agent TorG (Threat or Goal) contracts.
;;;; ---------------------------------------------------------------------
;
;(defclass multi-agent-TorG-contract (multi-agent-contract)
;  ;; threat or goal expression of form (<phase> <TorG-symbol>)
;  ;; Example (ingress ir-threat)
;  ((torg :initarg :expr :initform nil :accessor expr)
;   ))
;
;(defmethod get-torg-name (c multi-agent-contract)
;  (second (torg c)))
;
;(defun get-torg-name-from-expr (expr)
;  (second torg))
;
;;;; Does this agent have the skill to execute this contract?
;(defmethod skill-p ((c multi-agent-torg-contract))
;  (let* ((mission (mission *self*))
;        (phase (find-phase (phasename c) mission))
;        ))
;    (find-TorG-skill phase (torg c)))
;
;;;; No explicit bidding for multi-agent threats.
;;;; Eventually make these phase-specific.
;;;; For now, index on name of threat.
;
;;(defmethod cost ((c multi-agent-torg-contract))
;;  (let ((torg-name (get-torg-name (torg c))))
;;    (or (getassoc torg-name *torg-costs*)
;;      (error "No cost associated with TorG: ~A~%" torg-name))))
;
;
;;;; No explicit bidding for multi-agent threats.
;(defmethod benefit ((c multi-agent-torg-contract))
;  (let ((torg-name (get-torg-name (torg c))))
;    (or (getassoc torg-name *torg-benefits*)
;       (error "No benefit associated with TorG: ~A~%" torg-name))))
;
;;;; ---------------------------------------------------------------------
;;;; Multi-agent threat contracts
;;;; ---------------------------------------------------------------------
;
;(defclass multi-agent-threat-contract (multi-agent-TorG-contract)
;  ;; threat-specific stuff (later)
;  ((threat-stuff :initarg :threat-stuff :initform nil :accessor threat-stuff)
;   ))
;
;(defun multi-agent-threat-contract-p (x)
;  (typep x 'multi-agent-threat-contract))
;
;;;; ---------------------------------------------------------------------
;;;; Multi-agent goal contracts
;;;; ---------------------------------------------------------------------
;
;(defclass multi-agent-goal-contract (multi-agent-TorG-contract)
;  ;; goal-specific stuff (later)
;  ((goal-stuff :initarg :goal-stuff :initform nil :accessor goal-stuff)
;   ))
;
;(defun multi-agent-goal-contract-p (x)
;  (typep x 'multi-agent-goal-contract))

;;; ---------------------------------------------------------------------
;;; Single-agent contracts
;;;
;;; The old-fashioned kind of locking contract, in which bids are received
;;; and the manager awards the contract to only one contractor (at a time).
;;;
;;; ---------------------------------------------------------------------
(defclass single-agent-contract (contract)
   ;; the list of agents to whom the contract is announced.
  ((bidders :initform nil :initarg :bidders :accessor bidders)
   ;; the list of bids (estimates) published so far
   (bids :initform nil :initarg :bids :accessor bids)
   (contractor :initform nil :initarg :contractor :accessor contractor)
   (price :initarg :price :initform nil :accessor price)
   (bid-deadline :initform 10000000000000
                 :initarg :bid-deadline :accessor bid-deadline)
))

;;; ---------------------------------------------------------------------
;;; Single-agent (locking) threat contract.
;;; Named for compatibility with original locking protocol.
(defclass threat-contract (single-agent-contract)
  ((threat :initarg :threat :initform nil :accessor threat)
        ;; for now, a cheezy way to say how dangerous this thing is.
        ;; just a probability that you'll die if dont handle this threat
        ;; *in this phase*-- we dont factor this by phase duration or
        ;; whatever, that is supposed to be hand-set in domain-amp.lisp file.
   (lethality :initarg :lethality :initform nil :accessor lethality)
))

(defun threat-contract-p (x)
  (typep x 'threat-contract))

(defun find-threat-contract (threatname phasename)
  (find-if #'(lambda (c)
                (and (threat-contract-p c) (string-equal threatname (threat c))
                        (string-equal phasename (phasename c))))
        (contracts *self*)))

;;; ---------------------------------------------------------------------
;;; Single-agent (locking) goal contract.
;;; Named for compatibility with original locking protocol.
(defclass goal-contract (single-agent-contract)
  ((goal :initarg :goal :initform nil :accessor goal)
   (opp-prob :initarg :opp-prob :accessor opp-prob
        :documentation "Probability of opportunity arising to get this goal.")
))

(defun goal-contract-p (x)
  (typep x 'goal-contract))

(defun find-goal-contract (goalname phasename)
  (find-if #'(lambda (c)
                (and (goal-contract-p c) (string-equal goalname (goal c))
                        (string-equal phasename (phasename c))))
        (contracts *self*)))

;;; ---------------------------------------------------------------------
;;; Bidding for single-agent threats and goals.
;;;
;;; ---------------------------------------------------------------------
;;; Bidding taking individual agent skills into account.
(defmethod compute-bid ((c goal-contract))
  (let* ((mission (mission *self*))
         (phase (find-phase (phasename c) mission))
         (goal (goal c))
         (skill (find-goal-skill phase goal))
         )
    (if skill            ; if agent has this skill
        (quality skill)  ; return quality as bid
         -1)))           ; else return -1 flag for no-bid

;;; ---------------------------------------------------------------------
;;; Bidding for threats.
(defmethod compute-bid ((c threat-contract))
  (let* ((mission (mission *self*))
         (phase (find-phase (phasename c) mission))
         (threat (threat c))
         (skill (find-threat-skill phase threat))
         )
    (if skill            ; if agent has this skill
        (quality skill)  ; return quality as bid
         -1)))           ; else return -1 flag for no-bid


;;; ---------------------------------------------------------------------
;(defmethod cost ((c goal-contract))
;    (or (getassoc (goal c) *torg-costs*)
;       (error "No cost associated with goal ~A~%" (goal c))))

;;; ---------------------------------------------------------------------
;(defmethod benefit ((c goal-contract))
;    (or (getassoc (goal c) *torg-benefits*)
;       (error "No benefit associated with goal ~A~%" (goal c))))

;;; ---------------------------------------------------------------------
;(defmethod cost ((c threat-contract))
;    (or (getassoc (threat c) *torg-costs*)
;       (error "No cost associated with threat ~A~%" (threat c))))

;;; ---------------------------------------------------------------------
;(defmethod benefit ((c threat-contract))
;    (or (getassoc (threat c) *torg-benefits*)
;       (error "No benefit associated with threat ~A~%" (threat c))))

;;;-------------------------------------------------------------------------

(defmethod print-object ((obj contract) stream)
  (format stream "#<~A ~A>" (type-of obj) (name obj)))

;;;-------------------------------------------------------------------------
;;; Used for more detailed printout of objects;
;;; Another choice would be to set a flag and have each print-object
;;;     specialization detect the "verbose" flag and print more information to
;;;     its stream arg when the flag is on.  Then simple mods to printouts
;;;     and dbug stmts could turn on the verbose flag automatically around
;;;     the formatting calls that would yield print-object calls.
;;; - Its not clear to me that these are much of an improvement over the
;;;     native describe-object methods... is the output easier to parse?
;;;  : (lp) was not meant to replace describe-object entirely but be more
;;; compact; so, this used to override describe-object but I made it just lp.
;;; otherwise you have no way to get to all the slots that arent listed here.

(defmethod lp ((obj contract))
  (format T "#<~A ~A~%" (type-of obj) (name obj))
  (format T "   Price: ~A~%" (price obj))
  (format T "   Manager: ~A~%" (manager obj))
  (format T "   Contractor: ~A~%" (contractor obj))
  (format T "   Bids: ~A~%" (bids obj))
  (format T "   Bidders: ~A~%" (bidders obj))
  (format T "   Bid-deadline: ~A~%" (bid-deadline obj))
  (format T "   Status: ~A>~%" (status obj))
)

;;;-------------------------------------------------------------------------
;;; Generates unique contract name (string).

(defun generate-contract-name ()
  (format nil "~A.~A" (name *self*) (incf *contract-number*)))

;;;-------------------------------------------------------------------------

;;;-------------------------------------------------------------------------
;;;

;;; These must be methodized and specialized on contract class.

(defun make-contract-from-msg (msg)
  (case (getassoc :contract-type msg)
    (:threat (make-threat-contract-from-msg msg))
    (:goal   (make-goal-contract-from-msg msg))
    (t (error "Not a goal or threat msg: ~A~%" msg))))

(defun make-threat-contract-from-msg (msg)
  (make-instance 'threat-contract
                   :name (getassoc :contract-name msg)
                   :manager (getassoc :from msg)
                   :contractor *self*
                   :threat (second (getassoc :value msg))
                   :phasename (first (getassoc :value msg))))

(defun make-goal-contract-from-msg (msg)
  (make-instance 'goal-contract
                   :name (getassoc :contract-name msg)
                   :manager (getassoc :from msg)
                   :contractor *self*
                   :goal (second (getassoc :value msg))
                   :phasename (first (getassoc :value msg))))


;;; This needed to make new kinds of multi-agent contracts

(defun make-multi-agent-TorG-contract-from-msg (msg)
  (make-instance 'multi-agent-torg-contract
                   :name (getassoc :contract-name msg)
                   :manager (getassoc :from msg)
                   :contractors (list *self*)
                   :torg (second (getassoc :value msg))
                   :phasename (first (getassoc :value msg))))
