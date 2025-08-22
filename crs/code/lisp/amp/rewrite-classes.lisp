(cl:in-package :fuzzbomb)

;;; Ripped out of task-class.lisp

(defclass rewrite-task (task)
  ((svc-rev :initform nil :initarg :svc-rev :accessor svc-rev)
   (bin :initform nil :initarg :bin :accessor bin)))

;;; ------------------------------------------------------------
;;; Task to choose and apply a repair.
(defclass repair-task (rewrite-task)
  ((pov-result :initform nil :initarg :pov-result :accessor pov-result)
   (test-case :initform nil :initarg :test-case :accessor test-case)
   (problem-ip :initform nil :initarg :problem-ip :accessor problem-ip)
   (core-ips :initform nil :initarg :core-ips :accessor core-ips)
   (call-stack :initform nil :initarg :call-stack :accessor call-stack)
   (kitchen-sink :initform nil :initarg :kitchen-sink :accessor kitchen-sink)
   (repair-status :initform :unrepaired :initarg :repair-status :accessor repair-status)))

(defclass compound-repair-task (repair-task)
  ((simple-revs :initform nil :initarg :simple-revs :accessor simple-revs)))

;; Injects a cookie at the stack max, and then checks it at core-ips.
(defclass stacktop-cookie-repair-task (repair-task)
  ())

;; Checks the range of receive's ending address for within binary or stack.
(defclass receive-rangecheck-repair-task (repair-task)
  ())

;; Checks the range of a dereferenced register (i.e., pointer).
(defclass range-repair-task (repair-task)
  ())

(defclass cfi-repair-task (repair-task)
  ((func :initform nil :initarg :func :accessor func)))

(defclass stack-repair-task (repair-task)
  ((func :initform nil :initarg :func :accessor func)
   (addr-context :initform nil :initarg :addr-context :accessor addr-context)
   (addrs-searched :initform nil :initarg :addrs-searched :accessor addrs-searched)))

(defclass heap-repair-task (repair-task)
  ())

(defclass bss-repair-task (repair-task)
  ((bss-addr :initform nil :initarg :bss-addr :accessor bss-addr)))

(defclass null-ptr-repair-task (repair-task)
  ())

(defclass seppuku-repair-task (repair-task)
  ())

;;; Task to choose and apply a defensive rewrite.
(defclass defensive-rewrite-task (rewrite-task)
  ;; Attempted defenses a list of lists of remedies.
  ((attempted-defenses :initform nil :initarg :attempted-defenses :accessor attempted-defenses)
   ;; Ditto for failed defenses, but these are ones not to do again.
   (failed-defenses :initform nil :initarg :failed-defenses :accessor failed-defenses)
   ;; Last defense is a list of remedies most recenty attempted.
   (last-defense :initform nil :initarg :last-defense :accessor last-defense)
   ;; Track which binary revisions are "full" of rewrites, so we don't keep attempting.
   (full-bins :initform nil :initarg :full-bins :accessor full-bins)
   ;; Track which revision we're waiting to score.  There should only be one at any time.
   (awaiting-rev-score :initform nil :initarg :awaiting-rev-score :accessor awaiting-rev-score)
   ))

;;; Ripped out of amp.lisp

(defun make-repair-task-for-pov-result (result test-case svc-rev &aux resulting-tasks)
  (dbug :repair "Considering creating a repair task for ~A" result)
  (let* ((fault-index (position (find-if #'faulted (stats result)) (stats result)))
	 (fault-bin (nth fault-index (bins svc-rev)))
	 (child-tcs (get-descendant-test-cases test-case))
	 (all-tcs (remove-duplicates (cons test-case child-tcs)))
	 (all-results (results-for-test-cases all-tcs svc-rev))
	 (all-core-ips (valid-core-ips-for-test-results all-results fault-bin))
	 (addr-repair-list (remove nil (make-repair-tasks-from-test-cases all-tcs svc-rev)))
	 (restart-addrs (remove-duplicates (remove nil (mapcar #'car addr-repair-list))))
	 (in-cb-restart-addrs (remove-if-not
			       #'(lambda (addr) (insn-by-addr addr fault-bin))
			       restart-addrs))
	 (plausible-tasks (mapcar #'(lambda (rt)
				      (setf (core-ips rt) all-core-ips)
				      rt)
				  (remove nil (mapcar #'cdr addr-repair-list))))
	 (novel-tasks (remove-if #'equivalent-repair-task plausible-tasks)))
    (dbug :repair "Processed info files and histories to create ~A addr-repair-list in ~A and its ~A children."
	  (length addr-repair-list)
	  test-case
	  (length child-tcs))
    (dbug-core result svc-rev)
    (dbug :repair-deep "  Children: ~A" child-tcs)
    (dbug :repair-deep "  Regs: ~S"
	  (mapcar #'registers (apply #'append (mapcar #'stats all-results))))
    (dbug :repair "Produced ~A plausible repair tasks from info and histories files, ~A novel ones."
	  (length plausible-tasks) (length novel-tasks))
    (dbug :repair-deep "  Novel tasks: ~A" novel-tasks)
    (dbug :repair "All [poss invalid] core-ips: ~X"
	  (core-ips-for-test-results all-results))
    (dbug :repair "All [poss invalid] restart-addrs: ~X"
	  restart-addrs)
    (unless all-core-ips
      (dbug :repair "No in-CB core IPs, so will use in-CB restart-addrs ~X" in-cb-restart-addrs)
      (setf all-core-ips in-cb-restart-addrs))
    (dolist (task novel-tasks)
      (unless (equivalent-repair-task task)
	(push task resulting-tasks)
	;;(setf resulting-tasks (list (car novel-tasks)))
	(setf (core-ips task) all-core-ips)
	(let ((problem-ip (problem-ip task)))
	  (dbug :top "Created new repair task ~A at problem IP ~X, func ~A"
		task problem-ip (when problem-ip (func-by-addr problem-ip fault-bin))))
	(submit-repair-task task)))
    (when all-core-ips
      (let ((first-restart-ip (car (remove nil (mapcar #'car addr-repair-list)))))
	(setf resulting-tasks
	      (append resulting-tasks
		      (throw-the-kitchen-sink test-case result first-restart-ip all-core-ips fault-bin svc-rev)))))
    (unless *perform-repair-rewrites*
      (dbug :repair "Ignoring created repair tasks since *perform-repair-rewrites* = ~A" *perform-repair-rewrites*))
    (when *perform-repair-rewrites*
      resulting-tasks)))



(defun submit-repair-task (task)
  (when *perform-repair-rewrites*
    (push task (tasks *self*))
    (dbug :top "To ensure repair runs, suspending deliberation task")
    (suspend-delib-task *delib-task*)))



(defun throw-the-kitchen-sink (test-case result restart-ip core-ips bin svc-rev
			       &aux resulting-tasks submitted-tasks)
  (dbug :repair "Throwing the kitchen sink at tc ~A w/ core-ips ~X" test-case core-ips)
  (let* ((valid-addrs (remove-duplicates (remove nil (cons restart-ip core-ips))))
	 (all-bin-core-ips (all-valid-core-ips-for-bin bin svc-rev))
	 (funcs (remove nil (mapcar #'(lambda (a) (func-by-addr a bin)) valid-addrs)))
	 (stacked-funcs (remove-if-not #'stack-setting-insn-id funcs))
	 (effective-problem-ip (or restart-ip (car core-ips))))
    (dolist (func stacked-funcs)
      (let* ((stack-task (make-instance
			  'stack-repair-task :kitchen-sink t
			  :bin bin :svc-rev svc-rev :core-ips core-ips
			  :problem-ip effective-problem-ip
			  :test-case test-case :pov-result result
			  :func func))
	     (cfi-task (make-instance
			'cfi-repair-task :kitchen-sink t
			:bin bin :svc-rev svc-rev :core-ips core-ips
			:problem-ip effective-problem-ip
			:test-case test-case :pov-result result
			:func func)))
	(dbug :top "Created stack repair task ~A at func ~A" stack-task func)
	(push stack-task resulting-tasks)
	(dbug :top "Created CFI repair task ~A at func ~A" cfi-task func)
	(push cfi-task resulting-tasks)
	))
    (let ((range-task (make-instance
		       'range-repair-task :kitchen-sink t
			 :bin bin :svc-rev svc-rev :core-ips core-ips
			 :problem-ip effective-problem-ip
			 :test-case test-case :pov-result result)))
      (dbug :top "Created local range-checking repair task ~A" range-task)
      (push range-task resulting-tasks))
    (let ((stacktop-task (make-instance
			  'stacktop-cookie-repair-task :kitchen-sink t
			  :bin bin :svc-rev svc-rev :core-ips core-ips
			  :problem-ip effective-problem-ip
			  :test-case test-case :pov-result result)))
      (dbug :top "Created local stack-top repair task ~A" stacktop-task)
      (push stacktop-task resulting-tasks))
    (let ((seppuku-task (make-instance
			 'seppuku-repair-task :kitchen-sink t
			 :bin bin :svc-rev svc-rev :core-ips core-ips
			 :problem-ip effective-problem-ip
			 :test-case test-case :pov-result result)))
      (dbug :top "Die with honor: created a corresponding seppuku task ~A" seppuku-task)
      (push seppuku-task resulting-tasks))
    (dolist (a valid-addrs)
      (let ((null-ptr-task (make-instance
			    'null-ptr-repair-task :kitchen-sink t
			    :bin bin :svc-rev svc-rev :core-ips core-ips
			    :problem-ip a :test-case test-case :pov-result result)))
	(dbug :top "Created null ptr repair task ~A at ~X" null-ptr-task a)
	(push null-ptr-task resulting-tasks)))
    (let ((receive-task (make-instance
			'receive-rangecheck-repair-task :kitchen-sink t
			:bin bin :svc-rev svc-rev
			;; Don't specify core-ips or problem-ip so that it matches
			;; very generally for equivalence checking.
			:core-ips nil :problem-ip nil
			:test-case test-case :pov-result result)))
      (dbug :top "Created receive-rangecheck-repair-task ~A" receive-task)
      (push receive-task resulting-tasks))
    (when (lib-func-callsite 'allocate bin)
      (let ((heap-task (make-instance
			'heap-repair-task :kitchen-sink t
			:bin bin :svc-rev svc-rev
			:core-ips all-bin-core-ips
			:problem-ip effective-problem-ip
			:test-case test-case :pov-result result)))
	(dbug :top "Created heap repair task ~A" heap-task)
	(push heap-task resulting-tasks)))
    (when (set-difference all-bin-core-ips core-ips)
      (dbug :repair "Expanding some tasks to all core IPs ~X" all-bin-core-ips)
      (let ((range-task (make-instance
			 'range-repair-task :kitchen-sink t
                         :bin bin :svc-rev svc-rev :core-ips all-bin-core-ips
                         :problem-ip effective-problem-ip
                         :test-case test-case :pov-result result)))
	(dbug :top "Created global range-checking repair task ~A" range-task)
	(push range-task resulting-tasks))
      (let ((seppuku-task (make-instance
			   'seppuku-repair-task :kitchen-sink t
			   :bin bin :svc-rev svc-rev :core-ips all-bin-core-ips
			   :problem-ip effective-problem-ip
			   :test-case test-case :pov-result result)))
	(dbug :top "HARAKIRI!  Global seppuku task ~A" seppuku-task)
	(push seppuku-task resulting-tasks))
      (let ((stacktop-task (make-instance
			    'stacktop-cookie-repair-task :kitchen-sink t
			    :bin bin :svc-rev svc-rev :core-ips all-bin-core-ips
			    :problem-ip effective-problem-ip
			    :test-case test-case :pov-result result)))
	(dbug :top "Created global stack-top repair task ~A" stacktop-task)
	(push stacktop-task resulting-tasks)
	))
    (dolist (task resulting-tasks)
      (unless (equivalent-repair-task task)
	(dbug :repair "~A is enqueued; it's novel." task)
	(submit-repair-task task)
	(push task submitted-tasks)))
    submitted-tasks))



(defun all-valid-core-ips-for-bin (bin rev)
  (remove-duplicates
   (remove nil
           (valid-core-ips-for-test-results (pov-test-results rev) bin))))




;; -------------------------------------------------
;; Repair task equivalence
;;

(defmethod repair-tasks-equivalent ((t1 t) (t2 t))
  (and (eq (type-of t1) (type-of t2))
       ;; Don't include the bin right now.
       ;; (eq (bin t1) (bin t2))
       (= (length (core-ips t1)) (length (core-ips t2)))
       (null (set-difference (core-ips t1) (core-ips t2)))
       (eq (problem-ip t1) (problem-ip t2))))

(defmethod repair-tasks-equivalent ((t1 stack-repair-task) (t2 stack-repair-task))
  (eq (func-entry-addr (func t1) (bin t1))
      (func-entry-addr (func t2) (bin t2))))

(defmethod repair-tasks-equivalent ((t1 cfi-repair-task) (t2 cfi-repair-task))
  (eq (func-entry-addr (func t1) (bin t1))
      (func-entry-addr (func t2) (bin t2))))

(defmethod repair-tasks-equivalent ((t1 seppuku-repair-task) (t2 seppuku-repair-task))
  (same-core-ips-p t1 t2))

(defmethod repair-tasks-equivalent ((t1 range-repair-task) (t2 range-repair-task))
  (same-core-ips-p t1 t2))

(defmethod repair-tasks-equivalent ((t1 heap-repair-task) (t2 heap-repair-task))
  (same-core-ips-p t1 t2))

(defmethod repair-tasks-equivalent ((t1 receive-rangecheck-repair-task) (t2 receive-rangecheck-repair-task))
  (same-core-ips-p t1 t2))

(defmethod repair-tasks-equivalent ((t1 stacktop-cookie-repair-task) (t2 stacktop-cookie-repair-task))
  (same-core-ips-p t1 t2))

(defun same-core-ips-p (t1 t2)
  (and (= (length (core-ips t1)) (length (core-ips t2)))
       (null (set-difference (core-ips t1) (core-ips t2)))))




(defun equivalent-repair-task (task)
  (some #'(lambda (r)
	    (repair-tasks-equivalent task r))
	(append (tasks *self*) (repair-tasks *self*))))

;;;-------------------------------------------------------------------------
(defun make-seppuku-task-from-repair-task (task)
  (when (problem-ip task)
    (make-instance
     'seppuku-repair-task
     :bin (bin task)
     :core-ips (core-ips task)
     :svc-rev (svc-rev task)
     :problem-ip (problem-ip task))))

(defun convert-fball-call-stack (fball-call-stack)
  (mapcar #'(lambda (c)
              (list (cons 'esp (parse-hex (cdr (assoc 'esp c))))
                    (cons 'eip (parse-hex (cdr (assoc 'eip c))))
                    (cons 'last-eip (parse-hex (cdr (assoc 'last-eip c))))
                    (cons 'ret-addr (parse-hex (cdr (assoc 'ret-addr c))))))
          fball-call-stack))

(defun make-repair-tasks-from-test-cases (test-cases svc-rev)
  (apply #'append
	 (mapcar #'(lambda (tc)
		     (when (info tc)
		       (let* ((info-filename (info tc))
			      (result (car (results-for-test-cases (list tc) svc-rev)))
			      (info-path (probe-file info-filename))
			      (info (when info-path (decode-json-from-source info-path))))
			 (when (and info result)
			   (make-repair-tasks-from-info-list info result tc svc-rev)))))
		 test-cases)))

(defun make-repair-tasks-from-info-list (info result test-case svc-rev)
  ;; try all of the info list elements.
 (cond ((and (consp (car info))
	     (consp (caar info)))
	(apply #'append
	 (mapcar #'(lambda (e) (make-repair-tasks-from-info e result test-case svc-rev)) info)))
       (t (make-repair-tasks-from-info info result test-case svc-rev))))

(defun get-histories (info)
  (let* ((msg (cdr (assoc 'message info)))
	 (hist (mapcar #'cdr (cdr (assoc 'event_history msg)))))
    hist))

(defun make-repair-tasks-from-history (hist bin &aux tasks)
  (let* ((addr-str (cdr (assoc 'event-eip hist)))
	 (addr (when addr-str (parse-hex addr-str)))
	 (addr-insn (when addr (insn-by-addr addr bin)))
	 (addr-func (when addr (func-by-addr addr bin)))
	 (tag (cdr (assoc 'tag hist))))
    (dbug :repair-deep " Event ~A at ~X / ~A" tag addr addr-insn)
    (cond
      ((and (equal tag ":return-addr-overwrite") addr-func)
       (setf tasks (list (make-instance 'stack-repair-task :func addr-func)
			 (make-instance 'cfi-repair-task :func addr-func))))
      ((and addr-insn (member tag '(":uninit-read" ":unsafe-read" ":suspicious-write"
				    ":unsafe-write" ":weird-sym-addr" ":null-deref") :test #'equal))
       (setf tasks (list (make-instance 'null-ptr-repair-task)
			 (make-instance 'range-repair-task)))
       (when addr-func
	 (push (make-instance 'cfi-repair-task :func addr-func) tasks))))
    (dolist (task tasks)
      (setf (problem-ip task) addr)
      (setf (bin task) bin))
    (or
     (mapcar #'(lambda (task) (cons addr task)) tasks)
     (list (list addr)))))

;; After we create the history task, we need to:
;;    (setf (pov-result task) result)
;;    (setf (test-case task) test-case)
;;    (setf (core-ips task) core-ips))))

;;;-------------------------------------------------------------------------
(defun make-repair-tasks-from-info (info result test-case svc-rev &aux info-task tasks func)
  ;;   this should no longer be getting a info-element list, b/c of above fn
  ;;(when (and (consp (car info))
  ;;           (consp (caar info)))
  ;;  (setf info (car info)))
  (let* ((fault-index (position (find-if #'faulted (stats result)) (stats result)))
         (fault-bin (nth fault-index (bins svc-rev)))
	 (child-tcs (children test-case))
	 (pov-results (pov-test-results svc-rev))
	 (child-pov-results (remove-if-not #'(lambda (result) (member (test-case result) child-tcs)) pov-results))
	 (core-ips (valid-core-ips-for-test-results
		    (cons result child-pov-results) fault-bin))
	 (msg (cdr (assoc 'message info)))
	 (msg-type (cdr (assoc 'type msg)))
         (reason (cdr (assoc 'restart_reason msg)))
         (restart-ip-str (cdr (assoc 'restarted_at msg)))
	 (deref-ip-str (cdr (assoc 'dereferenced_at msg)))
         (restart-ip (when restart-ip-str (parse-hex restart-ip-str)))
	 (restart-ip-insn (when restart-ip (insn-by-addr restart-ip fault-bin)))
	 (deref-ip (when deref-ip-str (parse-hex deref-ip-str)))
	 (deref-ip-insn (when deref-ip (insn-by-addr deref-ip fault-bin)))
         (details (cdr (assoc 'extra_details msg)))
         (call-stack (when details (convert-fball-call-stack (cdr (assoc 'call-stack details)))))
         (tag (when details (cdr (assoc 'tag details)))))
    ;;(hist (get-histories info)))
    (dbug :repair-deep "Info JSON: ~A" info)
    ;;(dbug :repair-deep "Hist: ~S" hist)
    (dbug :repair "Core-ips ~X for ~A and children" core-ips test-case)
    (cond ((and
	    (or (equal msg-type ":null-dereference-info")
		(equal tag ":null-deref")
		(equal tag ":null_deref")
		(equal reason ":null_deref")
		(equal tag ":concrete-null-dereference")
		(equal reason ":concrete_null_dereference")
		(equal tag ":concrete_null_dereference"))
	    (or restart-ip-insn deref-ip-insn))
           (setf info-task (make-instance 'null-ptr-repair-task)))
          ((equal tag ":return-addr-overwrite")
           (let* ((stack-addr (parse-hex (cdr (assoc 'ret-addr-addr details))))
		  (store-addr (parse-hex (cdr (assoc 'store-addr details))))
                  (func-overwrite (or (find-if #'(lambda (c) (= (cdr (assoc 'esp c)) stack-addr))
					       call-stack)
				      (find-if #'(lambda (c) (= (cdr (assoc 'esp c)) store-addr))
					       call-stack)))
                  (func-addr (or (when func-overwrite (cdr (assoc 'eip func-overwrite)))
				 restart-ip)))
	     (setf func (when func-addr (func-by-addr func-addr fault-bin)))
             (unless func
               (dbug :repair "Unable to find function at ~X in bin ~A" func-addr fault-bin))
             (when func
               (dbug :repair "Stack of ~X overwritable at ~X" func-addr restart-ip)
	       (push (cons restart-ip (make-instance 'cfi-repair-task :func func)) tasks) ;; bonus task.
               (setf info-task (make-instance 'stack-repair-task
					      :func func)))))
          ((equal tag ":controlled-jump")
           (let* ((insn (insn-by-addr restart-ip fault-bin))
                  (is-ret (when insn (ret-p insn)))
                  (insn-func (when insn
                               (func-by-addr restart-ip fault-bin)))
                  (func-overwrite (car call-stack))
                  (func-addr (when func-overwrite (cdr (assoc 'eip func-overwrite))))
                  (func-addr-func (when func-addr (func-by-addr func-addr fault-bin))))
	     (setf func (or insn-func func-addr-func))
             (dbug :repair "Controllable indirect cf ~A in func ~A in bin ~A" insn func fault-bin)
             (when is-ret
               (dbug :repair " ...via ret addr overwrite."))
             (unless func
               (dbug :repair "Unable to find function at ~X in bin ~A" func-addr fault-bin))
             (when func
	       (push (cons restart-ip (make-instance 'cfi-repair-task :func func)) tasks) ;; bonus task.
               (setf info-task (make-instance 'stack-repair-task
                                         :func func)))))
	  ((or (equal reason ":too-deep-path")
	       (equal reason ":too_deep_path")
	       (equal reason ":jump_to_null")
	       (equal reason ":exit()"))
	   (cond ((and core-ips
		       (not (member restart-ip core-ips)))
		  (dbug :repair "Repairing restart ~A with null ptr attempt on core-ip ~X" reason (car core-ips))
		  (setf info-task (make-instance 'null-ptr-repair-task))
		  (setf restart-ip (car core-ips)))
		 (restart-ip
		  (dbug :repair "Repairing restart ~A with null ptr attempt on restart-ip ~X" reason restart-ip)
		  (setf info-task (make-instance 'null-ptr-repair-task)))
		 (t
		  (dbug :repair "Not repairing depth-based restart ~A, no instruction ptr or core-ips" reason))))

          (t
           (dbug :repair "Cannot specifically repair TAG ~A, REASON ~A, at INSN ~A, DEREF ~A."
		 tag reason restart-ip-insn deref-ip-insn)))
    (let* ((histories (get-histories info)))
      (setf tasks (append tasks (remove nil (apply #'append (mapcar #'(lambda (h) (make-repair-tasks-from-history h fault-bin)) histories))))))

    ;; If we don't have a dereference or restart or function, we don't want to do this.
    (cond (restart-ip-insn
	   (push (cons restart-ip info-task) tasks))
	  (deref-ip-insn
	   (push (cons deref-ip info-task) tasks))
	  (func
	   (push (cons restart-ip info-task) tasks))
	  (t
	   (push (cons restart-ip info-task) tasks)))

    (when info-task
      (setf (problem-ip info-task) (or restart-ip deref-ip))
      (setf (call-stack info-task) call-stack))

    (dolist (task-entry tasks)
      (let ((task (cdr task-entry)))
	(when task
	  (setf (bin task) fault-bin)
	  (setf (svc-rev task) svc-rev)
	  (setf (pov-result task) result)
	  (setf (test-case task) test-case)
	  (setf (core-ips task) core-ips))))

    tasks))

;;;-------------------------------------------------------------------------
(defun results-for-test-cases (tcs svc-rev)
  (let* ((pov-results (pov-test-results svc-rev))
	 (my-pov-results (remove-if-not #'(lambda (result) (member (test-case result) tcs)) pov-results)))
    my-pov-results))

(defun valid-core-ips-for-test-results (results bin)
  (remove-if-not
   #'(lambda (addr) (insn-by-addr addr bin))
   (core-ips-for-test-results results)))

(defun core-ips-for-test-results (results)
  (remove-duplicates
   (apply #'append (mapcar #'core-ips-for-test-result
                           results))))

(defun core-ips-for-test-result (result)
  (when (and result (stats result))
    (remove nil
     (mapcar #'parse-hex
      (remove-duplicates
       (remove nil (mapcar #'core-ip (stats result))))))))

;;; from amp.lisp
(defvar *test-cases-already-concolicd* nil)
(defvar *test-cases-being-concolicd* nil)	;; ones still in progress, so we can see if they dont produce anything , and maybe back off to repair
						;; the motivating PoV test case without concolic result

(defvar *test-cases-waiting-to-be-concolicd* nil)
;;; If this is T, in some cases brute-fuzzer is turned on for the i-am-brute-fuzzer fuzzbombs
(defvar *run-brute* T "Should we run brute fuzzer?")

(defvar *max-scoring-pollers* 1500 "If we make more than this many pollers, only run new ones once to see if they are PoVs, do not keep them for scoring")

(defvar *max-brute-povs* 100 "If we make more than this many povs from brute, stop!")
(defvar *num-brute-povs* 0 "counter")
(defvar *reload-file* nil)
(defvar *best-reloaded-rev* nil "Best revision from reloaded run.")
(defvar *reloaded-test-cases* nil "Test case objects from reload.")
(defvar *reloaded-pcap-xml-p* nil "Did we reload pcap-generated xml?")

;;; -------------------------------------------------------------------------
;;; Stuff for reloading from prior run.
;;; -------------------------------------------------------------------------
(defun add-reload-suffix (path)
  (mv path (strcat (namestring path) "-reload")))

(defun file-to-reload ()
  (declare (special *self*))
  (format nil "~A.reload" (shortname *self*)))

(defun reload ()
  (declare (special *best-reloaded-rev*
                    *reloaded-test-cases*))
  ;;(multiple-value-bind (stdout stderr ec)
      ;; (toolchain-command (strcat "tools/get-reload-file " (shortname *self*)) :list-of-lines t)
    ;;(declare (ignore stderr))
  (let ((ec (run-shell-command (strcat "../tools/get-reload-file " (shortname *self*))))
        (file (file-to-reload)))
    (cond ((= ec 0)
           (dbug :reload "Reloading ~A" file)
           (dont-error (load file))
           (dbug :reload "Done loading reload file.")
           (dbug :reload "Running ~A reloaded test cases" (length *reloaded-test-cases*))
           (collect-test-results *reloaded-test-cases* (original-rev (svc *self*)))
           (cond (*best-reloaded-rev*
                  (let* ((binpathnames (mapcar #'add-reload-suffix *best-reloaded-rev*))
			;;make a new list of ('bin, path), ('bin, path)....
			 (binpathnameslist (construct-instance-list binpathnames))
                         (rev (make-new-rev (svc *self*) (mapcar #'make-instance  binpathnameslist))))
                    (dbug :reload "Score reloaded revision ~A" rev)
                    (test-and-score rev)))
                 (t (dbug :reload "No reloaded revision"))))
          (t
           (dbug :reload "Couldn't find file to reload, starting fresh")
           ))))

(defun construct-instance-list (binpathnames)
	(loop for x in binpathnames
		collect (list ''bin x))
)

(defun remove-prefix (full prefix)
  (if (startswith full prefix)
      (subseq full (length prefix))
    full))

(defun path-for-reload (path)
  (list 'strcat '*experiment-dir* (remove-prefix (namestring path) *experiment-dir*)))

(defun bins-for-reload (rev)
  (loop for bin in (bins rev)
      collecting (path-for-reload (path bin))))

(defun make-reload-pathname ()
  (merge-pathnames (format nil "doit-~A.~A.reload" (condor-submit-node) (condor-cluster))))

(defun write-reload-sexp (sexp &key (dont-error t))
  (declare (special *reload-file*))
  (dbug :reload-deep "~A" (write-to-string sexp :pretty nil))
  (cond ((null *reload-file*)
         (dbug :top "ERROR: reload file null!"))
        (dont-error
         (write (list 'dont-error sexp) :stream *reload-file* :pretty nil)
         (format *reload-file* "~%")
         (force-output *reload-file*))
        (t
         (write sexp :stream *reload-file* :pretty nil)
         (format *reload-file* "~%")
         (force-output *reload-file*))))

(defun write-reload-test-case (test-case)
  (cond ((eq :pcap (provenance test-case))
         (dbug :reload "~A from pcap; not reloading" test-case))
        (t
  (write-reload-sexp (list 'push (list 'add-new-test-case-from-povml-file
                                       (path-for-reload (file test-case))
                                       (list 'svc '*self*)
                                       :info (when (info test-case) (path-for-reload (info test-case)))
                                       :has-prolog (has-prolog test-case)
                                       :provenance :reload
                                       :expected-pov (expected-pov test-case))
                           '*reloaded-test-cases*)))
  ))

(defun handle-concolic-test-case (test-case)
                 	 (dbug :top "Got a concolic result for ~A, no more minification, starting repair" test-case)
                 	 (dbug :top "	*test-cases-being-concolicd* is ~A" *test-cases-being-concolicd*)
			 (when (> (length *test-cases-being-concolicd*) 1)
                 	 	(dbug :top "	Warning: more than one *test-cases-being-concolicd*, I'm popping the head but that may be wrong"))
			 (setf (parent test-case) (pop *test-cases-being-concolicd*))
			 (if (parent test-case)
			   (pushnew test-case (children (parent test-case)))
			   (dbug :top "WARNING: got concolic result but *test-cases-being-concolicd* was nil, so we'd already moved to repair, this tc is orphaned")
			 )

			 (repair (get-root-test-case test-case))
			 (when *test-cases-waiting-to-be-concolicd*
			   (dbug :top "After starting repair on concolic result, I see there are others waiting to be concolicd...popping one")
			   (start-concolic (pop *test-cases-waiting-to-be-concolicd*)))
)
;;; -------------------------------------------------------------------------
(defun repair-saved-pollers ()
  (dbug :top "Attempting to repair ~A saved pre-protocol-bad-pollers" (length *pre-protocol-bad-pollers*))
  (collect-test-results
	(mapcar #'make-repaired-test-case *pre-protocol-bad-pollers*)
	(original-rev (svc *self*))))

;;; -------------------------------------------------------------------------
#+allegro
(defun make-repaired-test-case (tc)
  (let* ((svc (svc *self*))
	(file (file tc))
	(rootname (pathname-name file))
	(old-path (excl:path-pathname file))
	(newfile (merge-pathnames old-path (make-pathname :name (strcat rootname "-repaired.xml"))))
	(dir (protocol-dir svc))
	test-case
	)
  (dbug :top "Trying to create repaired test case of ~A in ~A with dir ~A" file newfile dir)
  (repair-pov (id svc) dir (namestring file) (namestring newfile))
  (cond ((probe-file newfile)
  	  (setf test-case (add-new-test-case-from-povml-file (namestring newfile) svc :provenance :REPAIRED))
  	  (push test-case (children tc))
  	  test-case)	;; return val
	(T (dbug :top "Failed to make new file ~A, no repaired test case" newfile)
	  nil))	;; return val
))

(defvar *max-batch-wait-time-minutes* 30)
;;
;;(defun check-for-old-test-batches ()
;;  (declare (special *pending-batches*))
;;  (when *pending-batches*
;;    (let* ((oldest-batch (first (last *pending-batches*)))
;;	   (test-batch-id (first oldest-batch))
;;	   (dvm (second oldest-batch))
;;	   (sent-time (third oldest-batch))
;;	   (time-waited (- (get-internal-real-time) sent-time))
;;	  )
;;    (dbug :top "oldest test batch ~A started at ~A on ~A, we've waited ~A" test-batch-id sent-time dvm time-waited)
;;    (when (> time-waited (* internal-time-units-per-second 60 *max-batch-wait-time-minutes*))
;;    	(dbug :top "WARNING: ~A did not respond in ~A, considering him DEAD" dvm time-waited)
;;        (setdelete test-batch-id *pending-batches* :key #'first)
;;	(let ((dvm-amp (find-amp dvm)))
;;          (dbug :top "Dead-to-me amp is ~A" dvm-amp)
;;          (when dvm-amp (handle-dying-amp dvm-amp)))
;;	)
;;   ))
;;)

;;; -------------------------------------------------------------------------
(defun count-pending-test-cases (amp)
  (if amp
      (loop for entry in (pending-test-cases amp) summing (length (cdr entry)))
      nil))

;;; -------------------------------------------------------------------------
(defun flooded-p ()
  (declare (special *max-concurrent-test-runners* *pending-batches*))
    (dolist (dvm (dvms *self*))
	(let* ((amp (find-amp dvm))
	       (sum (count-pending-test-cases amp)))
	  (when (and sum (= 0 sum)) (return-from flooded-p nil))	;; if any DVM has no pending tests, we are not flooded
	  (when (and sum (> sum 0)) (dbug :testing-deep "~A has ~A pending test cases" dvm sum))))

  (let* ((num-batches (length *pending-batches*))
         (num-dvms (length (dvms *self*)))
	 (num-amps (length *amps*))
	 (limit (/ (* num-dvms *max-concurrent-test-runners*) (/ num-amps 10)))
	)
   (when (>= num-batches limit)
	  (dbug :top "Flooded with ~A pending test batches over limit ~A -- suspending deliberation task" num-batches limit)
	  (suspend-delib-task *delib-task*)
	  T)
  )
)

;;; -------------------------------------------------------------------------

(defun from-minification (tc)
  (eq (provenance tc) :minification))

(defun not-from-minification (tc)
  (not (eq (provenance tc) :minification)))

(defun from-maxification (tc)
  (eq (provenance tc) :maxification))

(defun from-concolic (tc)
  (eq (provenance tc) :fuzzball-concolic))

(defun from-symbolic (tc)
  (eq (provenance tc) :fuzzball-symbolic))

(defun not-from-concolic (tc)
  (not (from-concolic tc)))


;;; -------------------------------------------------------------------------

(defun start-concolic (test-case)
  (declare (special *test-cases-already-concolicd*))
  (declare (special *test-cases-being-concolicd*))

  (when (member test-case *test-cases-already-concolicd*)
	(dbug :top "Test case ~A already concolicd, skipping in start-concolic" test-case)
	(return-from start-concolic nil))

  (when *test-cases-being-concolicd*
  	(dbug :top "There are already *test-cases-being-concolicd*, caching this one for later ~A" test-case)
	(setappend *test-cases-waiting-to-be-concolicd* (list test-case))
	(return-from start-concolic nil))

  (push test-case *test-cases-already-concolicd*)
  (push test-case *test-cases-being-concolicd*)	;; actually we only ever do one at a time, because fball-concolic is a blocking call (with timeout) now
	;; oh yes the concolic itself is blocking, but then we get back a return msg that may have been beaten by a bunch of other PoVs and they
	;; will want their own concolicing..ugh.   need to either have a diff way to attach the concolic result to the parent test-case (which is now
	;; relying on popping this supposedly singleton list) or cache these guys up ...
	;; Getting more info back in return msg would require ocaml changes; for now i'm caching them.

  (dbug :top "calling run-fuzzball-on-pov-bin for Fuzzball in concolic mode on ~A" test-case)
  (let ((rev (original-rev (svc test-case))))
	 ;; FIXME only works on the first bin
	;; this should make fball connect up again with a new test case with :provenance :fuzzball-concolic
	;; so that we can detect that and not minify it etc.
    (run-fuzzball-on-pov-bin (first (bins rev)) (file test-case))
	;; when fball returns, we'd better have a :new-test-case message waiting for us...if not, run repair on original pov.
	;; Challenges arise if concolic fball does *not* connect up with a new PoV test case; if it fails, we need to get back on our repair game...
	;; that's where we use the update-pending-work-when-idle functionality.
))
;;; -------------------------------------------------------------------------

;;; -------------------------------------------------------------------------
;;; WORKING HERE FIXME need to update this with a minification-task strategy
;;; This is a new and possibly generally useful hook place for when the main loop is idle (no msgs inbound on last check)
;;; and you want to know that...
(defun update-pending-work-when-idle ()
  (declare (special *self* *test-cases-being-concolicd*))
  (cond (*test-cases-being-concolicd*
		(dbug :top "Main AMP loop is idle and we were expecting to get a :new-test-case from concolic fball")
		(dbug :top "  Instead, I guess we'll repair on the original PoV that triggered concolic")
		(mapcar #'repair *test-cases-being-concolicd*)	;; Note this really should be a singleton list
		(setf *test-cases-being-concolicd* nil)
		)
;	((and *minifier-in-progress* *minifier-smallest-known-fault-case*)
;		(dbug :top "Main AMP loop is idle, *minifier-in-progress* is still true and we have a smallest minified fault case")
;		(start-concolic *minifier-smallest-known-fault-case*)
;   		(reset-minifier)
;		)
;	((and *minifier-in-progress* *non-minified-results*)
;		(dbug :top "Main AMP loop is idle, *minifier-in-progress* is still true and there are non-minified results")
;   		(reset-minifier)
;		(let ((result (pop *non-minified-results*)))
;		  (init-binary-minification (test-case result) (test-case-path result))
;		))
;	(*minifier-in-progress*
;		(dbug :top "Main AMP loop is idle, minification failed; bailing to repair on original pov if one exists")
;   		(reset-minifier)
;		(let ((povs (pov-test-cases (svc *self*))))
;		  (when povs (repair (first povs)))
;		))
	)
)

;;; -------------------------------------------------------------------------
(defvar *test-cases-already-being-repaired* nil)

(defun repair (test-case)
  (declare (special *test-cases-already-being-repaired*))
  (let ((root-test-case (get-root-test-case test-case)))
   (when (not (eq test-case root-test-case))
       (dbug :top "Note: repair called with non-root ~A, going up to root ~A" test-case root-test-case)
       (setf test-case root-test-case)))

  (when (member test-case *test-cases-already-being-repaired*)
	(dbug :top "Test case already under repair, skipping in repair")
	(return-from repair nil))

  (push test-case *test-cases-already-being-repaired*)

  (let* ((rev (original-rev (svc test-case)))
	 (result (find test-case (pov-test-results rev) :key #'test-case)))
  (dbug :top "repair ~A ~A ~A" test-case rev result)
  ;;(send-message-to-optimi :type :new-pov :svc-id (id (svc test-case)) :pov-path (file test-case))

	;; FIXME when the lookup of result fails, for some reason, this used to puke; why would there ever be a miss?

  (when (not result)
	(dbug :top "Warning: failed to find result for test-case ~A" test-case))
  (when (or (not result) (not (make-repair-task-for-pov-result result test-case rev)))	;; when repair task creation fails... punt to repairing an earlier POV?
	(dbug :top "Repair refused to work on ~A, trying a different non-concolic one if poss" test-case)
	(let* ((pov-test-cases (remove (get-root-test-case test-case) (mapcar #'get-root-test-case (pov-test-cases (svc test-case)))))
		(new-tc (or (any #'not-from-minification pov-test-cases)
			  	(any #'not-from-concolic pov-test-cases))))
		(dbug :top "Chose different test case ~A" new-tc)
		(when new-tc (repair new-tc))))
))

;;; -------------------------------------------------------------------------
(defun subscribe-to-svc-rev-results (subscriber svc-rev)
  (dbug :amp-deep "Subscribing ~A to svc-rev ~A results" subscriber svc-rev)
  (cond
   ((assoc svc-rev (svc-rev-subscriptions *self*))
    (pushnew subscriber (cdr (assoc svc-rev (svc-rev-subscriptions *self*)))))
   (t
    (push (list svc-rev subscriber) (svc-rev-subscriptions *self*))))
  (dbug :amp-deep "svc-rev ~A subscribers now ~A" svc-rev (assoc svc-rev (svc-rev-subscriptions *self*))))

(defun unsubscribe-to-svc-rev-results (subscriber svc-rev)
  (dbug :amp-deep "Unsubscribing ~A from svc-rev ~A results" subscriber svc-rev)
  (let ((curr-subscribers (cdr (assoc svc-rev (svc-rev-subscriptions *self*)))))
    (cond
     ((null curr-subscribers)
      t)
     ((equal curr-subscribers (list subscriber))
      (setf (svc-rev-subscriptions *self*)
        (remove svc-rev (svc-rev-subscriptions *self*) :key 'car)))
     (t
      (setf (cdr (assoc svc-rev (svc-rev-subscriptions *self*)))
        (remove subscriber curr-subscribers))))
    (dbug :amp-deep "svc-rev ~A subscribers now ~A" svc-rev (assoc svc-rev (svc-rev-subscriptions *self*)))))

(defun enable-defensive-rewrites ()
  (dbug :amp "Shields up -- enabling the defensive-rewrite-task")
  (setf *perform-defensive-rewrites* t)
  (make-new-defensive-rewrite-task))

(defun make-new-defensive-rewrite-task ()
  "Make defensive rewrite task for a new svc."
  (dbug :amp "make-new-defensive-rewrite-task")
  (unless *defensive-rewrite-task*
    (setf *defensive-rewrite-task* (make-instance 'defensive-rewrite-task))
    (add-defense-task)))

(defun add-defense-task ()
  (declare (special *fixed-a-pov*))
  (cond (*fixed-a-pov*
    	  (dbug :amp "Not adding defensive rewrite task to the task list b/c I've already fixed a PoV."))
        (T (dbug :amp "Adding defensive rewrite task to the task list (if not already there).")
  	   (pushnew *defensive-rewrite-task* (tasks *self*))
	))
)

(defun remove-defense-task ()
  (setf (tasks *self*) (remove *defensive-rewrite-task* (tasks *self*))))

(defun make-new-compound-repair-task ()
  "Make compound repair task for a new svc."
  (dbug :amp "make-new-compound-repair-task")
  (unless *compound-repair-task*
    (setf *compound-repair-task* (make-instance 'compound-repair-task))
    (add-compound-repair-task)))

(defun add-compound-repair-task ()
  (dbug :amp "Adding compound repair task to the task list.")
  (pushnew *compound-repair-task* (tasks *self*)))

(defun remove-compound-repair-task ()
  (setf (tasks *self*) (remove *compound-repair-task* (tasks *self*))))

;;; -------------------------------------------------------------------------
;;;
(defvar *maybe-dead-dvm-amps* nil)

;;; -------------------------------------------------------------------------
(defun resurrect-maybe-dead-amps ()
 (when *maybe-dead-dvm-amps*
  (dbug :top "resurrecting maybe dead dvm amp list ~A" *maybe-dead-dvm-amps*)
  (dolist (amp *maybe-dead-dvm-amps*)
      (when (socket-to amp)
  	(dbug :top "still have a socket to ~A, let's try resurrecting him" amp)
	(pushnew (shortname amp) *dvms* :test #'equal)
	(pushnew (shortname amp) (dvms *self*) :test #'equal)
	(pushnew amp *amps*)
	))
  (setf *maybe-dead-dvm-amps* nil)
  (setf *max-batch-wait-time-minutes* (+ 10 *max-batch-wait-time-minutes*))
  (dbug :top "Set new *max-batch-wait-time-minutes* to ~A" *max-batch-wait-time-minutes*)
 )
)

;;; -------------------------------------------------------------------------
	;; factored this out of above for use when some other method figures out an amp has died (eg, heartbeats)
(defun handle-dying-amp (dying-amp)
  (declare (special *self*
		*pending-batches*
	))
  (let ((name (shortname dying-amp)))
;    	;; if a DVM dies, we need to remove it and find out what jobs it hasnt finished and re-send them.
;    (when (dvm-p dying-amp)
;      (dbug :top "DVM ~A died, its shortname is ~A" dying-amp name)
;      (setdelete name (dvms *self*) :test #'equal)
;      (dbug :amp-deep "revised list of (dvms *self*): ~A" (dvms *self*))
;      (setdelete name *dvms* :test #'equal)
;      (dbug :amp-deep "revised list of *dvms*: ~A" *dvms*)
;      (when (socket-to dying-amp)
;        (dbug :top "he might not be dead, adding to maybe list ~A" *maybe-dead-dvm-amps*)
;	(pushnew dying-amp *maybe-dead-dvm-amps*))
;
;      (dbug :amp-deep "before delete, pending-batches has ~A entries" (length *pending-batches*))
;      (setf *pending-batches* (delete-if #'(lambda (entry) (equal name (second entry))) *pending-batches*))
;      (dbug :amp-deep "after delete, pending-batches has ~A entries" (length *pending-batches*))
;
;      (if (null *dvms*)
;	  (dbug :top "RESULT: Game over man, game over!  What the bleep are we gonna do now?? (no more DVMs!)")
;          (reassign-test-cases dying-amp))
;    )

    (setdelete name *optimi* :test #'equal)
    (setdelete name *fuzzbombs* :test #'equal)

    (setdelete dying-amp *amps*)

    	;; if the master dies, we have to elect new one...
    (when (master-p dying-amp)
      (dbug :top "Ding dong, the master's dead")
      (setf *master-amp* nil)

      (when (i-am-next-master dying-amp)
       (dbug :top "I am the new master, bwah hah hah")
       ;;   This error msg is OBE, at the very least.
       (error "Due to non-aligned svc-meta IDs, redundant Optimi are busted.  doh")
      (setf *master-amp* *self*)
      (setf (master-p *self*) T)
      (dolist (a (remove *self* *amps*))
        (send-msg (shortname a)
                  (list '(:type :new-master) (list :who (name *self*))))))
    )

    ;; Find the dying guy's commitments, :fail them, and re-task them
    ;; if you're the master.
    ;; note if a contract was finished, its status would not be
    ;; :awarded-to-another so it will not get retasked.
;    (when (not (eq halt-status 'stop-amp-task))
;      (dolist (c (contracts *self*))
;        (when (and (equal (status c) :awarded-to-another) ; if contract awarded
;                   (eq (contractor c) dying-amp)) ; to dying amp
;          (dbug :top "Failing ~A status: ~A contractor: ~A" c (status c) (contractor c))
;          (setf (status c) :failed)
;          ;(aid-set-contract-flag c :NOTCONTRACTED)
;          (when (master-p *self*) (push c (tasks *self*)))))
;      )
))


