;;; $Id: mr.lisp 3362 2014-05-11 18:20:49Z sfriedman $
;;; -------------------------------------------------------------------------
;;; COPYRIGHT START
;;; Copyright (c) 2012, Smart Information Flow Technologies (SIFT). Developed with the sponsorship of the Defense Advanced Research Projects Agency (DARPA) and Air Force Research Laboratory. 
;;; -------------------------------------------------------------------------
;;;
;;; Define the model repository. Initially, this is a set of
;;; unit-instance classes. Later this may be augmented with methods
;;; for managing relationships between the unit-instances.
;;;
;;; We defmethod print-instance-slots to make instances include slot
;;; information when they are displayed.

(in-package :fuzzbomb)
(eval-when (:compile-toplevel) (optimization-boilerplate))

;;; -------------------------------------------------------------------------

;;; ------------------------------------------------------------
;;; compressed-dir-p for pereti tasks

;;;(defmethod compressed-dir-p (value)
;;;  (declare (ignore value))
;;;  nil)

;;;(defmethod compressed-dir-p ((tgt target))
;;;  (and (path tgt)
;;;       (or (search "compressed data" (file-results tgt))
;;;      	   (search "archive data" (file-results tgt)))))

;;;   FIXME? dunno if we need this, but
;;; if we do, it needs to be defined after hist-tree-target-node.
;;;(defmethod compressed-dir-p ((tn hist-tree-target-node))
;;;  (compressed-dir-p (target tn)))
  
;;; ------------------------------------------------------------
;;; Binary Service
;;;
;;; A binary service (svc) is a set of one or more binaries (bins) that
;;; presumably communicate with each other and are thereby testable as a set.
;;; The service tracks all available test cases and has pointers to the original binaries.
;;; FuzzBomb will allow the same binary (bin) to be a member of multiple distinct services,
;;; though this might not occur in CGC.

(defclass svc (target)
  (
   (random-reachable :initarg :random-reachable :accessor random-reachable :initform nil)
   ;; The svc-rev for the original binaries of this svc.
   (original-rev :initarg :original-rev :accessor original-rev :initform nil)
   ;; A list of svc-rev for this svc.
   (fb-revs :initarg :fb-revs :accessor fb-revs :initform nil)	;; Mikey sez: IIRC revs don't get on fb-revs until they shield the pov
   ;; The list of svc-revs for this svc for which we want to compute a score.
   (revs-to-score :initarg :revs-to-score :accessor revs-to-score :initform nil)
   (poller-test-cases :initarg :poller-test-cases :accessor poller-test-cases :initform nil)
   (pov-test-cases :initarg :pov-test-cases :accessor pov-test-cases :initform nil)
   ;; Pointer to best svc-rev we've evaluated.
   (best-so-far :initarg :best-so-far :accessor best-so-far :initform nil)))

(defmethod print-object ((obj svc) stream)
  (format stream "#<~A ~A>" (type-of obj) (id obj)))

(defmethod test-cases ((obj svc))
  (concatenate 'list (poller-test-cases obj) (pov-test-cases obj)))

(defmethod strings-file ((svc svc))
  (strcat (namestring (dir svc)) "/strings.txt"))

;;; ------------------------------------------------------------
;;; Binary Service Revision
;;;
;;; A binary service revision (svc-rev) is a set of binaries which
;;; fill the slots in a binary service. One of these is the original
;;; revision of the svc.

(defclass svc-rev (named-object)
  ((svc :initarg :svc :accessor svc :initform nil)
   (bins :initarg :bins :accessor bins :initform nil)
   ;; Note: In CQE, if we introduce a vuln, it's probably okay since
   ;; competitors don't see our replacement binaries--it's unlikely
   ;; they'd submit a PoV that happens to hit it. In CFE, however, we
   ;; will care, and we might need to rethink how we handle that case
   ;; WRT test results.
   (poller-test-results :initarg :poller-test-results :accessor poller-test-results :initform nil)
   (pov-test-results :initarg :pov-test-results :accessor pov-test-results :initform nil)
   ;; Stash timeouts separately.
   (timeout-test-results :initarg :timeout-test-results :accessor timeout-test-results :initform nil)
   ;; List of all test cases which have been kicked off for this rev,
   ;; both ones we have results for and those that are still running.
   (executed-test-cases :initarg :executed-test-cases :accessor executed-test-cases :initform nil)
   ;; List of test cases which have been kicked off but not returned
   ;; results yet.
   (pending-test-cases :initarg :pending-test-cases :accessor pending-test-cases :initform nil)
   (scoring-mode :initarg :scoring-mode :accessor scoring-mode :initform nil)
   (score :initarg :score :accessor score :initform nil)
   (creation-time :initarg :creation-time :accessor creation-time :initform (get-internal-real-time))
   (component-revs :initarg :component-revs :accessor component-revs :initform nil)
   (patches :initarg :patches :accessor patches :initform nil)
))

(defvar *all-revs* nil)

(defun make-new-rev (svc bins)
 (declare (special *all-revs*))
 (let ((r (make-instance 'svc-rev :svc svc :bins bins)))
   (sieve-node r :shape "box3d" :color "green")
   (dolist (b bins)
     (sieve-edge b r))
   (push r *all-revs*)
   r))

(defmethod test-results ((obj svc-rev))
  (concatenate 'list (poller-test-results obj) (pov-test-results obj)))


;;; ------------------------------------------------------------
;;; Score
;;;
;;; Holds the estimated score for a svc-rev, including THE overall
;;; score and the component parts.

(defclass score ()
  ;; overall = (* availability security evaluation)
  ((overall :initarg :overall :accessor overall :initform nil)
   ;; availability = (min func-score perf-score)
   (availability :initarg :availability :accessor availability :initform nil)
   ;; func-score = (f func-factor) where f is a step function.
   (func-score :initarg :func-score :accessor func-score :initform nil)
   ;; func-factor = (/ #-pollers-passed #-pollers)
   (func-factor :initarg :func-factor :accessor func-factor :initform nil)
   ;; perf-score = (f perf-factor) where f is a step function.
   (perf-score :initarg :perf-score :accessor perf-score :initform nil)
   ;; perf-factor = (1+ (max (* 0.25 file-size-overhead) mem-use-overhead exec-time-overhead))
   (perf-factor :initarg :perf-factor :accessor perf-factor :initform nil)
   ;; Overhead slots are lists of overhead corresponding to the bins
   ;; of the svc-rev.
   (file-size-overhead :initarg :file-size-overhead :accessor file-size-overhead :initform nil)
   (mem-use-overhead :initarg :mem-use-overhead :accessor mem-use-overhead :initform nil)
   (exec-time-overhead :initarg :exec-time-overhead :accessor exec-time-overhead :initform nil)
   ;; security = 0 if reference-score 0, else (1+ (* 0.5 (+ reference-score consensus-score)))
   (security :initarg :security :accessor security :initform nil)
   ;; reference-score = (- 1 (/ #-failed-reference-povs #-reference-povs))
   (reference-score :initarg :reference-score :accessor reference-score :initform nil)
   ;; consensus-score = 0 if any submitted PoV failed, 1 otherwise.
   (consensus-score :initarg :consensus-score :accessor consensus-score :initform nil)
   ;; evaluation = 2 if submitted PoV is correct, 1 otherwise.
   (evaluation :initarg :evaluation :accessor evaluation :initform nil)
   ))

(defmethod print-object ((obj score) stream)
  (format stream "#<~A ~A>" (type-of obj) (overall obj)))

;;; ------------------------------------------------------------
;;; Challenge Binary
;;;
;;; In FuzzBomb, the bin is a binary with a unique path and (most likely) unique
;;; binary structure.  Each version is a different cb object.
;;; The original bin (unmodified by FuzzBomb) has no predecessor-bin-id.

;;; Not present (ripped out) from Fuzzbuster: source, test-cases (on svc now), runtime, timeout...

(defclass bin ()
  ((name :initarg :name :accessor name :initform nil)
   (filetype :initarg :filetype :accessor filetype :initform :cgcef)
   ;; Note: (path bin) tells you where the *original* was from; not the present revision.
   (path :initarg :path :accessor path :initform nil)
   (original-path :initarg :original-path :accessor original-path :initform nil)
   (elf :initarg :elf :accessor elf :initform nil)
   (plt :initarg :plt :accessor plt :initform nil)
   (func-ids :initarg :func-ids :accessor func-ids :initform nil)
   (block-edge-ids :initarg :block-edge-ids :accessor block-edge-ids :initform nil)
   (block-ids :initarg :block-ids :accessor block-ids :initform nil)
   ;;(out-edge-hash :initarg :out-edge-hash :accessor out-edge-hash :initform (make-hash-table))
   ;;(in-edge-hash :initarg :in-edge-hash :accessor in-edge-hash :initform (make-hash-table))
   (locked :initarg :locked :accessor locked :initform nil) ; Bin can be locked if it's a past version.
   (predecessor-bin-id :initarg :predecessor-bin-id :accessor predecessor-bin-id :initform nil)
   (original-filesize :initarg :original-filesize :accessor original-filesize :initform nil)
   (patches :initarg :patches :accessor patches :initform nil)
   (freespaces :initarg :freespaces :accessor freespaces :initform nil)
   (extended-freespaces :initarg :extended-freespaces :accessor extended-freespaces :initform nil)
   (extended-bin :initarg :extended-bin :accessor extended-bin :initform nil)
   (dataspaces :initarg :dataspaces :accessor dataspaces :initform nil)
   ;; Addresses of potential vulnerabilities, e.g., from static analysis.
   (potential-vuln-addrs :initform nil :accessor potential-vuln-addrs :initarg :potential-vuln-addrs)
   ;; new for neo-fuzz
   (target :initform nil :accessor target :initarg :target)
   (platform :initform nil :accessor platform :initarg :platform)
   ;; Vestiges from Fuzzbuster
   (amp-goal-name :initform nil)
   (amp-threat-name :initform nil)))

(defmethod print-object ((obj bin) stream)
  (format stream "#<~A-~d ~A v~A>" (type-of obj) (id obj) (or (name obj) (path obj)) (id obj)))

(defvar *bin-paths* (make-hash-table :test 'equal)
  "Hash from path to its bin object.")

;;; removing both of these... i'm hoping bin is unused actually
;;(defmethod initialize-instance :after ((o bin) &key)
;;  (setf (gethash (path o) *bin-paths*) o))
;;
;;(defmethod initialize-instance :after ((b bin) &key)
;;  (setf (original-path b) (truename (original-path b)))
;;  (init-obj b b)
;;  (setf (path b) (pathname-for-bin b))
;;  (setf (name b) (pathname-name (path b)))
;;  (sieve-node b :shape "box" :color "white")
;;  b)

(defun find-bin-by-path (path)
  "Return bin at PATH, or nil if there isn't already one."
  (gethash path *bin-paths*))

(defun pathname-for-bin (bin)
  "Compute where to write the binary (or where it was written)"
  (let ((my-id (id bin))
        (orig-path (original-path bin)))
    (cond ((= my-id 0)
           orig-path)
          (t
           (format nil "~A_~A" orig-path my-id)))))

;;; ------------------------------------------------------------
;;; Binary structures.

(defclass addr-range ()
  ((addr-begin :initarg :addr-begin :accessor addr-begin :initform nil)
   (addr-end :initarg :addr-end :accessor addr-end :initform nil)))

(defmethod print-object ((obj addr-range) stream)
  (format stream "#<~A @~X-~X>" (type-of obj)
          (addr-begin obj) (addr-end obj)))

;; Freespace is a range of memory for (re)writing.
(defclass freespace (addr-range)
  ())

(defclass dataspace (addr-range)
  ((data-name :initarg :data-name :accessor data-name :initform nil)))

(defun make-dataspace (addr-begin addr-end data-name)
  (make-instance 'dataspace
		 :data-name data-name
		 :addr-begin addr-begin
		 :addr-end addr-end))

(defun make-freespace (addr-begin addr-end)
  "Makes a new instance of a freespace address range."
  (make-instance 'freespace
    :addr-begin addr-begin
    :addr-end addr-end))

(defclass insn-edge ()
  ((from-insn-id :initarg :from-insn-id :accessor from-insn-id :initform nil)
   (to-insn-id :initarg :to-insn-id :accessor to-insn-id :initform nil)
   (op :initarg :op :accessor op :initform nil)
   (replacement-label-insn :initarg :replacement-label-insn :accessor replacement-label-insn :initform nil)))

;;; ------------------------------------------------------------
;;; Print defs for other objects, for cleanliness.

;;;(defmethod print-object ((obj elf::section) stream)
;;;  (format stream "#<~A ~A>" (type-of obj) (or (elf::name obj)
;;;                                              (position obj (elf::sections (elf::elf obj))))
;;;          ))

;;; ------------------------------------------------------------
;;; Instruction and block localization.


;;; ------------------------------------------------------------
;;; Bin mod

;;; An bin-mod is something that's been built or added to an bin by FuzzBomb.

(defclass bin-mod ()
  ((bin :initform nil :accessor bin :initarg :bin)
   (status :initform :new :accessor status :initarg :status)))

;;; ------------------------------------------------------------
;;; Remedies

;;; Remedies are high-level bin-mods for the sake of defense or repair.
;;; A single patch may encompass multiple remedies.
;;; When a aremedy inherits from another, the superordinate remedies are enacted
;;; first, e.g., as if call-next-method is the first thing that executes in
;;; any given remedy method.

(defclass remedy (bin-mod named-object)
  ((patch :initform nil :accessor patch :initarg :patch)
   (creator-task :initform nil :accessor creator-task :initarg :creator-task)
   (new-func-names :initform nil :accessor new-func-names :initarg :new-func-names)
   (new-func-addrs :initform nil :accessor new-func-addrs :initarg :new-func-addrs)
   (term-func-id :initform nil :accessor term-func-id :initarg :term-func-id)))

;(defmethod print-object ((obj remedy) stream)
;  (format stream "#<~A ~A ~A>" (type-of obj) (bin obj) (status obj)))

(defclass cleanup (remedy)
  ())

(defclass cookie ()
 ((cookie-value :initform nil :accessor cookie-value :initarg :cookie-value)))

(defclass block-remedy (remedy)
  ((block-id :initform nil :accessor block-id :initarg :block-id)))

;(defmethod print-object ((obj block-remedy) stream)
;  (format stream "#<~A @block ~A ~A>" (type-of obj) (block-id obj) (status obj)))

(defclass function-remedy (remedy)
  ((func-id :initform nil :accessor func-id :initarg :func-id)))

;(defmethod print-object ((obj function-remedy) stream)
;  (format stream "#<~A @func ~A ~A>" (type-of obj) (func-id obj) (status obj)))

(defclass stack-pad (function-remedy)
  ((pad-length :initform nil :accessor pad-length :initarg :pad-length)
   (pad-below :initform 0 :accessor pad-below :initarg :pad-below)))

(defclass stack-cookie (function-remedy cookie)
  ((cookie-offset :initform nil :accessor cookie-offset :initarg :cookie-offset)))

(defclass stacktop-cookie (remedy cookie)
  ((addrs :initform nil :accessor addrs :initarg :addrs)))

(defclass null-ptr-check (remedy)
  ((insn-id :initform nil :accessor insn-id :initarg :insn-id)))

(defclass clean-seppuku (remedy)
  ((addrs :initform nil :accessor addrs :initarg :addrs)))

(defclass messy-seppuku (remedy)
  ((addrs :initform nil :accessor addrs :initarg :addrs)))

(defclass cookie-check (remedy)
  ((addrs :initform nil :accessor addrs :initarg :addrs)))

(defclass range-check (remedy)
  ((addrs :initform nil :accessor addrs :initarg :addrs)
   (func-id :initform nil :accessor func-id :initarg :func-id)
   (check-ret-addr :initform nil :accessor check-ret-addr :initarg :check-ret-addr)))

(defclass static-cookies (remedy cookie)
  ((addrs :initform nil :accessor addrs :initarg :addrs)))

(defclass allocate-cookie (remedy cookie)
  ())

(defclass malloc-cookie (remedy cookie)
  ())

(defclass injected-function (remedy)
  ((injected-func-name :initform nil :accessor injected-func-name :initarg :injected-func-name)
   (injected-func-insn-strs :initform nil :accessor injected-func-insn-strs :initarg :injected-func-insn-strs)
   (writer-fn :initform nil :accessor writer-fn :initarg :writer-fn)
   (writer-dat :initform nil :accessor writer-dat :initarg :writer-dat)))

(defclass function-intercept (remedy)
  ((intercepted-func-name :initform nil :accessor intercepted-func-name :initarg :intercepted-func-name)
   (interceptor-func-name :initform nil :accessor interceptor-func-name :initarg :interceptor-func-name)))

(defclass erase-block-content (block-remedy)
  ())

(defclass claim-as-freespace (block-remedy)
  ())

(defclass add-text-section (remedy)
  ((sec-size :initform #x1000 :accessor sec-size :initarg :sec-size)
   (sec-name :initform ".text.new" :accessor sec-name :initarg :sec-name)
   (to-back :initform t :accessor to-back :initarg :to-back)))

(defclass add-dataspace (remedy)
  ((data-size :initform 128 :accessor data-size :initarg :data-size)
   (data-name :initform nil :accessor data-name :initarg :data-name)))

(defmethod remedy-cost ((rem remedy)) 0)
(defmethod remedy-cost ((rem t)) 0)
(defmethod remedy-cost ((rem add-text-section)) 1)
(defmethod remedy-cost ((rem function-intercept)) 1)
(defmethod remedy-cost ((rem range-check)) 4)
(defmethod remedy-cost ((rem cookie-check)) 2)
(defmethod remedy-cost ((rem add-dataspace)) 1)
(defmethod remedy-cost ((rem injected-function)) 0.25)
(defmethod remedy-cost ((rem null-ptr-check)) 1)
(defmethod remedy-cost ((rem stacktop-cookie)) 1)
(defmethod remedy-cost ((rem stack-cookie)) 2)
(defmethod remedy-cost ((rem stack-pad)) 1)
(defmethod remedy-cost ((rem messy-seppuku)) 1)
(defmethod remedy-cost ((rem clean-seppuku)) 1)


(defmethod copy-remedy-fields ((src t) (dest t))
  nil)

(defmethod copy-remedy-fields ((src cookie-check) (dest cookie-check))
  (setf (addrs dest) (addrs src))
  (call-next-method))

(defmethod copy-remedy-fields ((src range-check) (dest range-check))
  (setf (addrs dest) (addrs src))
  (setf (check-ret-addr dest) (check-ret-addr src))
  (call-next-method))

(defmethod copy-remedy-fields ((src static-cookies) (dest static-cookies))
  (setf (addrs src) (addrs dest))
  (call-next-method))

(defmethod copy-remedy-fields ((src allocate-cookie) (dest allocate-cookie))
  (call-next-method))

(defmethod copy-remedy-fields ((src malloc-cookie) (dest malloc-cookie))
  (call-next-method))

(defmethod copy-remedy-fields ((src injected-function) (dest injected-function))
  (setf (injected-func-name dest) (injected-func-name src))
  (setf (injected-func-insn-strs dest) (injected-func-insn-strs src))
  (setf (writer-fn dest) (writer-fn src))
  (setf (writer-dat dest) (writer-dat src))
  (call-next-method))


(defmethod copy-remedy-fields ((src function-intercept) (dest function-intercept))
  (setf (intercepted-func-name dest) (intercepted-func-name src))
  (setf (interceptor-func-name dest) (interceptor-func-name src))
  (call-next-method))

(defmethod copy-remedy-fields ((src add-text-section) (dest add-text-section))
  (setf (sec-size dest) (sec-size src))
  (setf (sec-name dest) (sec-name src))
  (setf (to-back dest) (to-back src))
  (call-next-method))

(defmethod copy-remedy-fields ((src add-dataspace) (dest add-dataspace))
  (setf (data-size dest) (data-size src))
  (setf (data-name dest) (data-name src))
  (call-next-method))

(defmethod copy-remedy-fields ((src remedy) (dest remedy))
  (setf (term-func-id dest) (term-func-id src))
  (setf (bin dest) (bin src))
  (call-next-method))

(defmethod copy-remedy-fields ((src function-remedy) (dest function-remedy))
  (setf (func-id dest) (func-id src))
  (call-next-method))

(defmethod copy-remedy-fields ((src cookie) (dest cookie))
  (setf (cookie-value dest) (cookie-value src))
  (call-next-method))

(defmethod copy-remedy-fields ((src stack-pad) (dest stack-pad))
  (setf (pad-length dest) (pad-length src))
  (call-next-method))

(defmethod copy-remedy-fields ((src stack-cookie) (dest stack-cookie))
  (setf (cookie-offset dest) (cookie-offset src))
  (setf (cookie-value dest) (cookie-value src))
  (call-next-method))

(defmethod copy-remedy-fields ((src stacktop-cookie) (dest stacktop-cookie))
  (setf (addrs dest) (addrs src))
  (setf (cookie-value dest) (cookie-value src))
  (call-next-method))

(defmethod copy-remedy-fields ((src null-ptr-check) (dest null-ptr-check))
  (setf (insn-id dest) (insn-id src))
  (call-next-method))

(defmethod copy-remedy-fields ((src clean-seppuku) (dest clean-seppuku))
  (setf (addrs dest) (addrs src))
  (call-next-method))

(defmethod copy-remedy-fields ((src messy-seppuku) (dest clean-seppuku))
  (setf (addrs dest) (addrs src))
  (call-next-method))



;;; ------------------------------------------------------------
;;; Transforms

;;; Transforms are specific changes (additions, revisions, deletions) to the binary
;;; for some higher purpose (i.e., remedy).  For instance, a stack cookie may be implemented
;;; as multiple trampolines and rewrites in an existing function, or as a single trampoline
;;; with a lot of additional code.

(defclass transform (bin-mod addr-range)
  ((remedy :initform nil :accessor remedy :initarg :remedy)
   (parent :initform nil :accessor parent :initarg :parent)
   (new-insns :initform nil :accessor new-insns :initarg :new-insns)
   (func-name :initform nil :accessor func-name :initarg :func-name)))

(defclass addition (transform)
  ((before-insn :initform nil :accessor before-insn :initarg :before-insn)
   (after-insn :initform nil :accessor after-insn :initarg :after-insn)))

;(defmethod print-object ((obj addition) stream)
;  (format stream "#<~A @~A:0x~X [~A] ~S>"
;          (type-of obj) (if (bin obj) (name (bin obj)) "") (addr-begin obj) (size-sum (new-insns obj)) (status obj)))

(defclass overwrite (transform)
  ((old-insns :initform nil :accessor old-insns :initarg :old-insns)))

(defclass hook (overwrite)
  ((from-addr :initform nil :accessor from-addr :initarg :from-addr)
   (to-addr :initform nil :accessor to-addr :initarg :to-addr)))

;; A trampoline is an addition, but it has a hook (overwrite) from other code.
;; It may or not resume (i.e., have a resume-addr) with the previous code.
(defclass trampoline (overwrite)
  ((hooks :initform nil :accessor hooks :initarg :hooks)
   (resume :initform nil :accessor resume :initarg :resume)
   (resume-addr :initform nil :accessor resume-addr :initarg :resume-addr)))

(defclass block-trampoline (trampoline)
  ((old-blocks :initform nil :accessor old-blocks :initarg :old-blocks)
   (new-blocks :initform nil :accessor new-blocks :initarg :new-blocks)))

(defclass function-trampoline (block-trampoline)
  ((old-func :initform nil :accessor old-func :initarg :old-func)
   (new-func :initform nil :accessor new-func :initarg :new-func)))

(defclass content-nop (overwrite)
  ((nopped-block :initform nil :accessor nopped-block :initarg :nopped-block)))


;;; ------------------------------------------------------------
;;; Exemplar


(defclass exemplar ()
  (app
   (failure-report :initform nil)
   (replication :initform nil)
   (context :initform nil)
   (time-to-patch :initform nil)
   (attackable-areas :initform nil)
   ))


;;; ------------------------------------------------------------
;;; Channels

;;;; [sfriedman:20140528.1123CST] This is from Fuzzbuster, necessary for C3PO.
(defmethod data ((instance t))
  "Return the (native) data of INSTANCE, or nil if there is none."
  nil)

;;; ------------------------------------------------------------
;;; Vulnerability Profile
;;;

(defclass vp ()
  (app
   predecessors
   successors
   ;; The proper output of the bin if the exploit were fixed, if known
   result
   ;; A list of test-case instances. These represent cases that led to
   ;; a detected fault. At some point later we may also record
   ;; non-fault cases.
   (test-cases :initform '())
   ;; Link to the multiple patches for this VP.
   patches))

;;; ------------------------------------------------------------
;;; Patch
;;;
;;; The patcher creates patch instances in response to new
;;; VPs. Once created, the patch becomes the IRM's responsibility. The
;;; IRM is responsible for identifying and removing obsolete patches,
;;; since the patches may require tasks to be executed prior to their
;;; removal.
;;;
;;; After creation, the patcher may still update the patch, but only
;;; while executing a patch-task.

;;; [sfriedman:20140511.1356CST] A lot -- or all -- of this class will have to change,
;;;   but there's no time to overthink and design that now.

(defclass patch (bin-mod)
  (
   ;; The status is one of:
   ;;  :new      - This is the initial state, assigned by the patcher
   ;;              when the patcher creates the instance.
   ;;  :verified - The patcher assigns this status after it verifies
   ;;              the patch. This might be as simple as verifying
   ;;              that the patch blocks the test cases from the VP,
   ;;              or it could be passing the complete system
   ;;              regression.
   ;;
   ;; FIXME [edosh:20110707.0950CST] When verification fails we will
   ;; have to understand the difference between applying a patch
   ;; failing and verification failing.
   ;;
   ;;  :failed   - The patch could not be applied.  This represents a
   ;;              serious error state as the patcher created the patch file
   ;;  :applied  - Assigned to the patch when it is applied to the
   ;;              system.
   ;;  :revoked  - Assigned if a patch was applied but has been taken
   ;;              off the system.
   ;; The bin that results from the patch.
   (resulting-bin :initform nil :initarg :resulting-bin :accessor resulting-bin)
   ;; The service revision that we used as input.
   (svc-rev :initform nil :initarg :svc-rev :accessor svc-rev)
   ;; The service revision that resulted.
   (resulting-svc-rev :initform nil :initarg :resulting-svc-rev :accessor resulting-svc-rev)
   ;; The transforms that distinguish the resulting-bin from the app.
   (tforms :initform nil :initarg :tforms :accessor tforms)
   ;; The remedies that this patch involved.
   (remedies :initform nil :initarg :remedies :accessor remedies)
   ;; Link to the single VP that this patches.
   vp
   ;; Rate patches so IRM can choose the best patch
   rating
   ;; file name of the patch in the staging dir *patch-staging-dir*
   ;; when it is applied it is copied to the patches dir
   (file-name :initform "")
   ;; Tasks for this patch.
   tasks
   ;; Amount of differences between "normal" operation and
   ;; operation when patch is temporarily applied.
   ;; FIXME: some sort of cumulation/average of all the test-cases's differnces
   (behavior-diff :initform 0)
   ;; Assessment alist of this patch's relevant test cases from its app
   (assessment :initform nil)
   ))

;;; ------------------------------------------------------------
;;; Test Replay

;; The replay-expr contains the CBID, etc. -- all content of the .xml file.
(defclass povml-replay ()
  ((replay-expr :initform nil :initarg :replay-expr :accessor replay-expr)))

;;; ------------------------------------------------------------
;;; Test Results

;; The statistics for one bin in a test result. Even though the CGC
;; score will summarize these, FB needs the details so it can see
;; which binaries fared well/poorly/faulted/etc.
(defclass test-stat ()
  ((faulted :initform nil :initarg :faulted :accessor faulted)
   (cpu-time :initform nil :initarg :cpu-time :accessor cpu-time)
   (max-mem-size :initform nil :initarg :max-mem-size :accessor max-mem-size)
   (minor-faults :initform nil :initarg :minor-faults :accessor minor-faults)
   (core-ip :initform nil :initarg :core-ip :accessor core-ip)
   (registers :initform nil :initarg :registers :accessor registers)
   (term-signal :initform nil :initarg :term-signal :accessor term-signal)))

(defmethod print-object ((obj test-stat) stream)
  (format stream "#<~A faulted: ~A cpu-time: ~A max-mem-size: ~A>"
          (type-of obj) (faulted obj) (cpu-time obj) (max-mem-size obj)))

(defclass test-result-base ()
  ((svc-rev-name :initarg :svc-rev-name :accessor svc-rev-name :initform nil)))

(defmethod svc-rev ((obj test-result-base))
  (find-object (svc-rev-name obj)))

;; Maybe add a timestamp or ID for absolute/relative ordering.
;; Maybe make output-matched a %, so we know how many reads we matched, and can make finer judgments.
(defclass test-result (test-result-base)
  ((stats :initform nil :initarg :stats :accessor stats)
   (test-case-path :initform nil :initarg :test-case-path :accessor test-case-path)
   ;; Output in POVML is not binary-specific, so output-matched
   ;; belongs here and not in test-stat. It's unfortunate we can't
   ;; know what binary caused an output mismatch...perhaps the first
   ;; is the "master" where all client I/O happens?
   (output-matched :initform nil :initarg :output-matched :accessor output-matched)
   (timeout :initform nil :initarg :timeout :accessor timeout)
   (task-clock :initform nil :initarg :task-clock :accessor task-clock)
   (wall-time :initform nil :initarg :wall-time :accessor wall-time)
   ))

;;(defmethod print-object ((obj test-result) stream)
;;  (let ((tc (test-case obj)))
;;    (format stream "#<~A TC: ~A matched: ~A task-clock: ~A stats: ~A>" 
;;	    (type-of obj) (if tc (name tc) nil) (output-matched obj) (task-clock obj) (stats obj))))

;(defmethod test-case ((test-result test-result))
;  (find-test-case-by-path (test-case-path test-result)))

;; Timed out tests.
(defclass timeout-test-result (test-result-base)
  ((test-case-paths :initarg :test-case-paths :accessor test-case-paths :initform nil)
   (timeout :initarg :timeout :accessor timeout :initform nil)))

;;; Bad XML.
(defclass poverror-test-result (test-result-base)
  ((test-case-path :initarg :test-case-path :accessor test-case-path :initform nil)))

;(defmethod test-case ((test-result poverror-test-result))
;  (find-test-case-by-path (test-case-path test-result)))

;;; Bad binary.
(defclass cbverify-fail-result (test-result-base)
  ())

 ;;; Perhaps these should have another home?
; (defmethod bins ((test-result test-result))
;   "Return a list of bins from TEST-RESULT."
;   (mapcar #'car (bin-stats test-result)))

; (defmethod test-stat ((test-result test-result))
;   "Return a lit of test-stat from TEST-RESULT."
;   (mapcar #'cdr (bin-stats test-result)))

;;; ------------------------------------------------------------
;;; Test Cases

;; Maybe add the source (e.g., FuzzBALL) of the TC.
;; Maybe add a verification-use param for efficient testing/sampling.
;; Add coverage data.
(defclass test-case (named-object)
  ((svc :initform nil :initarg :svc :accessor svc)
   ;; [mboldt:20150407] Keeping replay in memory is not sustainable with brute fuzz.
   ;; (replay :initform nil :initarg :replay :accessor replay)
   (info 
    :initform nil :initarg :info :accessor info)
   (file :initform nil :initarg :file :accessor file)
   (has-prolog :initform nil :initarg :has-prolog :accessor has-prolog)
   (provenance :initform nil :initarg :provenance :accessor provenance)	;; where did this case come from?  :fuzzball-symbolic/concolic, :minification, :maxification, :brute, :prior-povs, :repair, :reload
   (expected-pov :initform nil :initarg :expected-pov :accessor expected-pov)	;; did the originator think this was going to be a pov?
   (parent :initform nil :initarg :parent :accessor parent)	;; what was the test-case from which this one was derived, if any
   (children :initform nil :initarg :children :accessor children)	;; list of (immediate) child test-cases derived from this one
   (faults-on-original :initform nil :initarg :faults-on-original :accessor faults-on-original) 
   ;; List of revisions this test case causes to fault.
   (faulting-revs :initform nil :initarg :faulting-revs :accessor faulting-revs)
  )
  (:documentation "Records the information relevant to an individual test case."))

(defmethod initialize-instance :after ((o test-case) &key)
  (when (or (eq :brute (provenance o))
            (eq :pcap (provenance o))
            (eq :reload (provenance o))
            (eq :prior-pov (provenance o)))
    (setf (has-prolog o) t)))

(defun test-case-size (tc)
  (file-length (file tc)))

;;; ------------------------------------------------------------
;;; RPC related classes
;; examples:
;; {"method": "subtract", "params": [42, 23]}, "id": 2}
;; {"result": -19, "id": 2}
;; {"error": {"code": -32601, "message": "Method not found"}, "id": "1"}
;; {"error": {"code": -32602, "message": "Invalid params"}, "id": "1"}
;; {"error": {"code": -32603, "message": "Internal error"}, "id": "1"}
;; {"error":{"code":-32600,"message":"The JSON sent is not a valid Request object."},"id":null}}
;; {"error":{"code":-32700,"message":"Invalid JSON was received by the server."},"id":null}}

(defclass rpc-method ()
  ((method-lisp :initform nil :initarg :method-lisp :accessor method-lisp)
   (method-name :initform nil :initarg :method-name :accessor method-name)
   (params :initform nil :initarg :params :accessor params)
   (id :initform nil :initarg :id :accessor id))
  (:documentation "class encoding the structure of a remote procedure call."))

(defmethod print-object ((obj rpc-method) stream)
  (format stream "#<~A id: ~A~%(~A ~{~S~^ ~})~%docstring: ~A>"
          (type-of obj)
          (id obj)
          (method-name obj)
          (params obj)
          (documentation (method-lisp obj) 'function)))

(defclass rpc-result ()
  ((result :initform nil :initarg :result :accessor result)
   (id :initform nil :initarg :id :accessor id))
  (:documentation "class encoding the structure of a remote procedure call result."))

(defmethod print-object ((obj rpc-result) stream)
  (format stream "#<~A id: ~A result: ~A>"
          (type-of obj)
          (id obj)
          (result obj)))

(defclass rpc-error ()
  ((error-code :initform nil :initarg :error-code :accessor error-code)
   (error-msg :initform nil :initarg :error-msg :accessor error-msg)
   (id :initform nil :initarg :id :accessor id))
  (:documentation "class encoding the structure of a remote procedure call error."))

(defmethod print-object ((obj rpc-error) stream)
  (format stream "#<~A id: ~A error-code: ~A error-msg: ~A>"
          (type-of obj)
          (id obj)
          (error-code obj)
          (error-msg obj)))

