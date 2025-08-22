;;; -------------------------------------------------------------------------
;;; bundles... what a bundle of joy; hacked from a copy of sarif-ass-class
;;; -------------------------------------------------------------------------
;;; COPYRIGHT START
;;; COPYRIGHT END
;;; -------------------------------------------------------------------------

(cl:in-package :fuzzbomb)

(defclass bundle (named-object)
  (
   (id :initarg :id :initform -1 :accessor id
       :documentation "Unique lax id."
       :type int)

   (task-id :initarg :task-id :initform -1 :accessor task-id
       :documentation "task id from inbound msg from competition.")

   (pov-id :initarg :pov-id :initform nil :accessor pov-id )
   (patch-id :initarg :patch-id :initform nil :accessor patch-id )
   (sarif-id :initarg :sarif-id :initform nil :accessor sarif-id )
   (target :initarg :target :initform nil :accessor target
           :documentation "Ptr to the target and all of its state."
           :type (or null lacrosse-cp-target))

;;;   (decision :initform nil :accessor decision
;;;                :documentation "once decided, a bool-- nil or non-nil.")

   (description :initform "Multiple associated objects" :accessor description
		:initarg :description
                :documentation "Natural language description of bundle contents.")
   
   (submitted-p :initform nil :accessor submitted-p
                :documentation "Set to t after submission.")
   (status :initform nil :accessor status
           :documentation "Raw status returned by aixcc infra OR :deadline-exceeded.")

   ))


(defmethod initialize-instance :after ((bundle bundle) &key)
  ;; Unless id is supplied to make-instance, use named-object name.
  ;; It's intended that only optimus assign the id.
  (when (and (numberp (id bundle))
             (= -1 (id bundle)))
    (setf (id bundle) (name bundle)))
  (add-object-for-lax-id bundle (id bundle))
  (dbug :top "Initialized bundle ~a" bundle)
)

(defmethod lp-string ((sa bundle))
  (format nil "<BUNDLE: id: ~a task-id: ~a :decision ~a>" (id sa) (task-id sa) (decision sa)))

(defmethod ready-to-submit-p ((bundle bundle))
  "Does this bundle meet the min reqts for submission?"
  (dbug :top "testing bundle ready-to-submit-p: ~s" bundle)
  (describe bundle)
  (cond ((deadline-exceeded-p (target bundle))
         (dbug :top  "Deadline exceeded in ready-to-submit-p (bundle), dropping ~s ~a" (target bundle) (short-description (target bundle)))
         (setf (status bundle) :deadline-exceeded)
         (setf (all-bundles (target bundle)) (delete bundle (all-bundles (target bundle))))
         nil)
        ((and
          (not (submitted-p bundle))
	  ;; MUSTFIX something to say we won't submit too many bundles...esp not for same patch/pov
	  ;;;(decision bundle)	;; must be non-nil
          ;; FIXME: We should probably not submit bundles that do not have a PoV (vuln-cand) until
          ;; we are desperate, i.e. haven't found any PoVs or patches for PoVs and ran out of time or leads.
          )
         t)
        (t nil)))

;(defmethod same-bundle-p ((pc-1 bundle) (pc-2 bundle))
;  (same-file-contents-p (patch-file pc-1) (patch-file pc-2)))

;;; FIXEd need something to invent/create bundles to consider!
;;; - this is done when a pov-based patch is determined to be successfully accepted by the comp server
;;; and it sends a msg to OPT - see process-patch-status-msg (msg)

;;; WONTFIX this will only do one bundle per CT, which is certainly not The Right Thing
(defun consider-submit-bundle (ctask)
  (dbug :top "consider-submit-bundle")
  ;;(describe ctask)
  ;;(describe (first (vuln-cands ctask)))
  (let ((bun (find-if #'ready-to-submit-p (all-bundles ctask))))
    (dbug :top "bundle to submit: ~a" bun)
    (when bun
      (let ((cmd-str (format nil "~a/afc-submit.py ~a --lax_id ~a ~:[~; --task_id ~a~] ~:[~; --pov_id ~a~] ~:[~; --patch_id ~a~] ~:[~; --broadcast_sarif_id ~a~] ~:[~; --description ~a~]"

			     *lax-tools*
			     "bundle"
			     (id bun)
                             (task-id bun) (task-id bun)                              
			     (pov-id bun) (pov-id bun)
                             (patch-id bun) (patch-id bun)
                             (sarif-id bun) (sarif-id bun)
                             (description bun) (description bun)
                             )))

        (dbug :top "Submitting bundle to AFC: ~s" bun)
        (dbug :top "  cmd: ~s" cmd-str)
	(setf (submitted-p bun) T)
	;; FIXME this should give us a bundle-id to consider for future revision or retraction,
	;; but we'll never get there...alas
        (uiop:launch-program cmd-str :output *standard-output*
                                     :error-output :output
                                     :ignore-error-status t)
        ))))
