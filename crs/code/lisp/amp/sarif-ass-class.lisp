;;; -------------------------------------------------------------------------
;;; Sarif assessments... what joy; hacked from a copy of patch-cand-class
;;; -------------------------------------------------------------------------
;;; COPYRIGHT START
;;; COPYRIGHT END
;;; -------------------------------------------------------------------------

(cl:in-package :fuzzbomb)

(defclass sarif-ass (named-object)
  (
   (id :initarg :id :initform -1 :accessor id
       :documentation "Unique lax id."
       :type int)

   (sarif-id :initarg :sarif-id :initform -1 :accessor sarif-id
       :documentation "sarif id from inbound msg from competition.")

   (task-id :initarg :task-id :initform -1 :accessor task-id
       :documentation "task id from inbound msg from competition.")

   (vuln-cand :initarg :vuln-cand :initform nil :accessor vuln-cand
              :documentation "Ptr to parent vuln-cand."
              :type (or null vuln-cand))
   (target :initarg :target :initform nil :accessor target
           :documentation "Ptr to the target and all of its state."
           :type (or null lacrosse-cp-target))

   (decision :initform nil :accessor decision
                :documentation "once decided, a string, either correct or incorrect.")

   (description :initform "Just a Wild-n-Crazy Guess" :accessor description
                :documentation "a string describing why Lax things what it decided about this SARIF.")
   
   (submitted-p :initform nil :accessor submitted-p
                :documentation "Set to t after submission.")
   (status :initform nil :accessor status
           :documentation "Raw status returned by aixcc infra OR :deadline-exceeded.")
   (sarif-sexp :initarg :sarif-sexp :initform nil :accessor sarif-sexp
           :documentation "An s-expression representing the sarif json.")
   (failures :initarg :failures :initform nil :accessor failures
             :documentation "Records of failures on sarif assessment.")
   ))

(defmethod initialize-instance :after ((sarif-ass sarif-ass) &key)
  ;; Unless id is supplied to make-instance, use named-object name.
  ;; It's intended that only optimus assign the id.
  (when (and (numberp (id sarif-ass))
             (= -1 (id sarif-ass)))
    (setf (id sarif-ass) (name sarif-ass)))
  (add-object-for-lax-id sarif-ass (id sarif-ass))
  (dbug :top "Initialized sarif-ass ~a" sarif-ass)
)

(defmethod lp-string ((sa sarif-ass))
  (format nil "<SARIF-ASS: id: ~a task-id: ~a :decision ~a>" (id sa) (task-id sa) (decision sa)))

(defmethod ready-to-submit-p ((sarif-ass sarif-ass))
  "Does this sarif-ass meet the min reqts for submission?"
  (dbug :top "testing sarif ready-to-submit-p: ~s" sarif-ass)
  (describe sarif-ass)
  (cond ((deadline-exceeded-p (target sarif-ass))
         (dbug :top  "Deadline exceeded in ready-to-submit-p (sarif-ass), dropping ~s ~a" (target sarif-ass) (short-description (target sarif-ass)))
         (setf (status sarif-ass) :deadline-exceeded)
         nil)
        ((and
          (not (submitted-p sarif-ass))
	  (decision sarif-ass)	;; must be non-nil
          ;; FIXME: We should probably not submit sarif-asss that do not have a PoV (vuln-cand) until
          ;; we are desperate, i.e. haven't found any PoVs or patches for PoVs and ran out of time or leads.
          )
         t)
        (t nil)))

  
;(defmethod same-sarif-ass-p ((pc-1 sarif-ass) (pc-2 sarif-ass))
;  (same-file-contents-p (patch-file pc-1) (patch-file pc-2)))

;;; FIXME this will only do one SA per CT, which is certainly not The Right Thing
(defun consider-submit-sarif-ass (ctask)
  (dbug :top "consider-submit-sarif-ass")
  ;;(describe ctask)
  ;;(describe (first (vuln-cands ctask)))
  (let ((sa (find-if #'ready-to-submit-p (all-sarif-asss ctask))))
    (dbug :top "sa to submit: ~s" sa)
    (when sa
;      (when (vuln-cand sa-to-submit)
;	(setf (submitted-sa (vuln-cand sa)) sa))
      (let ((cmd-str (format nil "~a/afc-submit.py ~a --lax_id ~a --task_id ~a --broadcast_sarif_id ~a --sarif_assessment ~a --sarif_description ~s"
			     *lax-tools*
			     "broadcast"
			     (id sa)
                             (task-id sa)
                             (sarif-id sa)
			     (decision sa)
			     (description sa)
                             )))
        (dbug :top "Submitting sa to AFC: ~s" sa)
        (dbug :top "  cmd: ~s" cmd-str)
	(setf (submitted-p sa) t)
        (uiop:launch-program cmd-str :output *standard-output*
                                     :error-output :output
                                     :ignore-error-status t)
        ))))

(defun sarif-ass-dir (target)
  (strcat (output-dir target) "sarif-assessments"))

(defun make-sarif-json-file (sarif task-id sarif-id target)
  (dbug :top "make-sarif-json-file for task ~a sarif ~a" task-id sarif-id)
  (strcat (sarif-ass-dir target) "/sarif-" task-id "-" sarif-id ".json"))

(defun find-sarif-ass-by-lax-id (lax-sarif-id target)
  (find lax-sarif-id (all-sarif-asss target) :key #'id :test #'equal))
