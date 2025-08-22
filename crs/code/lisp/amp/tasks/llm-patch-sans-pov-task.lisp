;;; -------------------------------------------------------------------------
;;; COPYRIGHT START
;;; COPYRIGHT END
;;; -------------------------------------------------------------------------

(cl:in-package :fuzzbomb)

;; Patching without a PoV

(defclass llm-patch-sans-pov-task (llm-patch-task)
  ())

(defmethod task-applies-to-target-p ((task-class-name (eql 'llm-patch-sans-pov-task)) target-node)
  (let ((target (target target-node)))
    (when (and (next-task target)
               (let ((task-type (getassoc :task-type (next-task target))))
		 (eq task-type task-class-name)))
      (if *use-llm-patch-sans-pov-task* t
	  (progn (dbug :top "Discarding next-task ~a: *use-llm-patch-sans-pov-task* is nil!" task-class-name)
		 nil)))))

(defmethod pre-exec ((task llm-patch-sans-pov-task))
  (send-telemetry-event :patch-generation :llm_patch_sans_pov (metadata (target task))))

(defmethod failed-msg ((task llm-patch-sans-pov-task))
  (response-msg task :patch-sans-pov-failed))

(defmethod succeeded-msg ((task llm-patch-sans-pov-task))
  (response-msg task :patch-sans-pov-succeeded))

(defmethod scoring-fn ((task llm-patch-sans-pov-task)) 498)
