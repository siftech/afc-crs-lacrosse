;;; -------------------------------------------------------------------------
;;; COPYRIGHT START
;;; COPYRIGHT END
;;; -------------------------------------------------------------------------

(cl:in-package :fuzzbomb)

;; Patching with a PoV

(defclass llm-patch-with-pov-task (llm-patch-task llm-pov-task)
  ())

(defmethod task-applies-to-target-p ((task-class-name (eql 'llm-patch-with-pov-task)) target-node)
  (let ((target (target target-node)))
    (when (and (next-task target)
               (let ((task-type (getassoc :task-type (next-task target))))
		 (eq task-type task-class-name)))
      (if *use-llm-patch-with-pov-task* t
	  (progn (dbug :top "Discarding next-task ~a: *use-llm-patch-with-pov-task* is nil!" task-class-name)
		 nil)))))

(defmethod llm-cmd ((task llm-patch-with-pov-task))
  (dbug :top "llm-cmd for type llm-patch-with-pov-task")
  (append (llm-patch-base-cmd task)
	  (llm-cmd-pov-args task)))

(defmethod pre-exec ((task llm-patch-with-pov-task))
  (send-telemetry-event :patch-generation :llm_patch_with_pov (metadata (target task))))

(defmethod failed-msg ((task llm-patch-with-pov-task))
  (response-msg task :patch-with-pov-failed))

(defmethod succeeded-msg ((task llm-patch-with-pov-task))
  (response-msg task :patch-with-pov-succeeded))
	
(defmethod scoring-fn ((task llm-patch-with-pov-task)) 700)

