;;; -------------------------------------------------------------------------
;;; COPYRIGHT START
;;; COPYRIGHT END
;;; -------------------------------------------------------------------------

(cl:in-package :fuzzbomb)

;; The idea behind this task is to explain a PoV by generating vulnerability
;; hypotheses for it.

(defclass llm-explain-pov-task (llm-characterize-vulns-task llm-pov-task)
  ())

(defmethod task-applies-to-target-p ((task-class-name (eql 'llm-explain-pov-task)) target-node)
  (let ((target (target target-node)))
    (when (and (next-task target)
               (let ((task-type (getassoc :task-type (next-task target))))
		 (eq task-type task-class-name)))
      (if *use-llm-explain-pov-task* t
	  (progn (dbug :top "Discarding next-task ~a: *use-llm-explain-pov-task* is nil!" task-class-name)
		 nil)))))

(defmethod llm-cmd ((task llm-explain-pov-task))
  (append (llm-characterize-base-cmd task)
	  (llm-cmd-pov-args task)))

(defmethod pre-exec ((task llm-explain-pov-task))
  (send-telemetry-event :program-analysis :llm_explain_pov (metadata (target task))))

(defmethod failed-msg ((task llm-explain-pov-task))
  (response-msg task :explain-pov-failed))

(defmethod succeeded-msg ((task llm-explain-pov-task))
  (response-msg task :explain-pov-succeeded))

(defmethod scoring-fn ((task llm-explain-pov-task)) 600)
