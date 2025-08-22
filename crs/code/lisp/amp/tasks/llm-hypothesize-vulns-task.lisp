;;; -------------------------------------------------------------------------
;;; COPYRIGHT START
;;; COPYRIGHT END
;;; -------------------------------------------------------------------------

(cl:in-package :fuzzbomb)

;; As of now this does nothing different from its parent.

(defclass llm-hypothesize-vulns-task (llm-characterize-vulns-task)
  ((sanitizers :initarg :sanitizers :initform nil :accessor sanitizers
	       :documentation "A list of available sanitizers.")
   ))

(defmethod task-applies-to-target-p ((task-class-name (eql 'llm-hypothesize-vulns-task)) target-node)
  (let ((target (target target-node)))
    (when (and (next-task target)
               (let ((task-type (getassoc :task-type (next-task target))))
		 (eq task-type task-class-name)))
      (if *use-llm-hypothesize-vulns-task* t
	  (progn (dbug :top "Discarding next-task ~a: *use-llm-hypothesize-vulns-task* is nil!" task-class-name)
		 nil)))))

(defmethod set-slots-from-target :after ((task llm-hypothesize-vulns-task))
  (dbug :top "set-slots-from-target :after ((task llm-hypothesize-vulns-task): ~s" task)
  (when (next-task (target task))
    (setf (sanitizers task) (getassoc :sanitizers (next-task (target task))))
    (dbug :top "sanitizers is ~s" (sanitizers task))))

(defmethod initialize-instance :after ((task llm-hypothesize-vulns-task) &key)
  (dbug :top "initialize-instance :after ((task llm-hypothesize-vulns-task): ~s" task)
  (dbug :top "llm-hypothesize-vulns-task finished initialization"))

(defmethod pre-exec ((task llm-hypothesize-vulns-task))
  (send-telemetry-event :program-analysis :llm_hypothesize_vulns (metadata (target task))))

(defmethod llm-cmd ((task llm-hypothesize-vulns-task))
  (append (llm-characterize-base-cmd task)
	  (llm-cmd-hyp-args task)))

(defmethod llm-cmd-hyp-args ((task llm-hypothesize-vulns-task))
  (dbug :top "llm-cmd-hyp-args for type llm-hypothesize-vulns-task")
  (remove-if #'null ; To clean up the optional args below
	     (list (and (sanitizers task) "-saninames") (format-arg-list-for-cmd (sanitizers task))
		   )))

(defmethod failed-msg ((task llm-hypothesize-vulns-task))
  (response-msg task :hypothesize-vulns-failed))

(defmethod succeeded-msg ((task llm-hypothesize-vulns-task))
  (response-msg task :hypothesize-vulns-succeeded))

(defmethod incremental-success-msg ((task llm-hypothesize-vulns-task) vulns-file)
  (response-msg task :hypothesize-vulns-succeeded :result-file vulns-file))

(defmethod scoring-fn ((task llm-hypothesize-vulns-task)) 550)
