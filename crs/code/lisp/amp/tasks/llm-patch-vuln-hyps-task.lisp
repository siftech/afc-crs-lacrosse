;;; -------------------------------------------------------------------------
;;; COPYRIGHT START
;;; COPYRIGHT END
;;; -------------------------------------------------------------------------

(cl:in-package :fuzzbomb)

(defparameter *default-pipeline* "discover_then_patch_vulns" "The default pipeline to use for patch gen.")
(defparameter *default-llm-args* '("sonnet37" "gpt4o") "The default llm args to pass to the pipeline.")

;;;   refactoring this class hierarchy to more
;;; clearly divide semi and final tasks is left for future work.  (TODO)
(defclass llm-find-and-patch-vuln-task (llm-patch-task)
  ())

(defmethod task-applies-to-target-p ((task-class-name (eql 'llm-find-and-patch-vuln-task)) target-node)
  (let ((target (target target-node)))
    (and *use-llm-find-and-patch-vuln*
         (next-task target)
         (let ((task-type (getassoc :task-type (next-task target))))
           (eq task-type task-class-name)))))

(defmethod initialize-instance :after ((task llm-find-and-patch-vuln-task) &key)
  (dbug :top "initialize-instance :after ((task llm-find-and-patch-vuln-task): ~s" task)
  (set-prelim-slots task)
  (set-patching-cmd task)
  (dbug :top "llm-find-and-patch-vuln-task finished initialization"))

(defmethod post-exec ((task llm-find-and-patch-vuln-task))
  (dbug :top "llm-find-and-patch-vuln-task post-exec")
  (cond ((generated-p task)
	 (dbug :top "llm patch generation task succeeded.")
	 (send-patch-generation-succeeded-msg task)
	 )
	(t
	 (send-patch-generation-failed-msg task :desc "LLM failed to generate a patch.")
	 ))
  ;;(call-next-method)
  )

