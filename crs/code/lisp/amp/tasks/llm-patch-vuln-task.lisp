;;; -------------------------------------------------------------------------
;;; COPYRIGHT START
;;; COPYRIGHT END
;;; -------------------------------------------------------------------------

(cl:in-package :fuzzbomb)

;; This is the task that runs the LLM patcher in the case where we have a PoV

(defclass llm-patch-vuln-task (llm-patch-task)
  (
   (fixed-pov-p :initarg :fixed-pov-p :initform nil :accessor fixed-pov-p
      :documentation "Bool saying whether the patch successfully avoided reproducing the PoV.")
   ))

(defmethod task-applies-to-target-p ((task-class-name (eql 'llm-patch-vuln-task)) target-node)
  (let ((target (target target-node)))
    (next-task target)
    (let ((task-type (getassoc :task-type (next-task target))))
      (eq task-type task-class-name))))

(defmethod initialize-instance :after ((task llm-patch-vuln-task) &key)
  (dbug :top "initialize-instance :after ((task llm-patch-vuln-task): ~s" task)
  (set-prelim-slots task)
  ;; We have to build the helper.py image bc we'll want to test patches against the PoV using it.
  (build (target task))
  (set-patching-cmd task)
  (dbug :top "llm-patch-vuln-task finished initialization"))

;; FIXME: Right now we treat :none like a null value. Not sure this is right. Is :none a usable value?
(defun string-arg (kwd)
  (unless (eq kwd :none)
    (string-downcase (symbol-name kwd))))

(defmethod llm-cmd ((task llm-patch-vuln-task))
  (let ((base-cmd (call-next-method)))
    (append base-cmd
	    (remove-if #'null
		       (list (and (reproduce-path task) "-sani") (reproduce-path task) # FIXME update arg name
			     (and (blob task) "-blob") (blob task)
			     (and (harness task) "-harn") (harness task)
			     (and (string-arg (sanitizer task)) "-saniname") (string-arg (sanitizer task))
			     (and (string-arg (engine task)) "-fuzzeng") (string-arg (engine task))
			     (and (project-name (target task)) "-proj") (project-name (target task))
			     )))))

(defmethod process-line ((task llm-patch-vuln-task) line)
  (call-next-method)
  ;; Might need to become more sophisticated when we have functionality tests.
  (when (search "Patched code did not reproduce fuzzer crash" line)
    (setf (fixed-pov-p task) t))
  )

(defmethod post-exec ((task llm-patch-vuln-task))
  (dbug :top "llm-patch-vuln-task post-exec")
  (cond ((and (generated-p task) (fixed-pov-p task))
	 (dbug :top "llm patch generation task succeeded, including generating a patch and passing the pov.")
	 (send-patch-generation-succeeded-msg task)
	 )
	((generated-p task)
	 (send-patch-generation-failed-msg task :desc "LLM generated a patch but it did not pass pov.")
	 )
	(t
	 (send-patch-generation-failed-msg task :desc "LLM failed to generate a patch.")
	 ))
  ;;(call-next-method)
  )

(defmethod failed-msg ((task llm-patch-task))
  (let ((patch-path (or (and patch-file (namestring patch-file))
			(patch-path task))))
    `(:type :patch-generation-failed
	    :patch-file ,patch-path
	    ,@(when (vc-id task) (list :vc-id (vc-id task)))
	    :task-id ,(task-id (target task))))))

(defmethod succeeded-msg ((task llm-patch-task))
  (let ((patch-path (or (and patch-file (namestring patch-file))
			(patch-path task))))
    `(:type :patch-generation-succeeded
	    :patch-file ,patch-path
	    ,@(when (vc-id task) (list :vc-id (vc-id task)))
	    :task-id ,(task-id (target task))))))

