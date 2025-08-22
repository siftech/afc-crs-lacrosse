;;; -------------------------------------------------------------------------
;;; COPYRIGHT START
;;; COPYRIGHT END
;;; -------------------------------------------------------------------------

(cl:in-package :fuzzbomb)

(defclass llm-task (lacrosse-task)
  ;; Possibly should be on the parent class instead.
  ((pipeline :initarg :pipeline :accessor pipeline
	     :documentation "The pipeline to use for patch gen.")
   (scratch-path :initarg :scratch-path :initform nil :accessor scratch-path
	     :documentation "A directory for storing scratch materials.")
   (llm-args :initarg :llm-args :initform *default-llm-args* :accessor llm-args
	     :documentation "The LLM names to pass to the pipeline.")
   (generated-p :initarg :generated-p :initform nil :accessor generated-p
		:documentation "Bool saying whether the goal was generated successfully.")
   )
  )

(defmethod initialize-instance :after ((task llm-task) &key)
  (dbug :top "initialize-instance :after ((task llm-task): ~s" task)
  (setf (scratch-path task) (make-scratch-path task))
  (when (delta-type-p (target task))
    (let ((delta-applied-p (ensure-delta-applied (target task))))
      (unless delta-applied-p
	(error "Task is delta type but delta could not be applied for task ~s" task))))
  (set-slots-from-target task)
  (set-llm-cmd task)
  (dbug :top "llm-task finished initialization"))

(defmethod set-slots-from-target ((task llm-task))
  (dbug :top "set-slots-from-target llm-task"))

(defmethod set-llm-cmd ((task llm-task))
  (let* ((cmds (list (llm-cmd task)))
	 (cmd-str (format nil "~{~A~^ && ~}"
			  (mapcar #'(lambda (cmd-list)
				      (format nil "~{~A~^ ~}" cmd-list))
				  cmds))))
    (dbug :top "cmd-str is ~s" cmd-str)
    (setf (cmd task) cmd-str)
    (dbug :top "Task cmd: ~a" (cmd task))))

(defmethod task-output-dir ((task llm-task))
  (strcat (output-dir (target task)) (task-temp-dirname task)))

(defmethod task-temp-dirname ((task llm-task))
  (format nil "~a-~a-~a-~a-~a-~a"
          (class-name (class-of task)) (get-universal-time) (name task)
	  (ctask-type (target task))
	  (pipeline task)
	  (format-arg-list-for-path (llm-args task))
	  ))

(defun format-arg-list-for-path (args)
  (format nil "~{~A~^-~}" args))

(defun format-arg-list-for-cmd (args)
  (format nil "~{~A~^ ~}" args))

(defmethod scratch-name ((task llm-task))
  (format nil "~a-scratch" (class-name (class-of task))))

(defmethod make-scratch-path ((task llm-task))
  (strcat (task-output-dir task) "/" (scratch-name task)))

(defmethod poetry-preamble ((task llm-task))
  (let ((dspy-dir (strcat *lax-home* "code/dspy")))
    (format nil "poetry -P ~a run python3.10" dspy-dir)))

(defmethod vc-id ((task llm-task))
  nil)

(defmethod post-exec ((task llm-task))
  (dbug :top "post-exec for llm-task")
  (cond ((generated-p task)
	 (dbug :top "llm generation task succeeded.")
	 (send-llm-task-succeeded-msg task)
	 )
	(t
	 (dbug :top "llm generation task failed.")
	 (send-llm-task-failed-msg task :desc "LLM failed to generate.")
	 )))

(defmethod send-llm-task-failed-msg ((task llm-task) &key desc patch-file)
  (declare (ignore desc patch-file))
  (dbug :top "~a result: FAILED, ~a" (class-name (class-of task)) task)
  (apply #'send-message-to-optimi (failed-msg task)))

(defmethod send-llm-task-succeeded-msg ((task llm-task) &key desc patch-file)
  (declare (ignore desc patch-file))
  (dbug :top "~a result: SUCCEEDED, ~a" (class-name (class-of task)) task)
  (apply #'send-message-to-optimi (succeeded-msg task)))
