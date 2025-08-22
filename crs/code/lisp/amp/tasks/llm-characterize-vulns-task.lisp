;;; -------------------------------------------------------------------------
;;; COPYRIGHT START
;;; COPYRIGHT END
;;; -------------------------------------------------------------------------

(cl:in-package :fuzzbomb)

(defparameter *char-default-pipeline* "characterize_vulns" "The default pipeline to use for vuln characterization.")
(defparameter *char-python-script* "code/tools/characterize-vulns.py" "The python script that this task calls")

(defclass llm-characterize-vulns-task (llm-task)
  ((pipeline :initform *char-default-pipeline*)
   (known-vulns-json :initarg :known-vulns-json :initform nil :accessor known-vulns-json
		     :documentation "The path to the json storing known vulns before undertaking this task.")
   (known-inferior-vulns-json :initarg :known-inferior-vulns-json :initform nil :accessor known-inferior-vulns-json
			      :documentation "The path to the json storing known inferior vulns before undertaking this task.")
   (src-vuln-hyps :initarg :src-vun-hyps :initform nil :accessor src-vuln-hyps
		  :documentation "The source-based vulnerability hypotheses produced by this task.")
   (has-inferior-dupes-p :initarg :has-inferior-dupes-p :initform nil :accessor has-inferior-dupes-p
			 :documentation "A flag saying whether inferior duplicates were found")
   ))

(defmethod set-slots-from-target :after ((task llm-characterize-vulns-task))
  (dbug :top "set-slots-from-target :after ((task llm-characterize-vulns-task): ~s" task)
  (when (next-task (target task))
    ;; set local slots from next-task elements
    (setf (known-vulns-json task) (getassoc :known-vulns-json (next-task (target task))))
    (setf (known-inferior-vulns-json task) (getassoc :known-inferior-vulns-json (next-task (target task))))
    (dbug :top "known-vulns-json is ~s" (known-vulns-json task))
    (dbug :top "known-inferior-vulns-json is ~s" (known-inferior-vulns-json task))))

(defmethod hyps-json-path ((task llm-characterize-vulns-task))
  (strcat (scratch-path task) "/vuln-hyps-output.json"))

(defmethod inf-dupes-json-path ((task llm-characterize-vulns-task))
  (strcat (scratch-path task) "/inferior-dupes-output.json"))

(defmethod llm-characterize-base-cmd ((task llm-characterize-vulns-task))
  (remove-if #'null ; To clean up the optional args below
	     (list (poetry-preamble task)
		   (strcat *lax-home* *char-python-script*)
		   (focus-path (target task))
		   (hyps-json-path task)
		   "-pipeline" (pipeline task)
		   "-scratch" (scratch-path task)
		   "-llms" (format-arg-list-for-cmd (llm-args task))
		   (and (delta-path (target task)) "-delta") (delta-path (target task))
		   (and (known-vulns-json task) "-known") (known-vulns-json task)
		   (and (known-inferior-vulns-json task) "-knowninf") (known-inferior-vulns-json task)
		   (and (inf-dupes-json-path task) "-infpath") (inf-dupes-json-path task)
		   )))

(defmethod llm-cmd ((task llm-characterize-vulns-task))
  (llm-characterize-base-cmd task))

(defmethod process-line ((task llm-characterize-vulns-task) line)
  (dbug :top "got line [~a]" line)
  (cond ((search "Wrote final inferior dupes to" line)
	 (dbug :top "Found inferior duplicate vuln descriptions for task.")
	 (setf (has-inferior-dupes-p task) t)
	 (if (string-equal line (format nil "Wrote final inferior dupes to ~a" (inf-dupes-json-path task)))
	     (dbug :top "Match!")
	     (dbug :top "No match ~s ~s" (format nil "Wrote final inferior dupes to ~a" (inf-dupes-json-path task)) line))
	 (cond ((file-exists-p (pathname (inf-dupes-json-path task)))
		(dbug :top "File exists"))
	       (t (dbug :top "Inferior dupes file supposedly written but does not exist at path ~a with pathstring ~s" (pathname (inf-dupes-json-path task)) (inf-dupes-json-path task)))))
	((search "Wrote final vulns to" line)
	 (dbug :top "Successfully generated hyp-vulns for task!")
	 (setf (generated-p task) t)
	 (if (string-equal line (format nil "Wrote final vulns to ~a" (hyps-json-path task)))
	     (dbug :top "Match!")
	     (dbug :top "No match ~s ~s" (format nil "Wrote final vulns to ~a" (hyps-json-path task)) line))
	 (cond ((file-exists-p (pathname (hyps-json-path task)))
		(dbug :top "File exists"))
	       (t (dbug :top "Hypotheses file supposedly written but does not exist at path ~a with pathstring ~s" (pathname (hyps-json-path task)) (hyps-json-path task)))))
	((search "Incremental vulns stored at" line)
	 (let* ((regex "Incremental vulns stored at: (/.+)")
		(inc-path (cl-ppcre:register-groups-bind (ipath) (regex line) ipath)))
	   (dbug :top "llm-patch-task detected incremental vulns filepath: ~s" inc-path)
	   (send-incremental-success-msg task inc-path)))
	))

(defmethod send-incremental-success-msg ((task llm-characterize-vulns-task) vulns-file)
  (dbug :top "send-incremental-success-msg ~a" vulns-file)
  (let ((inc-success-msg (incremental-success-msg task vulns-file)))
    (cond (inc-success-msg
	   (apply #'send-message-to-optimi inc-success-msg))
	  (t
	   (dbug :top "send-incremental-success-msg sends nothing (no-op)")))))

;; default method is a no-op.
(defmethod incremental-success-msg ((task llm-characterize-vulns-task) vulns-file)
  (declare (ignore vulns-file))
  nil)

(defmethod post-exec ((task llm-characterize-vulns-task))
  (dbug :top "post-exec for llm-characterize-vulns-task")
  ;; This might be unnecessary now bc we just send back the json
  (when (probe-file (hyps-json-path task))
    (let ((svhs (src-vuln-hyps-from-json-file (hyps-json-path task))))
      (setf (src-vuln-hyps task) svhs)))
  (call-next-method))

(defmethod response-msg ((task llm-characterize-vulns-task) head &key result-file)
  (dbug :top "response-msg for ~s ~a, result-file override is ~s" task head result-file)
  `(:type ,head
	  :task-id ,(task-id (target task))
	  ,@(when (vc-id task) (list :vc-id (vc-id task)))
	  ;; :src-vuln-hyps ,(mapcar #'spec (src-vuln-hyps task))
	  :src-vuln-hyps-json ,(or result-file (hyps-json-path task))
	  ,@(when (has-inferior-dupes-p task) (list :inf-dupes-json (inf-dupes-json-path task)))
	  ))
