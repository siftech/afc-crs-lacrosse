;;; -------------------------------------------------------------------------
;;; COPYRIGHT START
;;; COPYRIGHT END
;;; -------------------------------------------------------------------------

(cl:in-package :fuzzbomb)

(defparameter *patch-default-pipeline* "patch_vulns_w_feedback" "The default pipeline to use for patch gen.")
(defparameter *patch-python-script* "code/tools/patch-vulns.py" "The python script that this task calls")

;; An abstract parent task for the PoV and non-PoV LLM-patching tasks.

(defclass llm-patch-task (llm-task)
  ;; Possibly should be on the parent class instead.
  ((pipeline :initform *patch-default-pipeline*)
   (svh-ids :initarg :svh-ids :initform nil :accessor svh-ids
	    :documentation "The IDs of the source-based vulnerability hypotheses to be patched.")
   (src-vuln-hyps-json :initarg :src-vuln-hyps-json :initform nil :accessor src-vuln-hyps-json
		       :documentation "The json of the source-based vulnerability hypotheses to be patched.")
   (patch-path :initarg :patch-path :initform nil :accessor patch-path
	       :documentation "The path where the generated patch will be stored.")
   (patch-applied-p :initarg :patch-applied-p :initform nil :accessor patch-applied-p
		    :documentation "Bool saying whether the patch applied successfully.")
   (patch-built-p :initarg :patch-built-p :initform nil :accessor patch-built-p
		  :documentation "Bool saying whether the patched code built successfully.")
   (fixed-pov-p :initarg :fixed-pov-p :initform nil :accessor fixed-pov-p
		:documentation "Bool saying whether the patch fixed the pov of the vc.")
   (passed-func-p :initarg :passed-func-p :initform nil :accessor passed-func-p
		  :documentation "Bool saying whether the patched code passed the functionality tests.")
   (passed-available-p :initarg :passed-available-p :initform nil :accessor passed-available-p
		  :documentation "Bool saying whether the patched code passed all available tests.")
  ))

(defmethod set-slots-from-target :after ((task llm-patch-task))
  (dbug :top "set-slots-from-target :after ((task llm-patch-task): ~s" task)
  (setf (patch-path task) (make-patch-path task))
  (when (next-task (target task))
    ;; set local slots from next-task elements
    (let ((svh-ids (getassoc :svh-ids (next-task (target task))))
	  (svhs-json (getassoc :src-vuln-hyps-json (next-task (target task)))))
      (dbug :top "svh-ids is ~s" svh-ids)
      (setf (svh-ids task) svh-ids)
      (dbug :top "svhs-json is ~s" svhs-json)
      (setf (src-vuln-hyps-json task) svhs-json)
      (let ((hyps-json-contents (uiop:read-file-string (hyps-json-path task))))
	(dbug :top "hyps-json contents: ~s" hyps-json-contents)))))

(defmethod make-patch-path ((task llm-patch-task))
  (let ((path-str (strcat (task-output-dir task) "/patch.diff")))
    (uiop:ensure-pathname path-str :ensure-directories-exist t)
    path-str))

(defmethod llm-patch-base-cmd ((task llm-patch-task))
  (remove-if #'null ; To clean up the optional args below
	     (list (poetry-preamble task)
		   (strcat *lax-home* *patch-python-script*)
		   (hyps-json-path task)
		   (focus-path (target task))
		   (patch-path task)
		   "-pipeline" (pipeline task)
		   "-scratch" (scratch-path task)
		   "-llms" (format-arg-list-for-cmd (llm-args task))
		   (and (delta-path (target task)) "-delta") (delta-path (target task)))))

(defmethod llm-cmd ((task llm-patch-task))
  (llm-patch-base-cmd task))

(defmethod hyps-json-path ((task llm-patch-task))
  (src-vuln-hyps-json task) ; Now just a pass-through
  )

(defmethod process-line ((task llm-patch-task) line)
  (dbug :top "got line [~a]" line)
  (cond ((search "Wrote final patch to" line)
	 (let ((target-line (format nil "Wrote final patch to ~a" (patch-path task))))
	   (dbug :top "target line is ~s" target-line)
	   (dbug :top "line is ~s" line)
	   ;; Ugliness here with duplicated (sub)strings, but if it ain't broke...
	   (when (string-equal line (format nil "Wrote final patch to ~a" (patch-path task)))
	     (cond ((file-exists-p (pathname (patch-path task)))
		    (dbug :top "Successfully generated a patch for task!")
		    (setf (generated-p task) t))
		   (t (dbug :top "Patch file supposedly written but does not exist at path ~a with pathstring ~s" (pathname (patch-path task)) (patch-path task)))))))
	((string-equal line "Final patch passed APPLY.")
	 (setf (patch-applied-p task) t)
	 (dbug :top "Generated patch passed APPLY."))
	((string-equal line "Final patch passed BUILD.")
	 (setf (patch-built-p task) t)
	 (dbug :top "Generated patch passed BUILD."))
	((string-equal line "Final patch passed POV.")
	 (setf (fixed-pov-p task) t)
	 (dbug :top "Generated patch passed POV."))
	((string-equal line "Final patch passed FUNCTIONALITY.")
	 (setf (passed-func-p task) t)
	 (dbug :top "Generated patch passed FUNCTIONALITY."))
	((string-equal line "Final patch passed all available tests.")
	 (setf (passed-available-p task) t)
	 (dbug :top "Generated patch passed all available tests."))
	))

(defmethod response-msg ((task llm-patch-task) head &key result-file)
  `(:type ,head
	  :patch-file ,(or result-file (patch-path task))
	  ,@(when (vc-id task) (list :vc-id (vc-id task)))
	  :task-id ,(task-id (target task))
	  :svh-ids ,(svh-ids task)
	  :src-vuln-hyps-json ,(src-vuln-hyps-json task)
	  :applied ,(patch-applied-p task)
	  :built ,(patch-built-p task)
	  :fixed-pov ,(fixed-pov-p task)
	  :passed-functionality ,(passed-func-p task)
	  :passed-available ,(passed-available-p task)
	  ))
