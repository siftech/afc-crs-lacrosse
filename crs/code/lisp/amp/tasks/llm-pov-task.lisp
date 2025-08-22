;;; -------------------------------------------------------------------------
;;; COPYRIGHT START
;;; COPYRIGHT END
;;; -------------------------------------------------------------------------

(cl:in-package :fuzzbomb)

(defclass llm-pov-task (llm-task)
  ;; Possibly should be on the parent class instead.
  ((vuln-cand :initarg :vuln-cand :initform nil :accessor vuln-cand
	      :documentation "The vulnerability candidate worked on by the LLM PoV task.")
   (llm-args :initform *pov-llm-args*)
   ))

(defmethod set-slots-from-target :after ((task llm-pov-task))
  (dbug :top "set-slots-from-target :after ((task llm-pov-task): ~s" task)
  (when (next-task (target task))
    ;; set local slots from next-task elements
    (let ((vc-spec (getassoc :vuln-cand (next-task (target task)))))
      (setf (vuln-cand task) (new-vuln-cand-from-spec vc-spec (target task))))))

(defmethod reproduce-path ((task llm-pov-task))
  (let ((vc (vuln-cand task)))
    (when vc
      (reproduce-path vc))))

(defmethod sanitizer ((task llm-pov-task))
  (let ((vc (vuln-cand task)))
    (when (and vc (sanitizer vc))
      (string-downcase (symbol-name (sanitizer vc))))))

(defmethod vc-id ((task llm-pov-task))
  (when (vuln-cand task)
    (id (vuln-cand task))))

(defmethod engine ((task llm-pov-task))
  (let ((vc (vuln-cand task)))
    (when (and vc (engine vc))
      (string-downcase (symbol-name (engine vc))))))

(defmethod harness ((task llm-pov-task))
  (let ((vc (vuln-cand task)))
    (when vc
      (harness vc))))

(defmethod blob ((task llm-pov-task))
  (let ((vc (vuln-cand task)))
    (when vc
      (blob vc))))

(defmethod project-name ((task llm-pov-task))
  (project-name (target task)))

(defmethod llm-cmd-pov-args ((task llm-pov-task))
  (dbug :top "llm-cmd-pov-args for type llm-pov-task")
  (remove-if #'null ; To clean up the optional args below
	     (list (and (reproduce-path task) "-sani") (reproduce-path task) ;; FIXME update name
		   (and (blob task) "-blob") (blob task)
		   (and (harness task) "-harn") (harness task)
		   (and (sanitizer task) "-saniname") (sanitizer task)
		   (and (engine task) "-fuzzeng") (engine task)
		   (and (project-name task) "-proj") (project-name task)
		   )))
