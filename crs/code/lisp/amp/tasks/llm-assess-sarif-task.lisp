;;; -------------------------------------------------------------------------
;;; COPYRIGHT START
;;; COPYRIGHT END
;;; -------------------------------------------------------------------------

(cl:in-package :fuzzbomb)

(defparameter *assess-sarif-python-script* "code/tools/solve-sarif.py" "The python script that this task calls")

(defclass llm-assess-sarif-task (llm-task)
  ;; Possibly should be on the parent class instead.
  ((pipeline :initform nil)
   (llm-args :initform *sarif-llm-args*)
   (sarif-id :initarg :sarif-id :initform nil :accessor sarif-id
	     :documentation "The sarif id from the original sarif msg.")
   (lax-sarif-id :initarg :lax-sarif-id :initform nil :accessor lax-sarif-id
		 :documentation "The lax-assigned unique id for the sarif.")
   (sarif-json :initarg :sarif-json :initform nil :accessor sarif-json
	       :documentation "The json file storing the sarif input.")
   (output-json :initarg :output-json :initform nil :accessor output-json
		:documentation "The json file storing the sarif assessment decision and description.")
   (generated-p :initarg :generated-p :initform nil :accessor generated-p
		:documentation "Was the sarif assessment successfully generated.")
   (decision :initarg :decision :initform nil :accessor decision
	     :documentation "The binary assessment generated, correct or incorrect.")
   (description :initarg :description :initform nil :accessor description
		:documentation "The justification for the binary assessment generated.")
   ))

(defmethod task-applies-to-target-p ((task-class-name (eql 'llm-assess-sarif-task)) target-node)
  (let ((target (target target-node)))
    (when (and (next-task target)
               (let ((task-type (getassoc :task-type (next-task target))))
		 (eq task-type task-class-name)))
      (if *use-llm-assess-sarif-task* t
	  (progn (dbug :top "Discarding next-task ~a: *use-llm-assess-sarif-task* is nil!" task-class-name)
		 nil)))))

(defmethod set-slots-from-target :after ((task llm-assess-sarif-task))
  (dbug :top "set-slots-from-target :after ((task llm-assess-sarif-task): ~s" task)
  (when (next-task (target task))
    ;; set local slots from next-task elements
    (let ((sarif-id (getassoc :sarif-id (next-task (target task))))
	  (lax-sarif-id (getassoc :lax-sarif-id (next-task (target task))))
	  (sarif-json (getassoc :sarif-json (next-task (target task)))))
      (dbug :top "ctask id is ~s" (task-id (target task)))
      (dbug :top "sarif-id is ~s" sarif-id)
      (dbug :top "lax-sarif-id is ~s" lax-sarif-id)
      (dbug :top "sarif-json is ~s" sarif-json)
      (setf (sarif-id task) sarif-id)
      (setf (lax-sarif-id task) lax-sarif-id)
      (setf (sarif-json task) sarif-json)
      (setf (output-json task) (make-assessment-path task)))))

(defmethod make-assessment-path ((task llm-assess-sarif-task))
  (let ((path-str (strcat (task-output-dir task)
			  (format nil "/sarif-assessment-~a-~a.json" (lax-sarif-id task) (sarif-id task)))))
    (uiop:ensure-pathname path-str :ensure-directories-exist t)
    path-str))

(defmethod llm-assess-sarif-cmd ((task llm-assess-sarif-task))
  (remove-if #'null ; To clean up the optional args below
	     (list (poetry-preamble task)
		   (strcat *lax-home* *assess-sarif-python-script*)
		   (focus-path (target task))
		   (sarif-json task)
		   (output-json task)
		   "-scratch" (scratch-path task)
		   "-llms" (format-arg-list-for-cmd (llm-args task)))))

(defmethod llm-cmd ((task llm-assess-sarif-task))
  (llm-assess-sarif-cmd task))

(defmethod process-line ((task llm-assess-sarif-task) line)
  (dbug :top "got line [~a]" line)
  (cond ((search "Wrote sarif assessment to" line)
	 (let ((target-line (format nil "Wrote sarif assessment to ~a" (output-json task))))
	   (dbug :top "target line is ~s" target-line)
	   (dbug :top "line is ~s" line)
	   (when (string-equal line target-line)
	     (cond ((file-exists-p (pathname (output-json task)))
		    (dbug :top "Successfully generated a sarif assessment for task!")
		    (setf (generated-p task) t)
		    (let ((output-json (load-json-from-file (output-json task))))
		      (dbug :top "output json is ~s" output-json)
		      (setf (decision task) (alist-get output-json :decision))
		      (setf (description task) (alist-get output-json :description))))
		   (t (dbug :top "Sarif assessment supposedly written but does not exist at path ~a with pathstring ~s" (pathname (output-json task)) (output-json task)))))))
	))

(defmethod response-msg ((task llm-assess-sarif-task) head &key result-file)
  (declare (ignore result-file))
  `(:type ,head
	  ,@(when (vc-id task) (list :vc-id (vc-id task)))
	  :task-id ,(task-id (target task))
	  :sarif-id ,(sarif-id task)
	  :lax-sarif-id ,(lax-sarif-id task)
	  ))

(defmethod failed-msg ((task llm-assess-sarif-task))
  `(,@(response-msg task :assess-sarif-failed)
      :failure-reason "None available."))

(defmethod succeeded-msg ((task llm-assess-sarif-task))
  `(,@(response-msg task :assess-sarif-succeeded)
      :decision ,(decision task)
      :description ,(description task)))

;;; more important than fuzzing, less than explaining pov
(defmethod scoring-fn ((task llm-assess-sarif-task)) 750)
