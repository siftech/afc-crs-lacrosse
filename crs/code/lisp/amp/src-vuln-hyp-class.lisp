;;; -------------------------------------------------------------------------
;;; vuln-cand-class.lisp
;;; - target objects that AMP works on...
;;; -------------------------------------------------------------------------
;;; COPYRIGHT START
;;; COPYRIGHT END
;;; -------------------------------------------------------------------------

(cl:in-package :fuzzbomb)

;;; FIXME   create id
;;; FIXME copy blob to shared dir

(defclass src-vuln-hyp (named-object)
  (
   (id :initarg :id :initform -1 :accessor id
         :documentation "Unique lax id."
         :type int)
   (vuln-cand :initarg :vuln-cand :initform nil :accessor vuln-cand
	      :documentation "The vuln-cand that this hypothesis tries to explain.")
   (description :initarg :description :initform nil :accessor description
		:documentation "A description of the memory vunerability, as grounded as possible in specifics (involved functions, variables, lines of code)."
		:type (or null string))
   (suggested-file-repairs :initarg :suggested-file-repairs :initform nil :accessor suggested-file-repairs
			   :documentation "A list of suggested repairs, separated by file, that together will address the vulnerability. (If there's only one file being repaired this list should have length one.)"
			   :type list)
   (notes :initarg :notes :initform nil :accessor notes
	  :documentation "Any extra brief noteworthy details about the code that might be useful in planning or implementing a repair, if any."
	  :type (or null string))
   (failed-patching-attempts :initarg :failed-patching-attempts :initform nil :accessor failed-patching-attempts
         :documentation "A list of specs, one spec representing one failed attempt."
         :type list)
   (patch-cands :initarg :patch-cands :initform nil :accessor patch-cands
		:documentation "Successfully generated patch-cands that purport to patch this src-vuln-hyp.")
   (duplicates :initarg :duplicates :initform nil :accessor duplicates
	       :documentation "A list of pointers to duplicate src-vuln-hyps, if any.")
   (cwe :initarg :cwe :initform nil :accessor cwe
	  :documentation "The Common Weakness Enumeration (CWE) for this vuln.")
   (score :initarg :score :initform nil :accessor score
	  :documentation "A score for the quality of the score.")
   ))

(defmethod initialize-instance :after ((src-vuln-hyp src-vuln-hyp) &key)
  ;; Unless id is supplied to make-instance, use named-object name.
  ;; It's intended that only optimus assign the id.
  (when (and (numberp (id src-vuln-hyp))
             (= -1 (id src-vuln-hyp)))
    (setf (id src-vuln-hyp) (name src-vuln-hyp))))

(defun find-svh-by-id (id target)
  (find id (src-vuln-hyps target) :key #'id :test #'=))

(defun svh-batches-dir (target)
  (strcat (output-dir target) "src-vuln-hyp-batches"))

(defun make-svhs-json-file (svhs target)
  (let* ((ids (mapcar #'id svhs))
	 (ids-str (format nil "~{~A~^-~}" ids)))
    (dbug :top "svh batch ids ~a" ids-str)
    (strcat (svh-batches-dir target) "/svhs-" ids-str ".json")))

(defmethod to-json ((vh src-vuln-hyp))
  `(("id" . ,(id vh))
    ("description" . ,(description vh))
    ("cwe" . ,(cwe vh))
    ("suggested_file_repairs" . ,(mapcar #'to-json (suggested-file-repairs vh)))
    ("notes" . ,(notes vh))))

(defun json->src-vuln-hyp (alist)
  (make-instance 'src-vuln-hyp
		 :id (or (alist-get alist :id) -1)
		 :description (alist-get alist :description)
		 :cwe (alist-get alist :cwe)
		 :suggested-file-repairs (mapcar #'json->file-repair
						 (alist-get alist :suggested--file--repairs))
		 :notes (alist-get alist :notes)
		 :score (alist-get alist :score)))

(defmethod src-vuln-hyps-from-json-file (hyps-json-path)
  (let ((hyps-json (load-json-from-file hyps-json-path)))
    (when hyps-json
      (mapcar #'json->src-vuln-hyp hyps-json))))

(defmethod spec-slots ((svh src-vuln-hyp))
  '(id vuln-cand description
    suggested-file-repairs notes))

(defmethod spec ((svh src-vuln-hyp))
  (let ((slots (spec-slots svh)))
    (iterate (for slot in slots)
      (collect (list (intern (symbol-name slot) :keyword)
                     (maybe-unpack-slot-val (slot-value svh slot)))))))

(defclass file-repair (named-object)
  (
   (target-filepath :initarg :target-filepath :initform nil :accessor target-filepath
		    :documentation "A relative filepath for the file to be fixed."
		    :type (or null string))
   (target-function-names :initarg :target-function-names :initform nil :accessor target-function-names
		    :documentation "The list of function names, defined in this particular target filepath, whose bodies must be changed to implement the repair."
		    :type list)
   (repair-plan :initarg :repair-plan :initform nil :accessor repair-plan
		:documentation "The plan for how to repair the target file, which much address all of the target functions."
		:type (or null string))
   ))

(defmethod to-json ((fr file-repair))
  `(("target_filepath" . ,(target-filepath fr))
    ("target_function_names" . ,(target-function-names fr))
    ("repair_plan" . ,(repair-plan fr))))

(defun json->file-repair (alist)
  (make-instance 'file-repair
		 :target-filepath (alist-get alist :target--filepath)
		 :target-function-names (alist-get alist :target--function--names)
		 :repair-plan (alist-get alist :repair--plan)))

(defmethod spec-slots ((fr file-repair))
  '(target-filepath target-function-names
    repair-plan))

(defmethod spec ((fr file-repair))
  (let ((slots (spec-slots fr)))
    (iterate (for slot in slots)
      (collect (list (intern (symbol-name slot) :keyword)
                     (maybe-unpack-slot-val (slot-value fr slot)))))))

(defmethod new-file-repair-from-spec ((spec list))
  "Given a list defining a file repair,
   create a new target object and return it."
  ;; The only relevant elements of the spec are the cp_path and the id (if supplied).
  (dbug :target "new-file-repair-from-spec: ~s" spec)
  ;; avoid infinite method loop!
  (unless spec
    (error "nil is not a spec!"))
  (let* ((file-repair-class 'file-repair)
         ;; flatten one level to turn pairs into keyword args.
         (make-inst-args (append (list file-repair-class)
                                 (musliner:flatten-one-level spec)))
        )
    (dbug :target "make-inst-args: ~s" make-inst-args)
    (apply #'make-instance make-inst-args)))
