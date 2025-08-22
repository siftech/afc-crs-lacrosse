;;; -------------------------------------------------------------------------
;;; COPYRIGHT START
;;; COPYRIGHT END
;;; -------------------------------------------------------------------------

;;; A class for tasks that invoke the langchain-based llm tool.

(cl:in-package :fuzzbomb)

(deftype pathname-designator ()
  `(or pathname string))

(defclass patch-path-mixin ()
  ((patch-path
    :initarg :patch-path
    :accessor patch-path
    :type pathname-designator
    :documentation "Path of patch file.")
   (harness-name
    :initform nil
    :accessor harness-name)
   (blob
    :initform nil
    :accessor blob)
   (bic
    ;;:type string
    :documentation "sha for bug inducing commit."
    :initform nil
    :accessor bic)
   (cp-src
    ;;:type string
    :documentation "Path component distinguishing this repo."
    :initform nil
    :accessor cp-src))
  (:documentation "This mixin class contains the info necessary to 
evaluate a patch, used by both LACROSSE-LLM-GEN-PATCH-TASK and
REVERT-TO-PATCH-TASK."))

(defgeneric patch-passes-test-p (patch-file task))

(defclass lacrosse-llm-gen-patch-task (patch-path-mixin lacrosse-llm-task)
  (
   (json-pathname
    :type pathname-designator
    :reader json-pathname
    :documentation "Path of JSON file with input to Python script."
    )
   (json-output-file
    :type pathname-designator
    :accessor json-output-file
    :documentation "Path of JSON file containing all outputs."
    )
   (json-input
    :type list                          ; plist
    :documentation "Contents of the JSON input file. Mostly for debugging."
    :accessor json-input
    )
   ;;   FIXME should this live in lacrosse-llm-task.lisp?
   ;;   FIXME is this more than a string?
   (llm-arg
    ;;:type (or null string)
    :documentation "String designating llm, passed as a command-line arg to lacrosse_llm."
    ;;   Today, opus started giving me bad patches, chat-gpt crashed, and claude won.
    ;;:initform "--opus"
    ;;:initform "--chat-gpt"
    :initform "--claude"
    :accessor llm-arg)
   (vc-id
    ;;:type (or null int)
    :documentation "Int id for vuln-cand."
    :initform nil
    :accessor vc-id)
   (file-to-patch
    :documentation "the string name of file to try to patch, chosen by OPT."
    :initform nil
    :accessor file-to-patch)
   (harness-id
    :initform nil
    :accessor harness-id)
   (task-output-dir
   ;;:type namestring
   :documentation "Unique dir for this task."
   :initform nil
   :accessor task-output-dir)
   )
  )

(defmethod scoring-fn ((task lacrosse-llm-gen-patch-task)) 15)

(defmethod print-object ((task lacrosse-llm-gen-patch-task) str)
  (print-unreadable-object (task str :type t)
    (format str "[~a] id: ~a" (name task) (vc-id task))))

;;; Eventually, every task should define or inherit next two methods.
;;; Don't call them until slots are populated!
(defmethod task-temp-dir-namestring ((task lacrosse-llm-gen-patch-task))
  (strcat (output-dir (target task)) (task-temp-dirname task) "/"))

(defmethod task-temp-dirname ((task lacrosse-llm-gen-patch-task))
  (format nil "~a-~a-vc~a-~a" (class-name (class-of task)) (get-universal-time) (vc-id task) (llm-arg task)))

(defmethod initialize-instance :after ((task lacrosse-llm-gen-patch-task) &key)
  (dbug :top "initialize-instance :after ((task lacrosse-llm-gen-patch-task): ~s" task)
  (if *lacrosse-llm-stub-patch-p*
      ;; - choose a vuln report (for now, the only isabel-report) for input
      ;; - make up a filename for output
      (let ((repo-name ""))
        (let* (
               (cmds (list (list (strcat *lax-home* "/code/tools/gen-patch-stub.sh")
                                 (task-output-dir task)
                                 repo-name)))
               ;;(cmd-str (format nil "~{~A~^ && ~}" (mapcar #'uiop:escape-sh-command cmds)))
               ;; the above escape is a lovely pattern, but foils the attempt to use a glob
               ;; in the activate step.
               (cmd-str (format nil "~{~A~^ && ~}"
                                (mapcar #'(lambda (cmd-list)
                                            (format nil "~{~A~^ ~}" cmd-list))
                                        cmds)))
               )
          (dbug :top "  cmd-str: ~s" cmd-str)
          (setf (cmd task) (uiop:escape-sh-command `("bash" "-c" ,cmd-str)))))

      (let ()
        ;;   FIXME this to happen magically in :around method
        ;;; moved this parsing up so can use below in build-json-input-file
        (when (next-task (target task))
          (setf (vc-id task) (getassoc :vc-id (next-task (target task))))
          (setf (harness-name task) (harness-prop :name (find-harness-by-id
                                                         (getassoc :harness-id (next-task (target task)))
                                                         (target task))))
          (setf (file-to-patch task) (getassoc :file-to-patch (next-task (target task))))
          (setf (blob task) (getassoc :blob (next-task (target task))))
          (setf (llm-arg task) (getassoc :llm-arg (next-task (target task))))
          (setf (bic task) (getassoc :bic (next-task (target task))))
          (setf (cp-src task) (getassoc :cp-src (next-task (target task))))
          )

        (setf (task-output-dir task) (task-temp-dir-namestring task))
        (ensure-directories-exist (task-output-dir task))
        
        ;; Real patch stuff here.
        (let* ((json-pathname (build-json-input-file task))
               (json-output-pathname (namestring
                                      (merge-pathnames (make-pathname :name "patch-outputs"
                                                                      :type "json")
                                                       (task-output-dir task))))
               (cmds `(("cd" ,(strcat *lax-home* "/code/langchain"))
                       ("python" "-m" "lacrosse_llm.patch"
                                 "--json-input-file"
                                 ,(namestring json-pathname)
                                 "--json-output-file "
                                 ,json-output-pathname
                                 ,(llm-arg task)
                                 )
                       ("echo" "GEN-PATCH-LLM - TASK RESULT: DONE")))
               ;;(cmd-str (format nil "~{~A~^ && ~}" (mapcar #'uiop:escape-sh-command cmds)))
               ;; the above escape is a lovely pattern, but foils the attempt to use a glob
               ;; in the activate step.
               (cmd-str (format nil "~{~A~^ && ~}"
                                (mapcar #'(lambda (cmd-list)
                                            (format nil "~{~A~^ ~}" cmd-list))
                                        cmds))))
          (setf (json-output-file task) json-output-pathname
                (slot-value task 'json-pathname) json-pathname)
          (dbug :top "  cmd-str: ~s" cmd-str)
          (debug-json-input-file task)
          (setf (cmd task) (uiop:escape-sh-command `("bash" "-c" ,cmd-str))
                (json-input task) (cl-json:decode-json-from-source (parse-namestring json-pathname))
                (json-output-file task) json-output-pathname)
          ))))

;;; Print information about the JSON file we have written to send to
;;; the python script.
(defmethod debug-json-input-file ((task lacrosse-llm-gen-patch-task))
  (dbug :top "  Contents of json input file:")
  (multiple-value-bind (results error-output exit-code)
      (uiop:run-program
       `("jq" "." ,(namestring (json-pathname task)))
       :ignore-error-status t
       :error-output :string
       :output :string)
    (if (zerop exit-code)
        (dbug :top "~a" results)
        (dbug :top "LACROSSE-LLM-GEN-PATCH-TASK Trying to format JSON file, got error ~d: ~a"
              exit-code error-output))))

(defmethod input-patch-path ((task lacrosse-llm-gen-patch-task) &key (error-p t))
  (or (getassoc :patch--file (json-input task) :proper t)
      (when error-p
       (error "Can't find patch file in cell ~s taken from JSON ~s"
              (assoc :patch--file (json-input task)) (json-input task)))))

(defmethod pre-exec ((task lacrosse-llm-gen-patch-task))
  (dbug :amp "Executing lacrosse-llm-gen-patch-task.")
  (dbug :amp "vc-id: ~s" (vc-id task)))

(defmethod post-exec ((task lacrosse-llm-gen-patch-task))
  "After running LACROSSE-LLM-GEN-PATCH-TASK, send a message to the optimi to
tell them whether or not a patch has been successfully generated, and where to
find it."
  (dbug :top "post-exec for lacrosse-llm-gen-patch-task")
  (if (file-exists-p (json-output-file task))
      (let* ((json-return
               (uiop:with-input-file (str (json-output-file task))
                 (cl-json:decode-json str)))
             (patch-files (getassoc :output-files json-return :proper t)))
        (dbug :top "Retrieved JSON output file from patch.py: ~a containing ~s" (json-output-file task) json-return)
        (dbug :top "Found returned patch files: ~{~A~%~}" patch-files)
        (if patch-files
            (iter (for patch-file in patch-files)
              (dbug :patch "Considering file: ~s" patch-file)
              ;; Copy patch to task dir for safe keeping and poss, cp to optimus.
              (cp patch-file (task-output-dir task))
              (if (file-exists-p patch-file)
                  (progn
                    (dbug :patch "Testing patch: ~s" patch-file)
                    (when (or (not *lacrosse-llm-gen-patch-test-p*)
                              ;; This pred also sends fail msgs.  FIXME sorry  
                              (patch-passes-test-p patch-file task))
                      (send-patch-generation-succeeded-msg task :patch-file patch-file)
                      ))
                  (let ((desc (format nil "ERROR: Failed to write patch file to ~a" patch-file)))
                    (dbug :top desc)
                    (send-patch-generation-failed-msg task :desc desc)
                    )))
            (dbug :top "JSON output file reports no patch files found.")))
      (let ((desc (format nil "ERROR: Python patch code failed to write JSON output file to ~a" (json-output-file task))))
        (dbug :top desc)
        (send-patch-generation-failed-msg task :desc desc)
        )))

(defmethod task-applies-to-target-p ((task-class-name (eql 'lacrosse-llm-gen-patch-task)) target-node)
  (dbug :docker-task "task-applies-to-target-p lacrosse-llm-gen-patch-task")
  (let ((target (target target-node)))
    (dbug :target "(dir target): ~s" (dir target))
    (dbug :target-deep "(target-sources target): ~s" (target-sources target))
    (dbug :target "(next-task target): ~s" (next-task target))

    (and (next-task target)
         (let ((task-type (getassoc :task-type (next-task target))))
           (eq task-type task-class-name)))))

(defmethod process-line ((task lacrosse-llm-gen-patch-task) line)
  (dbug :top "got line [~A]" line)
  (cond
;;;        ((cl-ppcre:scan-to-strings "\\[Error in checking file: 1 validation error for ChatOpenAI\\]" line)
;;;         (dbug :top "RESULT: validation error (no api key?)")
;;;         ;; FIXME Should be some kind of error response?  
;;;         (send-message-to-optimi :type :challenge-project-update
;;;                                 :target-id (id (target task)))
;;;         )
;;;        (;;(cl-ppcre:scan-to-strings "is_vulnerable" line)
;;;         (cl-ppcre:register-groups-bind (vuln-result)
;;;             ("TASK RESULT: val.is_vulnerable = (\\S+)" line)
;;;           (dbug :top "val.is_vulnerable string: ~s" vuln-result)
;;;           (let ((vuln-p (python-boolean-to-lisp vuln-result)))
;;;             (dbug :top "RESULT is ~s" vuln-p)
;;;             (dbug :top "RESULT: got a reply from llm")
;;;             (setf (vuln-confirmed-p task) vuln-p)
;;;             )))
    ((cl-ppcre:scan-to-strings "patch.py error:" line)
     (send-patch-generation-failed-msg task :desc line))
;;;    ((cl-ppcre:scan-to-strings "GEN-PATCH-STUB - TASK RESULT: DONE" line)
;;;     (let* ((output-dir (namestring (output-dir (target task))))
;;;            (patch-file (concatenate 'string output-dir "good_patch.diff"))
;;;            (patches nil))
;;;       (when (slot-exists-p (target task) 'patches)
;;;         (setf patches (append (patches (target task)) (list patch-file))))
;;;       (send-message-to-optimi :type :challenge-project-update
;;;                               :target-id (id (target task))
;;;                               :patches patches
;;;                               :dir (dir (target task))
;;;                               :build-with-patch T
;;;                               :make-patch nil
;;;                               :patch-p t)
;;;       ))
;;;    ((cl-ppcre:scan-to-strings "GEN-PATCH-LLM - TASK RESULT: DONE" line)
;;;     (let* ((patch-file (namestring (patch-path task)))
;;;            (patches nil)
;;;            (blobs nil))
;;;       (when (slot-exists-p (target task) 'patches)
;;;         (setf patches (append (patches (target task)) (list patch-file))))
;;;       (send-message-to-optimi :type :challenge-project-update
;;;                               :target-id (id (target task))
;;;                               :patches patches
;;;                               :blobs blobs
;;;                               :build-with-patch T
;;;                               :make-patch nil
;;;                               :dir (dir (target task))
;;;                               :patch-p t)
;;;       ))
       ))

(defgeneric build-json-input-file (task)
  (:documentation "Take a patch generation task, build the corresponding JSON file with its
inputs, and return the pathname of the JSON file.")
  (:method  ((task lacrosse-llm-gen-patch-task))
    (dbug :target "(src target): ~s" (src (target task)))
    (dbug :top "(file-to-patch task): ~s" (file-to-patch task))
    (let* ((output-directory (uiop:ensure-directory-pathname (task-output-dir task)))
           (json-pathname (merge-pathnames
                           (make-pathname :name "patch-input" :type "json")
                           output-directory))
           (target-pathname (parse-namestring (strcat (src (target task)) (cp-src task) "/" (file-to-patch task))))
           (patch-pathname (merge-pathnames
                            (make-pathname :name (pathname-name target-pathname)
                                           :type "patch")
                            output-directory))
           (patched-pathname (merge-pathnames
                              (make-pathname :name (format nil "~a-patched"
                                                           (pathname-name target-pathname))
                                             :type (pathname-type target-pathname))
                              output-directory))
           (output-obj (list
                        (cons "output_directory" (namestring output-directory))
                        (cons "file" (namestring target-pathname))
                        (cons "patch_file" (namestring patch-pathname))
                        (cons "patched_file" (namestring patched-pathname))
                        (cons "patch_function" "null")
                        (cons "line_number" "null")
                        ;; path to file, relative to project root.
                        (cons "file_to_patch" (file-to-patch task))
                        )))
      (declare (type pathname
                     json-pathname target-pathname patch-pathname
                     output-directory
                     patched-pathname))
      ;; FIXME: I don't love this buried initializer...
      (setf (patch-path task) patch-pathname)
      (when (not (uiop:file-exists-p json-pathname))
        (uiop:with-output-file (str json-pathname)
          (cl-json:encode-json-alist output-obj str)))
      (dbug :top "    JSON input file: ~a~%" json-pathname)
      json-pathname)))

;;;   FIXME clean up this spaghetti!
(defmethod patch-passes-test-p (patch-file (task patch-path-mixin))
  (let* ((target (target task))
         (cp-dir (path target))
         (source-dir-name (cp-src task))
         (source-rel-path (concatenate 'string "/" source-dir-name))
         (patch-file-to-apply (cond (*lacrosse-llm-gen-patch-test-canned-patch-p*
                                     (strcat cp-dir "exemplar_only/cpv_1/patches/samples/good_patch.diff"))
                                    (t (namestring patch-file))))
         (cmds `(("cd" ,cp-dir)
                 ;;("PATCH_EXTRA_ARGS=-p3" "./run.sh" "-v" "build" ,patch-file-to-apply ,source-rel-path)
                 ("./run.sh" "-x" "build" ,patch-file-to-apply ,source-rel-path)
                 ;;("./run.sh" "-v" "build")  JUST BUILD, NO patch
                 ))
         (cmd-str (format nil "~{~A~^ && ~}" (mapcar #'uiop:escape-sh-command cmds))))
    (dbug :patch "cp-dir: ~s" cp-dir)
    (dbug :patch "source-dir-name: ~s" source-dir-name)
    (dbug :patch "source-rel-path: ~s" source-rel-path)
    (dbug :patch "patch-file-to-apply: ~s" patch-file-to-apply)
    (dbug :patch "cmds: ~s" cmds)
    (dbug :patch "cmd-str: ~s" cmd-str)

    ;; Reset source dir.  llm team may have corrupted it!
    (reset-source target)

    ;; Apply patch
    (multiple-value-bind (output-slurp error-slurp exit-code)
        (uiop:run-program cmd-str :output t :force-shell t :ignore-error-status t :error-output :output)
      (declare (ignore output-slurp error-slurp))
      (dbug :patch "exit-code: ~a" exit-code)
      (unless (and (zerop exit-code)
                   (zerop (most-recent-build-exitcode target)))
        (send-patch-generation-failed-msg task :desc "Patch failed to apply." :patch-file patch-file)
        ;; Don't like early exits, but we're in a hurry... FIXME cleanup later  
        (return-from patch-passes-test-p nil))
      )

    ;; Now run default tests.
    (let* ((cmds `(("cd" ,cp-dir)
                   ("./run.sh" "run_tests")))
           (cmd-str (format nil "~{~A~^ && ~}" (mapcar #'uiop:escape-sh-command cmds))))
      ;; run tests
      (dbug :patch "Running tests: ~s" cmd-str)
      (multiple-value-bind (output-slurp error-slurp exit-code)
          (uiop:run-program cmd-str :output t :ignore-error-status t)
        (declare (ignore output-slurp error-slurp))
        (dbug :patch "exit-code: ~a" exit-code)
        (unless (and (zerop exit-code)
                     (zerop (most-recent-run-tests-exitcode target)))
          (send-patch-generation-failed-msg task :desc "Patch failed run_tests." :patch-file patch-file)
          ;; Don't like early exits, but we're in a hurry... FIXME cleanup later  
          (return-from patch-passes-test-p nil))
        ))

    ;; Does it fix the pov?
    ;; Pass 0 as fourth arg to prevent rebuild.
    (let ((cmd (format nil "~a/build-and-test-pov.sh ~a ~a ~a 1" *lax-tools*
                       (blob task) (harness-name task) (path target))))
      (dbug :patch "Running pov test: ~s" cmd)
     (multiple-value-bind (output-slurp error-slurp exit-code)
          (uiop:run-program cmd :output t :ignore-error-status t)
        (declare (ignore output-slurp error-slurp))
        (dbug :patch "exit-code: ~a" exit-code)
        (unless (zerop exit-code)
          (send-patch-generation-failed-msg task :desc "Patch failed run_pov." :patch-file patch-file)
          ;; Don't like early exits, but we're in a hurry... FIXME cleanup later  
          (return-from patch-passes-test-p nil))
       ))
    ;; If reaches here, all tests have passed.
    t
    ))

;;; FIXME should be method on a patch-task superclass
;;;(defmethod send-patch-generation-failed-msg ((task lacrosse-llm-gen-patch-task) &key desc)
(defmethod send-patch-generation-failed-msg ((task patch-path-mixin) &key desc patch-file)
  ;; task should be either a lacrosse-llm-gen-patch-task or a revert-to-patch-task
  (dbug :top "Proposed patch result: FAILED, reason: ~a for ~a" desc patch-file)
  (apply #'send-message-to-optimi `(:type :patch-generation-failed
                                    ,@(when desc (list :desc desc))
                                    :vc-id ,(vc-id task)
                                    :target-id ,(id (target task)))))

(defmethod send-patch-generation-succeeded-msg ((task patch-path-mixin) &key desc patch-file)
  ;; task should be either a lacrosse-llm-gen-patch-task or a revert-to-patch-task
  (dbug :top "Proposed patch result: SUCCEEDED, reason: ~a for ~a" desc patch-file)
  (apply #'send-message-to-optimi `(:type :patch-generation-succeeded
                                    ;;   Hack for revert-to-patch tasks
                                    ;; FIXME I think json-output-file is not used!  
                                    ,@(when (slot-exists-p task 'json-output-file)
                                       (list :json-output-file (namestring (json-output-file task))))
                                    :patch-file ,(namestring patch-file)
                                    ,@(when desc (list :desc desc))
                                    :vc-id ,(vc-id task)
                                    :target-id ,(id (target task)))))
