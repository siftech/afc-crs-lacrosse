;;; -------------------------------------------------------------------------
;;; COPYRIGHT START
;;; COPYRIGHT END
;;; -------------------------------------------------------------------------

;;; A class for tasks that run a harness to get a Proof-of-Vulnerability (PoV).
;;; Despite the possiblity of multiple harnesses, a single task runs a single harness.

(cl:in-package :fuzzbomb)

(defclass lacrosse-cp-run-pov-task (lacrosse-task)
  (
   (blob :initarg :blob :initform nil :accessor blob
         :documentation "The blob to run to see if triggers sanitizer.")

   ;; harness-id must be supplied, other harness-* slots are derived from it.
   (harness-id :initarg :harness-id :accessor harness-id
               :documentation "ID of the harness. Required for scoring."
               ;;:type symbol
               )

   (harness-name :initform nil :accessor harness-name
                 :documentation "Name of the harness. Required for getting a pov.")
   (harness-binary :initform nil :accessor harness-binary
                   :documentation "binary of the harness. Required for getting a pov.")

   ;; [Pavan K. - May not need these attributes, but doesn't hurt to have them.]
   (triggered-sanitizer :initarg :triggered-sanitizer :initform nil :accessor triggered-sanitizer
                        :documentation "Sanitizer triggered by running the harness with a given blob.")
   (triggered-sanitizer-id :initarg :triggered-sanitizer-id :initform nil :accessor triggered-sanitizer-id
                           :documentation "ID of triggered sanitizer")
   )
  )

(defmethod print-object ((task lacrosse-cp-run-pov-task) str)
  (print-unreadable-object (task str :type t)
    (format str "[~a] blob: ~a" (name task) (uiop-file-namestring (blob task)))))

(defmethod scoring-fn ((task lacrosse-cp-run-pov-task)) 5)

;; Note: keep in mind this is going to run when the task is created,
;; eg possibly in the middle of a fuzzing run when we detect a crash and
;; create this task to confirm-pov later, after fuzzing is over.

(defmethod initialize-instance :before ((task lacrosse-cp-run-pov-task) &key target)
  (dbug :top "initialize-instance :before ((task lacrosse-cp-run-pov-task)")
  (setf (target task) target
        (blob task) (getassoc :blob (next-task target))
        (harness-id task) (getassoc :harness-id (next-task target))))

(defmethod initialize-instance :after ((task lacrosse-cp-run-pov-task) &key)
  (dbug :top "initialize-instance :after ((task lacrosse-cp-run-pov-task): ~s" task)
  (dbug :top "(dir (target task)): ~s" (dir (target task)))
  (dbug :top "(path (target task)): ~s" (path (target task)))
  (let* (
         (cp-dir (path (target task)))
         (blob-file (blob task))
         (harness-props (find-harness-by-id (harness-id task) (target task)))
         (harness-name (harness-prop :name harness-props))
         (harness-binary (harness-prop :binary harness-props))
         ;; this works now b/c we only put one blob in the list, from fuzzer
         (cmds `(("cd" ,cp-dir)
                 ;; [nringo.20240709.1417CDT] Does this need -g set?
                 ("./run.sh" "build")
                 ("./run.sh" "run_pov" ,blob-file ,harness-name)))
         (cmd-str (format nil "~{~A~^ && ~}" (mapcar #'uiop:escape-sh-command cmds))))
    (dbug :top "  cmd-str: ~s" cmd-str)

    (setf (harness-name task) harness-name)
    (setf (harness-binary task) harness-binary)
    (setf (cmd task) (uiop:escape-sh-command `("bash" "-c" ,cmd-str)))
))

(defmethod pre-exec ((task lacrosse-cp-run-pov-task))
  (dbug :amp "Executing lacrosse-cp-run-pov-task.")
  (dbug :amp "  harness-name ~a" (harness-name task))
  ;;(dbug :target "(output-pathname task): ~s" (output-pathname task))
  (reset-source (target task))
  )

(defmethod process-line ((task lacrosse-cp-run-pov-task) line)
  (let ((sanitizers (cp-prop (target task) :sanitizers)))
    (dbug :top "got line [~A]" line)
    ;; When "SUMMARY" line arrives, check which/any sanitizer fired.
    (when (string-starts-with line "SUMMARY:")  ; could optimize for constant string
      (loop for sanitizer in sanitizers do
        (dbug :pov-deep "Checking sanitizer ~a" (cdr sanitizer))
        (when (cl-ppcre:scan-to-strings (cdr sanitizer) line)
          (let ((vuln-cand
                  `((:target-id ,(id (target task)))
                    (:harness-id ,(symbol-name (harness-id task)))
                    (:harness-name ,(harness-name task))
                    (:harness-binary ,(harness-binary task))
                    (:blob ,(blob task))
                    (:sanitizer-id ,(symbol-name (first sanitizer)))
                    (:sanitizer ,(rest sanitizer))
                    )))
          (push vuln-cand (vuln-cands (target task)))
            (apply 'send-message-to-optimi
                   :type :vuln-cand
                   (flatten-one-level vuln-cand))
            ))))
      ))
;;; FIXME   What else to do here?  Set a success flag in task?
;;; Create a json file w interesting info (eg, make output)?
;(defmethod post-exec ((task lacrosse-cp-run-pov-task))
;  (let ((target (target task)))
;    (dbug :top "post-exec for lacrosse-cp-run-pov-task. target: ~s" target)
;    ;;(reset-file-results target)      ;;   this blows up and dont know why we'd need it
;    ;; [Pavan K. - Success conditions (both have to be true to succeed):
;    ;;  - The most recent exitcode for run_pov is 0, indicating successful execution
;    ;;  - A sanitizer has been triggered
;    (let* (;;(repo-name (repo-name-from-repo-address (cp-address (target task))))
;           (outdir (toolchain-command "/lacrosse/code/tools/most-recent-runsh-dir"))
;           (exitcode-file (strcat outdir "/exitcode"))
;           ;; Exit code should be 0 for run_pov to be successful
;           (exitcode (uiop:read-file-string exitcode-file))
;           (triggered-sanitizer nil)
;           (triggered-sanitizer-id nil)
;           (sanitizers (sanitizers (target task))))
;
;      ;; [Pavan K. - This can be done in process-line, but I'm unsure if process-line is
;      ;;  done while the container is executing. If so, then doing this search could cause slowdowns?
;      ;;  Unsure, but safer (and not really an issue) if done here.]
;      (loop for line in (results task) do
;        (loop for sanitizer in sanitizers do
;          ;; [Pavan K. - FIXME: Which debug level to use here? I'm going based on an example from `task-applies-to-target-p`. ]
;          (dbug :pov-deep "Checking sanitizer ~a" (car (cdr sanitizer)))
;          (when (cl-ppcre:scan-to-strings (car (cdr sanitizer)) line)
;            (setf triggered-sanitizer (car (cdr sanitizer)))
;            (setf triggered-sanitizer-id (car sanitizer)))
;          )
;        )
;
;      (dbug :top "Sanitizer ~a with ID ~a triggered..." triggered-sanitizer triggered-sanitizer-id)
;      (dbug :top "Exit code file ~a for CP run pov contains exit code: ~a" exitcode-file exitcode)
;
;      (cond ((and (string= exitcode "0") triggered-sanitizer-id)
;             (dbug :top "Successfully got pov for target ~s" target)
;             (send-message-to-optimi :type :challenge-project-update
;                                     :target-id (id (target task))
;                                     :harness-id (harness-id task)
;                                     :harness-name (harness-name task)
;                                     :triggered-sanitizer-id triggered-sanitizer-id
;                                     :triggered-sanitizer triggered-sanitizer
;                                     :docker-exec-output (results task)
;                                     :run-pov nil
;                                     :make-pov-blob nil
;                                     :make-patch T
;                                     :contest-submission (format nil "CONTEST-SUBMISSION:HarnessID=~a,SanitizerID=~a" (harness-id task) triggered-sanitizer-id)
;                                     :comment (format nil "Successfully got pov for target ~s" target))
;             )
;            (t
;             (dbug :top "FAILED to get pov for target ~s" target)
;             (send-message-to-optimi :type :challenge-project-update
;                                     :target-id (id (target task))
;                                     :harness-id (harness-id task)
;                                     :harness-name (harness-name task)
;                                     :docker-exec-output (results task)
;                                     :run-pov nil
;                                     :make-pov-blob T
;                                     :comment (format nil "FAILED to get pov for target ~s" target))
;             )))))



;;; this is pretty much not used now... after fuzzer finds pov, we create these run-pov tasks directly
(defmethod task-applies-to-target-p ((task-class-name (eql 'lacrosse-cp-run-pov-task)) target-node)
  (let ((target (target target-node)))
    (and (next-task target)
         (let ((task-type (getassoc :task-type (next-task target))))
           (eq task-type task-class-name)))))

  ;; (dbug :docker-task "task-applies-to-target-p lacrosse-cp-run-pov-task")
  ;; ;;(dbug :top "Target makefile: ~a" (target-makefile (target target-node)))

  ;; (let ((target (target target-node)))

  ;;   (when (not (equalp (type-of target) 'lacrosse-cp-target))
  ;;     ;; This is not a challenge problem
  ;;     (return-from task-applies-to-target-p nil))

  ;;   (when (not (run-pov target))
  ;;     ;; Not the time to run pov
  ;;     (return-from task-applies-to-target-p nil))

  ;;   (let* (;;(repo-name (repo-name-from-repo-address (cp-address target)))
  ;;          (source-dir (source-path target))
  ;;          ;;(cp-dir (concatenate 'string (dir target) repo-name))
  ;;       (cp-dir (dir target))
  ;;          (blob-file (blob target-node)))

  ;;   (dbug :top "lacrosse-cp-run-pov-task - cp dir: ~a (~a), source dir: ~a (~a)"
  ;;     cp-dir (uiop:directory-exists-p cp-dir)
  ;;     source-dir (uiop:directory-exists-p source-dir))
  ;;   (dbug :top "lacrosse-cp-run-pov-task - Is executable in dir: ~a"
  ;;     (is-executable-in-dir source-dir))

  ;;   (and *lacrosse*
  ;;       source-dir
  ;;       cp-dir
  ;;       blob-file
  ;;       (uiop:directory-exists-p source-dir)
  ;;       (uiop:directory-exists-p cp-dir)
  ;;       (uiop:file-exists-p blob-file)
  ;;       ))))

;;stuff to mine from fake-fuzz
;;(defmethod post-exec ((task fake-fuzz-task))
;;  (with-slots (target) task
;;    (dbug :top "post-exec for fake-fuzz-task")
;;    (dbug :top "cp-path: ~s" (cp-path target))
;;    (let ((cp-blob-path (format nil "~a/exemplar_only/cpv_1/blobs/sample_solve.bin" (cp-path target)))
;;          (task-blob-path (format nil "~a/blob.bin" (output-pathname task))))
;;      (uiop:copy-file cp-blob-path task-blob-path)
;;      (with-slots (project-properties) target
;;        (let* ((harnesses (assoc-value :harnesses project-properties))
;;               (harness (first harnesses))
;;               (harness-props (rest harness)))
;;          (macrolet ((harness-prop-value (key)
;;                       `(or (getassoc ,key harness-props :proper t)
;;                            (error "Failed to find value for ~s in ~s" ,key harness-props))))
;;            (let ((vuln-cand
;;                    `((:target-id ,(id target))
;;                      ;; FIXME: need better logic for this!!!
;;                      (:id 1)
;;                    (:harness-id "id_1")
;;                      (:harness-name ,(harness-prop-value :name))
;;                      (:harness-source ,(harness-prop-value :source))
;;                      (:harness-binary ,(harness-prop-value :binary))
;;                      (:blob ,(namestring (parse-namestring task-blob-path)))
;;                      ;;(:sanitizer-id "AddressSanitizer: global-buffer-overflow")
;;                    (:sanitizer-id "id_1")
;;                    )))
;;              (push vuln-cand (vuln-cands target))
;;              (apply 'send-message-to-optimi
;;                     :type :vuln-cand
;;                     (flatten-one-level vuln-cand))
;;              (send-message-to-optimi :type :challenge-project-update
;;                                      :vuln-cand vuln-cand
;;                                      :target-id (id (target task))))))))))
;;
