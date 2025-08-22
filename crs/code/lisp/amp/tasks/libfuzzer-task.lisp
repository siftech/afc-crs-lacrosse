;;; -------------------------------------------------------------------------
;;; COPYRIGHT START
;;; COPYRIGHT END
;;; -------------------------------------------------------------------------

(cl:in-package :fuzzbomb)

(defclass libfuzzer-task (lacrosse-task)
  (
   (chosen-harness-id
    :initarg :chosen-harness-id
    :accessor chosen-harness-id
    :documentation "id for chosen harness supplied by :next-task"
    ;;:type symbol
    )
   (examples
    :initarg :examples
    :accessor examples
    :documentation "id for chosen harness supplied by :next-task"
    ;;:type symbol
    )
   (llm-arg
    :initarg :llm-arg
    :accessor llm-arg
    :documentation "id for chosen harness supplied by :next-task"
    ;;:type symbol
    )
   (chosen-harness-id-as-string
     :accessor chosen-harness-id-as-string
     :type string)
   (seeds-dir
     :accessor seeds-dir
     :documentation "the pathname of the directory to store fuzzer seeds in"
     :type pathname)
   (use-value-profile
     :accessor use-value-profile
     :initarg :use-value-profile
     :initform nil
     :documentation "whether to pass -use_value_profile=1 to the fuzzer"
     :type boolean)
   (already-ran-find-inputs
     :accessor already-ran-find-inputs
     :initform 0
     :documentation "how many times we've run the find_inputs script already"
     :type integer)
   (fuzzer-timeout
     :accessor fuzzer-timeout
     :initarg :fuzzer-timeout
;; FIXME FIXME really would like this to self-calibrate
     :initform (cond
                   (*fuzzer-timeout*)
                   (t (format nil "~am" (+ 20 (random 10)))))
     :documentation "duration to man:timeout(1)"
     :type string)
   (do-triage
     :accessor do-triage
     :initarg :do-triage
     :initform t
     :documentation "whether to triage crashes instead of passing to optimi"
     :type boolean)
   (crashes-to-triage
     :accessor crashes-to-triage
     :initform nil
     :documentation "list of crashes to triage after the task; pairs of (or pathname string) and stack-trace"
     :type list)
   (namestrings-of-run-povd-crashes
     :accessor namestrings-of-run-povd-crashes
     :initform (make-hash-table :test #'equal)
     ;;   TODO: Does this actually prevent AFL from
     ;; getting duplicates?
     :documentation "hashtable from namestrings to t, set when we run-pov a crash to avoid run-pov-ing something with the same hash twice"
     :type hash-table)
   (stack-traces-of-run-povd-crashes
     :accessor stack-traces-of-run-povd-crashes
     :initform (make-hash-table :test #'equal)
     :documentation "hashtable from stack-trace to pathname, set when we run-pov a crash, used in crash triage"
     :type hash-table)
   (next-stack-trace
     :accessor next-stack-trace
     :initform nil
     :documentation "an accumulator for the stack trace"
     :type submit-crash-stack-trace)))

(defmethod create-default-container-name ((task libfuzzer-task))
  nil)

(defmethod task-applies-to-target-p ((task-class-name (eql 'libfuzzer-task)) target-node)
  ;;(dbug :top "task-applies-to-target-p libfuzzer-task")
  (let ((target (target target-node)))
    (and *use-libfuzzer*
         (next-task target)
         (let ((task-type (getassoc :task-type (next-task target))))
           (eq task-type task-class-name)))))

(defmethod push-next-fuzzer ((task libfuzzer-task))
  (push task (tasks *self*)))

(defmethod pre-exec ((task libfuzzer-task))
  (dbug :top "starting pre-exec libfuzzer-task")

  (handler-bind ((serious-condition (lambda (condition)
                                      (declare (ignore condition))
                                      (push-next-fuzzer task))))

    (run-command (strcat "mkdir -p " (native-namestring (seeds-dir task))))

    (cond
      (*fake-fuzz*
       (dbug :top "WARNING: overriding cmd with fake fuzzer")
       ;; arg-string is space-delimited concat (or empty string when *fake-fuzz-args* is nil)
       (let ((arg-string (format nil "~{~a~^  ~}" *fake-fuzz-args*)))
	 (setf (cmd task) (format nil "cd ~a && /lacrosse/code/tools/fake-libfuzzer.sh ~a" (path (target task)) arg-string))
	 (dbug :top "Task cmd: ~a" (cmd task))))
      (t
       (when *use-llm-seeds*
         (let* ((harness-source (harness-prop :source (find-harness-by-id (chosen-harness-id task) (target task))))
                (cmd (format nil "cd /lacrosse/code/langchain && python -m lacrosse_llm.find_inputs --examples ~a --harness-file ~a --output-dir ~a ~a"
                             (examples task)
                             (strcat (path (target task)) "/" harness-source)
                             (native-namestring (seeds-dir task))
                             (llm-arg task))))
           (cond
            ((>= (already-ran-find-inputs task) 1)
             (dbug :top "skipping seed finding, already ran ~a times..." (already-ran-find-inputs task)))
            (t
             (incf (already-ran-find-inputs task))
             ;;(toolchain-command cmd)
             ;;(toolchain-command "mkdir -p work/seeds")
             (dbug :top "Running command to find seeds:")
             (dbug :top "~a" cmd)
             (multiple-value-bind (output error-output exit-code)
                 (uiop:run-program cmd :force-shell t :output :lines :error-output :lines :ignore-error-status t)
               (cond ((zerop exit-code)
                      (dbug :top "LLM fuzzer seed generation ran successfully. LLM fuzzer standard output:")
                      (iter (for line in output)
                            (dbug :top "[~a]" line)))
                     (t
                      (dbug :top "LLM fuzzer failed with exit status ~d.  Error output:" exit-code)
                      (iter (for line in error-output)
                            (dbug :top "[~a]" line))
                      (dbug :top "LLM fuzzer failed."))))))
           (dbug :top "list ~a:" (seeds-dir task))
                                        ;(toolchain-command (strcat "cd " (path (target task)) " && ls -l work/seeds"))
           (dbug :top "~s" (directory (uiop:merge-pathnames* #p"*" (seeds-dir task))))

         (dbug :top "about to build it")))
       (build-it-for-fuzzing task))))

  (dbug :top "finishing pre-exec libfuzzer-task"))

(defmethod build-it-for-fuzzing ((task libfuzzer-task))
  (dbug :top "~s ~s" #'build-it-for-fuzzing task)

  (uiop:run-program
    (format nil "cd ~a && /lacrosse/code/tools/lacrosse-libfuzzer-build-it.py"
            (path (target task)))
    :output t
    :error-output :output))

(defmethod initialize-instance :after ((task libfuzzer-task) &key)
  (dbug :top "initialize-instance :after ((task libfuzzer-task): ~s" task)
  (dbug :target "~A" (with-output-to-string (str) (describe (target task) str)))
  (when (next-task (target task))
    (setf (chosen-harness-id task) (getassoc :chosen-harness-id (next-task (target task))))
    (setf (examples task) (getassoc :examples (next-task (target task))))
    (setf (llm-arg task) (getassoc :llm-arg (next-task (target task))))
    (setf (use-value-profile task) (getassoc :use-value-profile (next-task (target task)))))

  (check-type (chosen-harness-id task) keyword)
  (setf (chosen-harness-id-as-string task)
        (symbol-name (chosen-harness-id task)))

  (setf (seeds-dir task) (uiop:parse-unix-namestring
                           (format nil "~a/fuzz/seeds/~a"
                                   (shared-path (target task))
                                   (chosen-harness-id-as-string task))
                           :type :directory))


  (setf (cmd task)
        (format nil "cd ~a && /lacrosse/code/tools/lacrosse-libfuzzer-fuzz-it.py ~a ~a ~a ~a -- ~a |& LC_ALL=C tr '\\200-\\377' '\\000-\\177'"
                (path (target task))
                (chosen-harness-id-as-string task)
                (seeds-dir task)
                (format nil "~a/fuzz/libfuzzer/~a"
                        (native-namestring (probe-file (shared-path (target task))))
                        (chosen-harness-id-as-string task))
                (fuzzer-timeout task)
                (if (use-value-profile task) "-use_value_profile=1" "")))
  (dbug :top "Task cmd: ~a" (cmd task))
  (dbug :top "libfuzzer-task finished initialization"))

;; Example from mock-cp
;; [6/28/2024 18:04:29] TOP: got line [[2024-06-28 18:04:29] artifact_prefix='./'; Test unit written to ./crash-1243aa9dab7bd452506fdc4f001ce62c7e98ac73]

;; Sometimes it seems like the fuzzer can give false-positives, so we immediately create a local task
;; to test the created crashing inputs (povs) alone.  This also sets us up to have a just-run run.sh run_pov
;; which is used by filter-*sanitizer to find relevant files/commit-hunks (IOW, those scripts look for the
;; most-recently-run sanitizer output).

;; NOTE this will all need to be similar but different for Java, I expect, but this file is libfuzzer so only C

(defmethod process-line ((task libfuzzer-task) line)
  (let (ret)
    (dbug :top "got line [~A]" line)
    ;; this is a code pattern for when you want to use matched stuff:
    (cond ((setf ret (cl-ppcre:register-groups-bind (f) ("Test unit written to (.*)$" line) f))
	   ;;(send-message-to-optimi :type :bic-found :bic ret)  ;;  send msgs first, for speed
	   ;;(setassoc :bic ret (vuln-cand task))
	   (dbug :top "RESULT: Found crashing input ~A" ret)
	   ;; Note this approach to sending info to the "subtask" will not work b/c later
	   ;; subtask assignments will over-write the slot
	   ;;(setf (blobs (target task)) (list (strcat "out/" ret)))
	   ;; this takes adv of the below tasks' default of trying the first blob in target's list
	   ;; FIXME also relying on first harness only behavior
	   
	   (submit-crash task (strcat (path (target task)) "/out/" ret)
			 (nreverse (next-stack-trace task)))
	   (setf (next-stack-trace task) nil))
	  ((setf ret (cl-ppcre:register-groups-bind (addr func file line col)
			 ("#[0-9]+ (0x[0-9a-f]+) in ([^ ]*) ([^ :]*):([0-9]*):([0-9]*)" line)
		       (list addr func file line col)))
	   (push (list (first ret) (third ret) (fourth ret))
		 (next-stack-trace task)))
	  ;; this is a code pattern for when you dont want to use matched stuff:
;;;        ((setf ret (cl-ppcre:scan-to-strings "ERROR - git bisect" line))
;;;         (dbug :top "RESULT: ~a" line)
;;;          (send-message-to-optimi :type :failed-to-bisect :reason line :vuln-cand (vuln-cand task)))
	  ;; see how easy it would be to add another clause?  Dont close all the parens here
	  ) ;; end cond
    ))

(deftype submit-crash-stack-trace ()
  "like:

   ((\"0x12345678\" \"/src/foo.c\" \"1234\")
    (\"0x9abcdef0\" nil nil)
    (\"0x22341233\" \"/src/bar.c\" \"1234\"))
   "
  'list)

(declaim (ftype (function (libfuzzer-task (or pathname string) &optional submit-crash-stack-trace)
                          (values &optional))
                submit-crash))
(defun submit-crash (task crash &optional stack-trace)
  "Maybe slightly misleading now; this might enqueue a crash for triage."

  (dbug :top "submit-crash ~s ~s ~s" task crash stack-trace)
  (let ((stack-trace stack-trace))      ; SBCL doesn't like overwriting arguments.
   (unless stack-trace
     (dbug :top "missing stack-trace, not gonna triage right")
     ;; give a fake but unique stack trace
     (setf stack-trace `(("0x00defac3" ,(symbol-name (gensym)) 1))))
    (cond
      ((do-triage task)
       (push (cons (native-namestring crash) stack-trace) (crashes-to-triage task)))
      (t
       (let ((vuln-cand
               `((:target-id ,(id (target task)))
                 (:harness-id ,(chosen-harness-id task))
                 (:blob ,(native-namestring crash)))))
         (push vuln-cand (vuln-cands (target task)))
         (apply 'send-message-to-optimi
                :type :vuln-cand
                (flatten-one-level vuln-cand))))))
  (values))

(defmethod post-exec ((task libfuzzer-task))
  (dbug :top "libfuzzer-task post-exec")

  (when (do-triage task)
    ;; Keep running the fuzzer after the run-pov tasks.
    (push-next-fuzzer task)

    (dbug :top "triage has ~s crashes" (length (crashes-to-triage task)))
    ;; Triage crashes. For now, this is stupid-simple -- if we've seen a crash
    ;; whose stack trace EQUALs the another we've run-pov'd (ever), don't run-pov
    ;; it again.
    (iter
      (for (crash . stack-trace) in (crashes-to-triage task))

      ;; Don't run on crashes whose names we've seen.
      (for old-crash-by-name = (gethash crash (namestrings-of-run-povd-crashes task)))
      (when old-crash-by-name
        (dbug :top "triage says, don't run-pov ~s cause we did ~s, which had the same name"
              crash old-crash-by-name)
        (next-iteration))
      (setf (gethash crash (namestrings-of-run-povd-crashes task)) t)

      ;; Don't run on crashes whose stack traces we've seen.
      (for old-crash-by-stack = (gethash stack-trace (stack-traces-of-run-povd-crashes task)))
      (when old-crash-by-stack
        (dbug :top "triage says, don't run-pov ~s cause we did ~s, which had the same stack"
              crash old-crash-by-stack)
        (next-iteration))
      (setf (gethash stack-trace (stack-traces-of-run-povd-crashes task)) crash)

      ;; Enqueue a run-pov.
      (push (make-instance 'lacrosse-cp-run-pov-task
                           :target (target task)
                           :harness-id (chosen-harness-id task)
                           :blob (native-namestring crash))
            (tasks *self*)))
    (setf (crashes-to-triage task) nil)))

;;; to mine from fake-fuzz
;;;(defmethod post-exec ((task fake-fuzz-task))
;;;  (with-slots (target) task
;;;    (dbug :top "post-exec for fake-fuzz-task")
;;;    (dbug :top "cp-path: ~s" (cp-path target))
;;;    (let ((cp-blob-path (format nil "~a/exemplar_only/cpv_1/blobs/sample_solve.bin" (cp-path target)))
;;;          (task-blob-path (format nil "~a/blob.bin" (output-pathname task))))
;;;      (uiop:copy-file cp-blob-path task-blob-path)
;;;      (with-slots (project-properties) target
;;;        (let* ((harnesses (assoc-value :harnesses project-properties))
;;;               (harness (first harnesses))
;;;               (harness-props (rest harness)))
;;;          (macrolet ((harness-prop-value (key)
;;;                       `(or (getassoc ,key harness-props :proper t)
;;;                            (error "Failed to find value for ~s in ~s" ,key harness-props))))
;;;            (let ((vuln-cand
;;;                    `((:target-id ,(id target))
;;;                      ;; FIXME: need better logic for this!!!
;;;                      (:id 1)
;;;                   (:harness-id "id_1")
;;;                      (:harness-name ,(harness-prop-value :name))
;;;                      (:harness-source ,(harness-prop-value :source))
;;;                      (:harness-binary ,(harness-prop-value :binary))
;;;                      (:blob ,(namestring (parse-namestring task-blob-path)))
;;;                      ;;(:sanitizer-id "AddressSanitizer: global-buffer-overflow")
;;;                   (:sanitizer-id "id_1")
;;;                   )))
;;;              (push vuln-cand (vuln-cands target))
;;;              (apply 'send-message-to-optimi
;;;                     :type :vuln-cand
;;;                     (flatten-one-level vuln-cand))
;;;              (send-message-to-optimi :type :challenge-project-update
;;;                                      :vuln-cand vuln-cand
;;;                                      :target-id (id (target task))))))))))
