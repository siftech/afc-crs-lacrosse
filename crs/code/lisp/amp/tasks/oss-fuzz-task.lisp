;;; -------------------------------------------------------------------------
;;; COPYRIGHT START
;;; COPYRIGHT END
;;; -------------------------------------------------------------------------

(cl:in-package :fuzzbomb)

;;;   refactoring this class hierarchy to more
;;; clearly divide semi and final tasks is left for future work.  (TODO)
;;;   Can't inherit from libfuzzer-task w/o some
;;; rework b/c libfuzzer-task assumes chosen-harness-id is a thing.
(defclass oss-fuzz-task (lacrosse-task)
  ((sanitizer :type (member :address :none :memory :undefined :thread :coverage
                            :introspector :hwaddress)
              :initarg :sanitizer
              :initform :address
              :accessor oss-fuzz-task/sanitizer
              :documentation "The sanitizer to build the harness with. The type indicates all the sanitizers implemented by OSS-Fuzz, but some of them won't work on our hardware (e.g. HWAsan) or aren't used for the competition (CovSan).")
   (engine :type (member :libfuzzer :afl :honggfuzz :centipede :none :wycheproof)
           :initarg :engine
           :initform :libfuzzer
           :accessor oss-fuzz-task/engine
           :documentation "The fuzzing engine to use.")
   (harness :type (or null string)
            :initarg :harness
            :initform nil
            :accessor oss-fuzz-task/harness
            :documentation "The harness to use.")
   (fuzzers-built :initarg :fuzzers-built
		  :initform nil
		  :accessor oss-fuzz-task/fuzzers-built
		  :documentation "Did build_fuzzers run.")
   ;; Maybe someday the restarting-task stuff is a mixin, but in lax this fuzzer is uniquely restarting.  
   (restarter-default-score :initarg :restarter-default-score
			    :initform 500
			    :accessor restarter-default-score
			    :documentation "Unadjusted score/priority for this task.")
   (restarter-score-factor :initarg :restarter-score-factor
			   :initform 0.2
			   :accessor restarter-score-factor
			   :documentation "Factor multiplied by number of restarts to adjust score/priority.")
   (restarter-restart-count :initform 0
			    :accessor restarter-restart-count
			    :documentation "Number of times this task has restarted.")
   ;; FIXME min magic number in scoring-fn should be a slot, restarter-restart-min-score  .
   ))

(defmethod task-applies-to-target-p ((task-class-name (eql 'oss-fuzz-task)) target-node)
  (let ((target (target target-node)))
    (and *use-oss-fuzz*
         (typep target 'lax-ctask)
         (next-task target)
         (let ((task-type (getassoc :task-type (next-task target))))
           (eq task-type task-class-name)))))

(defmethod initialize-instance :after ((task oss-fuzz-task) &key)
  (let ((ctask (target task)))
    (when (next-task ctask)
      (setf (oss-fuzz-task/engine task)    (getassoc :engine    (next-task ctask))
            (oss-fuzz-task/sanitizer task) (getassoc :sanitizer (next-task ctask))
	    (oss-fuzz-task/harness task) (getassoc :harness (next-task ctask))
            (restarter-score-factor task) (getassoc :restarter-score-factor (next-task ctask))))
    (let* ((engine (oss-fuzz-task/engine task))
	   ;; Cut down on verbosity. Currently just concerned with AFL and libfuzzer.
	   (env-vars (if (eq engine :afl) "-e AFL_QUIET=1 -e AFL_NO_UI=1" ""))
	   (fuzzer-args (if (eq engine :libfuzzer) "-- -verbosity=0 -close_fd_mask=3" "")))
      (dbug :top "run_fuzzer engine: ~A" engine)
      (dbug :top "run_fuzzer env-vars: ~A" env-vars)
      (dbug :top "run_fuzzer fuzzer-args: ~A" fuzzer-args)
      (setf (cmd task)
            (format nil "cd ~A/fuzz-tooling; DOCKER_HOST=10.0.2.2:~a /lacrosse/code/tools/timeout-with-docker-cleanup ~a /lacrosse/code/prt/timestamp infra/helper.py run_fuzzer ~A --sanitizer ~A --engine ~A ~A ~A ~A"
                    (path ctask)
                    (+ 50 (read-from-string (getenv "CIRCA_BASEPORT")))
                    *fuzzer-timeout*
		    env-vars
                    (string-downcase (symbol-name (oss-fuzz-task/sanitizer task)))
                    (string-downcase (symbol-name (oss-fuzz-task/engine task)))
                    (project-name ctask)
		    (oss-fuzz-task/harness task)
		    fuzzer-args))
      (dbug :top "Task cmd: ~a" (cmd task))
      (dbug :top "oss-fuzz-task finished initialization"))))
  
(defmethod pre-exec ((task oss-fuzz-task))
  (dbug :top "~s starting pre-exec" task)
  (when (delta-ctask-p (target task))
    (ensure-delta-applied (target task)))
  (unless (and (oss-fuzz-task/fuzzers-built task)
	       (harness-exists-p task))
    (lax2-build-fuzzers (target task) (oss-fuzz-task/sanitizer task) (oss-fuzz-task/engine task))
    (setf (oss-fuzz-task/fuzzers-built task) t))
  (send-telemetry-event :fuzzing (list (oss-fuzz-task/sanitizer task) (oss-fuzz-task/engine task) task) (metadata (target task)))
  (dbug :top "~s finished pre-exec" task))

(defmethod process-line ((task oss-fuzz-task) line)
  ;; Breaking the standard format here, because these contain ANSI escape
  ;; sequences that we do _not_ want to end up in our logs.
  (dbug :top "got line ~a" (cl-json:encode-json-to-string line)))

(defmethod reproduce ((task oss-fuzz-task) pov-path)
  (let* ((reproduce-exit-code nil)
	 (dind-error nil)
	 (fuzz-tooling-dir (format nil "~a/fuzz-tooling" (path (target task))))
	 ;; Run the reproducer.
	 (reproduce-output
	  (with-output-to-string (*standard-output*)
	    (multiple-value-bind (stdout stderr exit-code dind-error)
		(lax2-run-pov (target task) `("-p" ,(project-name (target task))
						   "-o" ,fuzz-tooling-dir
						   "-b" ,(uiop:unix-namestring pov-path)
						   "-e" "libfuzzer"
						   "-f" ,(oss-fuzz-task/harness task)
						   "-s" ,(string-downcase (symbol-name (oss-fuzz-task/sanitizer task))))
			      :ignore-error-status t)
	      (declare (ignore stdout stderr))
              (cond (dind-error
		     (setf reproduce-exit-code 0))
		    (t (setf reproduce-exit-code exit-code)))))))
    (values reproduce-exit-code reproduce-output dind-error)))

;; This gives back a new alist with the updated exit code counts
(defun update-exit-code-count-map (code-map exit-code)
  (let ((entry (assoc exit-code code-map)))
    (cond (entry (alist-update-new code-map (car entry) (+ (cdr entry) 1)))
	  (t (cons (cons exit-code 1) code-map)))))

;; This gives back a new alist with the updated exit code counts
(defun update-exit-code-outputs-map (code-map exit-code outputs)
  (let ((entry (assoc exit-code code-map)))
    (cond (entry code-map) ; pass it back unchanged if we already have outputs recorded for this exit code
	  (t (cons (cons exit-code outputs) code-map)))))
	  
(defmethod reproduce-n-times ((task oss-fuzz-task) pov-path)
  (dbug :top "reproduce-n-times")
  (let ((no-repro-exit-code 202) ; legit no crash
	(repro-exit-code 0) ; legit crash
	(error-exit-code 201) ; non-legit crash
	(no-repros-required 1) ; just need one no-crash to call it a no-crash
	(repros-required 2)  ; how many is enough reproductions of the error?
	(errors-required 5)) ; when do we give up and err out?
    (do* ((exit-code -1 exit-code)
	  (outputs nil outputs)
	  (dind-error nil dind-error)
	  (i 0 (+ i 1))
	  (exit-code-outputs nil (update-exit-code-outputs-map exit-code-outputs exit-code outputs))
	  (repro-count 0 (cond ((= exit-code repro-exit-code) (incf repro-count)) (t repro-count)))
	  (no-repro-count 0 (cond ((= exit-code no-repro-exit-code) (incf no-repro-count)) (t no-repro-count)))
	  (error-count 0 (cond ((= exit-code error-exit-code) (incf error-count)) (t error-count)))
	  (final-code nil (cond ((>= no-repro-count no-repros-required) ;; if we ever don't reproduce, treat it as not real right away
				 no-repro-exit-code)
				((>= repro-count repros-required)
				 repro-exit-code)
				((>= error-count errors-required)
				 error-exit-code)))
	  (final-outputs nil (and final-code (cdr (assoc final-code exit-code-outputs)))))
	 (final-code (values final-code final-outputs))
      (dbug :top "reproduce-n-times iteration ~s..." i)
      ;;(dbug :top "reproduce-n-times exit-code counts are ~s" exit-code-counts)
      (when (or (= exit-code error-exit-code) dind-error)
	(dbug :top "reproduce-n-times found error exit-code ~s or dind-error ~s. Delaying up to 30 sec." exit-code dind-error)
	(sleep (random 30)))
      (multiple-value-setq (exit-code outputs dind-error) ;; outputs here is the stdout
			   (reproduce task pov-path))
      (dbug :top "reproduce-n-times: reproduce returned ~d ~s ~s" exit-code (length outputs) dind-error)
      (dbug :top "outputs: ~s" outputs)
      )))

(defvar *povpaths-already-tried* nil)

(defmethod post-exec ((task oss-fuzz-task))
  (dbug :top "~s starting post-exec" task)
  ;; Find any PoVs.
  (let* ((pov-basename-wildcards
           (ecase (oss-fuzz-task/engine task)
             (:libfuzzer (list #p"crash-*" #p"timeout-*"))
             (:afl (list #p"crashes/id*"))))
         (project-out-dir
           (truename
             (uiop:parse-unix-namestring
               (format nil "~a/fuzz-tooling/build/out/~a"
                       (path (target task))
                       (project-name (target task))))))
         (pov-paths
	  (reduce #'append
		  (mapcar #'(lambda (pov-basename-wildcard)
			      (directory (uiop:merge-pathnames* pov-basename-wildcard project-out-dir)))
			  pov-basename-wildcards)))
         (pov-paths-and-reproduce-outputs-by-stack-trace
           (make-hash-table :test #'equal)))

    ;; List the number of pov-paths found
    (dbug :top "Total num pov-paths found: ~a." (length pov-paths))
    (setf pov-paths (set-difference pov-paths *povpaths-already-tried* :test #'equal))
    (dbug :top "Num new pov-paths found: ~a." (length pov-paths))
    (dolist (pov-path pov-paths)
      (dbug :top "- ~s" pov-path))
    
    ;; If we weren't libfuzzer-based, we need to rebuild to be libfuzzer-based.
    (when (and pov-paths (not (eql (oss-fuzz-task/engine task) :libfuzzer)))
      (dbug :top "rebuilding to use libfuzzer, so we can reproduce...")
      (lax2-build-fuzzers (target task) (oss-fuzz-task/sanitizer task) :libfuzzer))

    ;; Try and reproduce each one.
    (iter
     (for pov-path in pov-paths)
     (dbug :top "trying to reproduce ~s" pov-path)
     (push pov-path *povpaths-already-tried*)

     (for (values reproduce-exit-code reproduce-output) = (reproduce-n-times task pov-path))
      
      (dbug :top "reproduce output: ~a" (cl-json:encode-json-to-string reproduce-output))
      (dbug :top "reproduce exit code: ~d" reproduce-exit-code)

      (for crash-reproduced = (= reproduce-exit-code 0))
      
      (if crash-reproduced
	  (dbug :top "reproduce exited with zero exit status. PoV successfully reproduced.")
	;; else
	(progn
	  (dbug :top "reproduce exited with non-zero exit status. PoV failed to reproduce. Continue without adding stack trace.")
	  (next-iteration))) ;; abort early if crash did not reproduce
      
      (dbug :top "Parse stack trace from reproduce output...")
      (for stack-trace = nil)
      ;; Try parsing out the stack trace.
      (let ((stack-frame-regex "#[0-9]+ (0x[0-9a-f]+) in (.*) ([^ ]*):([0-9]*):([0-9]*)"))
        (cl-ppcre:do-register-groups (address function-name file-name line-number column-number)
                                     (stack-frame-regex reproduce-output)
                                     (declare (ignore address))
                                     (push (list function-name file-name line-number column-number)
                                           stack-trace))
        (setf stack-trace (nreverse stack-trace)))

      ;; If we didn't parse out a stack trace, make a unique one up.
      (if stack-trace
	  (dbug :top "Successfully parsed stack trace:~%~s" stack-trace)
          (progn (dbug :top "Making up a fake stack trace to avoid messing up triage...")
		 (setf stack-trace `((,(symbol-name (gensym)) "/dev/null" 0 0)))))

      ;; Note the stack trace as seen, overriding any previous one with the
      ;; same stack trace.
      (setf (gethash stack-trace pov-paths-and-reproduce-outputs-by-stack-trace)
            (cons pov-path reproduce-output)))

    ;; Announce the unique PoVs we're about to send off.
    (dbug :top "Send off ~a reproduced pov paths with unique stack traces..." (hash-table-count pov-paths-and-reproduce-outputs-by-stack-trace))
    (iter
      (for (stack-trace (pov-path . reproduce-output)) in-hashtable pov-paths-and-reproduce-outputs-by-stack-trace)
      (for i from 1)
      (declare (ignorable stack-trace))
      (let ((reproduce-path (format nil "~a.log" pov-path)))
	(dbug :top "*******************************************")
	(dbug :top "(~a) ~s" i pov-path)
	(dbug :top "reproduce path: ~s" reproduce-path)
	(dbug :top "reproduce output:")
	(dbug :top "~s" reproduce-output)))

    ;; Send off each reproduced PoV.
    (iter
      (for (stack-trace (pov-path . reproduce-output)) in-hashtable pov-paths-and-reproduce-outputs-by-stack-trace)
      (declare (ignorable stack-trace))
      (cond ((and (delta-ctask-p (target task))
                  (not (zerop (reproduce-for-delta task pov-path))))
             (dbug :top "Failing pov b/c it crashes base."))
            (t
             (let ((reproduce-path (format nil "~a.log" pov-path)))
               (alexandria:write-string-into-file reproduce-output (uiop:parse-unix-namestring reproduce-path)
                                                  :if-exists :overwrite
                                                  :if-does-not-exist :create)
               
               (send-message-to-optimi
                :type :vuln-cand
                :target-id (id (target task))
                :blob (uiop:unix-namestring pov-path)
                :reproduce-path reproduce-path
                :sanitizer (oss-fuzz-task/sanitizer task)
                :engine (oss-fuzz-task/engine task)
                :harness (oss-fuzz-task/harness task))))))
      
    ;; Continue fuzzing.
    (prep-for-restart task)
    )

  (dbug :top "~s finished post-exec" task))

(defmethod prep-for-restart ((task oss-fuzz-task))
  "Setup task to restart."
  (incf (restarter-restart-count task))
  (push task (tasks *self*)))

;;;(defmethod scoring-fn ((task oss-fuzz-task))
;;;  (dbug :top "oss-fuzz-task scoring:  FIXEDscore: ~s  restart-count: ~s" 500 (restarter-restart-count task))
;;;  500)
;;; Set a small, positive min, use ceiling so that score does not change every restart.
(defmethod scoring-fn ((task oss-fuzz-task))
  (let ((score
	 (max 
	  (ceiling (- (restarter-default-score task)
		      (* (restarter-score-factor task)
			 (restarter-restart-count task))))
          ;; FIXME min magic number in scoring-fn should be a slot, restarter-restart-min-score  .
	  10
	  )))
    (dbug :top "oss-fuzz-task scoring:  score: ~s  restart-count: ~s" score (restarter-restart-count task))
    score))

(defmethod harness-exists-p ((task oss-fuzz-task))
  ;; command.extend(['/bin/bash', '-c', 'test -f /out/%s' % fuzzer_name])
  (let ((out-dir (format nil "~a/fuzz-tooling/build/out/~a/" (path (target task)) (project-name (target task)))))
    (multiple-value-bind (stdout stderr code)
        (uiop:run-program (format nil "test -f ~a/~a" out-dir (oss-fuzz-task/harness task))
                          :output :string
                          :error-output :output
                          :ignore-error-status t)
      (declare (ignorable stdout stderr))
      (dbug :top "harness-exists-p: ~s ~s" task (oss-fuzz-task/harness task))
      ;;(dbug :top "  stdout: ~s" stdout)
      ;;(dbug :top "  stderr: ~s" stderr)
      ;;(dbug :top "  code: ~s" code)
      (= code 0))))

(defmethod reproduce-for-delta ((task oss-fuzz-task) pov-path)
  (dbug :top "reproduce-for-delta: fuzz-tooling-dir: ~s" (format nil "~a/fuzz-tooling" (cp-path (target task))))
  (let* ((reproduce-exit-code nil)
	 (dind-error nil)
	 (fuzz-tooling-dir (format nil "~a/fuzz-tooling" (cp-path (target task))))
	 ;; Run the reproducer.
	 (reproduce-output
	  (with-output-to-string (*standard-output*)
	    (multiple-value-bind (stdout stderr exit-code dind-error)
		(lax2-run-pov (target task) `("-x"
                                              "-p" ,(project-name (target task))
						   "-o" ,fuzz-tooling-dir
						   "-b" ,(uiop:unix-namestring pov-path)
						   "-e" "libfuzzer"
						   "-f" ,(oss-fuzz-task/harness task)
						   "-s" ,(string-downcase (symbol-name (oss-fuzz-task/sanitizer task))))
			      :ignore-error-status t)
	      (declare (ignore stdout stderr))
              (cond (dind-error
		     (setf reproduce-exit-code 0))
		    (t (setf reproduce-exit-code exit-code)))))))
    (dbug :dev "reproduce-for-delta output: ~s" reproduce-output)
    (values reproduce-exit-code reproduce-output dind-error)))
