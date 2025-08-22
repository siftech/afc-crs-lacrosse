;;; -------------------------------------------------------------------------
;;; COPYRIGHT START
;;; COPYRIGHT END
;;; -------------------------------------------------------------------------

(cl:in-package :fuzzbomb)

;;;   refactoring this class hierarchy to more
;;; clearly divide semi and final tasks is left for future work.  (TODO)
;;;   Can't inherit from libfuzzer-task w/o some
;;; rework b/c libfuzzer-task assumes chosen-harness-id is a thing.
(defclass swamp-fuzz-task (oss-fuzz-task)
  ((num-swamp-fuzz-msgs :initarg :num-swamp-fuzz-msgs
                   :initform 10
                   :accessor num-swamp-fuzz-msgs
                   :documentation "Number of fake swamp fuzz msgs to send.")
   ))

(defmethod task-applies-to-target-p ((task-class-name (eql 'swamp-fuzz-task)) target-node)
  (let ((target (target target-node)))
    (and *use-swamp-fuzz*
         (typep target 'lax-ctask)
         (next-task target)
         (let ((task-type (getassoc :task-type (next-task target))))
           (eq task-type task-class-name)))))

(defmethod initialize-instance :after ((task swamp-fuzz-task) &key)
  (let ((ctask (target task)))
    (when (next-task ctask)
      (setf (oss-fuzz-task/engine task)    (getassoc :engine    (next-task ctask))
            (oss-fuzz-task/sanitizer task) (getassoc :sanitizer (next-task ctask))
	    (oss-fuzz-task/harness task) (getassoc :harness (next-task ctask))
            (num-swamp-fuzz-msgs task) (getassoc :num-swamp-msgs (next-task ctask))))
;;;    (when (delta-ctask-p ctask)
;;;      (ensure-delta-applied ctask))
;;;    (let* ((engine (oss-fuzz-task/engine task))
;;;	   ;; Cut down on verbosity. Currently just concerned with AFL and libfuzzer.
;;;	   (env-vars (if (eq engine :afl) "-e AFL_QUIET=1 -e AFL_NO_UI=1" ""))
;;;	   (fuzzer-args (if (eq engine :libfuzzer) "-- -verbosity=0 -close_fd_mask=3" "")))
;;;      (dbug :top "run_fuzzer engine: ~A" engine)
;;;      (dbug :top "run_fuzzer env-vars: ~A" env-vars)
;;;      (dbug :top "run_fuzzer fuzzer-args: ~A" fuzzer-args)
;;;      (setf (cmd task)
;;;            (format nil "cd ~A/fuzz-tooling; DOCKER_HOST=10.0.2.2:~a /lacrosse/code/tools/timeout-with-docker-cleanup ~a /lacrosse/code/prt/timestamp infra/helper.py run_fuzzer ~A --sanitizer ~A --engine ~A ~A ~A ~A"
;;;                    (path ctask)
;;;                    (+ 50 (read-from-string (getenv "CIRCA_BASEPORT")))
;;;                    *fuzzer-timeout*
;;;		    env-vars
;;;                    (string-downcase (symbol-name (oss-fuzz-task/sanitizer task)))
;;;                    (string-downcase (symbol-name (oss-fuzz-task/engine task)))
;;;                    (project-name ctask)
;;;		    (oss-fuzz-task/harness task)
;;;		    fuzzer-args))
    (dbug :top "Task cmd: ~a" (cmd task))
    (dbug :top "oss-fuzz-task finished initialization")))
  
(defmethod pre-exec ((task swamp-fuzz-task))
  (dbug :top "~s starting pre-exec" task)
  ;; SJJ REMOVE MULTIPLE BUILDS FOR SINGLE HOST (build (target task))
;;;  (unless (oss-fuzz-task/fuzzers-built task)
;;;    (lax2-build-fuzzers (target task) (oss-fuzz-task/sanitizer task) (oss-fuzz-task/engine task))
;;;    (setf (oss-fuzz-task/fuzzers-built task) t))
;;;  (send-telemetry-event :fuzzing (list (oss-fuzz-task/sanitizer task) (oss-fuzz-task/engine task) task) (metadata (target task)))
  (dbug :top "~s finished pre-exec" task))

(defmethod process-line ((task swamp-fuzz-task) line)
  ;; Breaking the standard format here, because these contain ANSI escape
  ;; sequences that we do _not_ want to end up in our logs.
;;;(dbug :top "got line ~a" (cl-json:encode-json-to-string line))
)

(defmethod post-exec ((task swamp-fuzz-task))
  (dbug :top "~s starting post-exec" task)

  ;; Find any PoVs.
;;;  (let* ((pov-basename-wildcards
;;;           (ecase (oss-fuzz-task/engine task)
;;;             (:libfuzzer (list #p"crash-*" #p"timeout-*"))
;;;             (:afl (list #p"crashes/id*"))))
;;;         (project-out-dir
;;;           (truename
;;;             (uiop:parse-unix-namestring
;;;               (format nil "~a/fuzz-tooling/build/out/~a"
;;;                       (path (target task))
;;;                       (project-name (target task))))))
;;;         (pov-paths
;;;	  (reduce #'append
;;;		  (mapcar #'(lambda (pov-basename-wildcard)
;;;			      (directory (uiop:merge-pathnames* pov-basename-wildcard project-out-dir)))
;;;			  pov-basename-wildcards)))
;;;         (pov-paths-and-reproduce-outputs-by-stack-trace
;;;           (make-hash-table :test #'equal)))
;;;
;;;    ;; List the number of pov-paths found
;;;    (dbug :top "Num pov-paths found: ~a." (length pov-paths))
;;;    (dolist (pov-path pov-paths)
;;;      (dbug :top "- ~s" pov-path))
;;;    
;;;    ;; If we weren't libfuzzer-based, we need to rebuild to be libfuzzer-based.
;;;    (when (and pov-paths (not (eql (oss-fuzz-task/engine task) :libfuzzer)))
;;;      (dbug :top "rebuilding to use libfuzzer, so we can reproduce...")
;;;      (lax2-build-fuzzers (target task) (oss-fuzz-task/sanitizer task) :libfuzzer))

;;;    ;; Try and reproduce each one.
;;;    (iter
;;;      (for pov-path in pov-paths)
;;;      (dbug :top "trying to reproduce ~s" pov-path)
;;;
;;;      (for reproduce-exit-code = nil)
;;;      ;; Run the reproducer.
;;;      (for reproduce-output =
;;;	    (with-output-to-string (*standard-output*)
;;;	      (multiple-value-bind (stdout stderr exit-code)
;;;		  (lax2-helper (target task) `("reproduce"
;;;					       ,(project-name (target task))
;;;					       ,(oss-fuzz-task/harness task)
;;;					       ,(uiop:unix-namestring pov-path))
;;;			       :ignore-error-status t)
;;;		(declare (ignore stdout stderr))
;;;		(setf reproduce-exit-code exit-code))))
;;;      
;;;      (dbug :top "reproduce output: ~a" (cl-json:encode-json-to-string reproduce-output))
;;;      (dbug :top "reproduce exit code: ~d" reproduce-exit-code)
;;;
;;;      (for crash-reproduced = (not (equal reproduce-exit-code 0)))
;;;      
;;;      (if crash-reproduced
;;;	(dbug :top "reproduce exited with non-zero exit status. PoV successfully reproduced.")
;;;	(progn
;;;	  (dbug :top "reproduce exited with zero exit status. PoV failed to reproduce. Continue without adding stack trace.")
;;;	  (next-iteration))) ;; abort early if crash did not reproduce
;;;      
;;;      (dbug :top "Parse stack trace from reproduce output...")
;;;      (for stack-trace = nil)
;;;      ;; Try parsing out the stack trace.
;;;      (let ((stack-frame-regex "#[0-9]+ (0x[0-9a-f]+) in (.*) ([^ ]*):([0-9]*):([0-9]*)"))
;;;        (cl-ppcre:do-register-groups (address function-name file-name line-number column-number)
;;;                                     (stack-frame-regex reproduce-output)
;;;                                     (declare (ignore address))
;;;                                     (push (list function-name file-name line-number column-number)
;;;                                           stack-trace))
;;;        (setf stack-trace (nreverse stack-trace)))
;;;
;;;      ;; If we didn't parse out a stack trace, make a unique one up.
;;;      (if stack-trace
;;;	  (dbug :top "Successfully parsed stack trace:~%~s" stack-trace)
;;;          (progn (dbug :top "Making up a fake stack trace to avoid messing up triage...")
;;;		 (setf stack-trace `((,(symbol-name (gensym)) "/dev/null" 0 0)))))
;;;
;;;      ;; Note the stack trace as seen, overriding any previous one with the
;;;      ;; same stack trace.
;;;      (setf (gethash stack-trace pov-paths-and-reproduce-outputs-by-stack-trace)
;;;            (cons pov-path reproduce-output)))
;;;
;;;    ;; Announce the unique PoVs we're about to send off.
;;;    (dbug :top "Send off ~a reproduced pov paths with unique stack traces..." (hash-table-count pov-paths-and-reproduce-outputs-by-stack-trace))
;;;    (iter
;;;      (for (stack-trace (pov-path . reproduce-output)) in-hashtable pov-paths-and-reproduce-outputs-by-stack-trace)
;;;      (for i from 1)
;;;      (declare (ignorable stack-trace))
;;;      (let ((reproduce-path (format nil "~a.log" pov-path)))
;;;	(dbug :top "*******************************************")
;;;	(dbug :top "(~a) ~s" i pov-path)
;;;	(dbug :top "reproduce path: ~s" reproduce-path)
;;;	(dbug :top "reproduce output:")
;;;	(dbug :top "~s" reproduce-output)))
;;;
;;;    ;; Send off each reproduced PoV.
;;;    (iter
;;;      (for (stack-trace (pov-path . reproduce-output)) in-hashtable pov-paths-and-reproduce-outputs-by-stack-trace)
;;;      (declare (ignorable stack-trace))
;;;      (let ((reproduce-path (format nil "~a.log" pov-path)))
;;;        (alexandria:write-string-into-file reproduce-output (uiop:parse-unix-namestring reproduce-path)
;;;                                           :if-exists :overwrite
;;;                                           :if-does-not-exist :create)
  (dotimes (i (num-swamp-fuzz-msgs task))
    ;; First pass, empty msgs.  
    (send-message-to-optimi
     :type :swamp-fuzz-msg
     :target-id (id (target task))
     ;;:blob (uiop:unix-namestring pov-path)
     ;;:reproduce-path reproduce-path
     :sanitizer (oss-fuzz-task/sanitizer task)
     :engine (oss-fuzz-task/engine task)
     :harness (oss-fuzz-task/harness task)))

;; Continue fuzzing.
;;(prep-for-restart task)
;;)

  (dbug :top "~s finished post-exec" task))

;;;(defmethod prep-for-restart ((task oss-fuzz-task))
;;;  "Setup task to restart."
;;;  (incf (restarter-restart-count task))
;;;  (push task (tasks *self*)))

;;;(defmethod scoring-fn ((task oss-fuzz-task))
;;;  (dbug :top "oss-fuzz-task scoring:  FIXEDscore: ~s  restart-count: ~s" 500 (restarter-restart-count task))
;;;  500)
;;; Set a small, positive min, use ceiling so that score does not change every restart.
(defmethod scoring-fn ((task swamp-fuzz-task))
  (let ((score 950))
    (dbug :top "swamp-fuzz-task scoring:  score: ~s" score)
    score))
