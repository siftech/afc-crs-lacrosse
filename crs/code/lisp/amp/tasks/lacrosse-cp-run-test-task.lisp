;;; -------------------------------------------------------------------------
;;; COPYRIGHT START
;;; COPYRIGHT END
;;; -------------------------------------------------------------------------

;;; A class for tasks that <FIXME>

(cl:in-package :fuzzbomb)

(defclass lacrosse-cp-run-test-task (lacrosse-task)
  (
  )
)


(defmethod initialize-instance :after ((task lacrosse-cp-run-test-task) &key)
  (dbug :top "initialize-instance :after ((task lacrosse-cp-run-test-task): ~s" task)
  (dbug :top "(dir (target task)): ~s" (dir (target task)))
  (dbug :top "(target-makefile (target task)): ~s" (target-makefile (target task)))
  (let* (;;(repo-name (repo-name-from-repo-address (cp-address (target task))))
         ;;(cp-dir (concatenate 'string (dir (target task)) repo-name))
	 (cp-dir (dir (target task)))
         (cmds `(("cd" ,cp-dir)
                 ("./run.sh" "run_tests")))
         (cmd-str (format nil "~{~A~^ && ~}" (mapcar #'uiop:escape-sh-command cmds))))
    (dbug :top "  cmd-str: ~s" cmd-str)
    (setf (cmd task) (uiop:escape-sh-command `("bash" "-c" ,cmd-str)))))

(defmethod pre-exec ((task lacrosse-cp-run-test-task))
  (dbug :amp "Executing lacrosse-cp-run-test-task.")
  (dbug :target "(output-pathname task): ~s" (output-pathname task))
  )

;;; FIXME   What else to do here?  Set a success flag in task?
;;; Create a json file w interesting info (eg, make output)?
(defmethod post-exec ((task lacrosse-cp-run-test-task))
  (let ((target (target task)))
    (dbug :top "post-exec for lacrosse-cp-run-test-task. target: ~s" target)
    ;;(reset-file-results target)
    (let* (;;(repo-name (repo-name-from-repo-address (cp-address (target task))))
           ;;(cp-dir (concatenate 'string (dir (target task)) repo-name "/out/output/*--run_tests/exitcode"))
	   (cp-dir (dir target))
           ;; [Pavan K: Assume the last directory in the list is the most recent directory and find "exitcode"]
           (exitcode-file (car (last (directory cp-dir :directories t))))
           ;; Exit code should be 0 for build to be successful
           (exitcode (uiop:read-file-string exitcode-file)))
      (dbug :top "Exit code file ~a for CP build contains exit code: ~a" exitcode-file exitcode)

      (cond ((string= exitcode "0")
             (dbug :top "Successfully ran test on executable for target ~s" target)
             (send-message-to-optimi :type :challenge-project-update
                                     :target-id (id (target task))
                                     :run-tests-p nil ;; Do not run the test again until a new patch has been made...
                                     :comment (format nil "Successfully ran test on executable for target ~s" target))
             )
            (t
             (dbug :top "FAILED to run test on executable for target ~s" target)
             (send-message-to-optimi :type :challenge-project-update
                                     :target-id (id (target task))
                                     :run-tests-p nil ;; Do not run the test again until a new patch has been made...
                                     :comment (format nil "FAILED to run test on executable for target ~s" target))
             )))))

(defmethod task-applies-to-target-p ((task-class-name (eql 'lacrosse-cp-run-test-task)) target-node)
  (declare (ignore target-node))
  nil)

;; (dbug :docker-task "task-applies-to-target-p lacrosse-cp-run-test-task")
;;   (dbug :top "Target makefile: ~a" (target-makefile (target target-node)))

;;   (and
;;    ;; This is a challenge problem
;;    (equalp (type-of (target target-node)) 'lacrosse-cp-target)
;;     ;; We have no reason to run tests yet.
;;     (return-from task-applies-to-target-p nil))

;;   (let* (;;(repo-name (repo-name-from-repo-address (cp-address (target target-node))))
;;          ;;(cp-dir (concatenate 'string (dir (target target-node)) repo-name))
;; 	 (cp-dir (dir (target target-node)))
;;          (source-dir (source-path (target target-node))))

;;     (dbug :top "lacrosse-cp-run-test-task - cp dir: ~a (~a), source dir: ~a (~a)"
;;       cp-dir (uiop:directory-exists-p cp-dir)
;;       source-dir (uiop:directory-exists-p source-dir))
;;     (dbug :top "lacrosse-cp-run-test-task - Is there an executable in the source directory: ~a"
;;       (is-executable-in-dir source-dir))

;;     ;; We need to ensure that testing is done after a successful build 
;;     (and *lacrosse*
;;         cp-dir
;;         source-dir
;;         (uiop:directory-exists-p cp-dir)
;;         (uiop:directory-exists-p source-dir)
;;         (is-executable-in-dir source-dir))))

(defmethod process-line ((task lacrosse-cp-run-test-task) line)
  (dbug :top "got line [~A]" line)
  ;; FIXME anything reliable to check here?
  )

(defun is-executable-in-dir (dir)
  ; (dbug :top "Using the following directory: ~a" dir)
  (loop for file in (uiop:directory-files (concatenate 'string dir "/")) do
    (let ((file-desc (uiop:run-program (list "file" "--brief" (namestring file)) :output :string)))
      ; (dbug :top "File ~a with description: ~a" file file-desc)
      (when (search "executable" file-desc)
        (return-from is-executable-in-dir T)))))
