;;; -------------------------------------------------------------------------
;;; COPYRIGHT START
;;; COPYRIGHT END
;;; -------------------------------------------------------------------------

;;; A class for tasks that <FIXME>

(cl:in-package :fuzzbomb)

(defclass lacrosse-cp-build-task (lacrosse-task)
  (
  )
)

;; TODO: Add "-x" to the commands
(defmethod initialize-instance :after ((task lacrosse-cp-build-task) &key)
  (dbug :top "initialize-instance :after ((task lacrosse-cp-build-task): ~s" task)
  (dbug :top "(dir (target task)): ~s" (dir (target task)))
  (dbug :top "(target-makefile (target task)): ~s" (target-makefile (target task)))
  (let* (;;(repo-name (repo-name-from-repo-address (cp-address (target task))))
         ;;(cp-dir (concatenate 'string (dir (target task)) repo-name))
	 (cp-dir (dir (target task)))
         (cmds `(("cd" ,cp-dir) ("pwd") ("ls" "-l")
                 ("./run.sh" "build")))
         (cmd-str (format nil "~{~A~^ && ~}" (mapcar #'uiop:escape-sh-command cmds))))
    (dbug :top "  cmd-str: ~s" cmd-str)
    (setf (cmd task) (uiop:escape-sh-command `("bash" "-c" ,cmd-str)))))

(defmethod pre-exec ((task lacrosse-cp-build-task))
  (dbug :amp "Executing lacrosse-cp-build-task.")
  (dbug :target "(output-pathname task): ~s" (output-pathname task))
  )

;;; FIXME   What else to do here?  Set a success flag in task?
;;; Create a json file w interesting info (eg, make output)?
(defmethod post-exec ((task lacrosse-cp-build-task))
  (let ((target (target task)))
    (dbug :top "post-exec for lacrosse-cp-build-task. target: ~s" target)
    ;;(reset-file-results target)
    (let* (;;(repo-name (repo-name-from-repo-address (cp-address (target task))))
           ;;(cp-dir (concatenate 'string (dir (target task)) repo-name "/out/output/*--build/exitcode"))
	   (cp-dir (dir target))
           ;; [Pavan K: Assume the last directory in the list is the most recent directory and find "exitcode"]
           (exitcode-file (car (last (directory cp-dir :directories t))))
           ;; Exit code should be 0 for build to be successful
           (exitcode (uiop:read-file-string exitcode-file)))
      (dbug :top "Exit code file ~a for CP build contains exit code: ~a" exitcode-file exitcode)
      (cond ((string= exitcode "0")
             (dbug :top "Successfully built executable for target ~s" target)
             (send-message-to-optimi :type :challenge-project-update
                                     :target-id (id (target task))
                                     :built-p t
                                     :make-pov-blob t
                                     :comment (format nil "Successfully built executable for target ~s" target))
             )
            (t
             (dbug :top "FAILED to build executable for target ~s" target)
             (send-message-to-optimi :type :challenge-project-update
                                     :target-id (id (target task))
                                     :built-p nil
                                     :make-pov-blob nil
                                     :comment (format nil "FAILED to build executable for target ~s" target))
             )))))

(defmethod task-applies-to-target-p ((task-class-name (eql 'lacrosse-cp-build-task)) target-node)
  (dbug :docker-task "task-applies-to-target-p lacrosse-cp-build-task")
  (dbug :top "Target makefile: ~a" (target-makefile (target target-node)))

  (when (not (equalp (type-of (target target-node)) 'lacrosse-cp-target))
    ;; This is not a challenge problem
    (return-from task-applies-to-target-p nil))

  (when (built-p (target target-node))
    ;; Init build has already been done...don't redo.
    (return-from task-applies-to-target-p nil))

  (let* (;;(repo-name (repo-name-from-repo-address (cp-address (target target-node))))
         ;;(cp-dir (concatenate 'string (dir (target target-node)) repo-name))
	 (cp-dir (dir (target target-node)))
         (source-dir (source-path (target target-node))))

    ; (when source-dir
    ;   (setf source-dir (string-downcase (string (car (car source-dir)))))
    ;   (setf exec-dir (concatenate 'string cp-dir "/src/" source-dir)))

    (dbug :top "lacrosse-cp-build-task - cp dir: ~a (~a), source dir: ~a (~a)"
      cp-dir (uiop:directory-exists-p cp-dir)
      source-dir (uiop:directory-exists-p source-dir))

    ;; [Pavan K: This build task assumes that the executable has not been built before.
    ;;  This makes sense as subsequent builds should be taking in a patch file.]
    (and *lacrosse*
        source-dir
        cp-dir
        (uiop:directory-exists-p cp-dir)
        (uiop:directory-exists-p source-dir)
        (not (is-executable-in-dir source-dir)))
        ))

(defmethod process-line ((task lacrosse-cp-build-task) line)
  (dbug :top "got line [~A]" line)
  ;; FIXME anything reliable to check here?
  )

