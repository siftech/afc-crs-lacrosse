;;; -------------------------------------------------------------------------
;;; COPYRIGHT START
;;; COPYRIGHT END
;;; -------------------------------------------------------------------------

;;; A class for tasks that <FIXME>

(cl:in-package :fuzzbomb)

(defclass lacrosse-cp-build-patch-task (lacrosse-task)
  (
   (patch-filepath :initform nil :type (or null string) :accessor patch-filepath)
   (patch-failed :initform nil :type boolean :accessor patch-failed)
  ;  (patch-pathname :accessor patch-pathname)
  )
)


(defmethod initialize-instance :after ((task lacrosse-cp-build-patch-task) &key)
  (dbug :top "initialize-instance :after ((task lacrosse-cp-build-patch-task): ~s" task)
  (dbug :top "(dir (target task)): ~s" (dir (target task)))
  (dbug :top "(target-makefile (target task)): ~s" (target-makefile (target task)))
  (let* (;;(repo-name (repo-name-from-repo-address (cp-address (target task))))
         ;;(cp-dir (concatenate 'string (dir (target task)) repo-name))
         (cp-dir (dir (target task)))
         (source-dir-name (string (car (car (cp-prop (target task) :cp_sources)))))
         (source-rel-path (concatenate 'string "/" source-dir-name))
         (patch-file-to-apply (car (patches (target task))))
        ;  (patch-file-to-apply (concatenate 'string cp-dir "/exemplar_only/cpv_1/patches/samples/good_patch.diff"))
         (cmds `(("cd" ,cp-dir)
                 ("./run.sh" "build" ,patch-file-to-apply ,source-rel-path)))
         (cmd-str (format nil "~{~A~^ && ~}" (mapcar #'uiop:escape-sh-command cmds))))
    (dbug :top "  cmd-str: ~s" cmd-str)
    (setf (cmd task) (uiop:escape-sh-command `("bash" "-c" ,cmd-str)))))

(defmethod pre-exec ((task lacrosse-cp-build-patch-task))
  (dbug :amp "Executing lacrosse-cp-build-patch-task.")
  (dbug :target "(output-pathname task): ~s" (output-pathname task))
  )

;;; FIXME   What else to do here?  Set a success flag in task?
;;; Create a json file w interesting info (eg, make output)?
(defmethod post-exec ((task lacrosse-cp-build-patch-task))
  (let ((target (target task)))
    (dbug :top "post-exec for lacrosse-cp-build-patch-task. target: ~s" target)
    ;;(reset-file-results target)
    ;; [Pavan K: We need to search for the most recent directory and check if it's a build dir...]
    (let* (;;(repo-name (repo-name-from-repo-address (cp-address (target task))))
           ;;(cp-dir (concatenate 'string (dir (target task)) repo-name "/out/output/*--build/exitcode"))
           (cp-dir (dir (target task)))
           ;; [Pavan K: Assume the last directory in the list is the most recent directory and find "exitcode"]
           (exitcode-file (car (last (directory cp-dir :directories t))))
           ;; Exit code should be 0 for build to be successful
           (exitcode (uiop:read-file-string exitcode-file)))
      (dbug :top "Exit code file ~a for CP build with patch contains exit code: ~a" exitcode-file exitcode)
      (cond ((and (not (slot-value task 'patch-failed)) (string= exitcode "0"))
             (dbug :top "Successfully built executable with patch for target ~s" target)
             (send-message-to-optimi :type :challenge-project-update
                                     :target-id (id (target task))
                                     :run-tests-p t
                                     :build-with-patch nil
                                     :comment (format nil "Successfully built executable with patch for target ~s" target))
             )
            (t
             (dbug :top "FAILED to build executable with patch for target ~s" target)
             (send-message-to-optimi :type :challenge-project-update
                                     :target-id (id (target task))
                                     :make-patch T
                                     :build-with-patch nil
                                     :comment (format nil "FAILED to build executable with patch for target ~s" target))
             )))))

(defmethod task-applies-to-target-p ((task-class-name (eql 'lacrosse-cp-build-patch-task)) target-node)
  (declare (ignore target-node))
  nil)

;; (dbug :docker-task "task-applies-to-target-p lacrosse-cp-build-patch-task")
;;   (dbug :top "Target makefile: ~a" (target-makefile (target target-node)))

;;   (when (not (equalp (type-of (target target-node)) 'lacrosse-cp-target))
;;     ;; This is not a challenge problem
;;     (return-from task-applies-to-target-p nil))

;;   (when (not (build-with-patch (target target-node)))
;;     ;; Do not use this unless a patch has been made.
;;     (return-from task-applies-to-target-p nil))

;;   (let* (;;(repo-name (repo-name-from-repo-address (cp-address (target target-node))))
;;          (source-dir (source-path (target target-node)))

;;          ;;(cp-dir (concatenate 'string (dir (target target-node)) repo-name))
;;       (cp-dir (dir (target target-node)))
;;          ;; For now, consider this the patch. We just need this for testing purposes.
;;          (patch-file (car (patches (target target-node))))
;;          )

;;     (dbug :top "lacrosse-cp-build-patch-task - cp dir: ~a (~a), source dir: ~a (~a)"
;;       cp-dir (uiop:directory-exists-p cp-dir)
;;       source-dir (uiop:directory-exists-p source-dir))
;;     (dbug :top "lacrosse-cp-build-patch-task - Is there an executable in the source directory: ~a"
;;       (is-executable-in-dir source-dir))
;;     (dbug :top "lacrosse-cp-build-patch-task - Patch file: ~a (~a)"
;;       patch-file
;;       (uiop:file-exists-p patch-file))

;;     (and *lacrosse*
;;         cp-dir
;;         source-dir
;;         patch-file
;;         (uiop:file-exists-p patch-file)
;;         (uiop:directory-exists-p cp-dir)
;;         (uiop:directory-exists-p source-dir)
;;         )))

(defmethod process-line ((task lacrosse-cp-build-patch-task) line)
  (dbug :top "got line [~A]" line)
  ;; FIXME anything reliable to check here?
  (when (search "Patching failed using" line)
    (dbug :top "Error found while patching file....")
    (setf (slot-value task 'patch-failed) T))
  )
