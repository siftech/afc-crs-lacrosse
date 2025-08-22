;;; -------------------------------------------------------------------------
;;; COPYRIGHT START
;;; COPYRIGHT END
;;; -------------------------------------------------------------------------

;;; A class for tasks that <FIXME>

(cl:in-package :fuzzbomb)

(defclass lacrosse-test-patch-task (lacrosse-task)
  (
   (build-id :accessor build-id)
   (patch-pathname :accessor patch-pathname)
  )
)

(defmethod initialize-instance :after ((task lacrosse-test-patch-task) &key)
  (dbug :top "initialize-instance :after ((task lacrosse-test-patch-task): ~s" task)
  (dbug :top "(dir (target task)): ~s" (dir (target task)))
  (dbug :top "(target-makefile (target task)): ~s" (target-makefile (target task)))
  ;;   FIXME build-id will be computed.
  (setf (build-id task) 42)

  ;; read result data and populate task object
  (let ((patch-result (some-patch (target task))))
    (dbug :target "patch-result-path: ~s" patch-result)
    (let ((patch-result-data (cl-json:decode-json-from-source patch-result)))
      (dbug :target "patch-result-data: ~s" patch-result-data)
      (dbug :target "(getassoc :patch-filename patch-result-data :proper t): ~s" (getassoc :patch-filename patch-result-data :proper t))
      ;;(dbug :target "(cdr (getassoc :patch-filename patch-result-data)): ~s" (cdr (getassoc :patch-filename patch-result-data)))
      (let* ((patch-filename (getassoc :patch-filename patch-result-data :proper t))
             (patch-pathname (merge-pathnames patch-filename (output-pathname task)))
             )
        (dbug :target "patch-filename: ~s" patch-filename)
        (dbug :target "patch-pathname: ~s" patch-pathname)
        (setf (patch-pathname task) patch-pathname)
        
        (let* ((cmds `(("cd" ,(namestring (create-build-pathname (target task) (build-id task))))
                       ("patch" "-i" ,(namestring (patch-pathname task)))
                       ("make")))
               (ignore1 (dbug :target "cmds: ~s" cmds))
               (cmd-str (format nil "~{~A~^ && ~}" (mapcar #'uiop:escape-sh-command cmds))))
          (declare (ignore ignore1))
          (dbug :top "  cmd-str: ~s" cmd-str)
          (setf (cmd task) (uiop:escape-sh-command `("bash" "-c" ,cmd-str))))))))

(defmethod pre-exec ((task lacrosse-test-patch-task))
  (dbug :amp "Executing lacrosse-test-patch-task (pre-exec).")
  (dbug :target "(output-pathname task): ~s" (output-pathname task))
  (init-build-dir (target task) (build-id task))
  )

;;; FIXME   What else to do here?  Set a success flag in task?
;;; Create a json file w interesting info (eg, make output)?
(defmethod post-exec ((task lacrosse-test-patch-task))
  (let ((target (target task)))
    (dbug :top "post-exec for lacrosse-test-patch-task. target: ~s" target)
    ;;(reset-file-results target)
    (let ((exec-pathname (executable-in-directory (create-build-pathname (target task) (build-id task)))))
      (cond (exec-pathname
             (dbug :top "Successfully built executable for patched target ~s" target)
             (send-message-to-optimi :type :challenge-project-update
                                     :target-id (id (target task))
                                     :comment (format nil "Successfully built executable for patched target ~s" target))
             )
            (t
             (dbug :top "FAILED to build executable for patched target ~s" target)
             (send-message-to-optimi :type :challenge-project-update
                                     :target-id (id (target task))
                                     :comment (format nil "FAILED to build executable for patched target ~s" target))
             )))))

(defmethod task-applies-to-target-p ((task-class-name (eql 'lacrosse-test-patch-task)) target-node)
    (declare (ignore target-node))
  nil)
  ;; (dbug :docker-task "task-applies-to-target-p lacrosse-test-patch-task")
  ;; ;; FIXME   Check whether there are *untested* patches!
  ;; (and *lacrosse*
  ;;      (some-patch (target target-node))
  ;;      (target-makefile (target target-node))
  ;;      (target-sources (target target-node))))

(defmethod process-line ((task lacrosse-test-patch-task) line)
  (dbug :top "got line [~A]" line)
  ;; FIXME anything reliable to check here?
  )

