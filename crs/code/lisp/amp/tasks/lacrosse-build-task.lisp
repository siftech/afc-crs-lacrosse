;;; -------------------------------------------------------------------------
;;; COPYRIGHT START
;;; COPYRIGHT END
;;; -------------------------------------------------------------------------

;;; A class for tasks that <FIXME>

(cl:in-package :fuzzbomb)

(defvar *lacrosse-build-image-args*
  "")

(defclass lacrosse-build-task (lacrosse-task)
  (
  )
;  (:default-initargs
;   ;;   Switched to driller image b/c afl was failing on
;   ;; images built w newer distro.
;   ;;:image "neo-fuzz-ccl"
;   ;;:image "driller"
;   :image "lacrosse-utils"
;   :container-start-args *lacrosse-build-image-args*
;   :container-start-method :in-this-container
;  )
)

;;; FIXME add magic GCC args or whatever to get it to instrument for AFL
(defmethod initialize-instance :after ((task lacrosse-build-task) &key)
  (dbug :top "initialize-instance :after ((task lacrosse-build-task): ~s" task)
  (dbug :top "(dir (target task)): ~s" (dir (target task)))
  (dbug :top "(target-makefile (target task)): ~s" (target-makefile (target task)))
  (let* ((cmds `(("cd" ,(dir (target task)))
                 ("make")))
         (cmd-str (format nil "~{~A~^ && ~}" (mapcar #'uiop:escape-sh-command cmds))))
    (dbug :top "  cmd-str: ~s" cmd-str)
    (setf (cmd task) (uiop:escape-sh-command `("bash" "-c" ,cmd-str)))))

(defmethod pre-exec ((task lacrosse-build-task))
  (dbug :amp "Executing lacrosse-build-task.")
  (dbug :target "(output-pathname task): ~s" (output-pathname task))
  )

;;; FIXME   What else to do here?  Set a success flag in task?
;;; Create a json file w interesting info (eg, make output)?
(defmethod post-exec ((task lacrosse-build-task))
  (let ((target (target task)))
    (dbug :top "post-exec for lacrosse-build-task. target: ~s" target)
    ;;(reset-file-results target)
    (let ((exec-pathname (target-executable target)))
      (cond (exec-pathname
             ;;(uiop:rename-file-overwriting-target exec-pathname (output-pathname task))
             (mv exec-pathname (output-pathname task))
             (dbug :top "Successfully built executable for target ~s" target)
             (send-message-to-optimi :type :challenge-project-update
                                     :target-id (id (target task))
                                     :comment (format nil "Successfully built executable for target ~s" target))
             )
            (t
             (dbug :top "FAILED to build executable for target ~s" target)
             (send-message-to-optimi :type :challenge-project-update
                                     :target-id (id (target task))
                                     :comment (format nil "FAILED to build executable for target ~s" target))
             )))))

(defmethod task-applies-to-target-p ((task-class-name (eql 'lacrosse-build-task)) target-node)
  (dbug :docker-task "task-applies-to-target-p lacrosse-build-task")

  ;;(when (slot-exists-p (target target-node) 'cp-address)
    ;; This is a challenge problem..do not run that here..
    ;;(return-from task-applies-to-target-p nil))

  (and *lacrosse*
       (null (target-executable (target target-node)))
       (target-makefile (target target-node))
       (target-sources (target target-node))))

(defmethod process-line ((task lacrosse-build-task) line)
  (dbug :top "got line [~A]" line)
  ;; FIXME anything reliable to check here?
  )

