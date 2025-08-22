;;; -------------------------------------------------------------------------
;;; COPYRIGHT START
;;; COPYRIGHT END
;;; -------------------------------------------------------------------------

;;; A class for tasks that <FIXME>

(cl:in-package :fuzzbomb)

(defclass fake-git-bisect-task (lacrosse-task)
  (
  )
  )

(defmethod task-applies-to-target-p ((task-class-name (eql 'fake-git-bisect-task)) target-node)
  (dbug :docker-task "task-applies-to-target-p fake-git-bisect-task")
  (and *fake-git-bisect*
       ;;; FIXME This TEMP hack only works in 1fb mode.
       (not *fake-git-bisect-done-p*)
       (vuln-cands (target target-node))))

;;; hijack this for a bit of sleep
(defmethod exec-docker-command ((task fake-git-bisect-task))
  (sleep 2))

(defmethod post-exec ((task fake-git-bisect-task))
  (dbug :top "post-exec for fake-git-bisect-task")
  (dbug :top "cp-path: ~s" (cp-path (target task)))
  ;;   FIXME cheat cheat for golden thread
  
  (dbug :top "(first (vuln-cands (target task))): ~s" (first (vuln-cands (target task))))
  (dbug :top "(getassoc :id (first (vuln-cands (target task)))): ~s" (getassoc :id (first (vuln-cands (target task)))))
  (let ((vc-id (getassoc :id (first (vuln-cands (target task))))
	  ))
    (dbug :top "vc-id: ~s" vc-id)
    ;;; FIXME This TEMP hack only works in 1fb mode.
    (setf *fake-git-bisect-done-p* t)
    (send-message-to-optimi :type :vuln-cand
			    :target-id (id (target task))
			    :id vc-id
			    :bic "11dafa9a5babc127357d710ee090eb4c0c05154f")))
