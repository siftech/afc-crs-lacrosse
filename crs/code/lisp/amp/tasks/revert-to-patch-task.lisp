;;; -------------------------------------------------------------------------
;;; COPYRIGHT START
;;; COPYRIGHT END
;;; -------------------------------------------------------------------------

;;; A class for tasks that invoke the langchain-based llm tool.

(cl:in-package :fuzzbomb)

(defclass revert-to-patch-task (patch-path-mixin lacrosse-task)
  (
   (to-revert
    :initarg :to-revert
    :accessor to-revert
    :initform nil
    ;;:type list of strings
    :documentation "List of paths to patches to revert")
   (vc-id
    ;;:type (or null int)
    :documentation "Int id for vuln-cand."
    :initform nil
    :accessor vc-id)
   (task-output-dir
    ;;:type namestring
    :documentation "Unique dir for this task."
    :initform nil
    :accessor task-output-dir)
   ))

(defmethod print-object ((task revert-to-patch-task) str)
  (print-unreadable-object (task str :type t)
    (format str "[~a] id: ~a  ~a" (name task) (vc-id task) (mapcar #'uiop-file-namestring (to-revert task)))))

(defmethod scoring-fn ((task revert-to-patch-task)) 15)

;;; Eventually, every task should define or inherit next two methods.
;;; Don't call them until slots are populated!
;;; And don't call more than once.  Call it once and store the value.
(defmethod task-temp-dir-namestring ((task revert-to-patch-task))
  (strcat (output-dir (target task)) (task-temp-dirname task) "/"))

(defmethod task-temp-dirname ((task revert-to-patch-task))
  (format nil "~a-~a-vc-~a" (class-name (class-of task)) (get-universal-time) (vc-id task)))

(defmethod new-temp-patch-namestring ((task revert-to-patch-task))
  (strcat (task-temp-dir-namestring task) (new-unique-patch-filename)))

(defun new-unique-patch-filename ()
  (strcat (symbol-name (gensym "P")) ".patch"))

(defmethod task-applies-to-target-p ((task-class-name (eql 'revert-to-patch-task)) target-node)
  (dbug :docker-task "task-applies-to-target-p revert-to-patch-task")
  (and (next-task (target target-node))
       (let ((task-type (getassoc :task-type (next-task (target target-node)))))
         (eq task-type task-class-name))))
  
(defmethod initialize-instance :after ((task revert-to-patch-task) &key)
  (dbug :top "initialize-instance :after ((task revert-to-patch-task): ~s" task)
  (let ((target (target task))
        (next-task (next-task (target task))))
    (when next-task
      (setf (to-revert task) (getassoc :to-revert next-task))
      (setf (vc-id task) (getassoc :vc-id next-task))
      (setf (harness-name task) (harness-prop :name (find-harness-by-id
                                                     (getassoc :harness-id next-task)
                                                     target)))
      ;;(setf (file-to-patch task) (getassoc :file-to-patch next-task))
      (setf (blob task) (getassoc :blob next-task))
      (setf (bic task) (getassoc :bic (next-task (target task))))
      ;; maybe there should be patch-path-mixin init-inst?
      (setf (cp-src task) (getassoc :cp-src (next-task (target task))))
      )
    (setf (task-output-dir task) (task-temp-dir-namestring task))
    (ensure-directories-exist (task-output-dir task))
    (let ((patch-dir-namestring (format nil "~a/revert-to-patch-~9,'0d/" (task-output-dir task) (name task))))
      (ensure-directories-exist patch-dir-namestring)
      (setf (patch-path task) (strcat patch-dir-namestring "reversed.patch")))
    (dbug :top "(patch-path task): ~s" (patch-path task))

    ;;   FIXME First cut: one hunk at a time
    ;;   Should be easy to combine hunks from same commit using combinediff.
    ;;(let ((thing-to-revert (first (to-revert task))))
    (cond ((musliner::singleton-p (to-revert task))
	   (setf (cmd task) (format nil "interdiff -q ~a /dev/null > ~a" (first (to-revert task)) (patch-path task))))
	  (t
	   ;;(dbug :top "WARNWARN Haven't implemented multi-hunk handling, yet.")
	   (dbug :top "Combining multiple hunks: ~s" (to-revert task))
	   (flet ((combine-two-hunks (hunk-1 hunk-2)
		    (cond ((and hunk-1 hunk-2)
			   (let* ((new-patch (strcat (task-output-dir task)  (new-unique-patch-filename)))
				  (cmd (format nil "combinediff -q ~a ~a > ~a"
					       hunk-1
					       hunk-2
					       new-patch)))
			     (dbug :top "combine hunks cmd: ~s" cmd)
			     (multiple-value-bind (output-slurp error-slurp exit-code)
				 (uiop:run-program cmd :output t :force-shell t :ignore-error-status t :error-output :output)
			       (declare (ignore output-slurp error-slurp))
			       (cond ((zerop exit-code) new-patch)
				     (t nil)))))
			  (t nil))))
	     (let ((new-patch (reduce #'combine-two-hunks (to-revert task))))
	       (setf (cmd task) (format nil "interdiff -q ~a /dev/null > ~a" new-patch (patch-path task)))))))))

(defmethod process-line ((task revert-to-patch-task) line)
  (dbug :top "got line [~A]" line)
  (cond
    ((cl-ppcre:scan-to-strings "error" line)
     (send-patch-generation-failed-msg task :desc line)
;;;     (send-message-to-optimi :type :patch-generation-failed
;;;                             :desc line
;;;                             :vc-id (vc-id task)
;;;                             :target-id (id (target task)))
     )))

(defmethod post-exec ((task revert-to-patch-task))
  (dbug :top "post-exec for revert-to-patch-task")
  (cond ((not (file-exists-p (patch-path task)))
         ;; might send redundant msg (ie, process-line may have sent one, too)
         (send-patch-generation-failed-msg task :desc "patch not found")
;;;         (send-message-to-optimi :type :patch-generation-failed
;;;                           :desc "patch not found"
;;;                           :vc-id (vc-id task)
;;;                                 :target-id (id (target task)))
         )
        (t (when ;; This pred also sends fail msgs.  FIXME sorry  
               (patch-passes-test-p (patch-path task) task)
             (send-patch-generation-succeeded-msg task :patch-file (namestring (patch-path task)))
;;;             (send-message-to-optimi :type :patch-generation-succeeded
;;;                                     :patch-file (namestring (patch-path task))
;;;                                     :vc-id (vc-id task)
;;;                                     :target-id (id (target task)))
             ))))


;;;    IGNORE these prototypes for interactive dev
(defun new-temp-namestring ()
  (format nil "/tmp/~a.patch" (symbol-name (gensym "P"))))

(defun combine-two-hunks (hunk-1 hunk-2)
  "Combine hunk namestrings and return the namestring of the combined hunk."
  (cond ((and hunk-1 hunk-2)
	 (let* ((dir (directory-namestring hunk-1))
		(new-patch (strcat dir (new-unique-patch-filename)))
		(cmd (format nil "combinediff -q ~a ~a > ~a"
			     hunk-1
			     hunk-2
			     new-patch)))
	   (dbug :top "combine hunks cmd: ~s" cmd)
	   (multiple-value-bind (output-slurp error-slurp exit-code)
	       (uiop:run-program cmd :output t :force-shell t :ignore-error-status t :error-output :output)
	     (declare (ignore output-slurp error-slurp))
	     (cond ((zerop exit-code) new-patch)
		   (t nil)))))
	(t nil)))
