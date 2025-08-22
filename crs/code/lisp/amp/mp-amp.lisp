;;; -------------------------------------------------------------------------
;;; mp-amp.lisp
;;; - multiprocessing ver of AMP inner loop, allows CSM to run in parallel
;;; yet remain interruptible by incoming state msg from RTS (or potentially
;;; other interrupts/notifications).
;;; - $Revision: 1.8 $
;;; -------------------------------------------------------------------------
;;; COPYRIGHT START
;;; COPYRIGHT END
;;; -------------------------------------------------------------------------

(cl:in-package :fuzzbomb)

;;(use-package :multiprocessing)
#+allegro
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :process))

;;; -------------------------------------------------------------------------
(declaim (special *stars-printed* *max-stars-to-wait*))

#-(or allegro ccl sbcl)
(eval-when (:compile-toplevel :load-toplevel) (error "Need the equivalent of MP:WAIT-FOR-INPUT-AVAILABLE for this lisp."))

(defun mp-run-amp-inner-loop ()
  (declare (special *stars-printed* *self*))
  (let ((sent-idle-msg t))     ; don't send idle-msg until *after* initial tasking
    (loop until *halt*
       do
         (multiple-value-bind (task score) (mp-select-task)
           (cond ((and task (> score 0))
                  (dbug :top "Task queue: ~a" (tasks *self*))
                  (dbug :top "Selected ~A from ~A tasks, score = ~A" task (length (tasks *self*)) score)
                  (setf *stars-printed* 0)
                  (setf (tasks *self*) (delete task (tasks *self*)))
                  (dont-error (execute-task task))
                  (setf sent-idle-msg nil)
                  (dont-error (process-all-msgs))
                  )
                 ((and (not *delib-task*)
                       *send-idle-msg*
                       (not sent-idle-msg))
                  (dbug :top "Sending :idle msg to optimus.")
                  (send-message-to-optimi :type :idle)
                  (setf sent-idle-msg t))
                 (t
                  ;; (dbug :top "Waiting for input available on SOCKETS:~%~{~T~S~%~}." *sockets*)
                  (if (mp:wait-for-input-available *sockets* :timeout *heartbeat-period*)
                      (dont-error (progn (setf *stars-printed* 0) (process-all-msgs)))
                      (progn
                        ;; print a heartbeat . and newline if 30...
                        (print-heartbeat)
                        (incf *dot-heartbeat*)
                        (when (zerop (rem *dot-heartbeat* 30)) (format t "~%")(force-output))

                        (when (and *delib-task* *delib-process-suspended*)
                          (dbug :top "Pushing delib-task ~A back on tasks list" *delib-task*)
                          (pushnew *delib-task* (tasks *self*)) ;; see whether should resume delib task
                          ;; we do that by putting it on the task list so that if other tasks appear, they all get sorted properly.
                          )
                        ))))))))

;;; -------------------------------------------------------------------------

(defun print-heartbeat ()
  (declare (special *self*))
  ;;(dont-error (check-test-queue))
  ;; (defensive-rewrite-update-reload)
  (cond ((has-pending-work *self*)
         (format t "*")
         ;;(dont-error (check-for-old-test-batches))
         (incf *stars-printed*)                 ;; if we have pending stuff, wait a bit
         (when (> *stars-printed* *max-stars-to-wait*)
                (setf *stars-printed* 0)
                (dont-error (update-pending-work-when-idle)))
         )
        (T
         (format t ".")
         (dont-error (update-pending-work-when-idle))   ;; if no pending, update right away
        )
  ))


;;; -------------------------------------------------------------------------

(defun has-pending-work (amp)
  (declare (ignore amp))
  (declare (special *all-revs*))
  (any #'pending-test-cases *all-revs*)
)

;  (declare (special *is-this-best-task*))
;  (and (svc *self*)
;       (or (pending-test-cases (original-rev (svc *self*)))
;          (any #'pending-test-cases (find-all-instances 'svc-rev))))
;  (or (and (pending-tests *is-this-best-task*) (any #'second (pending-tests *is-this-best-task*)))

;;(trace has-pending-work)

;;; -------------------------------------------------------------------------
;;; If there is already/still a current task, select it.
;;; If there are no tasks, nil.
;;; Otherwise, pick the one with the highest incremental utility.

(defun mp-select-task ()
  (declare (special *self*))
  (dbug :mp-select "mp-select-task")
  (check-task-deadlines *self*)
  (check-tt-funcs *self*)
  (when (optimus-p *self*)
    (reassign-presumed-dead-tasks *self*))
  (cond ((current-task *self*))
        ((null (tasks *self*)) nil)
        (T
         (dbug :delib-trace "All tasks are ~A" (tasks *self*))
         (let ((tasks (cond (*delib-task*
                             (remove-if #'deliberation-task-p (tasks *self*)))
                            (t
                             (tasks *self*)))))
           (dbug :delib-trace "Tasks under consideration are ~A" tasks)
           (dbug :delib-trace "Scores are ~A" (mapcar #'scoring-fn tasks))
           (rank-and-choose #'scoring-fn #'max tasks)))))

;;; -------------------------------------------------------------------------

(defun check-task-deadlines (amp)
  (dbug :mp-select "check-task-deadlines: ~s" (tasks amp))
  (let ((dbug-on (when (tasks amp)))
        (filtered-tasks (iter (for task in (tasks amp))
                          (cond ((and
                                  ;; avoid interrupting tasks-in-progress (for now)  
                                  (not (eq task (current-task amp)))
                                  (deadline-exceeded-p task))
                                 (dbug :top  "Deadline exceeded in check-task-deadlines, dropping ~s for ~s ~a" task (target task) (short-description (target task)))
                                 (send-message-to-optimi :type :task-deadline-exceeded
                                                         :target-id (id (target task))
                                                         :next-task-sexp (next-task-sexp task))
                                 )
                                ;; otherwise, collect it
                                (t (collect task))))))
    (when dbug-on
      (dbug :top "before check-task-deadlines: ~s" (tasks amp)))
    (setf (tasks amp) filtered-tasks)
    (when dbug-on
      (dbug :top "after check-task-deadlines: ~s" (tasks amp)))
    ))

(defun check-tt-funcs (amp)
  (dbug :mp-select "check-tt-funcs: ~s" (tasks amp))
  (unless (empty-p (tt-func-q amp))
    (let ((now (get-unix-time-millisecs))
          (funcy-q (tt-func-q amp)))
      (dbug :mp-select " now: ~d  q: ~s" now (elts funcy-q))
      (iterate (while (and (not (empty-p funcy-q))
                           (< (first-index funcy-q) now)))
        (funcall (payload (dequeue funcy-q)))))))

;;; -------------------------------------------------------------------------

(defun reassign-presumed-dead-tasks (amp)
  (dbug :mp-select "reassign-presumed-dead-tasks ~s" (task-presumed-dead-times amp))
  (dolist (entry (task-presumed-dead-times amp))
    (let* ((now (get-unix-time-millisecs))
	   (key (car entry))
	   (next-task (first key))
	   (target (second key))
	   (prev-assignments (cdr entry))
	   (most-recent (first prev-assignments)) ; assume the first is the most recent
	   (deadline (first most-recent))
	   (presumed-dead-min (second most-recent))
	   (assigned-fbs (mapcar #'third prev-assignments))
	   (countdown-sec (/ (- deadline now) 1000))
           )
      (declare (ignorable countdown-sec))
      (dbug :mp-select "~a sec until presumed dead: ~s ~s" countdown-sec next-task target)
      (when (> now deadline)
	(dbug :top "Attempting to reassign task presumed dead: ~s" next-task)
	(dbug :top "fuzzbomb blacklist: ~a" assigned-fbs)
	(dbug :top "presumed-dead-min: ~a" presumed-dead-min)
        ;; Check task deadline to avoid attempt to reassign OBE tasks.
        (cond ((deadline-exceeded-p target)
               (dbug :top "Deadline exceeded in reassign-presumed-dead-tasks, not reassigning and removing.")
	       (dbug :mp-select "task-presumed-dead-times before: ~s" (task-presumed-dead-times amp))
	       (setf (task-presumed-dead-times *self*)
		     (alist-remove (task-presumed-dead-times *self*) key))
	       (dbug :mp-select "task-presumed-dead-times after: ~s" (task-presumed-dead-times amp)))
              (t
               (assign-challenge-project target
                                         :next-task next-task
                                         :presumed-dead-min presumed-dead-min
                                         :fb-blacklist assigned-fbs
                                         :allow-fail t ; allows task to be dropped if no appropriate FB is found
				  )))))))
