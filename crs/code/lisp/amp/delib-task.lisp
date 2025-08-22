;;; -------------------------------------------------------------------------
;;; COPYRIGHT START
;;; COPYRIGHT END
;;; -------------------------------------------------------------------------
;;; delib-task.lisp
;;;   Jan 2015 Starting on a more generalized form of deliberation task (time consumer) that will
;;; have old CSM plan-config-task type behavior, in which only one at a time will be run.
;;; And results produced should be captured in a process-deliberation-result task.
;;;------------------------------------------------------------------------
;;; Each new class of deliberation operation should declare:
#|
(defclass NEW-delib-task (deliberation-task)
 ())

(defmethod really-execute-task ((task NEW-delib-task))
 (setf (result task) 'foo) ;; do processing
 T      ;; return non-nil if valid result stored..could be partial
)

(defclass NEW-delib-result-task (process-delib-result-task)
 ())

;; optional
(defmethod expected-duration ((task NEW-delib-task))
 42)

(defmethod make-process-result-task (delib-result (task NEW-delib-task))
  (make-instance 'NEW-delib-result-task :delib-result delib-result :task task)
)

(defmethod execute-task ((task NEW-delib-result-task))
  "Process a deliberation task result."
    (cond ((eq (delib-result task) :timed-out)
           (dbug :amp "Deliberation task ~A timed out after ~A secs." task (expected-duration task)))
          (T
           (dbug :amp "Deliberation task ~A returned result ~A and stored result ~A." task (delib-result task)
                        (result (task task))))
        )
)


|#

(cl:in-package :fuzzbomb)

(optimization-boilerplate)

(defvar *delib-task* nil "The currently running deliberation-task, if any. [AMP]")
(defvar *delib-process* nil "The Lisp process handle for the thread running the deliberation task, if any. [AMP]")
(defvar *delib-process-suspended* nil)
(defvar *default-deliberation-timeout* 300 "Default timeout on deliberation tasks, in seconds [300s == 5min]. [AMP]")
(defvar *ignore-delib-errors* T "Should the multiprocessing delib task ignore errors and allow AMP to continue? [AMP]")

;;; -------------------------------------------------------------------------
;;; Applications of the MP-AMP should subclass this class to hold specific configuration information that
;;; is needed by a particular kind of deliberation task.  E.g., plan-config-task (which predates this superclass,
;;; so is not yet (Jan 19 2015) retrofitted to use this.

(defclass deliberation-task (task)
  ((result :initform nil :initarg :result :accessor result)
   (pid :initform nil :initarg :pid :accessor pid)
   ))

(defun deliberation-task-p (x)
  (typep x 'deliberation-task))

(defmethod expected-duration ((task deliberation-task))
  *default-deliberation-timeout*)

(defmethod ready-to-resume ((x task))
  T)

;;; -------------------------------------------------------------------------

(defmethod execute-task ((task deliberation-task))
  "Start the deliberation process, the result will be processed elsewhere."
  (dbug :amp-deep "execute-task for ~A" task)


  (when (and *delib-task* (not (eq task *delib-task*)))
    (dbug :top "Warning: we already have a *delib-task* ~A, cannot yet run different one: ~A" *delib-task* task)
    (push task (tasks *self*))  ;; this will make it be considered again...would be smart to just give zero value if already have a delib-task
    (return-from execute-task))

   ;; Store the task so that we don't try to run another delib-task until this one is done.
  (setf *delib-task* task)

  (cond ((eq (status task) :new)
         (setf (status task) :running)
         (setf *delib-process*
               (mp:process-run-function '(:name "delib-process") #'delib-process-preset-function
                                        task (expected-duration task))))
        ((eq (status task) :suspended)
         (cond ((ready-to-resume task)
                (setf (status task) :running)
                (resume-delib-task *delib-task*))
               (T (dbug :top "Suspended task ~A not ready to resume" task))))
        (T (dbug :top "Warning: Unhandled task status ~A for ~A" (status task) task)))
)

;;; -------------------------------------------------------------------------
(defmethod really-execute-task ((task deliberation-task))
  "Every deliberation-task subclass must define this method to really do the work, inside a Lisp thread handled by the
        shared (execute-task deliberation-task) framework."
  (dbug :amp-deep "really-execute-task for ~A" task)
  (setf (result task) 42)
  T)

;(defmethod prepare-to-suspend  ((task deliberation-task))
;)

(defmethod prepare-to-resume  ((task deliberation-task))
)

;;; -------------------------------------------------------------------------

(defun error-handler (condition)
  (format *error-output* "~&~A~&" condition)
  (dbug :amp "error-handler called with condition ~s~%~a" condition condition)
  #+allegro (top-level.debug::zoom *standard-output*)
  #-allegro (uiop:print-backtrace)
  (throw :delib-error nil))

;;; removed the unwind-protection b/c now we're sending into the delib-task the task obj onto which it should
;;; tack results.
(defun unwind-protected-deliberation (task)
  "Run delib task and capture intermediate result if interrupted."
;  (unwind-protect
        (if *ignore-delib-errors*
          (handler-bind ((error #'error-handler)) (really-execute-task task))
          (really-execute-task task))
;    (progn
;      ;;(dbug :top "Deliberation unwind protect triggered")
;      (when *deliberation-result*
;        ;;(dbug :top "Deliberation created a result before unwind protect ended.")
;        (setf (result task) *deliberation-result*)))))
)

;;; -------------------------------------------------------------------------
;;; When ya run the CSM in a subprocess, if it times out you do get
;;; an unwind-protect opportunity to grab the intermediate results,
;;; but you have to stash it somewhere that wont get whacked by the
;;; binding stacks changing.  So the way I do that is create an
;;; object that the CSM can actually set a slot of, and pass that object
;;; into the delib driver call.  With luck and all shining on us, the
;;; pointers should all work out...

#- (or allegro ccl sbcl)
(eval-when (:compile-toplevel :load-toplevel :execute)
 (error "Multiprocessing not implemented on this lisp: DELIB-PROCESS-PRESET-FUNCTION needs MP:WITH-TIMEOUT and MP:WITHOUT-SCHEDULING."))

(defun delib-process-preset-function (task timelimit)
  "Function to run in the delib-process."
  (dbug :amp-deep "delib-process-preset-function")
  (let ((delib-result nil))
    (unwind-protect
         ;; NOTE if you start anything below lisp, eg with run-command or toolchain-cmd, you must kill it and reap it if this timeout triggers!
         (setf delib-result
               (mp:with-timeout
                   (timelimit :timed-out)
                 (catch :delib-error
                   (unwind-protected-deliberation task))))
      (progn
        (dbug :amp "After unwind-protected-deliberation, delib-result is ~s and (result task) is ~A" delib-result (result task))
        (let ((new-task (make-process-result-task delib-result task)))

          (when new-task
            ;; Add the task to the list.
            (dbug :amp-deep "tasks before adding: ~s" (tasks *self*))
            (mp:without-scheduling (setf (tasks *self*) (append (tasks *self*) (list new-task))))
            (dbug :amp-deep "tasks after adding: ~s" (tasks *self*)))

          (setf *delib-process* nil)
          (setf *delib-process-suspended* nil)
          (setf (status *delib-task*) :complete)
          (when (typep *delib-task* 'oss-fuzz-task)
            (setf (status *delib-task*) :new))
          (setf *delib-task* nil)
          )))))

;;; -------------------------------------------------------------------------
;;; Note we might want to use process group in case the task has had children...
;;;     eg see http://unix.stackexchange.com/questions/9480/how-to-suspend-and-resume-processes-like-bash-does
;;; this might be as simple as negating the pid of original process.

#+allegro
(defmethod suspend-delib-task ((task deliberation-task))
  (when (and (not *delib-process-suspended*) *delib-process*)
    (dbug :top "Suspending the deliberation task! (really)")
    (prepare-to-suspend *delib-task*)
    (mp:process-disable *delib-process*)
    (when (pid *delib-task*)    ;; the delib-task has launched a sub-process in Unix land
      (excl.osi::kill (pid *delib-task*) excl.osi::*sigstop*)
    )
    (setf (status *delib-task*) :suspended)
    (push *delib-task* (tasks *self*))  ;; when it next gets selected due to utility score, it may indeed resume.
    (setf *delib-process-suspended* T)
  )
)

;;(defmethod kill-delib-task ((task deliberation-task))
;;  (when *delib-process*
;;    (dbug :top "Killing the deliberation task!")
;;    (when (pid *delib-task*)  ;; the delib-task has launched a sub-process in Unix land
;;      (excl.osi::kill (pid *delib-task*) excl.osi::*sigkill*)
;;    )
;;    (mp:process-kill *delib-process* :wait T)
;;    (setf (status *delib-task*) :killed)
;;))
;;

#+allegro
(defmethod resume-delib-task ((task deliberation-task))
  (when (and *delib-process-suspended* *delib-process*)
    (dbug :top "Resuming the deliberation task!")
    (prepare-to-resume *delib-task*)
    (mp:process-enable *delib-process*)
    (when (pid *delib-task*)    ;; the delib-task has launched a sub-process in Unix land
      (excl.osi::kill (pid *delib-task*) 18)    ;; could also just call as system kill -s CONT pid; strangely excl.osi::*sigcont* not defined
    )
    (setf *delib-process-suspended* nil)
  )
)

(defmethod kill-task ((task deliberation-task))
  (kill-delib-process task)
  )
;;; -------------------------------------------------------------------------
;;; These for when nil gets passed in

(defmethod suspend-delib-task ((task T))
  (dbug :top "Suspend: no delib-task yet, doing nothing"))

(defmethod resume-delib-task ((task T))
  (dbug :top "Resume: no delib-task yet, doing nothing"))

;;; -------------------------------------------------------------------------
#+(or allegro sbcl)
(defmethod kill-delib-process ((task t))
  "Kill the delib process."
  (let ((pid (and *delib-task* (pid *delib-task*))))
    (when pid
      (dbug :top "Killing delib task pid ~a!" pid)
      (signal-process pid *sigkill*)
      (setf (pid *delib-task*) nil)
      ;; Wait a tick for OS to clean up process.
        ;; FIXME maybe this should be a reap instead..maybe use os-utils:stop-background-cmd
      (sleep 1)))
  (when *delib-process*
    (dbug :top "Killing the delib process!")
    (mp:process-kill *delib-process* :wait T)
    (musliner:while (member *delib-process* mp:*all-processes*) (sleep 1))
    (setf *delib-process* nil))
  (setf *delib-task* nil)
  )

#+ccl
(defmethod kill-delib-process ((task t))
  "Kill the delib process."
  (let ((pid (and *delib-task* (pid *delib-task*))))
    (when pid
      (dbug :top "Killing delib task pid ~a!" pid)
      (signal-process pid *sigkill*)
      (setf (pid *delib-task*) nil)
      ;; Wait a tick for OS to clean up process.
        ;; FIXME maybe this should be a reap instead..maybe use os-utils:stop-background-cmd
      (sleep 1)))
  (when *delib-process*
    (dbug :top "Killing the delib process!")
    ;;(mp:process-kill *delib-process* :wait T)
    (mp:process-kill *delib-process*)
    ;;(while (member *delib-process* mp:*all-processes*) (sleep 1))
    (setf *delib-process* nil))
  (setf *delib-task* nil)
)

#- (or allegro ccl sbcl)
(eval-when (:compile-toplevel :load-toplevel :execute)
 (error "Multiprocessing not implemented on this lisp: KILL-DELIB-PROCESS-AND-ITS-RESULTS needs MP:WITHOUT-SCHEDULING."))

(defun kill-delib-process-and-its-results ()
  "Kill the delib process and the process-delib-result-task task it will turd onto *tasks*."
  (let ((curtask *delib-task*))
    (mp:without-scheduling
      (setf (tasks *self*) (delete-if #'(lambda (task) (and (eq (type-of task) 'process-delib-result-task) (eq (task task) curtask))) (tasks *self*)))
      (setf *delib-task* nil)
      )))

;;; -------------------------------------------------------------------------
;;; -------------------------------------------------------------------------
(defclass process-delib-result-task (task)
  ((delib-result :initform nil :initarg :delib-result :accessor delib-result)
   (task :initform nil :initarg :task :accessor task)
   ))

;;; It is always worth a lot to process deliberation task results.
(defmethod scoring-fn ((task process-delib-result-task))
  1000)

(defmethod make-process-result-task (delib-result (task deliberation-task))
  (make-instance 'process-delib-result-task :delib-result delib-result :task task)
)

;;; -------------------------------------------------------------------------

(defmethod execute-task ((task process-delib-result-task))
  "Process a deliberation task result."
    (cond ((eq (delib-result task) :timed-out)
           (dbug :amp "Deliberation task ~A timed out after ~A secs." task (expected-duration task)))
          (T
           (dbug :amp "Deliberation task ~A returned result ~A and stored result ~A." task (delib-result task)
                        (result (task task))))
        )
)

;;; -------------------------------------------------------------------------
