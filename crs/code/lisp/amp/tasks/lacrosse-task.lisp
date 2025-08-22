;;; -------------------------------------------------------------------------
;;; COPYRIGHT START
;;; COPYRIGHT END
;;; -------------------------------------------------------------------------

;;; A super class for tasks created for the Lacrosse agent.

(cl:in-package :fuzzbomb)

(defclass lacrosse-task (docker-task-new)
  (
   (next-task-sexp :initarg :next-task-sexp :initform nil :accessor next-task-sexp
                   :documentation "The next-task sexp used when this task was created.")
   )
  (:default-initargs
	;; in the contest way, much easier to have nf-ccl img run everything
   :container-start-method :in-this-container
  ))

(defmethod initialize-instance :before ((task lacrosse-task) &key target &allow-other-keys)
  ;; The next-task slot in the target (ctask) is volatile, so cache its contents here
  ;; for future ref.  
  (when (next-task target)
    (setf (next-task-sexp task) (next-task target))))

(defmethod initialize-instance :after ((task lacrosse-task) &key)
  (dbug :top "initialize-instance :after ((task lacrosse-task): ~s" task)
  (init-lacrosse-task-in-out-pathnames task)
  )

(defmethod print-object ((obj lacrosse-task) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~a ~a" (name obj) (short-description (target obj)))))

;; Broken out for splicing into other tasks that may be used in
;; lacrosse context, eg afl-task.
(defun init-lacrosse-task-in-out-pathnames (task)
  (setf (output-pathname task) (parse-namestring (output-dir (target task))))
  (ensure-directories-exist (output-pathname task))
  (setf (input-pathname task) (parse-namestring (input-dir (target task))))
  (ensure-directories-exist (input-pathname task))
  (dbug :target "set (output-pathname task) to: ~s" (output-pathname task))
  (dbug :target "set (input-pathname task) to: ~s" (input-pathname task)))

(defmethod deadline-exceeded-p ((lacrosse-task lacrosse-task))
  (deadline-exceeded-p (target lacrosse-task)))
