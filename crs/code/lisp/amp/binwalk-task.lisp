(cl:in-package :fuzzbomb)

(defclass binwalk-task (docker-task-new)
  ((target-pathname :initform nil :accessor target-pathname)
   (target-filename :initform nil :accessor target-filename)
   (target-exp-pathname :initform nil :accessor target-exp-pathname)
   (target-exp-namestring :initform nil :accessor target-exp-namestring))
  (:default-initargs
   :image "firmadyne")
  )
  
(defmethod initialize-instance :after ((task binwalk-task) &key)
  (dbug :docker-task "initialize-instance :after ((task binwalk-task): ~s" task))

(defmethod pre-exec ((task binwalk-task))
  (dbug :top "target ~A" (target task))
  ;; (setf *target-under-triage* target)
  (with-slots (target target-pathname target-filename
	       target-exp-pathname target-exp-namestring
	       cmd)
      task

    (setf target-pathname (namestring (path target)))
    (setf target-filename (file-namestring (path target)))
    (setf target-exp-pathname (merge-pathnames (format nil "target-~d" (name target))
                                               (make-pathname :directory *experiment-dir*)))
    (setf target-exp-namestring (namestring target-exp-pathname))
    (dbug :top "binwalk vars: ~A ~A ~A ~A" target-pathname target-filename target-exp-pathname target-exp-namestring)
    (let* ((mkdir-cmd (format nil "mkdir --parents ~a" target-exp-namestring))
	   (mkdir-output (uiop:run-program mkdir-cmd :output :string)))
      (dbug :top "mkdir-cmd: ~s~%" mkdir-cmd)
      (dbug :top "mkdir-output: ~s~%" mkdir-output))
    (let* ((cp-cmd (format nil "cp ~a ~a" target-pathname target-exp-namestring))
	   (cp-output (uiop:run-program cp-cmd :output :string)))
      (dbug :top "cp-cmd: ~s~%" cp-cmd)
      (dbug :top "cp-output: ~s~%" cp-output))
    (setf cmd (format nil "/realuser.sh /bin/bash -c \"cd ~A; binwalk -M -e ~A\"" target-exp-namestring target-filename))))

(defmethod post-exec ((task binwalk-task))
  (with-slots (target-exp-namestring) task
    (let* ((find-interesting-executables-cmd (format nil "../tools/find-interesting-executables ~a" target-exp-namestring))
	   (find-interesting-executables-output (uiop:run-program find-interesting-executables-cmd :output :string)))
      (dbug :top "find-interesting-executables-cmd: ~s" find-interesting-executables-cmd)
      (dbug :top "find-interesting-executables-output: ~s" find-interesting-executables-output)
      (dolist (target-msg (read-from-string find-interesting-executables-output))
	(dbug :top "Target msg ~s" target-msg)
	(add-new-target task (cons '(:tags (:binwalk-result :bin)) target-msg)))

      ;(eval-all-from-string find-interesting-executables-output)
      )
    (dbug :top "chmod cmd: ~A" (format nil "chmod -R 777 ~A" target-exp-namestring))
    (toolchain-command (format nil "chmod -R 777 ~A" target-exp-namestring))))

(defmethod task-applies-to-target-p ((task-class-name (eql 'binwalk-task)) target-node)
  ;; If pereti-unpack is turned on, it only works on the final products of pereti.
  (dbug :docker-task "(task-applies-to-target-p binwalk-task): *use-pereti-unpack*: ~s compressed-p: ~s" *use-pereti-unpack* (compressed-dir-p (target target-node)))
  (and (if *use-pereti-unpack*
           (eql (source (target target-node)) :pereti-final)
           t)
       (compressed-dir-p (target target-node) )))
