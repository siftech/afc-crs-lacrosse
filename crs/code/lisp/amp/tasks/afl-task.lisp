;;; -------------------------------------------------------------------------
;;; COPYRIGHT START
;;; COPYRIGHT END
;;; -------------------------------------------------------------------------

(cl:in-package :fuzzbomb)

(defclass afl-task (libfuzzer-task)
  ((distributed-fuzz-id
     :initarg :distributed-fuzz-id
     :accessor distributed-fuzz-id
     :type (or (eql :default) fixnum))
   (afl-shared-dir
     :accessor afl-shared-dir
     :documentation "The _shared_ AFL directory. This may need a directory component appended."
     :type pathname)
   (power-schedule
     :initarg :power-schedule
     :initform :fast ; The default.
     :accessor power-schedule
     :documentation "The power schedule to use."
     :type (member :explore :fast :coe :quad :lin :exploit :mmopt :rare :seek))
   (noticed-crashes
     :initform (make-hash-table :test #'equal)
     :accessor noticed-crashes
     :documentation "A set of known crash pathnames, represented as a hashtable with T values."
     :type hash-table)
   (successfully-launched
     :initform nil
     :accessor successfully-launched
     :documentation "Whether we've seen a \"All set and ready to roll!\" message from AFL since the last pre-exec."
     :type boolean)
   (consecutive-afl-fails
     :initform 0
     :accessor consecutive-afl-fails
     :documentation "The number of times AFL has failed to start."
     :type integer)))

(defmethod task-applies-to-target-p ((task-class-name (eql 'afl-task)) target-node)
  (let ((target (target target-node)))
    (and *use-afl*
         (next-task target)
         (let ((task-type (getassoc :task-type (next-task target))))
           (eq task-type task-class-name)))))

(defmethod initialize-instance :after ((task afl-task) &key)
  (when (next-task (target task))
    (setf (distributed-fuzz-id task) (getassoc :distributed-fuzz-id (next-task (target task))))
    (setf (power-schedule      task) (getassoc :power-schedule      (next-task (target task)))))

  (setf (afl-shared-dir task)
        (uiop:parse-unix-namestring
          (format nil "~a/fuzz/afl/~a/"
                  (shared-path (target task))
                  (chosen-harness-id-as-string task))
          :type :directory))
  (setf (cmd task)
        (format nil "cd ~a && /lacrosse/code/tools/lacrosse-afl-fuzz-it.py ~a ~a ~a ~a -- ~a -p ~a -t 10000"
                (path (target task))
                (chosen-harness-id-as-string task)
                (seeds-dir task)
                (afl-shared-dir task)
                (fuzzer-timeout task)
                (cond
                  ((and (eql (distributed-fuzz-id task) :default))
                   "-M default")
                  (t
                   (format nil "-S fuzz~a" (distributed-fuzz-id task))))
                (string-downcase (symbol-name (power-schedule task)))))
    (dbug :top "Task cmd: ~a" (cmd task))
    (dbug :top "afl-task finished initialization"))

(defmethod push-next-fuzzer ((task afl-task))
  (cond
    ((>= (consecutive-afl-fails task) 2)
     ;; We just send ourself a message to get the task constructed right. Ugh,
     ;; this does the wrong thing if *use-libfuzzer* is nil...
     (let* ((target (target task))
            (i (getassoc :poss-initial-fuzz-next-task-i (next-task target)))
            (next-task (poss-initial-fuzz-next-task target (1- i)))
            (msg `((:type :new-challenge-project)
                   (:target ((,@(spec target))))
                   ,next-task)))
       (dbug :top "~s ~s ~s" 'notice-afl-fail 'i i)
       (dbug :top "~s ~s ~s" 'notice-afl-fail 'next-task next-task)
       (process-new-challenge-project-msg msg)))
    (t
     (push task (tasks *self*)))))

(defun notice-afl-fail (task)
  (dbug :top "~s ~s" 'notice-afl-fail task)
  (incf (consecutive-afl-fails task)))

(defmethod build-it-for-fuzzing ((task afl-task))
  (dbug :top "~s ~s" #'build-it-for-fuzzing task)

  (uiop:run-program
    (format nil "cd ~a && /lacrosse/code/tools/lacrosse-afl-build-it.py"
            (path (target task)))
    :output t
    :error-output :output))

(defmethod pre-exec ((task afl-task))
  (setf (successfully-launched task) nil)
  (handler-bind ((serious-condition (lambda (condition)
                                      (declare (ignore condition))
                                      (notice-afl-fail task))))
    (call-next-method)))

(defmethod process-line ((task afl-task) line)
  (dbug :top "got line [~a]" line)

  (let (ret)
    (cond
      ((setf ret (cl-ppcre:register-groups-bind (path) ("Test case '(.*)' results in a crash" line)
                   (uiop:merge-pathnames*
                     (uiop:parse-unix-namestring path)
                     (uiop:merge-pathnames* #p"queue/" (afl-instance-dir task)))))
       (submit-crash task ret
                     ;; TODO!
                     ))
      ((setf ret (cl-ppcre:scan-to-strings "All set and ready to roll!" line))
       (setf (successfully-launched task) t))))

  (notice-new-crashes task))

(defmethod post-exec ((task afl-task))
  (cond
    ((successfully-launched task)
     (setf (consecutive-afl-fails task) 0))
    (t
     (notice-afl-fail task)))

  (call-next-method))

(declaim (ftype (function (afl-task) (values pathname &optional)) afl-instance-dir))
(defun afl-instance-dir (task)
  "The directory for this task's AFL instance."

  (let ((directory (if (eql (distributed-fuzz-id task) :default)
                       "default"
                       (format nil "~a" (distributed-fuzz-id task)))))
    (uiop:merge-pathnames*
      (make-pathname :directory `(:relative ,directory))
      (afl-shared-dir task))))

(defmethod notice-new-crashes ((task afl-task))
  (iter
    (for crash in (directory (uiop:merge-pathnames* #p"crashes/id*" (afl-instance-dir task))))
    (unless (gethash crash (noticed-crashes task))
      (setf (gethash crash (noticed-crashes task)) t)
      (dbug :top "RESULT: Found crashing input ~s" crash)
      (submit-crash task crash
                    ;; TODO!
                    ))))

;;; #+nil
;;; (defun afl-report-crash (task)
;;;   ;;(make-results-accessible task)
;;;      	;; How to know which AFL is ours, since the container is shared across agents on same host...hmmm...
;;;      	;; One way would be to have some agent-specific tag in the cmd line and find it with pkill.  AFL's -T to the rescue, see cmd
;;;   (dbug :top "Now killing my AFL inside the container")
;;;   ;;(uiop:run-program (list "docker" "exec" (container-name task) "/realuser.sh" "/bin/bash" "pkill" "-9" "-f" (strcat (shortname *self*) "-AFL")) :output t)
;;;   (with-docker-exec-stream
;;;             (io-stream (container-name task)
;;;                         (format nil "/realuser.sh /bin/bash --login -c 'pkill -9 -f ~A'" (strcat (shortname *self*) "-AFL"))
;;;                        :error-output (error-output task))
;;;           (dolines (line io-stream)
;;;             (format t "pkill output: ~A" line)))
;;;   (if (optimus-prime-p *self*)
;;;       (upload-pov-to-analystio (output-pathname task) (target task))
;;;       (send-message-to-optimi :type :new-pov :target-id (id (target task)) :pov-path (output-pathname task)))
;;;   (setf (reported task) T)
;;;   (setf (found-crash task) T)
;;; 	;; print this last, b/c PRT is gonna watch for it and might kill this process right after this gives success pattern
;;;   (dbug :top "RESULT: AFL has now pwned ~A targets!" (incf *successful-afl-tasks*))
;;; )
;;;
;;; #+nil
;;; (defmethod process-line ((task afl-task) line)
;;;   (with-slots (found-crash test-crash all-tests-crash
;;; 	       ret curtest res reported)
;;;       task
;;;
;;;     (dbug :top "got line [~A]" line)
;;;     (cond ((setf res (cl-ppcre:scan-to-strings "[123456789]+ uniq crashes" line))
;;; 	   (dbug :top "RESULT: AFL found: ~A!" res)
;;; 	   (when (not reported)
;;; 	     (afl-report-crash task)))
;;; 	  ;; [2/17/2018 2:09:16] TOP: got line [^[[1;94m[*] ^[[0mAttempting dry run with 'id:000002,orig:seed-2'...^[[0m]
;;; 	  ((setf ret (cl-ppcre:register-groups-bind (f) ("Attempting dry run with \'(.+)\'" line) f))
;;; 	   (setf curtest ret)
;;; 	   (dbug :top "RESULT: recognized dry run test case ~A" curtest))
;;; 	  ;; [2/17/2018 2:09:16] TOP: got line [^[[1;93m[!] ^[[1;97mWARNING: ^[[0mTest case results in a crash (skipping)^[[0m]
;;; 	  ((setf test-crash
;;; 		 (when (cl-ppcre:scan-to-strings "Test case results in a crash" line) t))
;;; 	   (dbug :top "RESULT: recognized test case crash, curtest was ~A" curtest)
;;; 	   (when (not reported)
;;; 	     (setf reported T)
;;; 	     (afl-report-crash task)))
;;; 	  ((setf res (cl-ppcre:scan-to-strings "All test cases time out or crash" line))
;;; 	   (dbug :top "RESULT: recognized all test case abort")
;;; 	   (setf all-tests-crash T))
;;; 	)
;;; ))
