(cl:in-package :fuzzbomb)

;; FIXME: Named docker-task-new for now to avoid conflicting with existing 'docker-task'.
;; Will need to rename and swap all existing docker task to use this one at some
;; point in the future.
(defclass docker-task-new (deliberation-task target-triggered)
  ((container-name :initarg :container-name :accessor container-name)
   (image :initform "" :initarg :image :accessor image)
   (cmd :initform nil :initarg :cmd :accessor cmd)
   (reuse-container-p :initform t :initarg :reuse-container-p :accessor reuse-container-p)
   (input-pathname :initform nil :initarg :input-pathname :accessor input-pathname)
   (output-pathname :initform nil :initarg :output-pathname :accessor output-pathname)
   (bin :initform nil :initarg :bin :accessor bin)
   (line-process-fn :initform nil :initarg :line-process-fn :accessor line-process-fn)
   (container-start-method :initform :ensure-image
                           :initarg :container-start-method
                           :accessor container-start-method)
   (container-start-args :initform "" :initarg :container-start-args
                      :accessor container-start-args)
   (return-val :initform nil :initarg :return-val :accessor return-val)
   (post-exec-fn :initform nil :initarg :post-exec-fn :accessor post-exec-fn)
   (pre-exec-fn :initform nil :initarg :pre-exec-fn :accessor pre-exec-fn)
   (results :initform nil :initarg :results :accessor results)
   (max-duration :initform 100000000 :initarg :max-duration :accessor max-duration)
   (error-output :initform :output :initarg :error-output :accessor error-output)))
   ;;(error-output :initform *error-output* :initarg :error-output :accessor error-output)))

;;; FIXME Remove the creation of the bin altogether
(defmethod initialize-instance :after ((task docker-task-new) &key)
  (dbug :docker-task "initialize-instance :after ((task docker-task-new): ~s" task)
  ;; Setup the bin and target slots with the path from the target
  ;; message if neither was initialized.
  (with-slots (parent bin target) task
    (sieve-subgraph parent)
    (sieve-node task :shape "box" :color "green")
    (sieve-node target :shape "triangle" :color "yellow")
    (sieve-edge task target)
    (sieve-member task parent)
    (sieve-member target parent)
    (when parent
      (let* ((target-msg (target-msg parent))
             (path (getassoc :path target-msg)))

        (unless bin
          (setf bin path))
        (unless target
          (setf target (make-instance 'target :path path))))))
;  (dbug :top "1 (bin task) = ~s" (bin task))
;  (dbug :top "1 (typep (bin task) 'bin) = ~s" (typep (bin task) 'bin))
;  (dbug :top "1 (type-of (bin task)) = ~s" (type-of (bin task)))
;  (dbug :top "1 (target task) = ~s" (target task))

  ;; Should there be both a target and a bin? Getting kind of
  ;; muddled...
  ;; (when (and (stringp (bin task))
  ;;         (executable-target-p (target task)))
  ;;   (setf (bin task)
  ;;     (make-instance 'bin
  ;;      :platform (with-slots (parent) task
  ;;         (when parent
  ;;            (getassoc :platform (target-msg parent))))
  ;;      :original-path (bin task))))

;  (dbug :top "2 (bin task) = ~s" (bin task))
;  (dbug :top "2 (typep (bin task) 'bin) = ~s" (typep (bin task) 'bin))
;  (dbug :top "2 (type-of (bin task)) = ~s" (type-of (bin task)))

  (when (input-pathname task)
    (ensure-directories-exist (input-pathname task)))
  (when (output-pathname task)
    (ensure-directories-exist (output-pathname task)))
  (unless (slot-boundp task 'container-name)
    (dbug :docker-task "Setting default container-name")
    ;;(setf (container-name task) (format nil "~A-~A" (uiop:getenv "CONTAINER_PREFIX") (image task)))
    (setf (container-name task) (create-default-container-name task))
    ))

(defmethod create-default-container-name ((task docker-task-new))
  (format nil "~A-~A" (uiop:getenv "CONTAINER_PREFIX") (image task)))

(defmethod kill-container ((task docker-task-new))
  ;; FIXME: What if another task is still using this container? May need some coordination.
  (let ((stop-cmd (format nil "../tools/docker stop ~A"  (container-name task)))
        (rm-cmd (format nil "../tools/docker rm -f ~A" (container-name task))))
    (dbug :top "Stopping container ~A" stop-cmd)
    (run-shell-command stop-cmd)
    (dbug :top "Removing container ~A" rm-cmd)
    (run-shell-command rm-cmd)))

(defmethod start-container ((task docker-task-new))
  (case (container-start-method task)
    (:ensure-image
     (let ((ensure-image-cmd (format nil "../tools/ensure-image ~A -d ~A --name ~A"
                                     (image task) (container-start-args task) (container-name task)))
           (ret 0))
       (dbug :top "Starting the container with command: ~A" ensure-image-cmd)
       ;; Start the container
       (setf ret (run-shell-command ensure-image-cmd :error-output *standard-output*))
       (when (not (= 0 ret))
         (dbug :top "ERROR: ensure-image failed, return code ~a!" ret)))
     (dbug :top "ensure-image finished"))
    (:docker-run
     (dbug :top "Using docker-run container start method. Container will be started by docker run command."))
    (:in-this-container
     (dbug :top "Using in-this-container start method. Container will not be started, we should already be inside."))
    (otherwise
     (when (and (not (container-start-method task))
                (not (container-name task)))
       (dbug :top "No container start method provided and container name is not set. Execution of command will likely fail. The container should either be started in pre-exec-fn and have the container-name slot set there, or a container-start-method should be provided.")))))

(defmethod process-line ((task docker-task-new) line)
  (if (null (line-process-fn task))
      (dbug :top "got line [~A]" line)
      (funcall (line-process-fn task) task line)))

;;; FIXME: This should really have better error-handling, but I
;;; don't know what we should do if this fails, or what happens
;;; if this returns NIL.  

; file: /home/rpg/projects/aixcc/asc-crs-lacrosse/crs/code/lisp/amp/docker-task.lisp
; in: DEFMETHOD EXEC-DOCKER-COMMAND (DOCKER-TASK-NEW)
;     (FUZZBOMB::IMAGE FUZZBOMB::TASK)
;
; note: deleting unreachable code
(defmethod exec-docker-command ((task docker-task-new))
  (let ((neo-fuzz-home (cond ((uiop:getenv "HOST_NEO_FUZZ_HOME"))
                             ((uiop:getenv "NEO_FUZZ_HOME"))
                             (t (asdf:system-relative-pathname :fuzzbomb "../../")))))
    (dbug :top "Running command: ~A" (cmd task))
    (case (container-start-method task)
      (:docker-run
       (with-docker-stream
           ;; FIXME: for some reason, SBCL thinks that (IMAGE TASK) here is
           ;; unreachable code.  Do not know why.  
           ;; file: amp/docker-task.lisp
           ;; in: DEFMETHOD EXEC-DOCKER-COMMAND (DOCKER-TASK-NEW)
           ;;     (FUZZBOMB::IMAGE FUZZBOMB::TASK)
           ;;
           ;; note: deleting unreachable code
           (io-stream (image task) (cmd task)
                      :remove-p t
                      :mounts (list (list neo-fuzz-home "/neo-fuzz"))
                      :extra-args (container-start-args task)
                      :use-image-name-for-container nil
                      :error-output (error-output task))
         (dolines (line io-stream)
                  (process-line task line))))
      (:ensure-image
       (with-docker-exec-stream
           (io-stream (container-name task) (cmd task)
                      :error-output (error-output task))
         (dolines (line io-stream)
                  (process-line task line))))
      (:in-this-container
       (with-external-program-stream (io-stream (cmd task) nil :error-output (error-output task))
         (dolines (line io-stream)
                  (process-line task line))))
      (otherwise
       (dont-error
        (error "EXEC-DOCKER-COMMAND failed with unexpected value for container-start-method: ~s"
               (container-start-method task)))
       nil))))

(defmethod post-exec ((task docker-task-new))
  (if (post-exec-fn task)
      (dont-error (funcall (post-exec-fn task) task))
      (dbug :top "Completed task: ~A ~A" (image task) (cmd task))))

(defmethod pre-exec ((task docker-task-new))
  (if (pre-exec-fn task)
      (dont-error (funcall (pre-exec-fn task) task))
      (dbug :top "Starting task: ~A ~A" (image task) (cmd task))))

(defmethod really-execute-task ((task docker-task-new))
  (let ((img (image task)))
    (dbug :timeline "BEGIN task ~a on target ~a (~a)." (type-of task) (target task) (cmd task))
    (dbug :top "Executing docker task: image=~A cmd=~A" img (cmd task))

    (pre-exec task)

    (when (and (eq :ensure-image (container-start-method task))
               (not (reuse-container-p task)))
      (kill-container task))

    (start-container task)

    (dont-error (exec-docker-command task))

    (post-exec task)
    (dbug :timeline "END task ~a on target ~a." (type-of task) (target task))))

(defmethod suspend-delib-task ((task docker-task-new))
  ;; FIXME: What if another task is using the same container? Can't suspend one without
  ;; suspending both. Same for resuming and killing.
  (when (container-name task)
    (dbug :top "Suspending docker task")
    (let ((pause-cmd (format nil "../tools/docker pause ~A" (container-name task))))
      (run-shell-command pause-cmd))))

(defmethod resume-delib-task ((task docker-task-new))
  (when (container-name task)
    (dbug :top "Resuming docker task")
    (let ((resume-cmd (format nil "../tools/docker unpause ~A" (container-name task))))
      (run-shell-command resume-cmd))))

(defmethod kill-delib-process ((task docker-task-new))
  (cond
    ((container-name task)
     (dbug :top "Killing docker task")
     (kill-container task))
    (t
     (call-next-method))))

(defmethod expected-duration ((task docker-task-new))
  (or (max-duration task) *default-deliberation-timeout*))

(defmethod write-result-to-target-dir (jsonable-obj (task docker-task-new))
  ;; in a perfect world this would be a method on lacrosse-task, but lacrosse re-uses
  ;; some legacy tasks.
  (assert *lacrosse*)
  (let ((output-path (results-pathname task)))
    (dbug :target "write-result-to-target-dir path: ~s" output-path)
    (with-open-file (output-stream output-path
                                   :if-does-not-exist :create
                                   :if-exists :supersede
                                   :direction :output)
                    (cl-json:encode-json jsonable-obj output-stream))))

(defmethod results-pathname ((task docker-task-new))
  (results-pathname-for-task-class (type-of task) (target task)))

(defmethod results-pathname-for-task-class ((task-class-name t) (target-node hist-tree-target-node))
  (results-pathname-for-task-class task-class-name (target target-node)))

(defmethod results-pathname-for-task-class ((task-class-name t) (target target))
  (let ((filename (format nil "~a-result.json" (string-downcase (symbol-name task-class-name)))))
    (merge-pathnames filename (output-dir target))))

;;;;;;;;;;;;;;
;; EXAMPLES ;;
;;;;;;;;;;;;;;

;; Start a NF image and execute a command in it then kill the container
;; (let ((task (make-instance 'docker-task-new
;;                         :image "neo-fuzz-ccl"
;;                         :cmd "/bin/bash -c \"cd ~ && pwd\"")))
;;   (really-execute-task task)
;;   (kill-container task))

;; ;; Start a NF image and execute a command in it then kill the container
;; ;; input-pathname and output-pathname should be automatically set
;; (let ((task (make-instance 'docker-task-new
;;                         :bin "json.lisp"
;;                         :image "neo-fuzz-ccl"
;;                         :cmd "/bin/bash -c \"cd ~ && pwd\"")))
;;   (format t "INPUT-PATHNAME: ~A" (input-pathname task))
;;   (format t "OUTPUT-PATHNAME: ~A" (output-pathname task)))

;; ;; Same as last one, but run two tasks. Container should be re-used
;; (let ((task-1 (make-instance 'docker-task-new
;;                         :image "neo-fuzz-ccl"
;;                         :cmd "/bin/bash -c \"cd ~ && pwd\""))
;;       (task-2 (make-instance 'docker-task-new
;;                         :image "neo-fuzz-ccl"
;;                         :cmd "/bin/bash -c \"cd ~ && pwd\"")))
;;   (really-execute-task task-1)
;;   ;; Container should stay alive
;;   (really-execute-task task-2)
;;   (kill-container task-2))

;; ;; Same as last one, but re-run a second task that does not reuse container.
;; ;; Should kill the container in between executions
;; (let ((task-1 (make-instance 'docker-task-new
;;                         :image "neo-fuzz-ccl"
;;                         :cmd "/bin/bash -c \"cd ~ && pwd\""))
;;       (task-2 (make-instance 'docker-task-new
;;                         :image "neo-fuzz-ccl"
;;                         :cmd "/bin/bash -c \"cd ~ && pwd\""
;;                         :reuse-container-p nil)))
;;   (really-execute-task task-1)
;;   ;; Should kill and restart container
;;   (really-execute-task task-2)
;;   (kill-container task-2))

;; ;; Run a command in a temporary container. Should use 'docker run --rm'
;; ;; Container should be removed automatically by docker.
;; ;; Does not use ensure-image tools that automatically mount /neo-fuzz
;; ;; /neo-fuzz should still be mounted due to mounts being specified
;; ;; when executed
;; (let ((task (make-instance 'docker-task-new
;;                         :image "ubuntu"
;;                         :cmd "/bin/bash -c \"cd /neo-fuzz && ls\""
;;                         :container-start-method :docker-run)))
;;   (really-execute-task task)
;;   ;; Container should automatically be cleaned up
;;   )

;; ;; User defined line processing function
;; (let ((task (make-instance 'docker-task-new
;;                         :image "ubuntu"
;;                         :cmd "/bin/bash -c \"cd /neo-fuzz && ls\""
;;                         :container-start-method :docker-run
;;                         :line-process-fn (lambda (task line)
;;                                            (when (string= line "code")
;;                                              (format t "!!!!! Found the code directory !!!!!"))))))
;;   (really-execute-task task))

;; ;; Run a task with a post process fn
;; (let ((task (make-instance 'docker-task-new
;;                         :image "neo-fuzz-ccl"
;;                         :cmd "/bin/bash -c \"cd ~ && pwd\""
;;                         :pre-exec-fn
;;                         (lambda (task) (format t "!!!!! STARTING !!!!!"))
;;                         :post-exec-fn
;;                         (lambda (task)
;;                           (format t "!!!!! FINISHED !!!!!")))))
;;   (really-execute-task task)
;;   (kill-container task))

;; ;; Port of existing driller task. Run driller on some binary.
;; ;; Does not run isabel afterwards. May need some way to perform some step
;; ;; after task completes.
;; (let* ((output-pathname "some-path")
;;        (cmd (format nil "/realuser.sh /bin/bash --login -c 'AFL_SKIP_CRASHES=1 LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:/neo-fuzz/code/docker/driller/cgc-libs /neo-fuzz/code/tools/run-driller -f ~A -C -d ~A -t ~A -w ~A ~A ~A'" *driller-force-interval* *driller-workers* *driller-timeout* task *driller-more-args* "some-bin"))
;;        (line-fn
;;      (lambda (task line)
;;        (dbug :top "got line [~A]" line)
;;        (cond ((setf res (cl-ppcre:scan-to-strings "Crash found" line))
;;               (dbug :top "RESULT: Driller found: ~A!" res)
;;               (setf (results task) res)
;;               (when (not reported)
;;                 (setf reported T)
;;                 (incf *successful-driller-tasks*)
;;                 (dbug :top "RESULT: Driller has now pwned ~A targets!" *successful-driller-tasks*)
;;                 (send-message-to-optimi :type :new-pov :svc-id (id task) :pov-path (output-pathname task))
;;                 (make-results-accessible task)))
;;              ((setf ret (cl-ppcre:register-groups-bind (f) ("Attempting dry run with \'(.+)\'" line) f))
;;               (setf curtest ret)
;;               (dbug :top "RESULT: recognized dry run test case ~A" curtest))
;;              ((setf testcrash (cl-ppcre:scan-to-strings "Test case results in a crash" line))
;;               (dbug :top "RESULT: recognized test case crash, curtest was ~A" curtest)
;;               (when (not reported)
;;                 (setf reported T)
;;                 (incf *successful-driller-tasks*)
;;                 (dbug :top "RESULT: Driller has now pwned ~A targets!" *successful-driller-tasks*)
;;                 (send-message-to-optimi :type :new-pov :svc-id (id task) :pov-path (output-pathname task))
;;                 (make-results-accessible task)))
;;              ((setf res (cl-ppcre:scan-to-strings "No amp connection to send data" line))
;;               (dbug :top "Driller not connected to amp socket. Sendind data from log to GUI.")
;;               (let ((msg (read-from-string (subseq line 32))))
;;                 (process-gui-update-msg msg)))
;;              ((setf res (cl-ppcre:scan-to-strings "All test cases time out or crash" line))
;;               (dbug :top "RESULT: recognized all test case abort")
;;               (setf alltestscrash T)))))
;;        (task (make-instance 'docker-task-new
;;                          :image "driller"
;;                          :cmd cmd
;;                          :output-pathname output-pathname))))

;; ;; Format port of existing tfuzz task
;; (let* ((cmd (format nil "/realuser.sh /bin/bash -c 'cd /root/release && git reset --hard && git checkout ~A && cd ~~ && source /root/.virtualenvs/tfuzz_test/bin/activate && /neo-fuzz/code/T-Fuzz/utils/run.sh ~A \"~A\" ~A'"
;;                   *tfuzz-code-branch*
;;                   "some-binary"
;;                   *tfuzz-target-options*
;;                   *tfuzz-fuzz-timeout*))
;;        (task (make-instance 'docker-task-new
;;                          :image "tfuzz"
;;                          :cmd cmd)))
;;   (really-execute-task task))

;; ;; Format port of existing afl task
;; (let* ((output-pathname "some/path")
;;        (cmd (format nil "/bin/bash --login -c 'AFL_NO_FORKSRV=1 AFL_SKIP_CPUFREQ=1 /home/afl-2.41b/afl-fuzz ~:[-n~;-Q~]  -m none -i /neo-fuzz/code/etc/default-afl-inputs -o ~A ~A'"
;;                  *afl-qemu-mode*
;;                  (output-pathname task)
;;                  "some-binary"))
;;        (task (make-instance 'docker-task-new
;;                          :image "afl-qemu:v1"
;;                          :container-start-method :docker-run
;;                          :cmd cmd
;;                          :line-process-fn
;;                          (lambda (task line)
;;                            (dbug :top "got line [~A]" line)
;;                            (when (setf res (cl-ppcre:scan-to-strings "[123456789]+ uniq crashes" line))
;;                              (dbug :top "RESULT: AFL found ~A!" res)
;;                              (setf (results task) res)
;;                              (incf *successful-afl-tasks*)
;;                              (make-results-accessible task)
;;                              (dbug :top "RESULT: AFL has now pwned ~A targets!" *successful-afl-tasks*)
;;                              (send-message-to-optimi :type :new-pov :svc-id (id task) :pov-path (output-pathname task))
;;                              (when *use-isabel* (run-isabel task))
;;                              (kill-task task))))))
;;   (really-execute-task task))

;; ;; Format port of existing grassmarlin task
;; (let* ((output-pathname "some-path")
;;        (cmd (format nil "/realuser.sh /bin/bash -c 'cp ~A /neo-fuzz/input.pcap xvfb-run grassmarlin'"
;;                  "path-to-pcap"))
;;        (task (make-instance 'docker-task-new
;;                          :image "grassmarlin"
;;                          :cmd cmd
;;                          :output-pathname output-pathname
;;                          :line-process-fn
;;                          (lambda (task line)
;;                            (dbug :top "got line [~A]" line)
;;                            (when (setf res (cl-ppcre:scan-to-strings "Launching GrassMarlin 3.2.1" line))
;;                              (dbug :top "RESULT: GrassMarlin started: ~A!" res)
;;                              (dbug :top "RESULT: GrassMarlin copied results to output-pathname.")))
;;                          :post-exec-fn
;;                          (lambda (task)
;;                            (cp (format nil "~a/input.pcap" *neo-fuzz-home*) (format nil "~a/~a" (output-pathname task) "input.pcap"))
;;                            (cp (format nil "~a/output.gm3" *neo-fuzz-home*) (format nil "~a/~a" (output-pathname task) "output.gm3"))
;;                            (let ((unzip-cmd (format nil "/unzip-wd.sh ~a ~a" (output-pathname task) (format nil "~a~a" (output-pathname task) "output.gm3"))))
;;                              (dbug :top "unzipping with: ~s~%" unzip-cmd)
;;                              (docker-exec (strcat (uiop:getenv "CONTAINER_PREFIX") "-grassmarlin") unzip-cmd)
;;                              ;;   host-pairs for demo-201801-2.prt (the demo_sys demo).
;;                              ;; host-objs for new demo
;;                              (multiple-value-bind (host-pairs host-objs)
;;                                  (process-grassmarlin-output (format nil "~a/~a" (output-pathname task) "output.gm3")
;;                                                              (output-pathname task))
;;                                (with-open-file (pairs-stream (format nil "~a/~a" (output-pathname task) "host-pairs.txt")
;;                                                              :direction :output :if-exists :supersede)
;;                                  (dolist (pair host-pairs)
;;                                    (format pairs-stream "~a~%~a~%" (first pair) (second pair))))

;;                                ;; Create task for protocol fuzzing.
;;                                (let* ((proto-bin (make-instance 'bin
;;                                                                 :original-path (format nil "~a~a" (output-pathname task) "input.pcap")))
;;                                       (proto-task (make-new-pcap-task proto-bin -1)))
;;                                  (setf (output-pathname proto-task) (output-pathname task))
;;                                  (setf (hosts proto-task) host-objs))))))))
;;   (really-execute-task task))
