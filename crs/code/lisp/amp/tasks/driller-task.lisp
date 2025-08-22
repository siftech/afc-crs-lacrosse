;;; -------------------------------------------------------------------------
;;; COPYRIGHT START
;;; Copyright (c) 2019, Smart Information Flow Technologies (SIFT).  Developed with the sponsorship of the Office of Naval Research.
;;; COPYRIGHT END
;;; -------------------------------------------------------------------------

;;; Callback functions for creating a driller docker-task

(cl:in-package :fuzzbomb)



(defvar *driller-image-args*
  ""
;  (strcat "--privileged "	;; required for isabel -- gdb wants to trace child process
;   (apply
;    #'strcat
;     (mapcar (lambda (x) (format nil x (getenv "HOST_LACROSSE_HOME")))
;	   '("-v ~A/code/afl-other-arch:/home/angr/.virtualenvs/angr/bin/afl-unix "
;	     "-v ~A/code/driller:/home/angr/angr-dev/driller "
;	     "-v ~A/code/tracer:/home/angr/angr-dev/tracer "
;	     "-v ~A/code/fuzzer:/home/angr/angr-dev/fuzzer "))))

  "Extra arguments that need to be passed when starting the driller image due to
the container expecting the the python libs to be installed elsewhere. Put this in
:container-start-args when creating a driller docker-task."
)

(defclass driller-task (docker-task-new)
  ((found-crash :initform nil :accessor found-crash)
   ;; An AFL test case crashed
   (test-crash :initform nil :accessor test-crash)
   ;; All AFL test cases crashed
   (all-tests-crash :initform nil :accessor all-tests-crash)
   (ret :initform nil :accessor ret)
   (curtest :initform nil :accessor curtest)
   (res :initform nil :accessor res)
   (reported :initform nil :accessor reported)
   (whatclib-path :initform nil :initarg whatclib-path :accessor whatclib-path))
  )

(defmethod initialize-instance :after ((task driller-task) &key)
  (declare (special *ppc-lib-path*))
  (dbug :top "initialize-instance :after ((task driller-task): ~s" task)
  (when *lacrosse*
    (init-lacrosse-task-in-out-pathnames task))
  (with-slots (output-pathname cmd parent whatclib-path) task
    (when (input-channel (target task))
      (dbug :top "input-channel ~A" (input-channel (target task)))
      (dbug :top "named-file-channel-p ~A" (named-file-channel-p (input-channel (target task))))
      (cond ((and (named-file-channel-p (input-channel (target task)))
                  (filename (input-channel (target task))))
             (setf *driller-more-args*
                   (format nil "~A --use-file ~A"
                           *driller-more-args*
                           (filename (input-channel (target task))))))
            ))

    ;; Running on the results of whatclib analysis. Set the
    ;; whatclib-path slot and get the bin that whatclib ran on
    (when (whatclib-result-p parent)
      (dbug :top "Whatclib result")
      ;;(setf whatclib-path (getassoc :path (target-msg parent)))
      (setf whatclib-path (getattr :lib-addr-map (target task)))
      (dbug :top "Whatclib path: ~A" whatclib-path)
      (dbug :top "parent: ~A" parent)
      (dbug :top "parent parent: ~A" (parent parent))
      (dbug :top "parent parent parent: ~A" (parent (parent parent)))
;;;      (dbug :top "parent parent parent path: ~A" (getassoc :path (target-msg
;;;								  (parent          ;; bin whatclib used
;;;								   (parent parent) ;; whatclib-task
;;;								   ))))
;;;      (let ((bin-path (getassoc :path (target-msg
;;;                                       (parent          ;; bin whatclib used
;;;					(parent parent) ;; whatclib-task
;;;					)))))
;;;	(setf (bin task) (make-instance 'bin :original-path bin-path)))
      )
    (setf (input-pathname task)
          (or (input-pathname task) (strcat (namestring (path (target task))) "-input/")))
    (setf (output-pathname task)
          (or (output-pathname task) (strcat (namestring (path (target task))) "-output/")))
    
    ;; cheezily bypass the driller task's cmd if using canned povs
    ;; FIXME this could use re-indentation
    (cond (*use-canned-povs*
           (setf cmd "/realuser.sh /bin/echo just running isabel"))
          ;; FIXME   the below oddly lexically redefines global... do we need that??
          (T ;; Reset the driller args to account for the whatclib file, if available.
           (let ((*driller-more-args*
                  (cond
                   (whatclib-path (format nil "--whatclib-path ~A ~A" whatclib-path *driller-more-args*))
                   (t *driller-more-args*))))
             (let ((target-path (cond (*lacrosse*
                                       (target-executable (target task)))
                                      (t (path (target task))))))
               (dbug :top "target-path: ~s" target-path)
               (setf cmd
                     (format nil "/realuser.sh /bin/bash --login -c 'LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:/neo-fuzz/code/docker/driller/cgc-libs/:~A /neo-fuzz/code/tools/run-driller --use-determ-steps -f ~A -C -bt ~A ~:[~;--no-afl-plusplus~] -d ~A -t ~A -w ~A ~A ~A'" *ppc-lib-path* *driller-force-interval* *use-afl-blind-thread* *no-afl-plusplus* *driller-workers* *driller-timeout* output-pathname *driller-more-args* (namestring target-path)))))
           ))
    ))

(defun driller-report-crash (task)
  (incf *successful-driller-tasks*)
  (cond ((optimus-prime-p *self*)
         (upload-pov-to-analystio (output-pathname task) (target task)))
	(T
         (send-message-to-optimi :type :new-pov :target-id (id (target task)) :pov-path (output-pathname task))))
  ;; print this last, b/c PRT is gonna watch for it and might kill this process right after this gives success pattern
  (dbug :top "RESULT: Driller has now pwned ~A targets!" *successful-driller-tasks*)
)

(defmethod process-line ((task driller-task) line)
  (with-slots (found-crash test-crash all-tests-crash
	       ret curtest res reported)
      task

    (dbug :top "got line [~A]" line)
    (cond ((setf res (cl-ppcre:scan-to-strings "Crash found" line))
	   (dbug :top "RESULT: Driller found: ~A!" res)
	   (setf found-crash t)
	   (when (not reported)
	     (setf reported T)
	     (driller-report-crash task)))

	  ;; [2/17/2018 2:09:16] TOP: got line [^[[1;94m[*] ^[[0mAttempting dry run with 'id:000002,orig:seed-2'...^[[0m]
	  ((setf ret (cl-ppcre:register-groups-bind (f) ("Attempting dry run with \'(.+)\'" line) f))
	   (setf curtest ret)
	   (dbug :top "RESULT: recognized dry run test case ~A" curtest))
	  ;; [2/17/2018 2:09:16] TOP: got line [^[[1;93m[!] ^[[1;97mWARNING: ^[[0mTest case results in a crash (skipping)^[[0m]
	  ((setf test-crash
		 (when (cl-ppcre:scan-to-strings "Test case results in a crash" line) t))
	   (dbug :top "RESULT: recognized test case crash, curtest was ~A" curtest)
	   (when (not reported)
	     (setf reported T)
	     (driller-report-crash task)))
	  ((setf res (cl-ppcre:scan-to-strings "All test cases time out or crash" line))
	   (dbug :top "RESULT: recognized all test case abort")
	   (setf all-tests-crash T))
;	  ((setf res (cl-ppcre:scan-to-strings "No amp connection to send data" line))
;	   (dbug :top "Ignoring logged msg for gui.")
;	   (dbug :top "Driller not connected to amp socket. Sending data from log to GUI.")
;	   ;; Hardcoded subseq should probably be changed
;	   (let ((msg (read-from-string (subseq line 32))))
;	     (process-gui-update-msg msg))
;	)
	)))

(defmethod pre-exec ((task driller-task))
  (dbug :amp "Executing driller-task.")
  (dbug :top "input-pathname ~A" (input-pathname task))
  (dbug :top "output-pathname ~A" (output-pathname task))
  (when (ppc-bin-unpatched-p (target task))
  	(dbug :top "Patching ELF for a PPC target")
	(let ((cmd (format nil "patchelf --set-interpreter ~A ~A" (ppc-lib-linker-path) (path (target task)))))
  	  (dbug :top "cmd is ~A" cmd)
          (dont-error (uiop:run-program cmd))
  	  (dbug :top "Done patching elf")
	  ) 
  )
)

(defmethod post-exec ((task driller-task))
  (dbug :top "post-exec for driller-task")
  ;; Run isabel if a crash was found and isabel is enabled
  (with-slots (found-crash test-crash all-tests-crash) task
    (cond ((or found-crash test-crash all-tests-crash *use-canned-povs*)
	   (when (or *lacrosse* *use-isabel*) (run-isabel task)))
	  (t	;; otherwise, timed out or otherwise failed
	   (dbug :top "RESULT: AFL/Driller timed out or failed")))))

;;;   FIXME cleanup/remove tags
(defun whatclib-result-p (target-node)
  (or (member :whatclib-result (getassoc :tags (target-msg target-node)))
      (member :whatclib-result (tags (target target-node)))
      (getattr :lib-addr-map (target target-node))))

(defmethod task-applies-to-target-p ((task-class-name (eql 'driller-task)) target-node)
  (dbug :docker-task "task-applies-to-target-p driller-task")
  (dbug :docker-task "whatclib-result-p: ~s  executable-target-p: ~s"
        (whatclib-result-p target-node)
        (executable-target-p target-node))
  (when *use-driller*
    (if *use-whatclib*
	(whatclib-result-p target-node)
      ;; Don't try running on an unpatched ppc binary. It will fail.
      (and (executable-target-p target-node)
           ;; Driller shouldn't run on intermediate pereti products
           (if *use-pereti-disasm* (not (eql (source (target target-node)) :pereti)) t))
  )))

;;; -------------------------------------------------------------------------
;;; remnants of former task-encapsulated simple ppc patchelf call, now done directly in nf-ccl image when appropriate

(defmethod ppc-bin-unpatched-p ((tgt target))
    (and
     (search "executable, PowerPC" (file-results tgt))
     (search "interpreter /lib/" (file-results tgt))))

(defmethod ppc-bin-unpatched-p ((tn hist-tree-target-node))
  (ppc-bin-unpatched-p (target tn)))

(defvar *ppc-lib-path* "/neo-fuzz/code/target-libs/ppc-libs/")

(defun ppc-lib-linker-path ()
  (strcat *ppc-lib-path* "ld.so.1"))
