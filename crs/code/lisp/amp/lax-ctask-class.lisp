;;; -------------------------------------------------------------------------
;;; lax-ctask-class.lisp
;;; - target class for AIXCC Finals. In their terminology, this is a task, but
;;;   in ours, it's a target.
;;; - much inherited from target-class.lisp, new stuff goes here!
;;; -------------------------------------------------------------------------
;;; COPYRIGHT START
;;; COPYRIGHT END
;;; -------------------------------------------------------------------------

(cl:in-package :fuzzbomb)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  lax-ctask
;;;
;;; New stuff (as of 20250306) goes here!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Re-interpreting some lacrosse-cp-target slots for lax-final:
;;; - cp-path : points to /cp_root/<task-id>/ dir
;;; - cp-sources : list of source dirs w/in /cp_root/<task-id>/repo/

;;; inheriting from target class: source, but we're using it
;;; quite differently
;;; source is an alist
(defclass lax-ctask (lacrosse-cp-target)
  (
   (task-id :initarg :task-id :initform nil :accessor task-id
            :documentation "Name assigned by competition organizers.")
   (project-name :initarg :project-name :initform nil :accessor project-name
                 :documentation "Name assigned by competition organizers.")
   (metadata :initarg :metadata :initform nil :accessor metadata
             :documentation "Metadata assigned by competition organizers.")
   (deadline :initarg :deadline :initform nil :accessor deadline
             :documentation "Deadline assigned by competition organizers.")
   (cached-deadline-exceeded-p :initform nil :accessor cached-deadline-exceeded-p
                        :documentation "t when deadline had passed last time it was checked.")
   (focus :initarg :focus :initform nil :accessor focus
          :documentation "Focus assigned by competition organizers.")
   (harnesses-included :initarg :harnesses-included :initform nil :accessor harnesses-included
                       :documentation "Harnesses-Included assigned by competition organizers.")
   (ctask-type :initarg :ctask-type :initform nil :accessor ctask-type
               :documentation "Ctask-Type assigned by competition organizers.  String, one of delta and full.")
   (delta-applied-p :initarg :delta-applied-p :initform nil :accessor delta-applied-p
		    :documentation "Bool to keep state of whether delta is currently applied to the focus repo.")
   ;; FIXME Is this the best way to deal with patches that don't have PoV's?
   (loose-patch-cands :initarg :loose-patch-cands :initform nil :accessor loose-patch-cands
		      :documentation "A list of patch cands that do not have associated vuln cands.")
   (all-sarif-asss :initform nil :accessor all-sarif-asss
		      :documentation "A list of sarif assessments for this CT.")
   (all-bundles :initform nil :accessor all-bundles
		:documentation "A list of potential bundles for this CT.")
   (src-vuln-hyps :initarg :src-vuln-hyps :initform nil :accessor src-vuln-hyps
		  :documentation "List of vuln-hyp objects. Each describes a hypothesized vulnerability in terms of source code, and may or may not explain a vuln-cand (PoV)."
		  :type list)
   )
  (:documentation "The target class for Lacrosse finals.")
  )

;;; Assign a dir before init'g slots.
(defmethod initialize-instance :around ((ctask lax-ctask) &rest initargs &key)
  ;;   HACK, override the optimus dir... prob shouldn't be sending this at all.
  ;; This HACK may be specific to semi?
  ;;(when (not (getf initargs :dir))
  (dbug :target "init-inst :around task-id: ~s" (getf initargs :task-id))
  (setf (getf initargs :dir) (format nil "~a/~a/~a/" *experiment-dir* (getf initargs :task-id) (shortname *self*)))
  (dbug :target "set :dir to ~s" (getf initargs :dir))
   ;; )
  (apply #'call-next-method ctask initargs))

(defmethod initialize-instance :after ((ctask lax-ctask) &key)
  ;;(setf (cp-name ctask) (alexandria:last-elt (pathname-directory (first (uiop:subdirectories *cp-root*)))))
  ;;(dbug :ctask "cp name is ~a" (cp-name ctask))
  (dbug :ctask "copying to my dir")
  ;;(run-command (format nil "rsync -a ~A ~A" (strcat *cp-root* "/") (dir ctask)))
  ;;(uiop:run-program (format nil "rsync -va ~A ~A" (strcat *cp-root* "/") (dir ctask)) :output t)
  ;;(uiop:run-program (format nil "cp -rf ~A ~A" (strcat *cp-root* "/") (dir ctask)) :output t)
  (let ((copy-cmd (format nil "cp -rf ~A ~A" (cp-path ctask) (dir ctask))))
    ;;(uiop:run-program (format nil "ls -al ~a" (cp-path ctask)) :output t :error-output :output)
    ;;(uiop:run-program (format nil "ls -al ~a" (dir ctask)) :output t :error-output :output)
    (dbug :target "copy-cmd: ~s" copy-cmd)
    (uiop:run-program copy-cmd :output t :error-output :output))
  (dbug :target "done copying cp to my dir")
  (setf (path ctask) (strcat (dir ctask) (task-id ctask)))
  ;; Try calling lax-asc-init-optimus.sh here.  
  (when (optimus-prime-p *self*)
    (uiop:run-program (format nil "~a/lax-final-init-optimus.sh --exp-dir ~a --task-id ~a" *lax-tools* *experiment-dir* (task-id ctask)) :output t :error-output :output)))
;; after artifacts are created, can make fuzzer dictionary
    ;;(uiop:run-program (format nil "cd ~A; /lacrosse/code/tools/make-string-dictionary.sh project.yaml work/strings.dict" (path target)) :output t :error-output :output)

(defmethod lp-string ((ctask lax-ctask))
  (format nil "#<LAX-CTASK:~a ~a  ctask-type: ~a>"
          (id ctask) (project-name ctask) (ctask-type ctask)))

(defmethod brief-string ((ctask lax-ctask))
  (format nil "#<LAX-CTASK ~a ~a>" (id ctask) (project-name ctask)))

(defmethod short-description ((ctask lax-ctask))
  (format nil "~a-~a-~a[~a]" (project-name ctask) (ctask-type ctask) (task-id ctask) (id ctask)))

(defmethod spec-slots ((ctask lax-ctask))
  '(dir id cp-path
    task-id project-name
    cp-name ctask-type
    focus
    metadata
    deadline))

(defmethod spec ((ctask lax-ctask))
  (let ((slots (spec-slots ctask)))
    (iterate (for slot in slots)
      (collect (list (intern (symbol-name slot) :keyword)
                     (maybe-unpack-slot-val (funcall slot ctask)))))))

(defmethod full-ctask-p ((ctask lax-ctask))
  (string-equal "full" (ctask-type ctask)))

(defmethod delta-ctask-p ((ctask lax-ctask))
  (string-equal "delta" (ctask-type ctask)))

(defmethod has-passed-pov-p ((ctask lax-ctask))
  (find-if #'passed-p (vuln-cands ctask)))

(defmethod has-passed-patch-p ((ctask lax-ctask))
  ;;(dbug :top "has-passed-patch-p, (all-patch-cands ctask): ~s" (all-patch-cands ctask))
  ;;(when-dbug :top
  ;;	     (dolist (cand (all-patch-cands ctask))
  ;;	       (dbug :top " passed-p: ~s ~s" cand (passed-p cand))))
  (find-if #'passed-p (all-patch-cands ctask)))

(defmethod has-passed-pov-w-passed-patch-p ((ctask lax-ctask))
  (find-if #'(lambda (pov)
	       (and (passed-p pov)
		    (has-passed-patch-p pov)))
	   (vuln-cands ctask)))

(defmethod deadline-exceeded-p ((ctask lax-ctask))
  "Return t, if task deadline has passed."
  (cond ((cached-deadline-exceeded-p ctask))
        ((> (get-unix-time-millisecs) (deadline ctask))
         (setf (cached-deadline-exceeded-p ctask) t))
        (t nil)))

;;; this is just making annoying warnings and unclear if it makes errors/fails or not
;(declaim (ftype (function (lax-ctask list ; TODO: (trivial-types:proper-list string)
;                                     &key
;                                     (:ignore-error-status boolean)
;                                     (:depth integer)
;                                     (:timeout (or null string)))
;                          (values t t t &optional)) ;; Had to extend this to 3 because I need the exit-code and sbcl optimizes compilation by dropping the others
;                lax2-helper))
(defun dind-stdout-p (stdout)
  (or
   (search "Cannot connect to the Docker daemon" stdout)
   (search "cannot connect to the Docker daemon" stdout)
   (search "connection reset by peer" stdout)
   (search "i/o timeout" stdout)
   (search "Building fuzzers failed" stdout)
   (search "failed to solve: DeadlineExceeded: context deadline exceeded" stdout)
   (search "failed to create shim task" stdout)
   (search "failed to run Build function: DeadlineExceeded: failed to read dockerfile" stdout)
   ))

;; modeled on lax2-helper
(defun lax2-run-pov (ctask args &key (ignore-error-status t) timeout (depth 0))
  "Calls the run-pov.sh script that is part of the AIxCC finals."  
  (dbug :top "~s" (list 'lax2-run-pov ctask args :timeout timeout :depth depth :ignore-error-status ignore-error-status))
  (multiple-value-bind (stdout stderr code)
     (uiop:run-program `("env"
			 "-u" "OSS_FUZZ_SAVE_CONTAINERS_NAME" ; This is needed now that we are using this env var when we fuzz. Note it's still not perfectly safe; an (unlikely) race could cause issues.
			 ,(format nil "DOCKER_HOST=10.0.2.2:~a" (+ 50 (parse-integer (getenv "CIRCA_BASEPORT"))))
			 "/lacrosse/code/prt/timestamp"
			 ,@(and timeout
				(list "timeout" timeout))
			 "/lacrosse/code/tools/action-run-pov/run_pov.sh"
			 ,@args)
		       :directory (format nil "~a/fuzz-tooling" (path ctask))
		       :output :string
		       :error-output :output
		       :ignore-error-status ignore-error-status
		       :output t)
    (let ((dind-error-p (and (/= code 0) (dind-stdout-p stdout))))
      ;;(dbug :top "code isnt zero and depth ~a qualifies for possible retry" depth)
      (dbug :top "lax2-run-pov output: ~s" stdout)
      (values stdout stderr code (and dind-error-p :dind-error)))))

;;; Note new ver captures stdout and prints later so it can search for DIND error strings.
;;; The later printout is still needed for other code that captures stdout later and searches there.

(defun lax2-helper (ctask args &key (ignore-error-status t) timeout (depth 0) path)
  "Calls the helper.py script that is part of the AIxCC finals."
  (dbug :top "~s" (list 'lax2-helper ctask args :timeout timeout :depth depth))
  (setf path (or path (path ctask)))
  (dbug :top "path: ~s" path)
  (multiple-value-bind (stdout stderr code)
     (uiop:run-program `("env"
		      "-u" "OSS_FUZZ_SAVE_CONTAINERS_NAME" ; This is needed now that we are using this env var when we fuzz. Note it's still not perfectly safe; an (unlikely) race could cause issues.
		      ,(format nil "DOCKER_HOST=10.0.2.2:~a" (+ 50 (parse-integer (getenv "CIRCA_BASEPORT"))))
		      "/lacrosse/code/prt/timestamp"
		      ,@(and timeout
                             (list "timeout" timeout))
		      "infra/helper.py"
		      ,@args)
		    :directory (format nil "~a/fuzz-tooling" path)
		    :output :string
		    :error-output :output
		    :ignore-error-status ignore-error-status
		    :output t)
    (when (and (/= code 0) (< depth 5))
      (dbug :top "code isnt zero and depth ~a qualifies for possible retry" depth))
    (cond 
      ((and (/= code 0) (< depth 5)	;; maybe retry-- note some helper calls *should* return nonzero
	    (dind-stdout-p stdout)
            )	;; really retry, after some random delay
       (dbug :top "Found DIND error, retrying after delay")
       (dbug :dev "  Handling: ~s" stdout)
       (sleep (random 30))
       (lax2-helper ctask args :ignore-error-status ignore-error-status :timeout timeout :depth (1+ depth) :path path))
      ;; give up after 5 attempts
      ((>= depth 5)
       (dbug :top "dind failed after retrying.")
       (values stdout stderr code :dind-error))
      ;; dont print out until we get to either non-DIND error or success
      (T 
       (dbug :top "run-program returned stdout:[~a] stderr: [~a] code: [~a]" stdout stderr code)
       (values stdout stderr code nil)))
    ))

(defmethod build ((ctask lax-ctask))
  (dbug :top "Building ~s" ctask)
  (send-telemetry-event :building `(build-image ,ctask) (metadata ctask))
  (lax2-helper ctask `("build_image" "--pull" ,(project-name ctask))))

(declaim (ftype (function (lax-ctask
                           (member :address :none :memory :undefined :thread
                                   :coverage :introspector :hwaddress)
                           (member :libfuzzer :afl :honggfuzz :centipede :none
                                   :wycheproof))
                          (values t t t &optional)) lax2-build-fuzzers))

(defun lax2-build-fuzzers (ctask sanitizer engine)
  (send-telemetry-event :building `(build-fuzzers ,ctask) (metadata ctask))
  (lax2-helper ctask `("build_fuzzers"
                       "--sanitizer" ,(string-downcase (symbol-name sanitizer))
                       "--engine" ,(string-downcase (symbol-name engine))
                       ,(project-name ctask)
                       ,(format nil "~a/~a" (path ctask) (focus ctask)))))

(defmethod build-cmd-str ((ctask lax-ctask))
  (format nil "cd ~A/fuzz-tooling; env -u OSS_FUZZ_SAVE_CONTAINERS_NAME DOCKER_HOST=10.0.2.2:~a /lacrosse/code/prt/timestamp infra/helper.py build_image --pull ~a"
          (path ctask)
          (+ 50 (read-from-string (getenv "CIRCA_BASEPORT")))
          (project-name ctask)))

;; FIXME Is this right? *experiment-dir* looks to be an absolute path, so it looks odd.
(defmethod shared-path ((ctask lax-ctask))
  (format nil "~a/~a/crs/shared/" *experiment-dir* (task-id ctask)))

(defmethod focus-path ((ctask lax-ctask))
  (strcat (path ctask) "/" (focus ctask)))

(defmethod delta-path ((ctask lax-ctask))
  (strcat (path ctask) "/" (delta-relpath ctask)))

(defmethod delta-relpath ((ctask lax-ctask))
  "diff/ref.diff")

(defmethod fuzz-tooling-path ((ctask lax-ctask))
  (strcat (path ctask) "/" (fuzz-tooling-relpath ctask)))

(defmethod fuzz-tooling-relpath ((ctask lax-ctask))
  "fuzz-tooling")

(defmethod project-yaml-path ((ctask lax-ctask))
  (strcat (fuzz-tooling-path ctask) "/projects/" (project-name ctask) "/project.yaml"))

(defmethod delta-type-p ((ctask lax-ctask))
  (string-equal (ctask-type ctask) "delta"))

;; Applies the delta to the focus repo if not already applied.
;; Return t if successful (or already applied), nil if failed to apply.
(defmethod ensure-delta-applied ((ctask lax-ctask))
  (dbug :top "Ensuring delta is applied to ctask ~a" (brief-string ctask))
  (if (delta-applied-p ctask)
      (progn (dbug :top "Delta is already applied.")
	     t)
      (let ((cmd ;;(format nil "cd ~A; patch -p1 < ../~A"
              (format nil "cd ~A; git apply --binary ../~A"
			 (focus-path ctask)
			 (delta-relpath ctask))))
	(dbug :top "Applying delta ~s: ~s" (brief-string ctask) cmd)
	(multiple-value-bind (_ err code)
	    (uiop:run-program cmd
			      :output t
			      :error-output :string
			      :ignore-error-status t)
          (declare (ignorable _))
	  (dbug :top "Captured stderr: ~A~%" err)
	  (dbug :top "Delta cmd status is ~a with type ~a." code (type-of code))
	  (if (zerop code)
	      (progn (dbug :top "Delta applied successfully.")
		     (setf (delta-applied-p ctask) t))
	      (progn
		(dbug :error "Failed to apply delta: ~s" err)
		nil))))))

;; FIXME [MDM] loose-patch-cands is a hack to get finals working.
;; Seems like not all patch cands will have vuln cands in the finals.
(defmethod all-patch-cands ((ctask lax-ctask))
  (remove-duplicates
     (append (loose-patch-cands ctask)
	     (patch-cands-w-vcs ctask))))

(defmethod patch-cands-w-vcs ((ctask lax-ctask))
  (iter (for vc in (vuln-cands ctask))
    (appending (patch-cands vc))))

(defmethod num-patches-submitted ((ctask lax-ctask))
  (iter (for patch in (all-patch-cands ctask))
    (counting (submitted-p patch) into submitted)
    (finally (return submitted))))

(defmethod summ-submission-status ((ctask lax-ctask))
  (multiple-value-bind (submitted-vcs passed-vcs rejected-vcs)
      (iter (for vc in (vuln-cands ctask))
        (counting (submitted-p vc) into submitted)
        (counting (passed-p vc) into passed)
        (counting (rejected-p vc) into rejected)
        (finally (return (values submitted passed rejected))))
    (multiple-value-bind (submitted-patches passed-patches rejected-patches)
        (iter (for patch in (all-patch-cands ctask))
          (counting (submitted-p patch) into submitted)
          (counting (passed-p patch) into passed)
          (counting (rejected-p patch) into rejected)
          (finally (return (values submitted passed rejected))))
      (dbug :top "ctask status: ~a (~s/~s/~s) (~s/~s/~s)" (short-description ctask)
            submitted-vcs passed-vcs rejected-vcs
            submitted-patches passed-patches rejected-patches)
      (values submitted-vcs passed-vcs rejected-vcs
              submitted-patches passed-patches rejected-patches))))

(defmethod update-known-pov-vulns-json ((ctask lax-ctask))
  ;; This should do atomic writes via rename, and should use a file name that is deliberately *not*
  ;; unique based on the contents, in order to dedupe against the latest known_vulns in the python.  
  (let* ((path (strcat (output-dir ctask) "known-pov-vulns.json"))
	 (tmp-name (generate-tempfile-name :prefix "tmp-pov-vulns" :suffix ".json"))
	 (tmp-path (strcat (output-dir ctask) tmp-name))
	 (vulns (remove-if-not #'(lambda (svh) (vuln-cand svh)) ; pov vulns
			       (src-vuln-hyps ctask))))
    (write-to-json-file vulns tmp-path)
    ;; A pattern from ChatGPT for atomic write to shared file:
    (rename-file tmp-path path)
    path))

(defmethod update-known-non-pov-vulns-json ((ctask lax-ctask))
  ;; This should do atomic writes via rename, and should use a file name that is deliberately *not*
  ;; unique based on the contents, in order to dedupe against the latest known_vulns in the python.  
  (let* ((path (strcat (output-dir ctask) "known-non-pov-vulns.json"))
	 (tmp-name (generate-tempfile-name :prefix "tmp-non-pov-vulns" :suffix ".json"))
	 (tmp-path (strcat (output-dir ctask) tmp-name))
	 (vulns (remove-if #'(lambda (svh) (vuln-cand svh)) ; pov vulns
			   (src-vuln-hyps ctask))))
    (write-to-json-file vulns tmp-path)
    ;; A pattern from ChatGPT for atomic write to shared file:
    (rename-file tmp-path path)
    path))

(defmethod update-known-vulns-json ((ctask lax-ctask))
  ;; This should do atomic writes via rename, and should use a file name that is deliberately *not*
  ;; unique based on the contents, in order to dedupe against the latest known_vulns in the python.  
  (let* ((path (strcat (output-dir ctask) "known-vulns.json"))
	 (tmp-name (generate-tempfile-name :prefix "tmp-vulns" :suffix ".json"))
	 (tmp-path (strcat (output-dir ctask) tmp-name))
	 (vulns (src-vuln-hyps ctask))) ; all vulns
    (write-to-json-file vulns tmp-path)
    ;; A pattern from ChatGPT for atomic write to shared file. Is it to be trusted? Let's assume yes.
    (rename-file tmp-path path)
    ;; (dolist (vuln vulns)
    ;;   (dbug :top "Vuln ~a score ~a CWE ~a" vuln (score vuln) (cwe vuln)))
    path))

(defun find-vuln-by-desc (desc target &key (type)) ; type can be :pov or :non-pov or nil
  (let ((cand-hyps (cond ((eq type :pov)
			  (remove-if-not #'vuln-cand (src-vuln-hyps target)))
			 ((eq type :non-pov)
			  (remove-if #'vuln-cand (src-vuln-hyps target)))
			 (t
			  (src-vuln-hyps target)))))
    (find desc cand-hyps
	  :key #'description
	  :test #'string-equal)))

(defun link-inferior-dupe-vulns-from-json (inf-dupes-json target)
  (dbug :top "link-inferior-dupe-vulns-from-json ~s ~s..." inf-dupes-json target)
  (dbug :top "src-vuln-hyps: (~s) ~s..." (length (src-vuln-hyps target)) (src-vuln-hyps target))
  (let ((dupe-map-json (load-json-from-file inf-dupes-json)))
    ;; (dbug :top "linking duplicate vulns ~s..." dupe-map-json)
    (dolist (entry dupe-map-json)
      (let* ((sup-str (first entry))
	     (sup-vuln (find-vuln-by-desc sup-str target :type :pov))
	     (inf-str (second entry))
	     (inf-vuln (find-vuln-by-desc inf-str target :type :non-pov)))
	(unless sup-vuln
	  (dbug :top "WARNING: link-inferior-dupe-vulns-from-json did not find superior vuln with desc:~%~s" sup-str))
	(unless inf-vuln
	  (dbug :top "WARNING: link-inferior-dupe-vulns-from-json did not find inferior vuln with desc:~%~s" inf-str))
	(dbug :top "linking duplicate vulns...")
	(dbug :top "superior (should be pov) vuln:~%~a (pov-p: ~a)~%~s" sup-vuln (if (vuln-cand sup-vuln) t nil) (description sup-vuln))
	(dbug :top "inferior (should be non-pov) vuln:~%~a (pov-p: ~a)~%~s~%" inf-vuln (if (vuln-cand inf-vuln) t nil) (description inf-vuln))
	(push inf-vuln (duplicates sup-vuln))
	(push sup-vuln (duplicates inf-vuln))))))

(defmethod create-deadline-dependent-funcs ((ctask lax-ctask))
  (dbug :top "create-deadline-dependent-funcs: ~s" ctask)
  (let ((funcy-q (tt-func-q *self*))
        (deadline (deadline ctask)))
    (declare (ignorable deadline))
    (when (eq :deadline-dependent *use-llm-hypothesize-vulns-task*)
      (enqueue funcy-q (time-to-hyp-vulns ctask)
               #'(lambda ()
                   (dbug :top "Let's hyp some vulns: ~s" ctask)
                   (dbug :top "now: ~s  trigger-time: ~s  deadline: ~s"
                         (get-unix-time-millisecs)
                         (time-to-hyp-vulns ctask)
                         (deadline ctask))
                   (unless (>= (num-patches-submitted ctask) (patch-submit-limit ctask))
                     (assign-next-tasks ctask (list (hyp-vulns-task-next-task ctask)))))))
    (when (eq :delayed *psp-submit-strategy*)
      (dbug :top "  enqueuing submit-delayed-psps: ~s" ctask)
      (enqueue funcy-q (time-to-submit-psps ctask) #'(lambda ()
                                                       (dbug :top "Time to submit some psps: ~s" ctask)
                                                       (dbug :top "now: ~s  trigger-time: ~s  deadline: ~s"
                                                                   (get-unix-time-millisecs)
                                                                   (time-to-hyp-vulns ctask)
                                                                   (deadline ctask))
                                                       (submit-delayed-psps ctask)
                                                       )))
    
    
    ;; for dbug'g
    ;;(enqueue funcy-q (- deadline (* 5 60000)) #'(lambda () (dbug :top "5 minute warning for: ~s" ctask)))
    ;;(enqueue funcy-q (- deadline (* 10 60000)) #'(lambda () (dbug :top "10 minute warning for: ~s" ctask)))
    ;;(enqueue funcy-q (- deadline (* 15 60000)) #'(lambda () (dbug :top "15 minute warning for: ~s" ctask)))
    ;;(enqueue funcy-q (- deadline (* 20 60000)) #'(lambda () (dbug :top "20 minute warning for: ~s" ctask)))
    ;;(enqueue funcy-q (- deadline (* 25 60000)) #'(lambda () (dbug :top "25 minute warning for: ~s" ctask)))
    ;;(enqueue funcy-q (- deadline (* 30 60000)) #'(lambda () (dbug :top "30 minute warning for: ~s" ctask)))
    ))

(defmethod time-to-hyp-vulns ((ctask lax-ctask))
  (let ((time-delta (* 1000
                       (cond ((full-ctask-p ctask)
                              *default-time-to-hyp-vulns-full-ctask*)
                             ((delta-ctask-p ctask)
                              *default-time-to-hyp-vulns-delta-ctask*)
                             (t
                              (error "Fell through cond in time-to-hyp-vulns"))))))
    (- (deadline ctask) time-delta)))

(defmethod time-to-submit-psps ((ctask lax-ctask))
  (- (deadline ctask) (* 1000 *default-time-to-submit-psps*)))

;;;(defmethod patch-submit-limit ((ctask lax-ctask))
;;;  (let ((pov-patches-submitted (num-patches-submitted ctask)))
;;;    (min (* 2.5 num-patches-submitted) *default-patch-submit-limit*)))

(defmethod psp-threshold-score ((ctask lax-ctask))
  *default-psp-threshold-score*)

;;;(defmethod patch-submit-limit-delta ((ctask lax-ctask))
;;;  (let ((pov-patches-submitted (num-patches-submitted ctask)))
;;;    (cond ((zerop pov-patches-submitted)
;;;           1)
;;;          (t pov-patches-submitted))))

(defmethod patch-submit-limit ((ctask lax-ctask))
  *default-patch-submit-limit*)


(defun lax2-build-fuzzers-delta (ctask sanitizer engine)
  (dbug :top "lax2-build-fuzzers-delta: ~s" `(build-fuzzers ,ctask) (metadata ctask))
  (send-telemetry-event :building `(build-fuzzers ,ctask) (metadata ctask))
  (lax2-helper ctask `("build_fuzzers"
                       "--sanitizer" ,(string-downcase (symbol-name sanitizer))
                       "--engine" ,(string-downcase (symbol-name engine))
                       ,(project-name ctask)
                       ,(format nil "~a/~a" (cp-path ctask) (focus ctask)))
	       :path (cp-path ctask)))
