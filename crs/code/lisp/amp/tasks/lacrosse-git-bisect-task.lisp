;;; -------------------------------------------------------------------------
;;; COPYRIGHT START
;;; COPYRIGHT END
;;; -------------------------------------------------------------------------

(cl:in-package :fuzzbomb)

(defclass lacrosse-git-bisect-task (lacrosse-task)
  (
   (vc-id
    ;;:type (or null int)
    :documentation "Int id for vuln-cand."
    :initform nil
    :accessor vc-id)
   (harness-id
    :initform nil
    :accessor harness-id)
   (harness-name
    :initform nil
    :accessor harness-name)
   (blob
    :initform nil
    :accessor blob)
   (cp-src
    :initform nil
    :accessor cp-src
    ;;:type string
    :documentation "string naming the src directory to target")
   ))

(defmethod print-object ((task lacrosse-git-bisect-task) str)
  (print-unreadable-object (task str :type t)
    (format str "[~a] id: ~a" (name task) (vc-id task))))

(defmethod scoring-fn ((task lacrosse-git-bisect-task)) 10)

(defmethod task-applies-to-target-p ((task-class-name (eql 'lacrosse-git-bisect-task)) target-node)
  (dbug :docker-task "task-applies-to-target-p lacrosse-git-bisect-task")
  (let ((target (target target-node)))
    (and (next-task target)
	 (let ((task-type (getassoc :task-type (next-task target))))
	   (eq task-type task-class-name)))))

(defmethod initialize-instance :after ((task lacrosse-git-bisect-task) &key)
  (dbug :top "initialize-instance :after ((task lacrosse-git-bisect-task): ~s" task)
  (cond ((next-task (target task))
	 (dbug :top "Setting slots from next-task")
	 (dbug :top " (next-task (target task)): ~s" (next-task (target task)))
	 ;;(dbug :top " (getassoc :vc-id (next-task (target task))): ~s" (getassoc :vc-id (next-task (target task))))
	 (setf (vc-id task) (getassoc :vc-id (next-task (target task))))
	 (setf (blob task) (getassoc :blob (next-task (target task))))
	 (setf (harness-id task) (getassoc :harness-id (next-task (target task))))
	 (setf (harness-name task) (harness-prop :name (find-harness-by-id (harness-id task) (target task))))
	 (setf (cp-src task) (getassoc :cp-src (next-task (target task))))
	 (setf (cmd task)
	       (format nil "cd ~a && /lacrosse/code/tools/bisect.sh ~a ~a ~a"
		       (path (target task))
		       (blob task)
		       (harness-name task)
		       (cp-src task))))
	(t
	 (dbug :top "WARNWARN lacrosse-git-bisect-task initializing without next-task.")
	 ))
  (dbug :top "Task cmd: ~a" (cmd task))
  (dbug :top "lacrosse-git-bisect-task done initialization"))

(defvar *last-run-pov-outdir* ".")  ;; Cheesy FIXME for fake mode, we dont fake the pov run so just use this for now
					;; this revised default shouldn't bother regular runs at all

(defmethod pre-exec ((task lacrosse-git-bisect-task))
  (dbug :amp "Executing lacrosse-git-bisect-task: ~a" task)
  (when *fake-git-bisect*
	(dbug :top "WARNING: overriding cmd with fake bisect")
  	(setf (cmd task) (format nil "cd ~a && /lacrosse/code/tools/fake-bisect.sh" (path (target task))))
  	(dbug :top "Task cmd: ~a" (cmd task)))
  (reset-source (target task)))

(defmethod process-line ((task lacrosse-git-bisect-task) line)
 (let (ret bic ) 
  (dbug :top "got line [~A]" line)
		;; this is a code pattern for when you want to use matched stuff,
		;; but you don't need it later when other lines match etc.
  (cond ((setf bic (cl-ppcre:register-groups-bind (f) ("(\[a-fA-F0-9\]+) is the first bad commit" line) f))
           ;;(send-message-to-optimi :type :bic-found :bic bic)  ;;  send msgs first, for speed
           ;;(setassoc :bic bic (vuln-cand task))
	   (dbug :top "RESULT: Found BIC ~A" bic)
		;; now we should have the POV proof output dir from run.sh run_pov and the BIC
		;; so we can filter the BIC and find relevant files and patch hunks.
	   ;;(let ((cmd (format nil "cd ~a && /lacrosse/code/tools/filter-commit-by-sanitizers ~a ~a"
		;; NOTE FIXME currently we're ignoring *last-run-pov-outdir* , but if we
		;; start using it (eg in commented-out filter call above) then need to improve the fake-bisect
	 (let ((cmd (format nil "cd ~a && /lacrosse/code/tools/files-in-commit.sh ~a ~a ~a"
			    ;; THIS DIDN'T WORK ON GITHUB, b/c the git-commits dir is empty.
			      ;;(namestring-realpath (path (target task))) bic *last-run-pov-outdir* (target-source-w-commit (target task) bic)))
			      (namestring-realpath (path (target task))) bic *last-run-pov-outdir* (cp-src task)))
		 ;;(patchfile (strcat *last-run-pov-outdir* "/reduced-commit"))
		 ;(filename (strcat *last-run-pov-outdir* "/files-in-commit"))
		 (files "")
		)
;;	   	(dbug :top "Running filter cmd ~A" cmd)
;;		(uiop::run-program cmd :output patchfile :ignore-error-status T)
;;	   	(dbug :top "Reduced patch in ~A" patchfile)

	   	(dbug :top "Running files-in-commit cmd ~A" cmd)
		;;(uiop::run-program cmd :output filename :ignore-error-status T)
		(setf files (uiop::run-program cmd :force-shell t :output :string :ignore-error-status T))
	   	(dbug :top "files-in-commit gave ~S " files)
           	(send-message-to-optimi :type :bic-found :vc-id (vc-id task) :bic bic :files-in-commit files :cp-src (cp-src task))))

		;; This is a code pattern for when you need to store matched results for later when another line shows up.
		;; Note in this pattern you have to use a global vble for storage (*last-run-pov-outdir*)
		;; b/c no local vble will persist, as this fn is called over and over (for each cmd-emitted line).
		;; Here, capture the output dir from lines like POV proof output dir: $outdir
		;; this actually comes out of bisect.sh before the Found BIC stuff above,
		;; allowing us to use the filter tools
  	 ((setf ret (cl-ppcre:register-groups-bind (f) ("POV proof output dir: (.*)$" line) f))
	   (setf *last-run-pov-outdir* ret)
	   (dbug :top "Captured POV proof output dir ~A" *last-run-pov-outdir*))

		;; this is a code pattern for when you dont want to use matched stuff:
	 ((cl-ppcre:scan-to-strings "ERROR - git bisect" line)
	  (dbug :top "RESULT: ~a" line)
          (send-message-to-optimi :type :failed-to-bisect :vc-id (vc-id task) :reason line))
	 ;; see how easy it would be to add another clause?  Dont close all the parens here
	 )  ;; end cond
))


;;; Note the post-exec can race against late process-line calls, so 
;;; only do cleanup here, don't decide about things like failure.
;;; So dont do what is commented out here.
;;;(defmethod post-exec ((task lacrosse-git-bisect-task))
;;;  (unless (getassoc :bic (vuln-cand task)))
;;;  (send-message-to-optimi :type :failed-to-bisect))

;; (defmethod really-execute-task :after ((task lacrosse-git-bisect-task))
;;   (setassoc :bic "11dafa9a5babc127357d710ee090eb4c0c05154f" (vuln-cand task)))
