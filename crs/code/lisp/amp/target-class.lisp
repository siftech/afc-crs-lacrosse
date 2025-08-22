;;; -------------------------------------------------------------------------
;;; target-class.lisp
;;; - target objects that AMP works on...
;;; -------------------------------------------------------------------------
;;; COPYRIGHT START
;;; COPYRIGHT END
;;; -------------------------------------------------------------------------

(cl:in-package :fuzzbomb)

;;Since we are now copying the CP to an agent-specific location, where you can
;;work on it with git and run.sh etc, that is (path (target task)) ; (cp-path
;;(target task)) is the original read-only location, so basically you can read
;;project.yaml from there or whatever, but dont try any funny stuff. and (dir
;;(target task)) is... the agent's play area (with (path (target task)) below)

;;; ------------------------------------------------------------
;;; target
;;; ------------------------------------------------------------
(defclass target (task)
  ((path :initarg :path :initform nil :accessor path)
   ;; The directory containing the files for this target, eg, pereti workspace, whatclib output.
   ;; FIXED   migrating use of bin path to here.
   (dir :initarg :dir :accessor dir :initform nil)
   ;; FIXED   fill these in after method.
   (input-dir :initarg :input-dir :accessor input-dir :initform nil)
   (output-dir :initarg :output-dir :accessor output-dir :initform nil)
   (id :initarg :id :accessor id :initform -1) ;; if target came from OPTIMUS, this is his target id for it, for use in reporting results
   ;; file-results accessor defined below!
   ;;(file-results :initarg :file-results) ;; string output from linux 'file' on path
  ;;   FIXME temporarily treating all neo
  ;; targets as having :source :external, but this should be explicit in msg.
   (source :initarg :source :initform :external :accessor source)
   (host :initarg :host :initform nil :accessor host)
   (url :initarg :url :initform nil :accessor url)
   (hash :initarg :hash :initform nil :accessor hash)
   (extra-info :initarg :extra-info :initform nil :accessor extra-info)
   ;;   FIXME? Can we do away w/ these ancillary objects or make them a mixin?
   (hist-tree-node :accessor hist-tree-node)
   (povs :initarg :povs :initform nil :accessor povs)
   (docker-image :initarg :docker-image :initform nil :accessor docker-image)

   ;;   FIXME would like to move these to "extra-info" (attrs?)
   (filetype :initarg :filetype :initform nil :accessor filetype)
   (platform :initarg :platform :initform nil :accessor platform)
   (type :initarg :type :initform nil :accessor target-type)
   (tags :initarg :tags :initform nil :accessor tags)

   (test-cmd :initarg :test-cmd :initform nil :accessor test-cmd)

   (input-channel :initarg :input-channel :initform (make-instance 'stdin-channel) :accessor input-channel)
   ))

;; (defmethod print-object ((object target) (stream stream))
;;   (format stream "#{~s ~s}" (type-of object)
;;           (loop for i in (get-slots object)
;;                 when (slot-boundp object i) collect (cons i (slot-value object i)))))

(defmethod lp-string ((target target))
  (format nil "#<TARGET:~a path: ~s  dir: ~s  source: ~s>"
          (id target) (path target) (dir target) (source target)))

(defmethod brief-string ((target target))
  (format nil "#<TARGET ~a file: ~a>" (id target) (file-namestring (path target))))

(defmethod initialize-instance :after ((target target) &key)
  ;; Unless id is supplied to make-instance, use named-object name.
  (dbug :top "init-inst :after (target) (id target): ~s" (id target))
  ;;(dbug :top "init-inst :after (target) (dir target): ~s" (dir target))
  ;;(dbug :top "init-inst :after (target) (path target): ~s" (path target))
  (when (and (numberp (id target))   ;; something is setting this to nil! (FIXME  )
             (= -1 (id target)))
    (setf (id target) (name target)))
  ;; Transform pathnames to strings.
  (when (and (path target) (pathnamep (path target)))
    (setf (path target) (namestring (path target))))
  (when (and (dir target) (pathnamep (dir target)))
    (setf (dir target) (namestring (dir target))))
    ;; FIXME HACK TEMP   Shim here until full target
  ;; persistence is in place.
  (when (stringp (input-channel target))
    (dbug :top "restoring input-channel: ~s" (input-channel target))
    (setf (input-channel target) (persistence:restore-from-string (input-channel target))))
  )


(defmethod spec ((target target))
  ;; FIXME define for non-lacrosse targets  
  (error "FIXME spec is only implemented (so far) for specific sub-classes of target class."))

;;;(defmethod file-results ((target target))
;;;  ;;(dbug :target "file-results on target: ~s ~s" target (namestring (path target)))
;;;  (cond ((slot-boundp target 'file-results)
;;;         ;;(dbug :target "file-results cached: ~s" (slot-value target 'file-results))
;;;         (slot-value target 'file-results))
;;;        (t (setf (slot-value target 'file-results)
;;;                 ;; Use --brief flag to suppress printing path.
;;;                 (uiop:run-program (list "file" "--brief" (namestring (path target))) :output :string))
;;;           ;;(dbug :target "file-results ran: ~s" (slot-value target 'file-results))
;;;           ;;(slot-value target 'file-results)
;;;           )))

(defmethod params ((target target))
  ;;   FIXME condense
  (list
        (serialize-attrs target)
        (list :path (path target))
        (list :dir (dir target))
        (list :input-dir (input-dir target))
        (list :output-dir (output-dir target))
        (list :id (id target))
        ;;(list :file-results (file-results target))
        (list :filetype (filetype target))
        (list :platform (platform target))
        (list :type (target-type target))
        (list :tags (tags target))))

(defmethod getattr (attr (target target))
  (getassoc attr (extra-info target)))

(defmethod setattr (attr  value (target target))
  (setassoc attr value (extra-info target)))

(defmethod serialize-attrs ((target target))
  (list :extra-info (extra-info target)))

(defmethod num-povs ((target target))
  (list-length (povs target)))

;;;(defmethod executable-target-p ((target target))
;;;  (or (search "executable" (file-results target))
;;;      (search "shared object" (file-results target))
;;;      (search "relocatable" (file-results target))
;;;      ))

;;; ------------------------------------------------------------
(defmethod execute-task ((task target))
 (declare (special *self*))
 (dbug :amp "Executing target for ~A" task)
 (when (optimus-prime-p *self*)
  (ecase (status task)
    (:new
     ;; Try to assign it a master FB.
     ;; If it finds one, wait for deep triage to come back.
     ;; If it doesn't, wait for resources to free up.
     (cond ((assign-master-fuzzbomb task)
            (setf (status task) :triaging))   ; FIXME?  
           (T
            (dbug :top "Warning: could not assign a master fuzzbomb for ~A" task)
            (push task (tasks *self*)))))
    (otherwise
            (dbug :top "Warning: unhandled target status ~A" (status task)))
    )
  )
)

(defmethod scoring-fn ((target target))
  (ecase (status target)
    (:new 1)
    (:blocked-waiting-for-fb 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; lacrosse-target-super
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass lacrosse-target-super (target)
  ()
  )

(defmethod initialize-instance :after ((target lacrosse-target-super) &key)
  (setf (dir target) (ensure-string-ends-w-slash (dir target)))
  (setf (output-dir target) (concatenate 'string (dir target) "out/"))
  (ensure-directories-exist (output-dir target))
  (setf (input-dir target) (concatenate 'string (dir target) "in/"))
  (ensure-directories-exist (input-dir target))
  )

(defmethod lp-string ((target lacrosse-target-super))
  (format nil "#<LACROSSE-TARGET-SUPER:~a path: ~s  dir: ~s  source: ~s>"
          (id target) (path target) (dir target) (source target)))

(defmethod brief-string ((target lacrosse-target-super))
  (format nil "#<LACROSSE-TARGET-SUPER ~a dir: ~a>" (id target) (file-namestring (dir target))))

(defmethod spec ((target lacrosse-target-super))
  `((:dir ,(dir target))
    (:id ,(id target))
    ))

(defmethod send-lacrosse-target (fb (target lacrosse-target-super))
  (let ((msg `((:type :new-challenge-project)
               (:target ((,@(spec target)))))))
    (dbug :amp "[send-lacrosse-target] Assigning and sending it to ~a" fb)
    (send-msg fb msg)))

;; FIXME this should poss be a keyword arg to the file-results method  
;; (defmethod reset-file-results ((target lacrosse-target-super))
;;   (slot-makunbound target 'file-results)
;;   (file-results target))

(defmethod files ((target lacrosse-target-super))
  (append (uiop:directory-files (dir target))
          (uiop:directory-files (merge-pathnames "out/" (dir target)))))

(defun file-results-for-file-pathname (file-pathname)
  (list file-pathname
        (uiop:run-program (list "file" "--brief" (namestring file-pathname)) :output :string)))

(defun file-results-for-directory-pathname (directory-pathname)
  (iterate (for file in (uiop:directory-files directory-pathname))
           (collect (file-results-for-file-pathname file))))

;; (defun executable-in-directory (directory-pathname)
;;   (executable-in-file-results (file-results-for-directory-pathname directory-pathname)))

;; (defmethod file-results ((target lacrosse-target-super))
;;   (dbug :target "file-results on target: ~s ~s" target (dir target))
;;   (cond ((slot-boundp target 'file-results)
;;          (slot-value target 'file-results))
;;         (t
;;          (dbug :target "(dir target): ~s" (dir target))
;;          (dbug :target "(uiop:directory-files (dir target)): ~s" (uiop:directory-files (dir target)))
;;          (dbug :target "(files target): ~s" (files target))
;;          (setf (slot-value target 'file-results)
;;                  (iterate (for file in (files target))
;;                           (dbug :target "file: ~s" file)
;;                           (collect
;;                            (list file
;;                                  (uiop:run-program (list "file" "--brief" (namestring file)) :output :string))))))))

;;; specialized method for lacrosse-target-super
;; (defmethod executable-target-p ((target lacrosse-target-super))
;;   (target-executable target))

;;;(defmethod buildable-target-p ((target lacrosse-target-super))
;;;  (target-makefile target))

(defmethod vuln-p ((target lacrosse-target-super))
  (let* ((isabel-results-pathname (results-pathname-for-task-class 'afl-task target)))
    (when (file-exists-p isabel-results-pathname)
      (let ((isabel-data (cl-json:decode-json-from-source isabel-results-pathname)))
        (dbug :target "isabel-data: ~s" isabel-data)
        ;;(dbug :target "result: ~s" (getassoc :result isabel-data :proper t))
        (dbug :target "classification: ~s" (getassoc :classification (getassoc :result isabel-data :proper t) :proper t))
        (string-equal "EXPLOITABLE" (getassoc :classification (getassoc :result isabel-data :proper t) :proper t))
        ))))

;; (defmethod some-patch ((target lacrosse-target-super))
;;   (dbug :target "some-patch-exists-p files: ~s" (file-results target))
;;   (iterate (for (file type-info) in (file-results target))
;;            (when (or (cl-ppcre:scan "^patch-result" (file-namestring file))
;;                      (cl-ppcre:scan "\.patch$" (file-namestring file))
;;                      (cl-ppcre:scan "diff output" type-info))
;;              (dbug :target "returning: ~s" file)
;;              (return file))
;;     (finally (dbug :target "returning NIL")
;;              (return nil))))

(defmethod create-build-pathname ((target lacrosse-target-super) build-id)
  (let* ((build-dir-name (format nil "build-~a/" build-id))
         (build-pathname (merge-pathnames build-dir-name (dir target))))
    (dbug :target "build-pathname: ~s" build-pathname)
    build-pathname))

(defmethod init-build-dir ((target lacrosse-target-super) build-id)
  (dbug :target "init-build-dir: ~s ~s" target build-id)
  (let ((build-pathname (create-build-pathname target build-id)))
    (ensure-directories-exist build-pathname)
    (uiop:run-program (format nil "cp ~a* ~a" (namestring (dir target)) (namestring build-pathname))
                      :ignore-error-status t)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;; Following helpers rely on file-results.
;; ;;; As of  , file-results is only
;; ;;; populated at initialization, so these won't reflect contents
;; ;;; of target produced by our tools.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defmethod target-makefile ((target lacrosse-target-super))
;;   "Return makefile path if one exists, or nil, if not."
;;   (iterate (for (path file-info) in (file-results target))
;;            (when (search "makefile script" file-info)
;;              (return path))))

;; (defmethod target-executable ((target lacrosse-target-super))
;;   "Return executable path if one exists, or nil, if not."
;;   (executable-in-file-results (file-results target)))

;; (defmethod executable-in-file-results ((file-results list))
;;   (iterate (for (path file-info) in file-results)
;;            (when (search "executable" file-info)
;;              (return path))))

;; (defmethod target-sources ((target lacrosse-target-super))
;;   "Return paths to target source files, if any."
;;   (iterate (for (path file-info) in (file-results target))
;;            (when
;;                (or (search "source" file-info)
;;                    ;; file missed at least one of my synthetic targets  
;;                    (and (search "ASCII text" file-info)
;;                         (pathname-looks-like-c-source-p path)))
;;              (collect path))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; annoying hack
;;;   Seems that asdf 3.3 version of uiop:directory-files
;;; doesn't DWIM when directory doesn't end w trailing slash.
;;; (I think this changed by somewhere in asdf 3.3.4.)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ensure-string-ends-w-slash (str)
  (cond ((not (eq #\/ (schar str (1- (length str)))))
         (concatenate 'string str "/"))
        (t str)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; lacrosse-cp-target
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass lacrosse-cp-target (lacrosse-target-super)
  (
   ;;(cp-address :initarg :cp-address :initform nil :accessor cp-address
   ;;            :documentation "git url for challenge project")
   ;; for now, this is the name of the dir w/in cp_root
   (cp-path :initarg :cp-path :initform nil :accessor cp-path
            :documentation "Path to the cp.")
   (cp-name :initarg :cp-name :initform nil :accessor cp-name
            :documentation "Name assigned by competition organizers.")
   (cp-sources :initarg :cp-sources :initform nil :accessor cp-sources
               :documentation "List of paths to source directories containing git repos.")
   (vuln-cands :initarg :vuln-cands :initform nil :accessor vuln-cands
               :documentation "List of vuln cands."
               :type list)
      ;; [Pavan K. - Adding these here instead having OPTIMUS add them into a message.
   ;;   Allows OPTIMUS' messaging to be target-agnostic.]
   (built-p :initarg :built-p :initform nil :accessor built-p
            :documentation "True if the initial run.sh build has been complete; false otherwise")
   (make-pov-blob :initarg :make-pov-blob :initform nil :accessor make-pov-blob
                  :documentation "True if agent should make a pov blob.")
   (run-pov :initarg :run-pov :initform nil :accessor run-pov
            :documentation "True if agent should run pov with generated blob.")
   (make-patch :initarg :make-patch :initform nil :accessor make-patch
            :documentation "True if agent should make patch; false otherweise")
   (build-with-patch :initarg :build-with-patch :initform nil :accessor build-with-patch
                     :documentation "True if agent should build with patch.")
   (run-tests-p :initarg :run-tests-p :initform nil :accessor run-tests-p
             :documentation "True if we should run tests; false otherwise")
   (source-name :initarg :source-name :initform nil :accessor source-name
                :documentation "Name of source directory. Required for building with patch.")
   (source-path :initarg :source-path :initform nil :accessor source-path
                :documentation "Path to source code according to project.yaml")
   (blobs :initarg :blobs :initform nil :accessor blobs
          :documentation "List of blobs for getting PoV.")
   (sanitizers :initarg :sanitizers :initform nil :accessor sanitizers
               :documentation "List of sanitizers.")
   (harnesses :initarg :harnesses :initform nil :accessor harnesses
              :documentation "List of harnesses for getting PoVs.")
   (chosen-harness :initarg :chosen-harness :initform nil :accessor chosen-harness
              :documentation "The harness assigned to this target for getting PoVs.")
   (patches :initarg :patches :initform nil :accessor patches
            :documentation "List of paths to patches")
   (project-properties :initarg :project-properties :initform nil :accessor project-properties
                       :documentation "Properties read from project.yaml (or its translation.")
   (next-task :initarg :next-task :initform nil :accessor next-task
              :documentation "Hint from optimus about what to do next w this target."
              :type (or symbol list))
   )
  )

(defmethod spec ((target lacrosse-cp-target))
  `((:cp-path ,(cp-path target))
    (:dir ,(dir target))
    (:id ,(id target))

    ;; Flags to help with control flow of tasks
    (:built-p ,(built-p target))
    (:make-pov-blob ,(make-pov-blob target))
    (:run-pov ,(run-pov target))
    (:make-patch ,(make-patch target))
    (:build-with-patch ,(build-with-patch target))
    (:run-tests-p ,(run-tests-p target))

    (:source-path ,(source-path target))
    (:source-name ,(source-name target))
    (:harnesses ,(harnesses target))
    (:chosen-harness ,(chosen-harness target))
    (:sanitizers ,(sanitizers target))
    (:blobs ,(blobs target))
    (:patches ,(patches target))
    (:project-properties ,(project-properties target))
    ))

(defmethod files ((target lacrosse-cp-target))
  (append (uiop:directory-files (dir target))
          (uiop:directory-files (merge-pathnames "out/" (dir target)))
          (uiop:directory-files (concatenate 'string (source-path target)  "/"))))

;;; FIXME sort this by ones that have git commits!
;(defmethod target-source-roots ((target lacrosse-cp-target))
;  (let ((retval
;         (let ((cp-srcs (cp-prop target :cp_sources)))
;           (iter (for cp-src in cp-srcs)
;                 (let ((dir-name (string-downcase (symbol-name (first cp-src)))))
;                   (collect (strcat (path target) "/src/" dir-name "/" )))))))
;    (dbug :top "target-source-roots: ~s" retval)
;    retval))

(defmethod target-source-roots ((target lacrosse-cp-target))
  (let ((retval
          (iter (for cp-src in (cp-srcs target))
            (collect (target-src-path-for-cp-src target cp-src)))))
    (dbug :top "target-source-roots: ~s" retval)
    retval))

(defmethod target-src-path-for-cp-src ((target lacrosse-cp-target) cp-src)
  (strcat (path target) "/src/" cp-src "/"))

;;;   FIXME DONT USE DOESNT WORK ON GITHUB
(defmethod target-source-w-commit ((target lacrosse-cp-target) commit)
  "Path to source root that contains the commit."
  (dbug :target "target-source-w-commit: ~s ~s" target commit)
  (iter (for cp-src in (cp-srcs target))
    (dbug :target "target-source-w-commit looking for ~s in ~s" commit (git-commits-for-cp-src target cp-src))
    (finding cp-src such-that
             (member commit (git-commits-for-cp-src target cp-src) :test #'string-equal))))

(defmethod git-info-dir-for-cp-src ((target lacrosse-cp-target) cp-src)
  ;; FIXME   maybe a little error checking
  (strcat (shared-path target) "git-info/" cp-src "/"))

;;;   FIXME DONT USE DOESNT WORK ON GITHUB
(defmethod git-commits-for-cp-src ((target lacrosse-cp-target) cp-src)
  (let ((ls-cmd (format nil "ls -1 ~a/git-commits/" (git-info-dir-for-cp-src target cp-src))))
    ;;(dbug :target "git-commits-for-cp-src: ~s" ls-cmd)
    (uiop:run-program ls-cmd :output :lines)))

;;;   FIXME DONT USE DOESNT WORK ON GITHUB
(defmethod hunk-dir ((target lacrosse-cp-target) commit)
  "namestring for hunk-dir"
  (let* ((cp-src (target-source-w-commit target commit))
         (namestring (strcat (git-info-dir-for-cp-src target cp-src) "/hunks/" commit "/")))
    ;;(dbug :target "hunk-dir: ~s" namestring)
    namestring))

;;;   FIXME DONT USE DOESNT WORK ON GITHUB
(defmethod hunks ((target lacrosse-cp-target) commit)
  (uiop:run-program (find-cmd (hunk-dir target commit) :args "-iregex \'.*\\.patch$\'") :output :lines))

;;;   FIXME DONT USE DOESNT WORK ON GITHUB
(defmethod commit-namestring ((target lacrosse-cp-target) commit)
  (let ((cp-src (target-source-w-commit target commit)))
    (strcat (git-info-dir-for-cp-src target cp-src) "/git-commits/" commit)))

(defmethod target-all-files-in-source ((target lacrosse-cp-target))
  (dbug :top "target-all-files-in-source")
  (let ((retval
          (iter (for root in (target-source-roots target))
                (appending
                 (uiop:run-program (find-cmd-no-hidden root) :output :lines)))))
    (dbug :top "target-all-files-in-source returning list of length ~s" (list-length retval))
    retval))

(defmethod target-c-files ((target lacrosse-cp-target))
  (let ((retval
          (iter (for root in (target-source-roots target))
                (appending
                 (uiop:run-program (find-cmd-c-files root) :output :lines)))))
    retval))

(defmethod target-java-files ((target lacrosse-cp-target))
  (let ((retval
          (iter (for root in (target-source-roots target))
                (appending
                 ;;   hack for jenkins bug? FIXME
                 (cond ((uiop:directory-exists-p root)
                        (uiop:run-program (find-cmd-java-files root) :output :lines))
                       (t (dbug :top "WARN: source path doesn't exist: ~s" root)))))))
    retval))

(defmethod target-sources ((target lacrosse-cp-target))
  "Return paths to target source files, if any."
  (cond ((target-language-c-p target)
         (target-c-files target))
        ((target-language-java-p target)
         (target-java-files target))
        (t (error "Unexpected target type: neither C nor Java."))))

;;;(defmethod file-results ((target lacrosse-cp-target))
;;;  (call-next-method target))

(defmethod init-marker-filename ((target lacrosse-cp-target))
  (merge-pathnames "lacrosse-init" (output-dir target)))

(defmethod project-json-path ((target lacrosse-cp-target))
  (strcat (shared-path target) "project.json"))

(defun read-rearranged-json (filename)
  "To handle their stupid format where identifiers are used as
keys rather than values, we must read a rearranged version of the
JSON, and then re-rearrange it."
  (let* ((cmds `(("cd" ,(strcat *lax-home* "/code/langchain"))
                 ("python" "-m" "lacrosse_llm.json_rearranger"
                           ,(namestring filename))))
         (cmd-str (format nil "~{~A~^ && ~}"
                          (mapcar #'(lambda (cmd-list)
                                      (format nil "~{~A~^ ~}" cmd-list))
                                  cmds)))
         (cmd (uiop:escape-sh-command `("bash" "-c" ,cmd-str))))
    (multiple-value-bind (output-slurp error-slurp exit-code)
        (uiop:run-program cmd :output :string :ignore-error-status t :error-output :string)
      (cond ((zerop exit-code)
             (dbug :target "Read rewritten JSON")
             output-slurp)
            (t (dbug :target "ERROR rewriting JSON: exit-code: ~d error output:~%~a"
                     exit-code error-slurp))))))

(defun remove-all (keys alist)
  "Remove the entries for all of the KEYS from ALIST."
  (let ((new (copy-list alist)))
    (iter (for key in keys)
      (setf new (remove key new :key #'car))
      (finally (return new)))))

(defun massage-json (json-str)
  "Take our rewritten json (see READ-REFORMATTED-JSON) and restore it to
the original format, but with case-sensitive symbols for the harness, sanitizer
and cp_source identifiers."
  (flet ((pop-id-out (alist)
           (let ((id
                   (intern
                    ;; case SENSITIVE symbol!
                    (getassoc :id alist :proper t)
                    :keyword)))
             (cons id (remove :id alist :key #'car)))))
    (let* ((json
             (let ((cl-json::*json-identifier-name-to-lisp* #'string-upcase))
               (cl-json:decode-json-from-string json-str)))
           (new (copy-tree json))
           ;; now the harnesses need to be transformed back to their original form
           (new-harnesses
             (mapcar #'pop-id-out (getassoc :harnesses json :proper t)))
           (new-cp-sources (mapcar #'pop-id-out (getassoc :cp_sources json :proper t)))
           (new-sanitizers
             (let ((tmp-list (mapcar #'pop-id-out (getassoc :sanitizers json :proper t))))
               (iter (for (id . alist) in tmp-list)
                 (collecting (cons id (getassoc :value alist :proper t)))))))
      ;; no :proper option for setassoc!!!
      (append
       (pairlis '(:harnesses :sanitizers :cp_sources)
                (list new-harnesses new-sanitizers new-cp-sources))
       (remove-all '(:harnesses :sanitizers :cp_sources)
                   new)))))

;;; Run the json-rearranger python script on the raw json.  this will
;;; give you NEW JSON where the harnesses, cp_sources, and sanitizers
;;; have LISTS as values, and each list element has a :name key. So:
;;; revise the harness property so it's not list-valued, it's an
;;; alist, with the names as keys and the rest of the values as the
;;; values associated with the key.  
(defmethod set-project-properties ((target lacrosse-cp-target))
  ;; Override the camelcase translator and just use the json property
  ;; names, inc underscores.
  (let* ((json-str
           (read-rearranged-json (namestring (uiop:ensure-absolute-pathname (project-json-path target)))))
         (new (massage-json json-str)))
    (setf (slot-value target 'project-properties)
          new)))

;;;   For now, leave cp properties in alist and access here.
(defmethod cp-prop ((target lacrosse-cp-target) prop)
;;; dunno what this was here for, but it looks like an empty/decoy 'when' to me.
;;  (when (and (not (slot-value target 'project-properties))
;;             (file-exists-p (project-json-pathname target))))
;;; and why are people using slot-value instead of just the accessor? obscurity bad.
  (getassoc prop (project-properties target) :proper t))

(defmethod find-harness-by-id (id (target target))
  (let ((harnesses (cp-prop target :harnesses))
        (id-sym (cond ((stringp id)
                       (intern id :keyword))
                      ((symbolp id)
                       id))))
    (find id-sym harnesses :key #'first)))

(defun harness-prop (prop h)
  (getassoc prop (rest h) :proper t))

(defun harness-prop-id (h)
  "Given the yaml->json->lisp harness spec, return the id (a keyword symbol)"
  (first h))

(defmethod target-language-c-p ((target lacrosse-cp-target))
  (or (string-equal "c" (cp-prop target :language))
      (string-equal "c++" (cp-prop target :language))
      (string-equal "C" (cp-prop target :language))
      (string-equal "C++" (cp-prop target :language)))
)

(defmethod target-language-java-p ((target lacrosse-cp-target))
  (or (string-equal "java" (cp-prop target :language))
      (string-equal "Java" (cp-prop target :language))
      (string-equal "JAVA" (cp-prop target :language))))

(defmethod src ((target lacrosse-cp-target))
  (strcat (path target) "/src/"))

(defmethod cp-srcs ((target lacrosse-cp-target))
  (iter (for cp-src in (cp-prop target :cp_sources))
        (dbug :target "cp-srcs cp-src: ~s" (first cp-src))
        (collect (symbol-name (first cp-src)))))

(defun find-vc-by-id (id target)
  (find id (vuln-cands target) :key #'id :test #'=))

(defmethod find-vc-w-equiv-blob (blob-namestring (target lacrosse-cp-target))
  "Return vc w equiv blob (for now, same file contents, which is enough
   to save work when using fake fuzzers, but prob of little value for
   real fuzzers which may find slight variants of blobs)."
  ;;(iter (for vc in (vuln-cands target))
  ;;  (finding vc such-that (same-file-contents-p blob-namestring (blob vc))))
  (let ((new-blob-octets (slurp-binary-file blob-namestring)))
    (iter (for vc in (vuln-cands target))
      (finding vc such-that (equalp new-blob-octets (blob-octets vc)))))
  )

(defmethod all-patch-cands ((target lacrosse-cp-target))
  ;;(dbug :top "all-patch-cands <lacrosse-cp-target>")
  ;;(describe target)
  ;;(describe (vuln-cands target))
  (iter (for vc in (vuln-cands target))
    (appending (patch-cands vc))))

(defmethod reset-source ((target lacrosse-cp-target))
  (iter (for src in (target-source-roots target))
    (let ((cmd (format nil "cd ~a && git -C ~a reset --hard HEAD" (path target) src)))
      (dbug :target "reset cmd: ~s" cmd)
      (multiple-value-bind (output-slurp error-slurp exit-code)
          (uiop:run-program cmd
                            :output t :force-shell t :ignore-error-status t :error-output :output)
          (declare (ignore output-slurp error-slurp))
          (dbug :target "exit-code: ~a" exit-code)))))

;;;----------------------------------------------------------------------
;;; Methods to get run.sh exitcodes from filesystem

(defmethod most-recent-build-exitcode ((target target))
  (most-recent-run-sh-exitcode target :job-glob "*build*"))

(defmethod most-recent-run-pov-exitcode ((target target))
  (most-recent-run-sh-exitcode target :job-glob "*run_pov*"))

(defmethod most-recent-run-tests-exitcode ((target target))
  (most-recent-run-sh-exitcode target :job-glob "*run_tests*"))

;;;   FIXME could return error for new file found
(defmethod most-recent-run-sh-exitcode ((target target) &key (job-glob "*"))
  (let* ((run-tests-output-dir-glob (strcat (path target) "/out/output/" job-glob))
         (ls-cmd (format nil "ls -1td ~a | head -1" run-tests-output-dir-glob))
         (runsh-output-namestring (first (uiop:run-program ls-cmd :output :lines))))
    (dbug :target-deep "run-tests-output-dir-glob: ~s" run-tests-output-dir-glob)
    (dbug :target-deep "ls-cmd: ~s" ls-cmd)
    (dbug :target "runsh-output-namestring: ~s" runsh-output-namestring)
    (let ((exitcode-namestring (strcat runsh-output-namestring "/exitcode")))
      (cond ((file-exists-p exitcode-namestring)
             (let ((exitcode (uiop:read-file-string exitcode-namestring)))
               (cond
                 ;; file empty - call this an error
                 ((string-equal "" exitcode) 1)
                 ;; return the exitcode as int
                 (t (dbug :target-deep "exitcode-namestring: ~s" exitcode-namestring)
                    (dbug :target "exitcode: ~s" exitcode)
                    (parse-integer exitcode)))))
            ;; file missing - call this an error
            (t 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  lacrosse-semi-target
;;;
;;;  This class semifinal specific behavior should go here, whif
;;;  mods are made to classes up the tree to support the lax-final.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass lacrosse-semi-target (lacrosse-cp-target)
  ()
  )


;;; Assign a dir before init'g slots.
(defmethod initialize-instance :around ((target lacrosse-semi-target) &rest initargs &key)
  ;;   HACK, override the optimus dir... prob shouldn't be sending this at all.
  ;; This HACK may be specific to semi?
  ;;(when (not (getf initargs :dir))
    (setf (getf initargs :dir) (format nil "~a/~a/" *experiment-dir* (shortname *self*)))
    (dbug :target "set :dir to ~s" (getf initargs :dir))
   ;; )
  (apply #'call-next-method target initargs))

(defmethod initialize-instance :after ((target lacrosse-semi-target) &key)
  (setf (cp-name target) (alexandria:last-elt (pathname-directory (first (uiop:subdirectories *cp-root*)))))
  (dbug :target "cp name is ~a" (cp-name target))
  (dbug :target "copying to my dir")
  ;;(run-command (format nil "rsync -a ~A ~A" (strcat *cp-root* "/") (dir target)))
  ;;(uiop:run-program (format nil "rsync -va ~A ~A" (strcat *cp-root* "/") (dir target)) :output t)
  ;;(uiop:run-program (format nil "cp -rf ~A ~A" (strcat *cp-root* "/") (dir target)) :output t)
  (let ((copy-cmd (format nil "cp -rf ~A ~A" (cp-path target) (dir target))))
    ;;(uiop:run-program (format nil "ls -al ~a" (cp-path target)) :output t :error-output :output)
    ;;(uiop:run-program (format nil "ls -al ~a" (dir target)) :output t :error-output :output)
    (dbug :target "copy-cmd: ~s" copy-cmd)
    (uiop:run-program copy-cmd :output t :error-output :output))
  (dbug :target "done copying cp to my dir")
  (dbug :target "CP sources = ~s" (cp-srcs target))
  (dbug :target "Target source roots = ~s" (target-source-roots target))
  (setf (path target) (strcat (dir target) (cp-name target)))
  ;; Try calling lax-asc-init-optimus.sh here.  
  (when (optimus-prime-p *self*)
    (uiop:run-program (format nil "~a/lax-asc-init-optimus.sh --exp-dir ~a" *lax-tools* *experiment-dir*) :output t))
  (set-project-properties target)
  (dbug :target "Target project properties are:~{~S~%~}" (project-properties target))
  ;;   Why build until/unless reqd?
  (unless (or (optimus-prime-p *self*)
              *fake-fuzz*) ;;   currently I dont think OPT needs to do this and it speeds up startup
    ;;(uiop:run-program (format nil "cd ~A; ./run.sh -v build" (path target)) :output t :error-output :output)
    (uiop:run-program (format nil "cd ~A; ./run.sh build" (path target)) :output t :error-output :output)
    ;; after artifacts are created, can make fuzzer dictionary
    (uiop:run-program (format nil "cd ~A; /lacrosse/code/tools/make-string-dictionary.sh project.yaml work/strings.dict" (path target)) :output t :error-output :output)
    )
  )

(defmethod shared-path ((target lacrosse-semi-target))
  (format nil "~a/crs/shared/" *experiment-dir*))

