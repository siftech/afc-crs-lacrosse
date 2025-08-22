;;; -------------------------------------------------------------------------
;;; COPYRIGHT START
;;; COPYRIGHT END
;;; -------------------------------------------------------------------------

(cl:in-package :fuzzbomb)

(defclass init-ctask-task (lacrosse-task)
  ()
  )

(defmethod task-applies-to-target-p ((task-class-name (eql 'init-ctask-task)) target-node)
  (let ((target (target target-node)))
    (and (next-task target)
         (let ((task-type (getassoc :task-type (next-task target))))
           (eq task-type task-class-name)))))

(defmethod initialize-instance :after ((task init-ctask-task) &key)
  (dbug :top "initialize-instance :after ((task init-ctask-task): ~s" task)
  (dbug :top "(dir (target task)) is ~s" (dir (target task)))
  (dbug :top "(path (target task)) is ~s" (path (target task)))
  (dbug :top "(project-name (target task)) is ~s" (project-name (target task)))
  (let ((cmd-str (build-cmd-str (target task))))
    (dbug :top "cmd-str is ~s" cmd-str)
    (setf (cmd task) cmd-str)
    ))

(defmethod pre-exec ((task init-ctask-task))
  (send-telemetry-event :building `(init-ctask ,task) (metadata (target task))))

(defvar *non-harness-executables* '("llvm-symbolizer"
                                    "jazzer_driver"
                                    "jazzer_driver_with_sanitizer"
                                    ))

(defun non-harness-executable-p (filename)
  "t if name is a member of *non-harness-executables* or is a jar or class file."
  (or (member filename *non-harness-executables* :test #'string-equal)
      (cl-ppcre:scan ".jar$|.class$" filename)))

;;; Overriding really-execute-task b/c we've been building twice and I suspect
;;; the lax2-build-fuzzers func may be more correct than the build-cmd-str.
(defmethod really-execute-task ((task init-ctask-task))
  (pre-exec task)
  (dbug :top "really-execute-task (init-ctask-task): Nothing to do here.")
  (post-exec task))

(defmethod post-exec ((task init-ctask-task))
  (dbug :top "post-exec for init-ctask-task")
  ;;(dbug :top "container-name for init-ctask-task: ~a" (container-name task))
  ;;(dbug :top "image for init-ctask-task: ~a" (image task))
  ;;(dbug :top "container-start-method for init-ctask-task: ~a" (container-start-method task))
  ;; [MDM] Here's where I instrumented an artificial failure on the init-ctask.
  ;; (if (flip 0.8)
  ;; (progn (dbug :top "init-ctask has flipped a bad coin and is doomed to crash")
  ;;	   (setf suffix "break"))
  ;;  (setf suffix ""))
  ;; suffix was a tmp extra optional arg here to append to the call dir and break things
  ;;(dbug :top "project-name: ~s" (project-name (target task)))
  ;;(dbug :top "(get-canned-harnesses (project-name (target task))): ~s" (get-canned-harnesses (project-name (target task))))
  (cond ((and *use-canned-harnesses* (get-canned-harnesses (project-name (target task))))
         (let ((harness-names (get-canned-harnesses (project-name (target task)))))
           (dbug :top "canned harness-names: ~s" harness-names)
           (send-message-to-optimi :type :ctask-init
                                   :target-id (id (target task))
                                   :harnesses harness-names)))
        (t
         (when (delta-ctask-p (target task))
	   (dbug :top "calling lax2-build-fuzzers-delta")
           (lax2-build-fuzzers-delta (target task) :address :libfuzzer))
         (multiple-value-bind (stdout stderr code)
             (lax2-build-fuzzers (target task) :address :libfuzzer)
           (declare (ignore stdout stderr))
           (cond ((/= code 0)
                  (send-message-to-optimi :type :ctask-init-failed
                                          :target-id (id (target task))))
                 (t
                  (let* ((harnesses-dir (format nil "~a/fuzz-tooling/build/out/~a/" (path (target task)) (project-name (target task))))
                         (harness-paths (executable-pathstrings-in-dir harnesses-dir))
                         (harness-names (iter (for ps in harness-paths)
                                          (let ((filename (uiop-file-namestring ps)))
                                            (unless (non-harness-executable-p filename)
                                              (collect filename))))))
                    ;;(dbug :top "harness-paths: ~s" harness-paths)
                    (dbug :top "harness-names: ~s" harness-names)
                    (send-message-to-optimi :type :ctask-init
                                            :target-id (id (target task))
                                            :harnesses harness-names)
                    )))))))

(defun executable-pathstrings-in-dir (dir-namestring)
  (let* ((exec-paths (split-sequence:split-sequence #\Newline
                                                    (uiop:run-program (format nil "find ~a -maxdepth 1 -type f -executable" dir-namestring)
                                                                      :output :string
                                                                      :error-output t)))
         (retval (remove "" exec-paths :test #'string-equal)))
    retval))

(defun get-canned-harnesses (project-name)
  (getassoc project-name *canned-harnesses* :test #'string-equal))
         
(defvar *canned-harnesses*
  `(("libxml2"
     ("api" "html" "xpath" "schema"
      "valid" "lint" "xml" "reader"
      "uri" "regexp" "xinclude"))
    ("libpng"
     ("libpng_read_fuzzer"))
    ("zookeeper"
     ("SerializeFuzzer"
      "ProcessTxnFuzzer"
      "DataTreeFuzzer"
      "MessageTrackerPeekReceivedFuzzer"
      "MultiProcessTxnFuzzer"))
    ("tika"
     ("ThreeDXMLParserFuzzer"
      "HtmlParserFuzzer"
      "TikaAppUntarringFuzzer"
      "RTFParserFuzzer"
      "XliffParserFuzzer"
      "TikaAppRUnpackerFuzzer"
      "TikaAppUnpackerFuzzer"
      "TextAndCSVParserFuzzer"
      "M3U8ParserFuzzer"))
    ("dropbear"
     ("fuzzer-cliconf"
      "fuzzer-client"
      "fuzzer-client_nomaths"
      "fuzzer-kexcurve25519"
      "fuzzer-kexdh"
      "fuzzer-kexecdh"
      "fuzzer-kexmlkem-cli"
      "fuzzer-kexmlkem-srv"
      "fuzzer-kexsntrup-cli"
      "fuzzer-kexsntrup-srv"
      "fuzzer-postauth_nomaths"
      "fuzzer-preauth"
      "fuzzer-preauth_nomaths"
      "fuzzer-pubkey"
      "fuzzer-verify")
     )
    ("apache-commons-compress"
     ("ArchiverArFuzzer"
      "ArchiverArjFuzzer"
      "ArchiverCpioFuzzer"
      "ArchiverDumpFuzzer"
      "ArchiverTarStreamFuzzer"
      "ArchiverZipStreamFuzzer"
      "CompressorBZip2Fuzzer"
      "CompressorDeflate64Fuzzer"
      "CompressorGzipFuzzer"
      "CompressorLZ4Fuzzer"
      "CompressorSnappyFuzzer"
      "CompressorZFuzzer"
      "CompressSevenZFuzzer"
      "CompressTarFuzzer"
      "CompressZipFuzzer"
      "ExpanderFuzzer"))
    )
  )

(defmethod scoring-fn ((task init-ctask-task)) 900)
