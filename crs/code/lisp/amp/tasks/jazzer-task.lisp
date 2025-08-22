;;; -------------------------------------------------------------------------
;;; COPYRIGHT START
;;; COPYRIGHT END
;;; -------------------------------------------------------------------------

(cl:in-package :fuzzbomb)

(defvar *sanitizer-classes* '("com.code_intelligence.jazzer.sanitizers.ClojureLangHooks"
                                    "com.code_intelligence.jazzer.sanitizers.Deserialization"
                                    "com.code_intelligence.jazzer.sanitizers.ExpressionLanguageInjection"
                                    "com.code_intelligence.jazzer.sanitizers.FileReadWrite"
                                    "com.code_intelligence.jazzer.sanitizers.FileSystemTraversal"
                                    "com.code_intelligence.jazzer.sanitizers.IntegerOverflow"
                                    "com.code_intelligence.jazzer.sanitizers.LdapInjection"
                                    "com.code_intelligence.jazzer.sanitizers.NamingContextLookup"
                                    "com.code_intelligence.jazzer.sanitizers.OsCommandInjection"
                                    "com.code_intelligence.jazzer.sanitizers.ReflectiveCall"
                                    "com.code_intelligence.jazzer.sanitizers.RegexInjection"
                                    "com.code_intelligence.jazzer.sanitizers.RegexRoadblocks"
                                    "com.code_intelligence.jazzer.sanitizers.ScriptEngineInjection"
                                    "com.code_intelligence.jazzer.sanitizers.ServerSideRequestForgery"
                                    "com.code_intelligence.jazzer.sanitizers.SqlInjection"
                                    "com.code_intelligence.jazzer.sanitizers.XPathInjection"
                                    ))

(defclass jazzer-task (libfuzzer-task)
  (
   (sanitizer-classname
    :accessor sanitizer-classname
    :type string)
   ))


(defmethod task-applies-to-target-p ((task-class-name (eql 'jazzer-task)) target-node)
  ;;(dbug :top "task-applies-to-target-p libfuzzer-task")
  (let ((target (target target-node)))
    (and (next-task target)
         (let ((task-type (getassoc :task-type (next-task target))))
           (eq task-type task-class-name)))))

;; No pre-exec method, defaults to libfuzzer-task, which we want

(defmethod initialize-instance :after ((task jazzer-task) &key)
  (dbug :top "initialize-instance :after ((task jazzer-task): ~s" task)
  (dbug :target "~A" (with-output-to-string (str) (describe (target task) str)))
  (when (next-task (target task))
    (setf (chosen-harness-id task) (getassoc :chosen-harness-id (next-task (target task))))
    (setf (examples task) (getassoc :examples (next-task (target task))))
    (setf (llm-arg task) (getassoc :llm-arg (next-task (target task))))
    (setf (use-value-profile task) (getassoc :use-value-profile (next-task (target task))))
    (setf (sanitizer-classname task) (getassoc :sanitizer-classname (next-task (target task))))
    )
  (check-type (chosen-harness-id task) keyword)
  (setf (chosen-harness-id-as-string task) (string-downcase (symbol-name (chosen-harness-id task))))
  (setf (seeds-dir task) (uiop:parse-unix-namestring
                          (format nil "~a/jazzer/seeds/~a"
                                  (shared-path (target task))
                                  (chosen-harness-id-as-string task))
                          :type :directory))
  (let* ((harness-props (find-harness-by-id (chosen-harness-id task) (target task)))
         (classes-string (format nil "~{~A~^:~}" (remove-if (lambda (elem) (equal elem (sanitizer-classname task)))
                                                         *sanitizer-classes*)) )
         (disable-arg (format nil " --disabled_hooks=~a " classes-string))
         (val-profile-arg (if (use-value-profile task) "-use_value_profile=1" ""))
         (string-arg (format nil " ~a ~a " disable-arg val-profile-arg))
         )
    (setf (cmd task)
          (format nil "cd ~a && /lacrosse/code/tools/run-jazzer.sh ~a ~a ~a ~a ~a ~a ~a |& LC_ALL=C tr '\\200-\\377' '\\000-\\177'"
                  (path (target task))
                  (harness-prop :name harness-props)
                  (harness-prop :source harness-props)
                  (harness-prop :binary harness-props)
                  (seeds-dir task)
                  (format nil "~a/fuzz/jazzer/~a"
                          (native-namestring (probe-file (shared-path (target task))))
                          (chosen-harness-id-as-string task))
                  (fuzzer-timeout task)
                  string-arg
                  ))
    )
  (dbug :top "Task cmd: ~a" (cmd task))
  (dbug :top "jazzer-task finished initialization"))

(defmethod build-it-for-fuzzing ((task jazzer-task))
  (dbug :top "~s ~s" #'build-it-for-fuzzing task))

(defmethod process-line ((task jazzer-task) line)
  (let (ret)
    (dbug :top "got line [~A]" line)
    (cond ((setf ret (cl-ppcre:register-groups-bind (f) ("Test unit written to (.*)$" line) f))
           (dbug :top "RESULT: Found crashing input ~A" ret)
           (submit-crash task ret (nreverse (next-stack-trace task)))
           (setf (next-stack-trace task) nil)
           )
          ((setf ret (cl-ppcre:register-groups-bind (func file line)
                         (".*at ([^(]+)\\(([^:]+):([0-9]+)\\)" line)
                       (list nil func file line nil)))
           (dbug :top "RESULT: Found stack trace vals ~A" ret)
           (push (list (first ret) (third ret) (fourth ret))
                 (next-stack-trace task)))
    )))
