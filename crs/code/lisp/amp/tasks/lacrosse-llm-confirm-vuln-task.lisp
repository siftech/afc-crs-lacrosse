;;; -------------------------------------------------------------------------
;;; COPYRIGHT START
;;; COPYRIGHT END
;;; -------------------------------------------------------------------------

;;; A class for tasks that invoke the langchain-based llm tool.

(cl:in-package :fuzzbomb)

(defclass lacrosse-llm-confirm-vuln-task (lacrosse-llm-task)
  (
   (vuln-confirmed-p
    :initform nil
    :accessor vuln-confirmed-p)
  )
)

(defmethod initialize-instance :after ((task lacrosse-llm-confirm-vuln-task) &key)
  (dbug :top "initialize-instance :after ((task lacrosse-llm-confirm-vuln-task): ~s" task)
  ;; - choose a vuln report (for now, the only isabel-report) for input
  ;; - make up a filename for output
  (let* ((isabel-results-pathname (results-pathname-for-task-class 'afl-task (target task)))
         ;;(source-pathname (first (target-sources (target task))))
         (cmds `(("cd" ,(strcat (getenv "LACROSSE_HOME") "/code/langchain"))
                ;  ("source" "/build/venvs/virtualenvs/lacrosse-llm-*/bin/activate")
                 ("python" "-m" "lacrosse_llm.confirm_vuln"
                  "--claude"
                  "--json-input-file" ,(namestring isabel-results-pathname)
                  "--json-output-file" ,(namestring (results-pathname task)))))
         ;;(cmd-str (format nil "~{~A~^ && ~}" (mapcar #'uiop:escape-sh-command cmds)))
         ;; the above escape is a lovely pattern, but foils the attempt to use a glob
         ;; in the activate step.
         (cmd-str (format nil "~{~A~^ && ~}"
                          (mapcar #'(lambda (cmd-list)
                                      (format nil "~{~A~^ ~}" cmd-list))
                                  cmds)))
         )
    (dbug :top "  cmd-str: ~s" cmd-str)
    (setf (cmd task) (uiop:escape-sh-command `("bash" "-c" ,cmd-str)))))


(defmethod pre-exec ((task lacrosse-llm-confirm-vuln-task))
  (dbug :amp "Executing lacrosse-llm-confirm-vuln-task."))

(defmethod post-exec ((task lacrosse-llm-confirm-vuln-task))
  (dbug :top "post-exec for lacrosse-llm-confirm-vuln-task"))

(defmethod task-applies-to-target-p ((task-class-name (eql 'lacrosse-llm-confirm-vuln-task)) target-node)
    (declare (ignore target-node))
  nil)

  ;; (dbug :docker-task "task-applies-to-target-p lacrosse-llm-confirm-vuln-task")
  ;; (let ((target (target target-node)))
  ;;   (when *lacrosse*
  ;;     (dbug :target "(dir target): ~s" (dir target))
  ;;     (dbug :target "(executable-target-p target): ~s" (executable-target-p target))
  ;;     (dbug :target "(buildable-target-p target): ~s" (buildable-target-p target))
  ;;     (dbug :target "(target-sources target): ~s" (target-sources target)))
  ;;   (and *lacrosse*
  ;;        ;; nominal case: vuln and source
  ;;        (and (vuln-p target)
  ;;             (target-sources target)
  ;;             ;; don't repeat!
  ;;             (not (file-exists-p (results-pathname-for-task-class task-class-name target)))
  ;;             ))
  ;;   ))

(defmethod process-line ((task lacrosse-llm-confirm-vuln-task) line)
  (dbug :top "got line [~A]" line)
  (cond ((cl-ppcre:scan-to-strings "\\[Error in checking file: 1 validation error for ChatOpenAI\\]" line)
         (dbug :top "RESULT: validation error (no api key?)")
         ;; FIXME Should be some kind of error response?  
         (send-message-to-optimi :type :challenge-project-update
                                 :target-id (id (target task)))
         )
        (;;(cl-ppcre:scan-to-strings "is_vulnerable" line)
         (cl-ppcre:register-groups-bind (vuln-result)
             ("TASK RESULT: val.is_vulnerable = (\\S+)" line)
           (dbug :top "val.is_vulnerable string: ~s" vuln-result)
           (let ((vuln-p (python-boolean-to-lisp vuln-result)))
             (dbug :top "RESULT is ~s" vuln-p)
             (dbug :top "RESULT: got a reply from llm")
             (setf (vuln-confirmed-p task) vuln-p)
             )))
        ((cl-ppcre:scan-to-strings "TASK RESULT: DONE" line)
         (send-message-to-optimi :type :challenge-project-update
                                 :target-id (id (target task))
                                 :dir (dir (target task))
                                 :vuln-confirmed-p (vuln-confirmed-p task)))
         ))
