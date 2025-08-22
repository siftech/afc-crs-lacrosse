;;; -------------------------------------------------------------------------
;;; COPYRIGHT START
;;; COPYRIGHT END
;;; -------------------------------------------------------------------------

;;; A class for tasks that invoke the langchain-based llm tool.
;;;   NOTE ONLY FOR SMOKETEST as of now.

(cl:in-package :fuzzbomb)

;;; These should be included in all subclasses by strcat'g with subclass
;;; specific args when setting container-start-args default-initarg.
;(defvar *lacrosse-llm-image-args*
;  (strcat
;   ;; pass api keys into langchain container's environment.
;   (format nil " -e OPENAI_API_KEY=~a " (getenv "OPENAI_API_KEY"))
;   (format nil " -e ANTHROPIC_API_KEY=~a " (getenv "ANTHROPIC_API_KEY"))
;   ))

(defclass lacrosse-llm-task (lacrosse-task)
  (
  )
  (:documentation "LACROSSE-LLM-TASK will run a python module inside the \"langchain\" docker image."))

(defmethod initialize-instance :after ((task lacrosse-llm-task) &key)
  ;; FIXME when vuln has been id'd, focus query  
  (dbug :top "initialize-instance :after ((task lacrosse-llm-task): ~s" task)

;; FIXME this needs updte for lax... /neo-fuzz maybe no more?
  ; (let* ((source-pathname (first (target-sources (target task))))
  ;        (cmds `(("cd" "/neo-fuzz/code/lacrosse/langchain")
  ;                ("source" "/build/venvs/virtualenvs/lacrosse-llm-*/bin/activate")
  ;                ("python" "-m" "lacrosse_llm.check_file" ,(namestring source-pathname))))
  ;        ;;(cmd-str (format nil "~{~A~^ && ~}" (mapcar #'uiop:escape-sh-command cmds)))
  ;        ;; the above escape is a lovely pattern, but foils the attempt to use a glob
  ;        ;; in the activate step.
  ;        (cmd-str (format nil "~{~A~^ && ~}"
  ;                         (mapcar #'(lambda (cmd-list)
  ;                                     (format nil "~{~A~^ ~}" cmd-list))
  ;                                 cmds)))
  ;        )
  ;   (dbug :top "  cmd-str: ~s" cmd-str)
  ;   (setf (cmd task) (uiop:escape-sh-command `("bash" "-c" ,cmd-str))))
    )


(defmethod pre-exec ((task lacrosse-llm-task))
  (dbug :amp "Executing lacrosse-llm-task."))

(defmethod post-exec ((task lacrosse-llm-task))
  (dbug :top "post-exec for lacrosse-llm-task"))

(defmethod task-applies-to-target-p ((task-class-name (eql 'lacrosse-llm-task)) target-node)
  (declare (ignore target-node))
  nil)
  ;; (dbug :docker-task "task-applies-to-target-p lacrosse-llm-task")
  ;; (let ((target (target target-node)))
  ;;   (when *lacrosse*
  ;;     (dbug :target "(dir target): ~s" (dir target))
  ;;     (dbug :target "(executable-target-p target): ~s" (executable-target-p target))
  ;;     (dbug :target "(buildable-target-p target): ~s" (buildable-target-p target))
  ;;     (dbug :target "(target-sources target): ~s" (target-sources target)))
  ;;   (and *lacrosse*
  ;;        (or
  ;;         ;; smoketest case: not buildable, no executable,
  ;;         (and (dir target)
  ;;              (not (executable-target-p target))
  ;;              (not (buildable-target-p target))
  ;;              (target-sources target))
  ;;         ;; nominal case: vuln and source
  ;;         ;; DISABLED, use lacrosse-llm-confirm-vuln instead  
  ;;         ;;(and (vuln-p target)
  ;;         ;;     (target-sources target))
  ;;         ))))

(defmethod process-line ((task lacrosse-llm-task) line)
  (dbug :top "got line [~A]" line)
  (cond ((cl-ppcre:scan-to-strings "\\[Error in checking file: 1 validation error for ChatOpenAI\\]" line)
         (dbug :top "RESULT: validation error (no api key?)")
         ;; FIXME Should be some kind of error response?  
         (send-message-to-optimi :type :challenge-project-update :target-id (id (target task)))
         )
        ((cl-ppcre:scan-to-strings "is_vulnerable" line)
         (dbug :top "RESULT: got a reply from llm")
         (send-message-to-optimi :type :challenge-project-update :target-id (id (target task)))
         )
        )
  )
