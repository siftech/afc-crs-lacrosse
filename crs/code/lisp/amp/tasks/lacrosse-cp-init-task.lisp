;;; -------------------------------------------------------------------------
;;; COPYRIGHT START
;;; COPYRIGHT END
;;; -------------------------------------------------------------------------

;;; A class for tasks that pulls the challenge problem from a git repo and
;;; extracts information necessary to address the challenge problem.

(cl:in-package :fuzzbomb)

(defclass lacrosse-cp-init-task (lacrosse-task)
  (
  )
)

(defmethod initialize-instance :after ((task lacrosse-cp-init-task) &key)
  (dbug :top "initialize-instance :after ((task lacrosse-cp-init-task): ~s" task)
  (let* (;;(cp-address (cp-address (target task)))
         ;;(repo-name (repo-name-from-repo-address cp-address))

         (target-dir (dir (target task)))
         (output-dir (output-dir (target task)))
         (cmd-str (format nil "/lacrosse/code/tools/lacrosse-cp-init.sh --target-dir ~a  --output-dir ~a" target-dir output-dir))
         )
    (dbug :top "  cmd-str: ~s" cmd-str)
    (setf (cmd task) (uiop:escape-sh-command `("bash" "-c" ,cmd-str)))))

(defmethod pre-exec ((task lacrosse-cp-init-task))
  (dbug :amp "Executing lacrosse-cp-init-task."))

;; TODO: Make a mapping between "harness-name" to id
(defmethod post-exec ((task lacrosse-cp-init-task))
  (dbug :top "post-exec for lacrosse-cp-init-task")
  ;; set project-properties from translated project.yaml
  ;;   FIXME this needs to be done in each fb
  (let ((target (target task)))
    (set-project-properties target)

    ;; [Pavan K. - Pre-populate harness information here. These will be used for getting PoVs and there can be multiple harnesses.
    ;;  Having this in list form would help with allocating out multiple harnesses to multiple PoV tasks.]
    ; (dbug :top "Harnesses: ~a" (cp-prop (target task) :harnesses))
    (loop for harness in (cp-prop target :harnesses) do
        ; (dbug :top "Harness element ~a - car of element: ~a, cdr of element: ~a" harness (car harness) (cdr harness))
        ;; [Pavan K. - "(cdr (car (cdr harness)))" gets the "name" of the harness]
        (dbug :top "Harness with id ~a and name ~a found. Adding to list of harnesses..." (car harness) (cdr (car (cdr harness))))
        (setf (harnesses target) (append (harnesses target) (list (list (car harness) (cdr (car (cdr harness))))))))

    (dbug :top "Current harnesses in list: ~a" (harnesses target))

    (loop for sanitizer in (cp-prop target :sanitizers) do
        ; (dbug :top "Sanitizer element ~a - car of element: ~a, cdr of element: ~a" sanitizer (car sanitizer) (cdr sanitizer))
        (dbug :top "Sanitizer with id ~a and name ~a found. Adding to list of sanitizers" (car sanitizer) (cdr sanitizer))
        (setf (sanitizers target) (append (sanitizers target) (list (list (car sanitizer) (cdr sanitizer))))))

    (dbug :top "Current santitizers in list: ~a" (sanitizers target))

    ;; [Pavan K. - No longer needed, but keep in case you need to debug something.....]
    ; (let* ((repo-name (repo-name-from-repo-address (cp-address target)))
    ;        (cp-dir (concatenate 'string (dir target) repo-name))
    ;        (blob-file (concatenate 'string cp-dir "/exemplar_only/cpv_1/blobs/sample_solve.bin")))
    ;   (setf (blobs target) (append (blobs target) (list blob-file))))

    ; (let* ((repo-name (repo-name-from-repo-address (cp-address target)))
    ;        (cp-dir (concatenate 'string (dir target) repo-name))
    ;        (patch-file (concatenate 'string cp-dir "/exemplar_only/cpv_1/patches/samples/good_patch.diff")))
    ;   (setf (patches target) (append (patches target) (list patch-file))))

    (let* (;;(repo-name (repo-name-from-repo-address (cp-address target)))
           ;;(cp-dir (concatenate 'string (dir target) repo-name))
           (cp-dir (dir target))
           (source-name (string (car (car (cp-prop (target task) :cp_sources)))))
           (source-path (concatenate 'string cp-dir "/src/" source-name)))
       (setf (source-path target) source-path)
       (setf (source-name target) source-name))

    (dbug :top "Read project.json w :cp_name ~s and :language ~s." (cp-prop target :cp_name) (cp-prop target :language))
    ;; write a file as a flag.  FIXME ? Need our own lacrosse.json data file?
    (with-open-file (out (init-marker-filename target)
                         :direction :output
                         :if-does-not-exist :create)
                    (format out "Project initialized."))
    (send-message-to-optimi :type :challenge-project-update
                            :project-prop (slot-value target 'project-properties)
                            :source-path (source-path target)
                            :source-name (source-name target)
                            :harnesses (harnesses target)
                            :sanitizers (sanitizers target)
                            ; :patches (patches (target task))
                            ; :blobs (blobs (target task))
                            :target-id (id (target task))
                            :dir (dir (target task)))))


(defmethod task-applies-to-target-p ((task-class-name (eql 'lacrosse-cp-init-task)) target-node)
  (dbug :docker-task "task-applies-to-target-p lacrosse-cp-init-task")
  (and *lacrosse*
       (lacrosse-cp-target-p (target target-node))
       ;;(cp-address (target target-node))
       (not (uiop:file-exists-p (init-marker-filename (target target-node))))
       ))

(defmethod process-line ((task lacrosse-cp-init-task) line)
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

(defun repo-name-from-repo-address (address)
  "Returns string between rightmost slash and dot git suffix."
  (cl-ppcre:register-groups-bind (name)
                                 ("/([^/]+)\\.git$" address)
                                 name))
