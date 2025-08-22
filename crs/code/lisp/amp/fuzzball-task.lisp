;;; Callback functions for creating a fuzzball docker-task

(cl:in-package :fuzzbomb)


(defvar *fuzzball-bin* "/neo-fuzz/code/fuzzball/exec_utils/fuzzball")
(defvar *z3-bin* "/neo-fuzz/code/ext/z3/build/z3")
(defvar *solver-path* *z3-bin*)

(defclass fuzzball-docker-task (docker-task-new)
  ((found-crash :initform nil :accessor found-crash)
   (res :initform nil :accessor res)
   (reported :initform nil :accessor reported)
   ;; This should be just a filename, contained in output-pathname of the task
   ;; representing the input from stdin required to trigger the heap sequence.
   (heap-sequence-input :initform "" :accessor heap-sequence-input)
   ;; This represents the path
   ;; representing the input from stdin required to trigger the heap sequence.
   (results :initform "" :accessor results))
  (:default-initargs
   :image "fuzzball"
   :container-start-args "--cap-add=SYS_PTRACE --security-opt seccomp=unconfined"
  ))

(defmethod initialize-instance :after ((task fuzzball-docker-task) &key)
  (dbug :docker-task "initialize-instance :after ((task fuzzball-docker-task): ~s" task)
  (setf (output-pathname task)
          (or (output-pathname task)
              (strcat (namestring (path (target task))) "-output/")))
  (dbug :top "Fuzzball task output-pathname is ~s" (output-pathname task))

  (with-slots (cmd) task
    (let ((arg-file (setup-temp-file))) (setf cmd
          (format nil "/realuser.sh /bin/bash --login -c 'env -i ~A ~A ~A ~A ~A ~A ~A ~A ~A ~A ~A ~A ~A -- ~A~A ~A < /dev/zero' | LANG=C tr -cd '[:print:][:cntrl:]'"
                  ;;(bin-directory bin)
                  *fuzzball-bin*
                  ;;(arch-from-bin bin)
                  ;;(platform-to-string (platform (target task)))
                  (fuzzball-static-args)
                  *fuzzball-more-args*
                  (let ((warning-path (getattr :pereti-warning-path (target task))))
                    (if warning-path
                        (format nil "-pereti-warning ~A" warning-path)
                        ""))
                  (let ((cfg-path (getattr :pereti-cfg-path (target task))))
                    (if cfg-path
                        (format nil "-pereti-cfg ~A" cfg-path)
                        ""))
                  (let ((jump-path (getattr :pereti-jump-path (target task))))
                    (if jump-path
                        (format nil "-pereti-cfg-jumps ~A" jump-path)
                        ""))
                  ;; file containing trace of heap vulnerability found with
                  (if (getattr :clib-vuln (target task))
                      (get-heaphopper-trace task)
                      "")

                  (format nil "-output-path ~A" (output-pathname task))
                  ;; Whatclib file
                  ;; There probably exists a macro for this, but I cant find it atm.
                  (if (getattr :LIB-ADDR-MAP (target task))
                      (format nil "-whatclib-file ~A" (getattr :LIB-ADDR-MAP (target task)) )
                      ""
                      )

                  ;; reads symbolic input from file
                  "-symbolic-file"
                  arg-file
                  "-symbolic-stdin-concrete-size"
                  (namestring (path (target task)))
                  (if *heapbuster-mode*
                    "./"
                    "")
                  (if *heapbuster-mode*
                    (file-namestring (path (target task)))
                    (path (target task)))
                  ;; Added argument to have file read from symbolic file
                  ;; "args"
                  (if *heapbuster-mode*
                    ""
                    arg-file)
                  )))))

(defun fuzzball-static-args ()
  (format nil "~{~a~^  ~}"
          (list

           "-linux-syscalls"
           "-setup-initial-proc-state" "true"

           "-solver" "smtlib"
           "-solver-path" *solver-path*
           "-ignore-div-0" "raise"
           "-zero-memory"
           "-noop-unhandled-special"
           "-skip-output-concretize"

           "-table-limit" "12"
           "-target-guidance" "1.0"
           "-no-sym-regions"
           ;; "-check-for-jump-to" "42"  ;;"-finish-on-controlled-jump"
           "-check-for-null" ;;"-finish-on-null-deref"
           "-solve-final-pc"
           ;; "-check-for-ret-addr-overwrite"  ;;"-finish-on-ret-addr-overwrite"
           ;; "-solver-timeout" "120"
           ;; "-timeout-as-unsat"
           "-trace-assigns-string"
           "-trace-stopping"
           "-trace-heaphopper"
           )))

(defmethod pre-exec ((task fuzzball-docker-task))
  "Run before fuzzball task starts."
  (dbug :amp "Called run-fuzzball-on-bin."))


(defmethod post-exec ((task fuzzball-docker-task))
  "Run after fuzzball finishes."
  (when (heap-sequence-input task)
    (let ((json-file (concatenate 'string (output-pathname task) (heap-sequence-input task))))
      (dbug :amp "Looking for json-file ~s" json-file)
      (when (file-exists-p json-file)
        (dbug :amp "Looking for stdin in ~s" json-file)
        (let ((path-list (getassoc :path-translator (json-from-path json-file) :proper t)))
          (iterate:iter (iterate:for elem iterate::in-vector path-list)
            (when (equal (getassoc :source elem :proper t) "stdin")
              (let ((exploit (getassoc :mapped elem :proper t)))
                (setattr :exploit exploit (target task))
                (dbug :amp "exploit exists at ~s" (getattr :exploit (target task)))
                (add-new-target task (params (target task)))
                (iterate:finish))))))))
  (dbug :amp "Finished run-fuzzball-on-bin."))

(defmethod task-applies-to-target-p ((task-class-name (eql 'fuzzball-docker-task)) target-node)
  ; So pereti-cfg-task, pereti-jump-task, and pereti-warnings-task have a chance to run first.
  (when (or (task-applies-to-target-p 'pereti-cfg-task target-node)
            (task-applies-to-target-p 'pereti-jump-task target-node)
            (task-applies-to-target-p 'pereti-warnings-task target-node))
    (dbug :top "Fuzzball waiting for Pereti for target ~s" (target target-node))
    (return-from task-applies-to-target-p nil))

  (and 
    (not (getattr :exploit (target target-node)))
    (or (getattr :clib-vuln (target target-node))
        (and *use-fuzzball* (executable-target-p target-node)))))

(defun arch-from-bin (bin)
  "Return the architecture of the bin as a string."
  (case (platform bin)
          (:arm "arm")
          ;; Fuzzball doesn't actually have mips support btw,
          ;; but this is a valid symbol for the 'platform' slot
          (:mips "mips")
          (:x86-64 "x64")
          (:x86 "x86")
          ;; The default architecture, if one is not specified.
          (nil "x86")
          ))

(defun platform-to-string (platform-keyword)
  (getassoc platform-keyword
            '((:arm "arm")
              ;; Fuzzball doesn't actually have mips support btw,
              ;; but this is a valid symbol for the 'platform' slot
              (:mips "mips")
              (:x86-64 "x64")
              (:x86 "x86")
              ;; The default architecture, if one is not specified.
              (:unknown "x86")
              (nil "x86"))))

(defun xml-output-dir (bin)
  "Return the directory to put POVML test cases for BIN."
  (with-temp-dir (dir "fuzzball-test-cases" :delete-on-exit nil)
    (dbug :fball "Putting test cases for ~A in ~A" bin dir)
    dir))

(defun fuzzball-report-crash (task)
  (incf *successful-fuzzball-tasks*)
  (dbug :top "RESULT: Fuzzball has now pwned ~A targets!" *successful-fuzzball-tasks*)
  (if (heap-sequence-input task)
      (send-message-to-optimi :type :new-pov 
                              :svc-id -1
                              :target-id (id (target task))
                              :pov-path (output-pathname task)
                              :heap-sequence-input (format nil "~A~A"
                                                           (output-pathname task)
                                                           (heap-sequence-input task)))
      (send-message-to-optimi :type :new-pov 
                              :svc-id -1 
                              :target-id (id (target task))
                              :pov-path (output-pathname task))
      ))


;; checks line to see if it's an actual crash
;;
(defmethod fuzzball-check-crash ((task fuzzball-docker-task) str)
  (cond
    ;; Heap vulnerability search, in which case we only care about the heap sequence.
    ((getattr :clib-vuln (target task))
     (setf-if-match (heap-sequence-input task) "Heap Sequence Done with SOLUTION:(\\w+\.json)" str)
     (cl-ppcre:scan-to-strings "Heap Sequence Done" str))
    ;; Default case, scanning for any vulnerability
    (t (or
        (cl-ppcre:scan-to-strings "Stopping at null deref" str)
        (cl-ppcre:scan-to-strings "Stopping at division by zero" str)
        (cl-ppcre:scan-to-strings "Stopping at symbolic jump" str)
        (cl-ppcre:scan-to-strings "Stopping on double free" str)
        (cl-ppcre:scan-to-strings "Stopping at jump to null " str)))))

(defun get-heaphopper-trace (task)
  "Return the argument string to pass in to fuzzball with the heaphopper trace identified for this libc"
  (format nil "-heaphopper-file ~A" (getattr :clib-vuln (target task))))

(defmethod process-line ((task fuzzball-docker-task) line)
  (with-slots (found-crash res reported)
      task
    (dbug :top "got line [~A]" line)
    (cond ((setf res (fuzzball-check-crash task line))
                (dbug :top "RESULT: Fuzzball found: ~A!" res)
                (setf found-crash t)
                (when (not reported)
                  (setf reported T)
                  (fuzzball-report-crash task))))))

;;; FIXME   replace w/ something lispy
(defun bin-directory (bin)
  "Returns the directory of the binary."
  (cl-ppcre:regex-replace (namestring (name bin)) (namestring (path bin)) "" ))


;; Fuzzball needs a temporary file to act as the file to read from in the case
;; of using a symbolic file. This file not only needs to exist but has to have something
;; in it. The values written here will be ignored by fb when it runs.
(defun setup-temp-file ()
  (with-temp-filename (arg-file "args" :delete-on-exit nil)
    (blurt-binary-file arg-file "UNUSED" )
    arg-file
    )
  )
