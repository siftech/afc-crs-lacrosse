;;; -------------------------------------------------------------------------
;;; isabel.lisp
;;; - Isabel project support.
;;; -------------------------------------------------------------------------
;;; COPYRIGHT START
;;; COPYRIGHT END
;;; -------------------------------------------------------------------------

(cl:in-package :fuzzbomb)

;;; ------------------------------------------------------------
;;; isabel-task  ISABEL
;;; ------------------------------------------------------------

(defmacro match-one-regex (regex target)
  (let ((f (gensym)))
    `(cl-ppcre:register-groups-bind (,f) (,regex ,target) ,f)))

;; use this so that dont over-write vars to nil when there is no match
(defmacro setf-if-match (var regex target)
  `(let ((a (match-one-regex ,regex ,target)))
      (when a (setf ,var a))))

;;; ------------------------------------------------------------
;; (defun run-isabel (task)
;;   (dbug :amp "Called run-isabel on ~S" task)
;;   (dbug :top "input-pathname ~S" (input-pathname task))
;;   (dbug :top "output-pathname ~S" (output-pathname task))
;;   (dbug :top "target ~S" (target task))
;;   (dbug :top "input-channel ~S" (input-channel (target task)))

;; ;;      ;; when using canned povs that are in the same dir as the target,
;; ;;      ;; we con the find-crashes tool by setting the output-pathname
;; ;;      ;; of this mostly-fake driller-task to the dir of target, and making sure to
;; ;;      ;; manually name the pov correctly (containing 'id' and 'crashes' in the name)
;; ;;      ;; FIXME currently this will only work with one pov for the one target in the tgz
;; ;;  (when *use-canned-povs*
;; ;;      (setf (output-pathname task) (directory-namestring (path (target task))))
;; ;;      (dbug :top "revised output-pathname ~S" (output-pathname task)))

;;         ;; When using canned povs that are in the same dir as the target,
;;         ;; we use a special finder that expects the pov for X[.exe] to be pov-X
;;         ;; Note there *must* be a pov-X file for each X or isabel will puke.
;;         ;; I'm not in the mood to robustify this enough to handle that.
;;   (let* ((input-channel (input-channel (target task)))
;;          (target-path (cond (*lacrosse*
;;                             (target-executable (target task)))
;;                            (t (path (target task)))))
;;          (find-crash-cmd
;;            (if *use-canned-povs*
;;                 (format nil "~a/code/tools/find-canned-crashes ~A |head -1" (getenv "LACROSSE_HOME") (namestring target-path))
;;                 (format nil "~a/code/tools/find-crashes ~A |head -1" (getenv "LACROSSE_HOME") (output-pathname task))))
;;          (povname
;;            (string-trim '(#\Space #\Tab #\Newline)
;;                         (values (uiop:run-program find-crash-cmd :output :string))))
;;          (run-cmd
;;            (format nil "run ~A" (cmdline-args input-channel povname)))
;;          (cmd
;;            (format nil "/realuser.sh bash --login -c \"~a/code/tools/run-exploitable ~A ~A ~A '~A' \""
;;                    (getenv "LACROSSE_HOME")
;;                    (namestring (output-pathname task))
;;                    (namestring target-path)
;;                    povname
;;                    run-cmd))
;;         (result (make-instance 'exploitable-result))
;;         (site (make-instance 'bug-site)))

;;   (dbug :top "find-crash-cmd is ~S" find-crash-cmd)
;;   (dbug :top "povname is ~S" povname )
;;   (dbug :top "cmd is ~S" cmd)
;;         ;; if the input has to be in a specifically named file, copy it.
;;   (when (named-file-channel-p input-channel)
;;      (let ((cp-cmd (format  nil "cp ~a ./~a" povname (filename input-channel))))
;;         (dbug :top "For named-file-channel, cp-cmd: ~s~%" cp-cmd)
;;         (dbug :top "cp-output: ~s~%" (uiop:run-program cp-cmd :output :string))))

;;     ;; The lines that will populate the 'exploitable' command section of the
;;     ;; exploitable-result look like this.

;; ;; [1/4/2018 12:49:34] TOP: got line [(gdb) Starting program: /scratch/cgc-data/musliner/localhost/0-3724080387/target-2/kl_2_3.exe < ./kl_2_3.exe/sync/fuzzer-master/crashes/id:000000,sig:11,sync:driller,src:000001]

;; ;; [1/4/2018 12:49:34] TOP: got line [Description: Access violation near NULL on source operand]
;; ;; [1/4/2018 12:49:34] TOP: got line [Short description: SourceAvNearNull (16/22)]
;; ;; [1/4/2018 12:49:34] TOP: got line [Hash: 1a1e6b9318070a64f853c328ca390536.b08e0284ef4dc7c17aaf1807bf167197]
;; ;; [1/4/2018 12:49:34] TOP: got line [Exploitability Classification: PROBABLY_NOT_EXPLOITABLE]
;; ;; [1/4/2018 12:49:34] TOP: got line [Explanation: The target crashed on an access violation at an address matching the source operand of the current instruction. This likely indicates a read access violation, which may mean the application crashed on a simple NULL dereference to data structure that has no immediate effect on control of the processor.]
;; ;; [1/4/2018 12:49:34] TOP: got line [Other tags: AccessViolation (21/22)]

;; ;; here's a variant output where the crash address is on multi-line, alas:
;; ;; [1/29/2018 13:32:56] TOP: got line [(gdb) Starting program: /scratch/cgc-data/musliner/localhost/0-3726243096/target-4/kl_2_3.exe < ./kl_2_3.exe/sync/fuzzer-master/crashes/id:000000,sig:11,sync:driller,src:000001]
;; ;; [1/29/2018 13:32:56] TOP: got line []
;; ;; [1/29/2018 13:32:56] TOP: got line [Program received signal SIGSEGV, Segmentation fault.]
;; ;; [1/29/2018 13:32:56] TOP: got line [0x00007ffff7a5bcc0 in _IO_vfprintf_internal (]
;; ;; [1/29/2018 13:32:56] TOP: got line [    s=0x7ffff7dd2620 <_IO_2_1_stdout_>, format=<optimized out>, ]
;; ;; [1/29/2018 13:32:56] TOP: got line [    ap=ap@entry=0x7fffffffe928) at vfprintf.c:1632]
;; ;; [1/29/2018 13:32:56] TOP: got line [(gdb) 1632       vfprintf.c: No such file or directory.]
;; ;; [1/29/2018 13:32:56] TOP: got line [__main__:99: UserWarning: GDB v7.11 may not support required Python API]



;;     ;; The lines that will populate the 'fuzzy_pov_stack_search' portion of the
;;     ;; exploitable result instance looks like this (one example).

;; ;; ;; [8/3/2019 14:00:06] TOP: got line [RESULT: [FPSS] BOF: VeryUnlikely [3 / 8]. Reason: PoV not in stack, Retaddr bad, Retaddr not in PoV]

;;     ;; NOTE: When 'exploitable' is executed in gdb, it is also the case that
;;     ;; the 'fuzzy_pov_stack_search' is executed--regardless of the outcome of
;;     ;; the 'exploitable' test. Later in the lisp, we can join the information
;;     ;; together appropriately.


;;   (setf (timestamp result) (musliner::tstamp nil))
;;   (with-docker-exec-stream (io-stream (strcat (uiop:getenv "CONTAINER_PREFIX") "-driller") cmd)
;;       (dolines (line io-stream)
;;         (dbug :top "got line [~A]" line)

;;                 ;;Program received signal SIGFPE, Arithmetic exception.
;;                 ;; FIXME add slot and store signal in result

;;         ;; should do another more simple one that works if no symbols and so dont get file etc., eg:
;; ;;              0x0000000000400861 in main ()
;; ;;              0x00000000004006f5 in main () at kl_2_3_divbyzero.c:21

;;         ;; Attempt to match the 'exploitable' result.
;;         (cl-ppcre:register-groups-bind (addr fn file linenum)
;;                 ("(0x\\S*) in (\\S+) \\([\\S ]*\\) at (\\S+):(\\d+)" line)
;;                 (dbug :top "Found crash loc: ~s ~s ~s ~s" addr fn file linenum)
;;                 (cond (*lacrosse*
;;                        (setf (source-file site) (strcat (namestring (dir (target task))) file)))
;;                       (t
;;                        (setf (source-file site) file)))
;;                 (setf (pc site) addr)
;;                 (setf (source-line site) linenum)
;;         )

;;         (setf-if-match (description result)
;;                        "Description: (.*)" line)
;;         (setf-if-match (short-description result)
;;                        "Short description: (\\w+)" line)
;;         (setf-if-match (hash result)
;;                        "Hash: (.*)" line)
;;         (setf-if-match (classification result)
;;                        "Classification: (.*)" line)
;;         (setf-if-match (explanation result)
;;                        "Explanation: (.*)" line)
;;         (setf-if-match (other-tags result)
;;                        "Other tags: (.*)" line)

;;         ;; Attempt to match the 'fuzzy_pov_stack_search' result and make it
;;         ;; suitable for use in CL.
;;         (let ((result-text nil))
;;           (setf-if-match
;;            result-text "RESULT: \\[FPSS\\] BOF: (\\w\(?:\[-/\\w\]*\\w\))" line)
;;           (when result-text
;;             ;; NOTE: At the time of this writing, you can find what the
;;             ;; valid keywords would be by looking in the BOFCategory
;;             ;; class definition in the fuzzy_pov_stack_search.py file 
;;             (setf (fpss-classification result)
;;                   (intern (string-upcase result-text) "KEYWORD"))))

;;         ;; [3/30/2020 19:00:18] TOP: got line [RESULT: [FPSS] CHAIN: (SBOFW -> DBZ): PoV-Intersected-Dataflow-DBZ-Stkvars-With-Zeros. The locations of these stkvars intersect with the PoV extent, are actually zero according to the process memory, and are in the direct dataflow to the division: ['var_4']]

;;   ))

;;     (describe result)
;;     ;;(describe site)   ;; now happens inside result description due to print-readably

;; ;;  (dbug :top "exploitable description: ~A" (description result))
;; ;;  (dbug :top "exploitable short-description: ~A" (short-description result))
;; ;;  (dbug :top "exploitable hash: ~A" (hash result))
;; ;;  (dbug :top "exploitable classification: ~A" (classification result))
;; ;;  (dbug :top "exploitable explanation: ~A" (explanation result))
;; ;;  (dbug :top "exploitable other-tags: ~A" (other-tags result))

;;     (let ((classified-bug (generate-bf-classification result site)))
;;       (cond
;;         (classified-bug
;;          (setf (site classified-bug) site)
;;          (dbug :top "RESULT: Inferred BF bug: ~A" (class-shortname classified-bug))
;;          (dbug :top "json encoding: ~s" (cl-json:encode-json-to-string classified-bug))
;;          (write-result-to-target-dir classified-bug task)
;;          classified-bug)        ;; return it
;;         (t
;;          (dbug :top "No inferred BF bug detected.  Creating non-specific bug.")
;;          (let ((bug (make-instance 'bug :result result :site site)))
;;            (dbug :top "json encoding: ~s" (cl-json:encode-json-to-string bug))
;;            (write-result-to-target-dir bug task)
;;            bug))))
;;     ))

(defun some-string= (str str-maybe-atom-or-list)
  "If STR-MAYBE-ATOM-OR-LIST is an atomic string, then return (STRING= STR
STR-MAYBE-ATOM-OR-LIST), if STR-MAYBE-ATOM-OR-LIST is a list of strings then
return T if SOME string in STR-MAYBE-ATOM-OR-LIST is STRING= to STR."
  (etypecase str-maybe-atom-or-list
    (string (string= str str-maybe-atom-or-list))
    (list (some (lambda (elem) (string= str elem))
                str-maybe-atom-or-list))))

;; part of isabel, maybe refactor to new file.
(defun generate-bf-classification (result site)
  (let ((sd (short-description result)))

    ;; TODO: Currently, we have no way to decide between multiple pieces of
    ;; strong evidence for one BF or another in certain contexts.

    ;; This control form is unfortunate because it is reactive to changes in
    ;; run-exploitable's source code.
    (cond
      ((or (string= sd "ReturnAv")
           (string= sd "PossibleStackCorruption")
           (string= sd "StackBufferOverflow")
           (string= sd "SegFaultOnPc"))
       ;; For now, restrict the FPSS analysis to this.
       (case (fpss-classification result)
         (:unknown-classification
          (dbug :top "GENERATE-BF-CLASSIFCATION(): unknown classification!"))
         ;; This test can be annoyingly sensitive, so let's only record when we
         ;; think we've hit all of the marks for a stack BOF.
         ((:almost-targetted :likely :targetted :very-likely :almost-surely)
          (make-instance 'bof
                         :site site
                         :access 'write
                         :boundary 'above
                         :location 'stack
                         :magnitude 'unknown
                         :data-size 'unknown
                         :reach 'unknown))))
      ((string= sd "UseAfterFree") nil)
      ((string= sd "BranchAv") nil)
      ((string= sd "StackCodeExecution") nil)
      ((string= sd "DestAv") nil)
      ((string= sd "BadInstruction") nil)
      ((string= sd "HeapError") nil)
      ((string= sd "StackOverflow") nil)
      ((string= sd "SegFaultOnPcNearNull") nil)
      ((string= sd "BranchAvNearNull") nil)
      ((string= sd "BlockMoveAv") nil)
      ((string= sd "DestAvNearNull") nil)
      ((string= sd "SourceAvNearNull") nil)
      ((string= sd "DivideByZero")
       (make-instance 'arc
                      :site site
                      :result-fault 'undefined
                      :operation '/
                      :operand-error 'domain-error
                      :types 'unknown
                      :data-size 'little
                      :excursion 'discrete))
      ((string= sd "FloatingPointException") nil)
      ((string= sd "BenignSignal") nil)
      ((string= sd "SourceAv") nil)
      ((string= sd "AbortSignal") nil)
      ((string= sd "AccessViolation") nil)
      ((string= sd "UncategorizedSignal") nil)
      (t
       (dbug :top "Unable classify run-exploitable bug: ~S~%" sd)
       nil))
  ))
