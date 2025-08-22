;;; -------------------------------------------------------------------------
;;; globals.lisp
;;; - Global definitions (mainly variables) for CIRCA Adaptive Mission Planner.
;;; - $Revision: 1.13 $
;;; -------------------------------------------------------------------------
;;; COPYRIGHT START
;;; COPYRIGHT END
;;; -------------------------------------------------------------------------

(in-package :fuzzbomb)

(defvar *long-program-name* "CIRCA AMP")

(defvar *use-telemetry* t
  "If t, use AIxCC-specific telemetry output.
   If required env vars are not set, lax will warn and set to nil.")

(defvar *amp-directory* nil
  "Runtime location of amp executable.  Default location of
   domain and performance profile files.  Set in init-globals.")

(defvar *neo-fuzz-home* nil
  "Runtime location of NEO-FUZZ home directory.  Defaults to
   value of NEO_FUZZ_HOME env var, and falls back to a system
   relative path otherwise.  Set in init-globals.")

(defvar *lacrosse* t
  "Is this Neo-Fuzz instance a Lacrosse (AIxCC entry) agent?")

(defvar *lax-final* t
  "Is this Neo-Fuzz instance a Lacrosse Final (AIxCC final round entry) agent?")

(defvar *lax-final-one-ctask-mode* nil
  "If t, only work on the first challenge task that is received.")

(defvar *lax-home* nil
  "Runtime location of lacrosse, set in init-globals.")

(defvar *lax-tools* nil
  "Runtime location of lacrosse tools dir for convenience, set in init-globals.")
(declaim (type (or null string pathname) *lax-tools*))

(defvar *cp-root* nil
  "Supplied by env, set in init-globals.")

(defvar *crs-scratch* nil
  "Supplied by env, set in init-globals.")

(defvar *fake-fuzz* nil
  "When t, fake fuzzing by returning canned povs.")
(declaim (type boolean *fake-fuzz*))


(defvar *fake-fuzz-args* nil
  "When *fake-fuzz* is t, use this value to find canned povs:
    - when nil, use the default (selected by cpname)
    - otherwise, this should be a list of strings specifying pov file.")

;;; TEMP flag to use fake git bisect task for task dispatch testing.
(defvar *fake-git-bisect* nil
  "TEMP flag to use fake git bisect task for task dispatch testing.")

;;; FIXME This TEMP hack only works in 1fb mode.
(defvar *fake-git-bisect-done-p* nil
  "TEMP flag to ensure fake-git-bisect only runs once.")

(defvar *fake-file-to-patch* nil
  "Use canned table to identify file-to-patch by bic.")

(defvar *use-afl* t "If non-nil, AFL tasks can be created.")

(defvar *use-libfuzzer* t "If non-nil, libfuzzer tasks can be created.")

(defvar *use-oss-fuzz* t "If non-nil, oss-fuzz-tasks can be created.")

(defvar *use-llm-find-and-patch-vuln* t "If non-nil, llm-find-and-patch-vuln-task can be created.")

(defvar *use-llm-hypothesize-vulns-task* t
  "If t, llm-hypothesize-vulns-task will be created for each ctask as it arrives.
   If :deadline-dependent, llm-hypothesize-vulns-task will be created at a time relative
   to the ctask deadline.
   If nil, llm-hypothesize-vulns-task will not be created.")


(defvar *use-llm-explain-pov-task* t "If non-nil, llm-explain-pov-task can be created.")

(defvar *use-llm-patch-with-pov-task* t "If non-nil, llm-patch-with-pov-task can be created.")

(defvar *use-llm-patch-sans-pov-task* t "If non-nil, llm-patch-sans-pov-task can be created.")

(defvar *use-llm-assess-sarif-task* t "If non-nil, llm-assess-sarif-task can be created.")

(defvar *default-fuzzing-engines* '(:libfuzzer :afl))

(defvar *lacrosse-llm-stub-patch-p* nil
  "Run with the stubbed patching script?")

(defvar *use-lacrosse-llm-gen-patch-p* t
  "When t, Use llm patch gen task.")

(defvar *lacrosse-llm-gen-patch-test-p* t
  "Test the patch after generation.")

(defvar *use-swamp-fuzz* nil
  "When non-nil, send a bunch of fake msgs to optimus.")

(defvar *num-swamp-fuzz-msgs* 10
  "Number of empty msgs sent by an instance of the swamp-fuzz-task.")

(defvar *profile-lax* nil
  "When non-nil, engage the profiler.  Currently   assumes sbcl.")

(defvar *oss-fuzz-restarter-score-factor* 1.0
  "Factor multiplied by number of restarts to adjust score/priority.")

(defvar *default-time-to-hyp-vulns-full-ctask* (* 180 60)
  "Default time in SECONDS before deadline to start hypothesizing vulnerabilities.")

(defvar *default-time-to-hyp-vulns-delta-ctask* (* 45 60)
  "Default time in SECONDS before deadline to start hypothesizing vulnerabilities.")

(defvar *default-time-to-submit-psps* (* 30 60)
  "Default time in SECONDS before deadline to start submitting psps (patches sans povs).")

(defvar *psp-submit-strategy* :delayed
  "When :greedy, submit psps as the arrive until patch-submit-limit is met.
   When :delayed, hold psps until time-to-submit-psps and then submit best
   until patch-submit-limit is met.")

;;; FIXME This is only partially honored now:
;;;  - When *use-llm-hypothesize-vulns-task* is :deadline-dependent, hyp-vulns won't be
;;;    started when there are more than *default-patch-submit-limit* submitted patches.
;;;  
(defvar *default-patch-submit-limit* 10
  "Default limit on number of patches submitted per ctask. NOTE: limit only applies when
   lax considers submitting psps (patch-sans-povs).  When nil, submit all the patches.")

(defvar *mid-confidence-psp-limit* 7)
(defvar *high-confidence-psp-limit* 13)

(defvar *default-psp-threshold-score* 5.0
  "Default confidence score which psp must exceed to be considered for submission.")

(defvar *high-confidence-psp-threshold* 15.0
  "Confidence score which psp must exceed to be considered high confidence.")

(defvar *lacrosse-llm-gen-patch-test-canned-patch-p* nil
  "Use the example patch for testing.")

;;; Checked by lacrosse-gen-pov-blob-task
(defvar *lacrosse-stub-pov-blob-p* nil  ;;   dunno if this is active, but it should be off by default
  "Run with the stubbed pov blob script?")

;;; Checked by init-ctask-task
(defvar *use-canned-harnesses* nil
  "When non-nil, init-ctask-task returns a canned list of harnesses
   (as selected by project-name, instead of building the ctask.")

;;; valid options: "--opus" "--chat-gpt" "--sonnet-3.5" "--haiku"
;; (defvar *lacrosse-gen-patch-llms* '("--chat-gpt" "--sonnet-3.5" "--gemini"))
;;(defvar *lacrosse-gen-patch-llms* '("--chat-gpt" "--sonnet-3.5"))
(defvar *lacrosse-gen-patch-llms* '("--chat-gpt" "--sonnet-3.5"))
(defvar *lacrosse-gen-seeds-llms* '("--chat-gpt" "--sonnet-3.5"))

;; The list of default LLM "resorts" used in the AIxCC finals (first resort, second resort, etc)
(defvar *default-llm-args* '("gpt41" "sonnet37" "gemini25pro" "gpt4o" "sonnet4" "gemini25flash" "o4mini" "haiku35" "gemini20flash" "gpt41mini" "sonnet35" "gemini15pro" "gpt41nano")
  "The default llm args to pass to the pipeline.")

;; The list of LLM "resorts" used in the AIxCC finals for cases where we have a POV.
;; (Opus could get much more expensive for much less payoff in the non-pov case, and we don't
;; want to burn budget that could be used for POV-based patching)
(defvar *pov-llm-args* '("gpt41" "sonnet37" "gemini25pro" "gpt4o" "sonnet4" "gemini25flash" "o4mini" "haiku35" "gemini20flash" "gpt41mini" "sonnet35" "gemini15pro" "gpt41nano" "o3" "opus4")
  "The llm args to pass to the pipeline for POV-based characterization and patching.")

(defvar *sarif-llm-args* '("o3" "opus4" "o1" "gpt41" "sonnet4" "sonnet37" "gemini25pro" "gpt4o" "gemini25flash" "o4mini" "haiku35" "gemini20flash" "gpt41mini" "sonnet35" "gemini15pro" "gpt41nano")
  "The llm args to pass to the pipeline for sarif assessments.")

(defvar *use-llm-seeds* t
  "When t, use llms to gen seeds for fuzzing. (See libfuzzer-task.lisp, pre-exec (libfuzzer-task).")

(defvar *libfuzzer-examples-default* 20
  "Default number of examples per fuzzer task to generate as seeds before fuzzing")

(defvar *use-revert-to-patch-p* t
  "When t, use the revert-to-patch task to gen patches.")

(defvar *revert-to-patch-limit-per-bic* 50
  "Max number of revert-to-patch to tasks to create per bic found.")

(defvar *reassign-dead-amp-tasks* T
  "If an AMP dies, should OPT try to reassign his task?")

(defvar *fuzzer-timeout* nil
  "Overrides the default fuzzer-timeout with this value.
Must be either NIL or a string that is a number with a time
unit suffix.")
(declaim (type (or string null) *fuzzer-timeout*))

(defvar *init-ctask-task-presumed-dead-time* 18
  "The number of minutes to wait for an init-ctask task to complete
before presuming it dead and reassigning a duplicate init-ctask to another FB.")

(defvar *cover-fuzzbombs-with-fuzz-tasks* t
  "If non-nil, create at least one initial fuzz task for every fuzzbomb,
   poss including duplicate engine/harness/sanitizer combos.")

(defvar *no-cover-min* 5
  "Min number of fbs to assign to fuzz tasks for any ctask.")

(defvar *cover-factor* (/ 1 6.0)
  "When *cover-fuzzbombs-with-fuzz-tasks* is non-nil, multiply the number
   of fuzzbombs by this factor to decide how many fuzzer tasks to create
   for each ctask.  *cover-factor* should be positive (0,1].")

;;; -------------------------------------------------------------------------
;;; *-amp.lisp

(defvar *phasename-order*)

(defvar *default-mission-planner-fn*)

;;; -------------------------------------------------------------------------

(defvar *upload-every-revision* nil "Should we upload all CB revisions we ever make?  Only useful for Scored Events")


;;; When you introduce a new global variable that should be displayed,
;;; add the variable name to the special declaration and the list of
;;; flags in the DOLIST.  
(defun print-flags (&optional (stream t))
  (declare (special
            *run-brute*
            *max-stars-to-wait*
            *send-idle-msg*
            *init-task-limit-per-fuzzbomb*
            *run-fake-bridge*
            *afl-qemu-mode*
            *use-afl*
            *use-libfuzzer*
            *no-afl-plusplus*
            *use-afl-blind-thread*
            *use-afl-via-driller*
            *use-driller*
            *use-canned-povs*
            *use-isabel*
            *use-gui*
            *driller-timeout*
            *driller-workers*
            *driller-force-interval*
            *driller-more-args*
            *lacrosse*
            *lax-final*
            *lax-final-one-ctask-mode*
            *lacrosse-llm-stub-patch-p*
            *lacrosse-stub-pov-blob-p*
            *fuzzer-timeout*
            *fake-fuzz*
            *fake-fuzz-args*
            *fake-git-bisect*
            *fake-file-to-patch*
            *lacrosse-gen-patch-llms*
            *lacrosse-gen-seeds-llms*
            *use-revert-to-patch-p*
            *use-telemetry*
            *revert-to-patch-limit-per-bic*
            *reassign-dead-amp-tasks*
	    *init-ctask-task-presumed-dead-time*
            *cover-fuzzbombs-with-fuzz-tasks*
            *use-oss-fuzz*
	    *use-llm-hypothesize-vulns-task*
	    *use-llm-patch-sans-pov-task*
            *use-swamp-fuzz*
            *num-swamp-fuzz-msgs*
            *profile-lax*
            *oss-fuzz-restarter-score-factor*
            *use-canned-harnesses*
            *cover-fuzzbombs-with-fuzz-tasks*
            *cover-factor*
            *psp-submit-strategy*
            *default-time-to-submit-psps*
            *default-patch-submit-limit*
            *mid-confidence-psp-limit*
            *high-confidence-psp-limit*
            *default-psp-threshold-score*
            *high-confidence-psp-threshold*
            ))
  (dbug :top "printenv: ~s" (uiop:run-program "printenv" :output :string))
  (dbug :top "docker ps -a:~%~s" (uiop:run-program "docker ps -a" :output :string))
  (dbug :top "*experiment-dir*: ~a" *experiment-dir*)

  (format stream "----------------------------------------------~%")
  (format stream "DOCKER_TAG: ~A~%" (uiop:getenv "DOCKER_TAG"))
  (format stream "(asdf:asdf-version): ~s~%" (asdf:asdf-version))
  (format stream "FUZZBOMB flags:~%")
  ;;(format stream "start time:  ~A~%~%" (tstamp nil))
  (dolist (flag '(*run-brute*
                  *max-stars-to-wait*
                  *send-idle-msg*
                  *init-task-limit-per-fuzzbomb*
                  *run-fake-bridge*
                  *upload-povs*
                  *use-afl*
                  *use-libfuzzer*
                  *no-afl-plusplus*
                  *use-afl-blind-thread*
                  *afl-qemu-mode*
                  *use-afl-via-driller*
                  *use-driller*
                  *use-canned-povs*
                  *use-isabel*
                  *use-gui*
                  *driller-timeout*
                  *driller-workers*
                  *driller-force-interval*
                  *driller-more-args*
                  *lacrosse*
                  *lax-final*
                  *lax-final-one-ctask-mode*
                  *lacrosse-llm-stub-patch-p*
                  *lacrosse-stub-pov-blob-p*
                  *fuzzer-timeout*
                  *fake-fuzz*
                  *fake-fuzz-args*
                  *fake-git-bisect*
                  *fake-file-to-patch*
                  *lacrosse-gen-patch-llms*
                  *lacrosse-gen-seeds-llms*
                  *use-llm-seeds*
                  *use-revert-to-patch-p*
                  *revert-to-patch-limit-per-bic*
                  *reassign-dead-amp-tasks*
                  *use-telemetry*
                  *features*
		  *init-ctask-task-presumed-dead-time*
                  *cover-fuzzbombs-with-fuzz-tasks*
                  *use-oss-fuzz*
		  *use-llm-hypothesize-vulns-task*
		  *use-llm-patch-sans-pov-task*
                  *use-swamp-fuzz*
                  *num-swamp-fuzz-msgs*
                  *profile-lax*
                  *oss-fuzz-restarter-score-factor*
                  *use-canned-harnesses*
                  *cover-fuzzbombs-with-fuzz-tasks*
                  *cover-factor*
                  *psp-submit-strategy*
                  *default-patch-submit-limit*
                  *default-time-to-submit-psps*
                  *default-patch-submit-limit*
                  *mid-confidence-psp-limit*
                  *high-confidence-psp-limit*
                  *default-psp-threshold-score*
                  *high-confidence-psp-threshold*
                  ))
    (format stream "   ~A      ~S~%" (string-downcase flag) (symbol-value flag)))
  (dbug :top "*debug-list* is ~s" *debug-list*)
  )

;;; -------------------------------------------------------------------------
;;; domain.lisp

(defvar *domain* nil
  "The string naming an AMP/CSM domain (prefix to -amp.lisp and -csm.lisp).
        [AMP]")

;;;-------------------------------------------------------------------------
;;; time.lisp

(defvar *secs-per-quantum* 1 "Seconds per deliberation quantum.  [AMP]")

(defvar *cur-quantum* nil "The current time quantum.  [AMP]")

(defvar *zero-time* nil "The internal-time-units zero reference.  [AMP]")

;;;-------------------------------------------------------------------------
;;; amp.lisp

(defvar *halt* nil "When non-nil, the infinite AMP loop stops.  [AMP]")

(defvar *use-timeouts* T
  "If non-nil (the default), use timeouts to limit CSM processing
time according to performance profiles.  Turn this off if you have
problems with a CSM run inside the AMP, to keep the break point available. [AMP]")

(defvar *sockets* nil)  ;; list of sockets we wait for input on...

(defvar *run-aid* nil "Should we run the AMP Information Display?  [AMP]")

(declaim (type integer *dot-heartbeat*))
(defvar *dot-heartbeat* 0 "Should we print dots for AMP heartbeat? If integer, include newline every 30 beats [AMP]")

(defvar *heartbeat-period* .75 "Time between idle heartbeats.  [AMP]")

;; MP-AMP.
(defvar *max-stars-to-wait* (* 20 60))  ;; this is approx multiples of *heartbeat-period*, at .75 approx 1 per sec, ...so, currently approx 20 min

(defvar *send-idle-msg* t
  "When non-null, non-optimus fuzzbombs send a msg to optimus when they have no tasks ready to execute.")

(defvar *ask-to-start* nil "Should we wait for user to say go?  [AMP]")

(defvar *amp-paused* nil)       ;; T at start if *ask-to-start*, reset by :sync msg from master.

(defvar *perf-prof-fallback-function* nil
  "If non-null, should name a function that takes two args (goals threats)
   and returns estimate of the time required by the CSM to create a plan.
   This function will be called when there is not an entry in the
   *perf-prof-hash* for that combination of goals and threats.")

(defvar *greedy-discount-factor* .99)

(defvar *cur-phase* nil "The current phase object")

(define-restricted-var *negotiation-protocol*
    (:bid
     )
  "Valid values:
      * :bid    - only mode avail.")

(defvar *use-stop-amp* nil "Should the AMP use a STOP-AMP task? If set, the AMP creates a STOP-AMP task after calling the *DEFAULT-MISSION-PLANNER-FN*. [AMP]")

(defvar *upload-povs* nil "Should OPTIMUS upload PoVs to Leidos analytics console? [AMP]")

;;;----------------------------------------------------------------------
;;; skill class
(defvar *use-old-phase-level-skill-code* t
  "When non-nil, we use backward-compatibility mode for skills.")

;;;-------------------------------------------------------------------------
;;; aid-interface.lisp

;; eventually should handle trying to run AID on diff host; not for now...
;;(defvar *aid-hostname* (hostname))

;; NOTE eventually if we can get init-aid to happen after the
;; default-misison-planner, we may be able to define number of slots automaticly
(defvar *aid-command* "../aid/aid -s 10")
(defvar *aid-directory* "../aid/")

(defvar *aid-meters* nil "Should we display the AID quality meters?")

;;; this holds socket to AID; it should be one of those global
;;; specials w/ a dynamic binding in run-amp, to handle multiprocessing.
(defvar *aid-sock* nil)

(defvar *expect-aid-dont-runit* nil
  "Set to T if you want to run the AID by hand (esp on a diff machine)...
        AMP will prompt you to start it at appropriate time.  [AMP]")

;;;-------------------------------------------------------------------------
;;; amp-class.lisp

(defvar *amps* nil
  "List of all amp objects. [AMP]")

(declaim (type (or null amp) *self* *master-amp*))
(defvar *master-amp* nil
  "bound to local obj rep the master. [AMP]")

(defvar *self* nil
  "pointer to this AMP's object... , ie me. [AMP]")

(declaim (type fixnum *num-amps*))
(defvar *num-amps* 0)

;;;-------------------------------------------------------------------------
;;; base.lisp

;;;  All set in init-globals now, to decouple build and run-time environments.
;;; [mboldt:20140929] In FB, these are defined in defs.lisp.
; (defvar *circa-baseport* nil
;   "Value used to compute socket ports used by this AMP and its collaborators.
;    Depends on environment variable CIRCA\\_BASEPORT [CIRCA]")
; (defvar *circa-basename* nil
;   "Value used to compute matchmaker names used by this AMP and its collaborators.
;    Depends on environment variable CIRCA\\_BASENAME [CIRCA]")
(defvar *matchmaker-host* nil
  "Name of a host for the matchmaker instance this AMP will connect to.
   Set to value of the environment variable CIRCA\\_MM\\_HOST or localhost
   if the environment variable isnt set. [CIRCA]")
(defvar *matchmaker-port* nil
  "Port used for connecting to a matchmaker. [CIRCA]")
(defvar *amp-port* nil
  "Port used for connecting to this amp [CIRCA]")
(declaim (type (or null fixnum) *bridge-port*))
(defvar *bridge-port* nil
  "Port used for connecting to the bridge [CIRCA]")

;;;-------------------------------------------------------------------------
;;; collab.lisp
#-(or allegro sbcl ccl)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (error "No known type for *MM-SOCK* for this lisp."))
;;; FIXME: figure out what sort of thing *MM-SOCK* is on CCL.
(declaim (type (or null #+(or allegro sbcl) stream #+ccl t ;ccl:socket
                                                   )
               *mm-sock*))
(defvar *mm-sock* nil)

;;;-------------------------------------------------------------------------
;;; gantt.lisp

(defvar *contract-statuses* nil
  "assoc list of cur state of each contract for agent")

(defvar *gantt* nil
  "assoc list of contracts w/ pairs of start/end coverage times")

;;;-------------------------------------------------------------------------
;;; mission.lisp

;;; Number to decrement by 2 for each new tap index.
;;; The base index is always odd.
(defvar *rts-current-index-number* -1
  "Negated odd index of next new rts index.")

;;;-------------------------------------------------------------------------
;;; perf-prof.lisp

(defvar *perf-prof-hash* nil)

;;; Initialized in init-globals based on run-time environment.
;;;(defvar *default-perf-prof-file* "circa:amp;default-perf-prof.lisp")
(defvar *default-perf-prof-file* nil)

(defvar *perf-prof-suffix* "perf-prof.lisp")

(defvar *perf-prof-prob* nil)

;;;-------------------------------------------------------------------------
;;; rts-interface.lisp

(defvar *reboot-in-progress* nil
  "T if the AMP is currently rebooting. This process starts with a :reboot msg and initializes
an RTS handoff process. The entire process ends when the old RTS acknowledges the handoff and then
sends up a :rts-handoff msg to the AMP. NOTE: (or *generate-handoff-taps* *rts-handles-handoff-requests*)
and *run-rts* must be T, and the RTS should be started via run-rts-locally.")

#|

(define-restricted-var *rts-run-mode*
    (:auto-start :manual-start :plexil-manual-start :plexil-auto-start)
  "Determines how RTS is executed. Valid values:
        * :auto-start   - forks RTS in separate process, on this machine.
        * :manual-start - prompts user to run RTS, waits for it to connect.
        * :plexil-auto-start - forks RTS in separate process, sends schedules in Plexilisp.
        * :plexil-manual-start - prompts user to run RTS, waits, sends schedules in Plexilisp.")

;; eventually should handle trying to run RTS on diff host; not for now...
;;(defvar *rts-hostname* (hostname))

;; uses old config, that sets standalone=1
;;(defvar *rts-command* "../rts/rts -nocomm -config ../rts/solo-config -mm foom -acm foom")

;; this generic version doesnt specify config filename, leaves
;; that to be auto-generated and appended at calling time, below
(defvar *rts-command* "../rts/rts -nocomm -schedpair_mode -config ")

;; AMP RTS handoff globals
(defvar *rts-port-offset* 7)
(defvar *rts-handoff-offset* 5)
(defvar *rts-handoff-sock* nil)

(defvar *always-use-same-rts-sched-index-for-phase* nil
  "If nil (default), AMP tries to pick the not-currently-running schedpair index to download an improved
        plan to the RTS. If T, AMP will just use the same schedule index for a phase over and over,
        and rely on the new-as-of-Nov2012 RTS ability to detect this and put the schedule into the
        other schedpair slot.  [AMP]")

;; for debugging RTS...
;;(defvar *rts-command* "../rts/rts -d -nocomm -config ")

;;(defvar *rts-command* "../rts/rts -nocomm -config ../rts/solo-config -mm foom -acm foom")


(defvar *rts-domain-options* nil
  "A domain-specific string of options added to the RTS function call.")

;;; if we're going to use this to hold socket to RTS, it
;;; should be one of those global specials w/ a dynamic binding
;;; in run-amp.
(defvar *rts-sock* nil)

(defvar *redirect-rts-output-to-file* t
  "If t, redirect the rts output to a log file in the /tmp directory.")

(defvar *handle-state-requests* t
  "Should the RTS respond automatically to SEND-STATE requests from AMP or
        should it require AMP to include a special TAP to do so?  [AMP]")

(defvar *handle-handoff-requests* t
  "Should the RTS respond automatically to HANDOFF requests from another RTS?  [AMP]")

(defvar *RTS-strings-for-goals* nil
  "This is the semi-hack that allows AMP to download custom strings
to RTS when it is awarded goals; this can be used to give RTS target
specification information when an agent wins a target contract.  See
5plane-oep-amp.lisp for example.")

|#
;;;-------------------------------------------------------------------------
;;; contract-class.lisp

(defvar *contract-number* 0 "Index of next new contract generated locally")

;;;-------------------------------------------------------------------------
;;; task-class.lisp

(defvar *phase-number* 0 "Index of next new phase generated locally")

;;;-------------------------------------------------------------------------
;;; gui-interface.lisp

(defvar *run-gui* nil)
(defvar *gui-sock* nil)

;;;-------------------------------------------------------------------------
;;; fuzzball-wrapper.lisp
(defvar *fuzzball-seed* (cl-variates:make-random-number-generator 42))
