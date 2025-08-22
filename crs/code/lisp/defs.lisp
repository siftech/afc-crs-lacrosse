;;; -------------------------------------------------------------------------
;;; -------------------------------------------------------------------------
;;; COPYRIGHT START
;;; Copyright (c) 2012, Smart Information Flow Technologies (SIFT). Developed with the sponsorship of the Defense Advanced Research Projects Agency (DARPA) and Air Force Research Laboratory.
;;; COPYRIGHT END
;;; -------------------------------------------------------------------------

(in-package :fuzzbomb)
(eval-when (:compile-toplevel) (optimization-boilerplate))

(defvar *fb-running-state* :stopped
  "Set to :running, :shutting-down, or :stopped as appropriate. [FUZZBOMB]")

(defun fb-running-p (&rest args)
  (declare (ignore args))
  (eq :running *fb-running-state*))

(defvar *c3po-fault-port* nil
  "This is where C3PO listens for fault notifications. Set in FB-INIT-ENV. [FUZZBUSTER]")

(defvar *circa-baseport* nil
  "This is set from the CIRCA\\_BASEPORT environment variable. See Dave to get an assigned value. [FUZZBUSTER]")

(defvar *circa-basename* nil
  "This is set from the CIRCA\\_BASENAME environment variable. Typically set to something like JRYE or DAVE. [FUZZBUSTER]")

(defvar *fb-delib-mode* :utility)

;; Where we should begin new code sections, by default.
(defvar *default-new-text-start-address* #x08047000)

(defvar *use-scanned-addresses-in-cfg* t
  "This determines whether the addresses found in the binary-- possibly func ptrs or jump tables-- should be passed to make-cfg.")

;; Uhhh no dont make tests non-reproducible.
;; tmarble: WAIT we really need something like this to uniquify run-test-cases
;; renaming from fb-seed
#+allegro
(defvar *fb-rand-gen* (cl-variates:make-random-number-generator
                       (+ (excl.osi:getpid) (get-universal-time)))
  "Random seed for use with cl-variates")

;; [tmarble:20140929.0938CST] added for additional configuration debugging
;; *optimus-prime* is a name (string)
(defvar *optimus-prime* nil)
;; *optimi* is a list of optimi
(defvar *optimi* nil)
;; List of fuzzbomb amp names.
(defvar *fuzzbombs* nil)
;; List of DVM amp names.
(defvar *dvms* nil)

(defvar *libcgc-func-table* nil "libcgc.a function table (we got the func).")

;;; -------------------------------------------------------------------------
(defun condor-cluster ()
  (or (getenv "DOIT_CLUSTER") "default"))

;;; -------------------------------------------------------------------------
(defun condor-submit-node ()
  (or (getenv "DOIT_SUBMIT") "default"))

;;(defun experiment-path ()
;;  (cond ((or (getenv "DOIT_CLUSTER") (getenv "DOIT_SUBMIT"))
;;         (format nil "/scratch/cgc-data/~A/~A/~A/" (getenv "CIRCA_BASENAME") (condor-submit-node) (condor-cluster)))
;;        (t
;;         (format nil "/scratch/cgc-data/~A/~A/~A-~A" (getenv "CIRCA_BASENAME") "localhost" (or *circa-baseport* 0) (get-universal-time)))))

(defun experiment-path ()
  ;;(format nil "~A/../../scratch/~A" (directory-namestring cl-user::*fb-code-directory*) (get-universal-time)))
;; for AIxCC we *must* use the crs_scratch dir at the crs-sandbox level, too many things depend on it
  ;; (most notably, it gets mounted into dind for CP run.sh to work... nothing else will do!
  ;;(format nil "~a/../../../~a/~a/" (directory-namestring cl-user::*fb-code-directory*) *crs-scratch* (get-universal-time))
  ;;(format nil "~a/../../../~a/~a/" (directory-namestring cl-user::*fb-code-directory*) (getenv "AIXCC_CRS_SCRATCH_SPACE") (get-universal-time))
  ;;(format nil "~a/../../../crs_scratch/~a/" (directory-namestring cl-user::*fb-code-directory*) (get-universal-time))
  (let ((timestamp
          (cond
                ((file-exists-p (format nil "~a/timestamp" (getenv "AIXCC_CRS_SCRATCH_SPACE")))
                 (trim-whitespace
                  (uiop:run-program (format nil "cat ~a/timestamp" (getenv "AIXCC_CRS_SCRATCH_SPACE"))
                                    :output :string)))
                ((getenv "LOGDIR")
                 ;; match year-date-month-hour-min-nanosecs in prt logdir
                 (cl-ppcre:scan-to-strings "\\d\\d\\d\\d-\\d\\d-\\d\\d-\\d\\d\\d\\d\\d\\d" (getenv "LOGDIR")))
                (t (get-universal-time)))))
    ;;(format nil "~a/../../../~a/~a/" (directory-namestring cl-user::*fb-code-directory*) (getenv "AIXCC_CRS_SCRATCH_SPACE") timestamp)
    ;; Set scratch directly from env var.
    (format nil "~a/~a/" (getenv "AIXCC_CRS_SCRATCH_SPACE") timestamp)
    ))

(defvar *experiment-dir* (experiment-path)
  "The top-level directory for the currently running experiment in the shared file system.")

(defun name-optimus-prime-p (&key (name nil))
  (when (and common-lisp-user::*fb-instance* *optimus-prime*)
    (let ((amp-name (or name common-lisp-user::*fb-instance*)))
      (not (null (string= amp-name *optimus-prime*))))))

(defun name-optimus-p (&key (name nil))
  (when (and common-lisp-user::*fb-instance* *optimi*)
    (let ((amp-name (or name common-lisp-user::*fb-instance*)))
      (not (null (find amp-name *optimi* :test #'string=))))))

(defun name-fuzzbomb-p (&key (name nil))
  (when (and common-lisp-user::*fb-instance* *fuzzbombs*)
    (let ((amp-name (or name common-lisp-user::*fb-instance*)))
      (not (null (find amp-name *fuzzbombs* :test #'string=))))))

(defvar *run-brute* nil)

(defvar *score-all-revs* nil)

(defvar *perform-repair-rewrites* t)

(defvar *perform-defensive-rewrites* nil)

(defvar *max-defensive-rewrites-per-cbs* 3)

(defvar *perform-compound-repairs* t)

(defvar *perform-static-analysis* t)

(defvar *defensive-rewrite-task* nil)
(defvar *compound-repair-task* nil)

(defvar *active-repair-tasks* nil)

(defparameter *resource-monitor-thread* nil
  "A thread that periodically logs snapshots of resource usage. [FUZZBUSTER]")

(defparameter *snapshot-interval* 60
  "A snapshot of resource usage is taken every *snapshot-interval* seconds while fuzzbuster is running. [FUZZBUSTER]")

(defparameter *fb-initial-threads* nil
  "This is set during startup to a list of names of all threads, and then used during shutdown to detect any leaked threads. [FUZZBUSTER]")

(defparameter *fb-allowed-threads* (list "Domain Name Server Client"
                                         "Scheduled-Function Scheduler")
  "This contains a list of names for threads that are allowed to be left running when Fuzzbuster shuts down. [FUZZBUSTER]")

(define-restricted-var *patch-verification-mode* (:strict :relaxed)
  "Valid values:
      * :strict - Patches must preserve the behavior of all non-faulting test cases
                  and fix all faulting test cases of its associated vuln target.
      * :relaxed - Assess each patch based on the number of faulting test cases it fixes
                   and the number of non-faulting test cases whose behavior it preserves.
                   Patches that do not fix all faults and preserve all behaviors are allowed,
                   but preferences is given to greater fixes and greater preservation.")

#-sbcl                                  ; can't type declare generic functions in sbcl.
(declaim (ftype (function (t) (values (or null pathname) &optional))
                some-patch))
(defgeneric some-patch (target)
  (:documentation "Does the TARGET already have a patch? If so, return the patch's pathname,
else NIL."))

(defun fb-init-env ()
  (let ((baseport-string (getenv "CIRCA_BASEPORT"))
        (basename-string (getenv "CIRCA_BASENAME")))
    (unless (and baseport-string basename-string)
      (error "You must set the CIRCA_BASEPORT and CIRCA_BASENAME environment variables!"))
    (setf *circa-baseport* (read-from-string baseport-string))
    (setf *circa-basename* basename-string)
    ;;(setf *exgen-fault-port* (+ *circa-baseport* 556))
    ;;(setf *c3po-fault-port* (+ *circa-baseport* 557))
    ;;(setf *c3po-test-port* (+ *circa-baseport* 558))
    ;;(setf *test-controller-port* (+ *circa-baseport* 560))
    (when (or (not *circa-baseport*) (not *circa-basename*))
      (error "Problem with CIRCA_BASEPORT and CIRCA_BASENAME environment variables"))
    )

  ;;(setf *fb-initial-threads* (all-thread-names))
  (setf *patch-verification-mode* :strict)

  ;;   Make sure that we use a consistent
  ;; locale. Running under Aquamacs, I got #<locale "C" [:LATIN1-BASE]
  ;; @ #x201ea93a>, but running the unit tests in a shell I got
  ;; #<locale "en_US" [:UTF8-BASE]>.
  #+allegro
  (setf excl:*locale* (excl:find-locale "C"))
  )
