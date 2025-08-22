;;; -------------------------------------------------------------------------
;;;: os-utils.lisp 3274 2014-03-13 21:12:08Z mboldt $
;;; -------------------------------------------------------------------------
;;; COPYRIGHT START
;;; Copyright (c) 2012, Smart Information Flow Technologies (SIFT). Developed with the sponsorship of the Defense Advanced Research Projects Agency (DARPA) and Air Force Research Laboratory.  
;;; -------------------------------------------------------------------------

#+allegro
(let ((comp:*cltl1-compile-file-toplevel-compatibility-p* t))
  (require :asdf))
#-allegro (require :asdf)
(in-package :fuzzbomb)
(eval-when (:compile-toplevel) (optimization-boilerplate))

(debug-list-value :os-utils
  "Main debugging variable for os interface operations")

(pushnew :os-utils *debug-list*)

(debug-list-value :os-utils-deep
                  "Deep debugging variable for os interface operations")

;;; ------------------------------------------------------------
(eval-when (:compile-toplevel :load-toplevel :execute)
  #+cmu
  (progn
    (load "clocc:src;port;port.system")
    (mk:operate-on-system 'port :load)
    )

  (import
;;   #+cmu 'port:getenv
;;   #+sbcl 'sb-posix:getenv
   #+asdf3 'uiop:getenv
   ;;#-(or cmu sbcl asdf3)
   #-asdf3
   (error "Need to find a GETENV to import into CL-USER for this lisp.")
   ;; :common-lisp-user)
   :fuzzbomb)
  )

(deftype only-values (&rest value-types)
  "Signals that the called function will ONLY return the
set of value types specified -- there are no additional values."
  `(values ,@value-types &optional))

(deftype only-value (&rest value-type)
  "Signals that the called function will ONLY return a single
value of the specified value type."
  `(only-values ,value-type))


;;; ------------------------------------------------------------
(defun signal-process (p s)
 #+ccl (ccl:signal-external-process p s)
 #+allegro (excl.osi:kill p s)
  #+sbcl (sb-ext:process-kill p s)
  #-(or ccl allegro sbcl)
   (error "Need to find a signal-process to define for :fuzzbomb for this Lisp.")
)

(defvar *sigterm*
 #+ccl 15
 #+allegro excl.osi:*sigterm*
 #+sbcl sb-unix:sigterm
 #-(or ccl allegro sbcl)
   (error "Need to find a *sigterm* to define for :fuzzbomb for this Lisp.")
)

(defvar *sigkill*
 #+ccl 9
 #+allegro excl.osi:*sigkill*
 #+sbcl sb-unix:sigkill
 #-(or ccl allegro sbcl)
   (error "Need to find a *sigkill* to define for :fuzzbomb for this Lisp.")
)

;;; ------------------------------------------------------------
;;; grabbed from https://stackoverflow.com/questions/24765355/handling-concurrent-file-access-in-common-lisp

(defun lock-file-once (f)
  "try to lock file once"
  (open f :direction :probe :if-exists nil))

(defun lock-file (f)
  "block until the file is locked"
  (loop :until (lock-file-once f)
    :do (sleep 1)))

(defun unlock-file (f)
  "remove the lock"
  (delete-file f))

(defmacro with-file-lock (f &body body)
  "lock the file, run body, unlock it"
  `(unwind-protect
         (progn (lock-file ,f) ,@body)
         (unlock-file ,f)))

;;; ------------------------------------------------------------
;;; fixnump
;;;
;;; [sfriedman:20140511.1408CST] Taken from GBB.

;;; CLs that don't have short-float-p predicates:
#+(or abcl allegro ecl gcl sbcl)
(defun short-float-p (obj)
  (typep obj 'short-float))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (import
   #+abcl
   '(ext:fixnump)
   #+allegro
   '(excl:fixnump excl:single-float-p excl:double-float-p)
   #+clisp
   '(sys::fixnump sys::short-float-p sys::single-float-p sys::double-float-p
     sys::long-float-p)
   #+clozure
   '(ccl:fixnump ccl::short-float-p ccl::double-float-p)
   #+cmu
   '(extensions:fixnump lisp::short-float-p kernel:single-float-p
     kernel:double-float-p kernel:long-float-p)
   #+cormanlisp
   '(lisp::fixnump lisp::short-float-p lisp::single-float-p
     lisp::double-float-p lisp::long-float-p)
   #+digitool-mcl
   '(ccl:fixnump ccl::double-float-p)
   #+ecl
   '(si:fixnump)
   #+gcl
   '(system:fixnump)
   #+lispworks
   '(lispworks:fixnump lispworks:short-float-p hcl:single-float-p
     hcl:double-float-p lispworks:long-float-p)
   #+sbcl
   '(sb-int:fixnump short-float-p sb-int:single-float-p
     sb-int:double-float-p sb-int:long-float-p)
   #+scl
   '(ext:fixnump lisp::short-float-p kernel:single-float-p
     kernel:double-float-p kernel:long-float-p)
))

#+allegro
(defmacro run-shell-command (&body body)
  `(excl:run-shell-command ,@body))


;;; ------------------------------------------------------------
;;; External Programs
;;; After much consternation, I've set it up so that os-utils uses (mostly) UIOP package crap when in CCL,
;;; but NOT when in ACL, b/c the old ACL on cgc-submit doesnt have a new enuf UIOP and besides, the old ACL stuff just worked for ages.
;;; At some future point when all ACLs have UIOP launch-program and other stuff, we could nuke all the #+allegro stuff probably.

#-allegro
(#+sbcl locally #+sbcl (declare (sb-ext:muffle-conditions style-warning))
 #-sbcl progn
 (defmacro with-external-program-stream ((stream cmd &optional (args nil) &key (error-output :output)) &body body)
   "Runs the CMD with ARGS with STREAM bound to a bidirectional stream for the input and output of the external program. stderr is redirected to the output stream. Ensures that STREAM is always closed properly."
   `(let (pinfo
          ,stream
          exit-code
          )
      (dbug :os-utils "with-external-program-stream cmd and args: ~s ~s~%" ,cmd ,args)
      (unwind-protect
           (handler-case (progn
                           (multiple-value-setq (pinfo ,stream)
                             (invoke-external-program ,cmd ,args :error-output ,error-output))
                           ,@body
                           (dbug :top "with-external-program-stream body ended"))
             (t (c) (dbug :top "got an error ~A" c)))

        ;; Start cleanup clauses.
        (dbug :os-utils "Unwinding in with-external-program-stream for ~A in thread '~A'"
              ,cmd (thread-name (current-thread)))

        (when (uiop:process-alive-p pinfo)
          (uiop:terminate-process pinfo :urgent t))
        (setf exit-code (uiop:wait-process pinfo))
        (dbug :os-utils "After the unwind in with-external-program-stream for ~A in thread '~A'" ,cmd (thread-name (current-thread)))
        exit-code))))

;;; ------------------------------------------------------------

;;; ------------------------------------------------------------
;;; here's the older ACL version of above, much more complicated, could be trimmed down no doubt
;;; ------------------------------------------------------------
#+allegro
(defmacro with-external-program-stream ((stream cmd &optional (args nil) &key (error-output :output)) &body body)
  "Runs the CMD with STREAM bound to a bidirectional stream for the input and output of the external program. stderr is redirected to the output stream. Ensures that STREAM is always closed properly."

  (with-gensyms (process exit-code start-time)
    (declare (ignorable start-time))
    `(let (,process
           ,stream
           (,exit-code nil))
       (dbug :os-utils "with-external-program-stream cmd and args: ~s ~s~%" ,cmd ,args)
       (unwind-protect
            (progn
              (multiple-value-setq (,process ,stream)
                (invoke-external-program ,cmd ,args :error-output ,error-output))
              ,@body
              (dbug :os-utils "body of with-external-program-stream ended, about to sigterm and reap")
              ;;(excl.osi:kill ,process excl.osi:*sigterm*)
              (unless (setf ,exit-code (sys:reap-os-subprocess :pid ,process :wait nil))
                (signal-process ,process *sigterm*)
                (setf ,exit-code (sys:reap-os-subprocess :pid ,process :wait nil)))
              (dbug :os-utils "after sigterm and reap, exit-code for ~S is ~S" ,process ,exit-code)
              )

         ;; Start cleanup clauses.
         (dbug :os-utils "Unwinding in with-external-program-stream for ~A in thread '~A'"
               ,cmd (thread-name (current-thread)))

         (when ,process
           ;; This osi:kill is the only way we can clean up an external process,
           ;; which we must do if we want to avoid leaving behind processes
           ;; (for example, still-running fuzz-wrapper tasks) after FB is shut down.
           ;;
           ;; First try to sigterm. Wait for a little bit if
           ;; necessary. If the child doesn't go away, sigkill.

           (when ,stream
             (close (two-way-stream-output-stream ,stream)))

           ;;(excl.osi:kill ,process excl.osi:*sigterm*)
           (unless (or ,exit-code (setf ,exit-code (sys:reap-os-subprocess :pid ,process :wait nil)))
             (dbug :top "Child ~S didn't exit after sigterm, now killing and waiting" ,process)
             (signal-process ,process *sigkill*)
             (setf ,exit-code (sys:reap-os-subprocess :pid ,process :wait t)))

           (when ,stream
             (dbug :os-utils-deep "Leftover output from child process:")
             (musliner:while (excl:read-no-hang-p ,stream)
               (dbug :os-utils-deep "  ~A" (read-line ,stream nil :eof)))
             (close (two-way-stream-input-stream ,stream))
             )
           )
         ;;(mp::process-wait "for run-shell-command to finish"
         ;;   #'(lambda ()
         ;;     (setq ,exit-code
         ;;       (or ,exit-code (sys:reap-os-subprocess :pid ,process :wait nil)))))
         ;; End of flip-flopping code...
         (dbug :os-utils "After the unwind in with-external-program-stream for ~A in thread '~A'"
               ,cmd (thread-name (current-thread)))
         ,exit-code))))

(setf (documentation 'invoke-external-program 'function)
      "Takes a command (CMD) and arguments (ARGS) as arguments and returns 2 values:
a process-id for the running process and a two-way stream for process input and output.
ERROR-OUTPUT, keyword argument, if :OUTPUT (the default), will send the error output
to the output stream. Any other value will cause the output to be lost (and is likely to
cause the function to error out).")

#-allegro
(declaim (ftype (function (string list &key (:error-output t))
                          (only-values uiop/launch-program::process-info
                                       two-way-stream))
                invoke-external-program))

;; this takes args for historic reasons; just strcats onto cmd
;; returns pinfo instead of old pid from before, but other stuff should be mapped to uiop to just handle it

;;; ------------------------------------------------------------
#+allegro
(defun invoke-external-program (cmd args &key (error-output :output))
      (multiple-value-bind (is os es process)
          (excl:run-shell-command (if args (apply #'vector cmd cmd args) cmd)
                                  :input :stream
                                  :output :stream
                                  :error-output error-output
                                  :separate-streams t
                                  :wait nil)
        (declare (ignore es))
        (let ((stream (make-two-way-stream os is)))
          (values process stream))))

#-allegro
(defun invoke-external-program (cmd args &key (error-output :output))
  (let ((process-info
          (uiop:launch-program (concatenate 'string cmd args) ; not sure why we use a shell, but that is what we do
                               :input :stream
                               :output :stream
                               :error-output error-output)))
    (declare (type uiop/launch-program::process-info process-info))
    (let ((stream (make-two-way-stream (uiop:process-info-output process-info)
                                       (uiop:process-info-input process-info))))
      (values process-info stream))))

#|  test zone for CCL & uiop stuff
                                        ; ;
(ccl:run-program "ls" nil :output t)    ;; second (arg) argument is not optional and must be list (of strings) or nil ; ;
(ccl:run-program "ls" (list "/neo-fuzz") :output t)     ;; goes to stdout ; ;
                                        ; ;
(ccl:run-program "docker" (list "run --rm -t ubuntu:16.04 ls") :output t)       ;;; fails ; ;
                                        ; ;
(ccl:run-program "docker run --rm -t ubuntu:16.04 ls" nil :output t)    ;;; fails ; ;
                                        ; ;
(ccl:run-program "docker" (list "ps" "-a") :output t)   ;;; works ; ;
                                        ; ;
(ccl:run-program "docker" (list "run" "--rm" "-t" "ubuntu:16.04" "ls") :output t)       ;;; works in ccl ; ;
                                        ; ;
(excl:run-shell-command "docker run --rm -t ubuntu:16.04 ls" :output *standard-output*) ;; works in acl ; ;
                                        ; ;
(uiop:run-program "docker run --rm -t ubuntu:16.04 ls" :output *standard-output*)       ;; works in acl and ccl ; ;
                                        ; ;
(ccl:run-program "echo" (list "foo") :output t) ;;; works in ccl ; ;
                                        ; ;
(excl:run-shell-command "echo 'foo'" :output *standard-output*) ;; works in acl but i ccl it prints the single quotes ; ;
? (excl:run-shell-command "echo 'foo'" :output *standard-output*) ; ;
'foo'                                   ; ;
0                                       ; ;
                                        ; ;
(excl:run-shell-command "echo foo" :output t) ;; works in acl, no single quotes printed ; ;
                                        ; ;
(excl:run-shell-command "echo 'foo bar'" :output *standard-output*)     ;; works in acl, no single quotes printed ; ;
                                        ; ;
(ccl:run-program "echo" (list "foo bar") :output t)     ;;; works in ccl ; ;
(ccl:run-program "echo" (list "'foo bar'") :output t)   ;;; prints the single quotes in ccl ; ;
                                        ; ;
;;(excl:run-shell-command #("ls" "ls" "-l"))    ;; fail in ccl - the split /vector command stuff is not set up right in excl-ccl ; ;
                                        ; ;
(excl:run-shell-command "docker exec musliner-driller pwd" ; ;
:input :stream                          ; ;
:output :stream                         ; ;
:error-output :output                   ; ;
:separate-streams t                     ; ;
:wait nil)                              ; ;
                                        ; ;
(invoke-external-program "docker exec musliner-driller ls" nil) ; ;
                                        ; ;
# NOTE the vector thing below seems borked...wont concat strings ...?  hence nil second arg above ; ;
# above gives streams, but we dont know what it is saying.. ; ;
                                        ; ;
(with-external-program-stream (io-stream "docker exec musliner-driller pwd" nil) ; ;
(dolines (line io-stream) (dbug :top "got line [~A]" line))) ; ;
                                        ; ;
# above fails in ccl but works in acl   ; ;
# ;; [3/23/2018 3:38:27] TOP: got line [docker: 'exec musliner-driller pwd' is not a docker command.] ; ;
                                        ; ;
(with-external-program-stream (io-stream "docker" (list "exec" "musliner-driller" "pwd")) ; ;
(dolines (line io-stream) (dbug :top "got line [~A]" line))) ; ;
# above works in acl                    ; ;
# in ccl above fails w/ just a string of args b/c the suspect vector thing below is borked ; ;
                                        ; ;
|#


;;;; give cmd as string
;;;; returns PID
;;;; you MUST call (reap-background-cmd pid)
;;(defun start-background-cmd (cmd stdout-path)
;;  (let ((pid
;;         (nth-value 2 (excl:run-shell-command cmd
;;                                              :input "/dev/null"
;;                                              :output stdout-path
;;                                              :error-output :output
;;                                              :separate-streams nil
;;                                              :wait nil))))
;;    (dbug :os-utils "run-background-cmd ~A > ~A [~A]" cmd stdout-path pid)
;;    pid))
;;
;;;; must reap sub-process
;;;; http://www.franz.com/support/documentation/current/doc/operators/system/reap-os-subprocess.htm
;;;; http://www.franz.com/support/documentation/8.1/doc/osi-constants.htm
;;;; Use *sigterm* intead of *sigkill* so the process can clean up
;;#+allegro
;;(defun stop-background-cmd (pid)
;;  (excl.osi:kill pid 15)
;;  (multiple-value-bind (exit rpid signal)
;;      (sys:reap-os-subprocess :pid pid :wait t)
;;    (dbug :os-utils "stop-background-cmd pid ~A exited ~A with signal ~A" rpid exit signal)
;;    exit))
;;
;;(defun read-pidfile (pidfile)
;;  (let* ((pidstring (if pidfile (slurp-text-file pidfile)))
;;         (pid (if pidstring (parse-integer pidstring :junk-allowed t))))
;;    pid))
;;
;;(defun signal-via-pidfile (pidfile signal)
;;  (dbug :os-utils "signal-via-pidfile ~A with ~A" pidfile signal)
;;  (let* ((pid (read-pidfile pidfile)))
;;    (if (and pid (> pid 0))
;;        (progn
;;          (dbug :os-utils "signal-via-pidfile kill -~A ~A" signal pid)
;;          (signal-process pid signal)))))
;;
;;; ------------------------------------------------------------
;;; Streams

(defmacro dolines ((var stream) &body body)
  "Execute BODY with VAR bound to each line from the STREAM."
  `(map-lines #'(lambda (,var) ,@body) ,stream))

(defun map-lines (fn stream)
  "Call FN on each line from STREAM."
  ;; This looping mechanism was taken from:
  ;; http://stackoverflow.com/questions/1310783/how-to-read-input-until-eof-in-lisp
  (loop for line = (read-line stream nil :eof)
      until (eq line :eof)
      do (funcall fn line)))

(defmacro dochars ((var stream) &body body)
  "Execute BODY with VAR bound to each char from the STREAM."
  `(map-chars #'(lambda (,var) ,@body) , stream))

(defun map-chars (fn stream)
  "Call FN on each char from STREAM."
  (loop for char = (read-char stream nil :eof)
      until (eq char :eof)
      do (funcall fn char)))

;;; ----------------------------------------------------------
;;; OS helper routines
;;; this seems to be dead code, and SBCL doesn't like it: thinks that the IF expression
;;; is unreachable, which gives me a very icky feeling.  
#+nil
(defun which-os-app-path (name-string)
  "use the which command to get the path for the application on this host."
  (let ((result nil)
        (fstr (make-array '(0) :element-type 'base-char :fill-pointer 0 :adjustable t)))
    (with-output-to-string (stream fstr)
      (setf result (with-external-program-stream (io-stream "which" (list name-string))
                     (dolines (rline io-stream)
                              (format stream "~A" rline)))))
    (if (equal result 0)
        fstr
        nil)))


;;; ------------------------------------------------------------
;;; Temporary Filename

(defun make-timestamped-filename (basename)
  (declare (special *circa-baseport*))
  (format nil "~A-~A-~A"
          basename
          (or *circa-baseport* 0)
          (get-universal-time)))

(defmacro with-temp-filename ((var basename &key (delete-on-exit t)) &body body)
  "Execute BODY with VAR bound to a unique filename around the BASENAME. The file is generally created in the /tmp directory and includes both *CIRCA-BASENAME* and *CIRCA-BASEPORT*. This uses PROBE-FILE to ensure that the file is unique. The file is created (but not opened) so that subsequent calls will not return a duplicate filename.  When delete-on-exit is t (the default) the temp file is deleted when the form exits."
  (with-gensyms (temp-filename count)
    `(let ((,var (do (,temp-filename (,count 0 (incf ,count)))
                ((and (not (null ,temp-filename))
                      (not (probe-file ,temp-filename))) ,temp-filename)
              (setf ,temp-filename
                (format nil "/tmp/fuzzbomb-~A/~A-~A.txt"
                        *circa-basename*
                        (make-timestamped-filename ,basename)
                        ,count))
              (ensure-directories-exist ,temp-filename))))
       (unwind-protect
           (progn
             ;; Create but do not keep the file open.
             (with-open-file (stream ,var :direction :output)
                    (declare (ignore stream)))  ;; to quiet a ccl warning
             ,@body)
         (when ,delete-on-exit
           (delete-file ,var))))))

(defun temp-filename (basename extension directory)
  "Return a unique filename around BASENAME in DIRECTORY. Useful for creating a (unique) file in a directory made by WITH-TEMP-DIR."
  (do* ((count 0 (1+ count))
        (filename (format nil "~A/~A-~A.~A" directory basename count extension) (format nil "~A/~A-~A.~A" directory basename count extension)))
      ((not (probe-file filename)) filename)))

;;; ------------------------------------------------------------
;;; Temporary Directory

(defvar *uid* 0)  ;; this will incr so that this same agent cannot try to make same dir

(defmacro with-temp-dir ((var basename &key (delete-on-exit t)) &body body)
  "Execute BODY with VAR bound to a unique directory around the BASENAME. The directory is generally created in the /tmp directory and includes both *CIRCA-BASENAME* and *CIRCA-BASEPORT*. This uses PROBE-FILE to ensure that the directory name is unique.
When delete-on-exit is t (the default) temp-dir and its contents are deleted when the form exits."
  (declare (special *self*))
  (with-gensyms (hidden-filename temp-filename count)
    `(let* ((,hidden-filename
             (do (,temp-filename
                  (,count 0 (incf ,count)))
                 ((and (not (null ,temp-filename))
                       (not (probe-file ,temp-filename))) ,temp-filename)
               (setf ,temp-filename
                 (format nil "~A~A-~A-~A-~A-~A/.hidden"
                         (if (svc *self*)
                             (dir (svc *self*))
                           *experiment-dir*)
                         ,basename
                         (get-universal-time)
                         (shortname *self*)
                         (incf *uid*)
                         ,count))
               (ensure-directories-exist ,temp-filename)))
            (,var (directory-namestring ,hidden-filename)))
       (unwind-protect
           (progn
             ,@body)
         (when ,delete-on-exit
           #+uiop3 (uiop:delete-directory-tree ,var)
                ;;(excl:delete-directory-and-files ,var)
           )
         ))))


;;; -----------------------------------------------------------------------------
;;; Thread convenience functions
;;;
;;; Thread-related functions, included here for lack of a more relevant location

(defun current-thread-name ()
  "Convenience function to obtain the name of the current thread for display in log messages"
  (thread-name (current-thread)))

(defun all-thread-names ()
  "Convenience function to retrieve a list of the names of all thread running at a point in time"
  (mapcar #'(lambda (thread) (thread-name thread)) (all-threads)))


;;; -----------------------------------------------------------------------------
;;; octet manipulation functions

(deftype octet () '(unsigned-byte 8))

(defun octetp (x)
  (typep x 'octet))

(deftype octet-array () '(array octet))

(defun octet-arrayp (x)
  (typep x 'octet-array))

(defun make-octet-array ()
  (make-array '(0)
              :element-type 'octet
              :fill-pointer t
              :adjustable t))

(defun octet-array-extend (new-element octet-array &optional extension)
  (if (not (octetp new-element))
      (error "octet-array-extend can only add elements of type octet, not: ~A" new-element)
    (vector-push-extend new-element octet-array extension)))

;; compare a binary byte arrays (for equality, by default)
(defun vector-compare (vec1 vec2 &key (cmp #'eql))
  (and (= (length vec1) (length vec2))
       (every cmp vec1 vec2)))

;; return a character or integer as a character
(defun as-char (ch)
  (if (characterp ch)
      ch
    (if (and (fixnump ch) (>= ch 0) (<= ch 255))
        (code-char ch)
      (error "as-char cannot convert: ~A" ch))))

;; return an array as a string
(defmethod as-string ((s string))
  s)

(defmethod as-string ((l list))
  (if l
      (map 'string #'as-char l)
    "")) ;; return the empty string for NIL

(defmethod as-string ((seq sequence))
  (if seq
      (coerce (loop for ch across seq collect (as-char ch)) 'string)
    "")) ;; return the empty string for NIL

(defmethod as-string ((ch character))
  (string ch))

(defmethod as-string ((n fixnum))
  (string (as-char n)))

(defmethod as-string ((x t))
  (error "as-string: do not know how to convert: ~A = ~A" (type-of x) x))

(defun as-octet (ch)
  (if (characterp ch)
      (char-code ch)
    (if (and (fixnump ch) (>= ch 0) (<= ch 255))
        ch
      (error "as-octet cannot convert: ~A" ch))))

(defmethod as-octet-array ((s string))
  (let ((octet-array (make-octet-array)))
    (loop for ch across s do
          (octet-array-extend (as-octet ch) octet-array))
    octet-array))

(defmethod as-octet-array ((l list))
  (let ((octet-array (make-octet-array)))
    (loop for ch in l do
          (octet-array-extend (as-octet ch) octet-array))
    octet-array))

(defmethod as-octet-array ((a array))
  (if (octet-arrayp a)
      a
    (let ((data (make-octet-array)))
      (loop for ch across a do
            (octet-array-extend (as-octet ch) data))
              data)))

(defmethod as-octet-array ((x t))
  (error "as-octet-array: do not know how to convert: ~A" x))

;; return hex representation of ch like \xFF
(defun hex-char (ch)
  (format nil "\\x~2,'0X" ch))

(defun printable-char (ch)
  (let ((c (if (characterp ch) (char-code ch) ch)))
    (cond ((= c 8) "\\b")
          ((= c 9) "\\t")
          ((= c 10) "\\n")
          ((= c 12) "\\f")
          ((= c 13) "\\r")
          ;; ((= c 34) "\\\"")
          ((= c 36) "\\$") ;; escape PERL variable chars
          ((= c 37) "\\%") ;; escape PERL variable chars
          ;; ((= c 39) "\\'")
          ((= c 64) "\\@") ;; escape PERL variable chars
          ;; ((= c 92) "\\\\")
          ;; ((= c 123) "\\{") ;; escape PERL variable chars
          ((and (>= c 32) (<= c 126))
           (string (code-char c)))
          (t
           (hex-char c)))))

(defun printable-char-nohex (ch)
  (let ((c (if (characterp ch) (char-code ch) ch)))
    (cond ((= c 8) "\\b")
          ((= c 9) "\\t")
          ((= c 10) "\\n")
          ((= c 12) "\\f")
          ((= c 13) "\\r")
          ;; ((= c 34) "\\\"")
          ((= c 36) "\\$") ;; escape PERL variable chars
          ((= c 37) "\\%") ;; escape PERL variable chars
          ;; ((= c 39) "\\'")
          ((= c 64) "\\@") ;; escape PERL variable chars
          ;; ((= c 92) "\\\\")
          ;; ((= c 123) "\\{") ;; escape PERL variable chars
          ((and (>= c 32) (<= c 126))
           (string (code-char c)))
          (t
           ch))))

;; convert a binary array of data into a string of printable characters
(defmethod printable-string ((seq sequence)) ;; matches string, array
  (if seq
      (apply 'concatenate (cons 'string (map 'list 'printable-char seq)))
    "")) ;; return empty string for nil

(defmethod printable-string-nohex ((seq sequence)) ;; matches string, array
  (if seq
      (apply 'concatenate (cons 'string (map 'list 'printable-string-nohex seq)))
    ""))

(defmethod printable-string-nohex ((ch character)) ;; matches string, array
  (string (printable-char-nohex ch)))


(defmethod printable-string ((ch character))
  (string (printable-char ch)))

(defmethod printable-string ((n fixnum))
  (string (printable-char n)))

(defmethod printable-string ((x t))
  (error "printable-string: do not know how to convert: ~A = ~A" (type-of x) x))

;; convert EVERY character to hex (without \x)
(defun hex-format-char (ch)
  (let ((c (if (characterp ch) (char-code ch) ch)))
    (format nil "~2,'0X" c)))

;; convert a binary array of data into a string of hex-format characters
(defmethod hex-format-string ((seq sequence)) ;; matches string, array
  (if seq
      (apply 'concatenate (cons 'string (map 'list 'hex-format-char seq)))
    "")) ;; return empty string for nil

(defmethod hex-format-string ((ch character))
  (string (hex-format-char ch)))

(defmethod hex-format-string ((n fixnum))
  (string (hex-format-char n)))

(defmethod hex-format-string ((x t))
  (error "hex-format-string: do not know how to convert: ~A = ~A" (type-of x) x))

;; like printable-char BUT also hex-escapes ' < > &
(defun xml-format-char (ch)
  (let ((c (if (characterp ch) (char-code ch) ch)))
    (cond ((= c 9) "\\t")
          ((= c 10) "\\n")
          ((= c 13) "\\r")
          ((or (= c 34) (= c 38) (= c 39) (= c 60) (= c 62)) ;; hex code html entitities
           (hex-char c))
          ((and (>= c 32) (<= c 126))
           (string (code-char c)))
          (t
           (hex-char c)))))

;; convert a binary array of data into a string of xml-format characters
(defmethod xml-format-string ((seq sequence)) ;; matches string, array
  (if seq
      (apply 'concatenate (cons 'string (map 'list 'xml-format-char seq)))
    "")) ;; return empty string for nil

(defmethod xml-format-string ((ch character))
  (string (xml-format-char ch)))

(defmethod xml-format-string ((n fixnum))
  (string (xml-format-char n)))

(defmethod xml-format-string ((x t))
  (error "xml-format-string: do not know how to convert: ~A = ~A" (type-of x) x))

(defvar *regex-metachars* "{}[]()^$.|*+?\\\"")

(defun regex-metachar-p (ch)
  (let ((c (if (characterp ch) (char-code ch) ch)))
    (find c (as-octet-array *regex-metachars*) :test 'eql)))

;; works like printable-char AND escapes perl regexp metchars
;; http://perldoc.perl.org/perlretut.html
;; [tmarble:20121019.1051CST] Instead of turning regex chars into hex \x00 codes,
;; just backslash escape them (the result is easier to debug)
(defun regex-printable-char (ch)
  (let ((c (if (characterp ch) (char-code ch) ch)))
    (cond ((regex-metachar-p c)
           (format nil "\\~A" (code-char c))) ;; backslash escape regex
          (t
           (printable-char c))))) ;; Defer to printable char

;; convert a binary array of data into a string of printable characters with regex metachars escaped
(defmethod regex-printable-string ((seq sequence)) ;; matches string, array
  (if seq
      (apply 'concatenate (cons 'string (map 'list 'regex-printable-char seq)))
    "")) ;; return empty string for nil

(defmethod regex-printable-string ((ch character))
  (string (regex-printable-char ch)))

(defmethod regex-printable-string ((n fixnum))
  (string (regex-printable-char n)))

(defmethod regex-printable-string ((x t))
  (error "regex-printable-string: do not know how to convert: ~A = ~A" (type-of x) x))

(defun regex-escape-char (ch)
  (let ((c (if (characterp ch) (char-code ch) ch)))
    (cond ((regex-metachar-p c)
           (format nil "\\~A" (code-char c))) ;; backslash escape regex
          (t
           (string (code-char c)))))) ;; other character

;; convert a binary array of data into a string of printable characters with regex metachars escaped
(defmethod regex-escape-string ((seq sequence)) ;; matches string, array
  (if seq
      (apply 'concatenate (cons 'string (map 'list 'regex-escape-char seq)))
    "")) ;; return empty string for nil

(defmethod regex-escape-string ((ch character))
  (string (regex-escape-char ch)))

(defmethod regex-escape-string ((n fixnum))
  (string (regex-escape-char n)))

(defmethod regex-escape-string ((x t))
  (error "regex-escape-string: do not know how to convert: ~A = ~A" (type-of x) x))

(defun backslash-escape-char (ch)
  (let ((c (if (characterp ch) (char-code ch) ch)))
    (if (or (eql c 34) (eql c 92)) ;; " \
    ;; (if (eql c 92) ;; " \
        (format nil "\\~A" (code-char c))
      (string (code-char c)))))

;; backslash-escape-string will only alter a string by adding an additional
;; backslash for \ or ".  This is useful if the output will be interpreted by perl.
(defmethod backslash-escape-string ((seq sequence)) ;; matches string, array
  (if seq
      (apply 'concatenate (cons 'string (map 'list 'backslash-escape-char seq)))
    "")) ;; return empty string for nil

(defmethod backslash-escape-string ((ch character))
  (string (backslash-escape-char ch)))

(defmethod backslash-escape-string ((n fixnum))
  (string (backslash-escape-char n)))

(defmethod backslash-escape-string ((x t))
  (error "backslash-escape-string: do not know how to convert: ~A = ~A" (type-of x) x))

;; the shell-*-string functions are intended to protect strings (like env-var
;; variables and values) from being interpreted as shell meta chars

(defun shell-variable-char (ch)
  (let ((c (if (characterp ch) (char-code ch) ch)))
    (if (or (and (>= c 65) (<= c 90))  ;; upper
            (and (>= c 97) (<= c 122)) ;; lower
            (and (>= c 48) (<= c 57))  ;; numeric
            (= c 95)) ;; underbar
        (string (code-char c)) ;; variable OK character
      ""))) ;; ignore non-variable characters

(defmethod shell-variable-string ((seq sequence)) ;; matches string, array
  (if seq
      (apply 'concatenate (cons 'string (map 'list 'shell-variable-char seq)))
    "")) ;; return empty string for nil

(defmethod shell-variable-string ((ch character))
  (string (shell-variable-char ch)))

(defmethod shell-variable-string ((n fixnum))
  (string (shell-variable-char n)))

(defmethod shell-variable-string ((x t))
  (error "shell-variable-string: do not know how to convert: ~A = ~A" (type-of x) x))

;; [tmarble:20130225.1754CST] it's tempting to think we could escape single quote in
;; the shell like this "\\'", but the shell does NOT recognize that!
;; Therefore we'll use double quotes!
;; from the dash(1) man page:
;; Single Quotes
;;   Enclosing characters in single quotes preserves the literal meaning of
;;   all the characters (except single quotes, making it impossible to put
;;   single-quotes in a single-quoted string).
;; Double Quotes
;;   Enclosing characters within double quotes preserves the literal meaning
;;   of all characters except dollarsign ($), backquote (`), and backslash
;;   (\).  The backslash inside double quotes is historically weird, and
;;   serves to quote only the following characters:
;;       $ ` " \ <newline>.
;;   Otherwise it remains literal.

;; note: deliberately omitting newline from this list
(defvar *shell-value-metachars* "$`\\\"")

(defun shell-value-metachar-p (ch)
  (let ((c (if (characterp ch) (char-code ch) ch)))
    (find c (as-octet-array *shell-value-metachars*) :test 'eql)))

(defun shell-value-char (ch)
  (let ((c (if (characterp ch) (char-code ch) ch)))
    (if (shell-value-metachar-p c)
        (format nil "\\~A" (code-char c))
      (string (code-char c))))) ;; value OK character

(defmethod shell-value-string ((seq sequence)) ;; matches string, array
  (if seq
      (apply 'concatenate (append
                           (list 'string)
                           (list "\"")
                           (map 'list 'shell-value-char seq)
                           (list "\"")))

    "\"\"")) ;; return empty string for nil

(defmethod shell-value-string ((ch character))
  (concatenate 'string "\"" (shell-value-char ch) "\""))

(defmethod shell-value-string ((n fixnum))
  (concatenate 'string "\"" (shell-value-char n) "\""))

(defmethod shell-value-string ((x t))
  (error "shell-value-string: do not know how to convert: ~A = ~A" (type-of x) x))


;; return the first n characters of the string (or all of it if length < n)
(defun string-maxlength (str maxlen)
  (let ((len (length str)))
    (if (<= len maxlen)
        str
      (subseq str 0 maxlen))))

;; returns true if the last part of str is suffix
(defun string-ends-with (str suffix)
  (declare (type string str suffix))
  (let ((len-str (length str))
        (len-suffix (length suffix)))
    (and (>= len-str len-suffix)
         (string-equal str suffix :start1 (- len-str len-suffix)))))

;; returns true if the first part of str is prefix
(defun string-starts-with (str prefix)
  (declare (type string str prefix))
  (let ((len-str (length str))
        (len-prefix (length prefix)))
    (and (>= len-str len-prefix)
         (string-equal str prefix :end1 len-prefix))))

;; convert hex digit to a number
(defun h2i (h)
  (if (and (>= h 48) (<= h 57))
      (- h 48)
    (if (and (>= h 97) (<= h 102))
        (- h 87)
      0))) ;; handle non-hex digits

;; intepret escaped  printable characters as a binary string
(defun hex-string (s)
  (let* ((a (coerce (as-octet-array s) 'list))
         (b nil))
    (musliner:while a
      (let* ((c (pop a)))
        (if (and (= c 92)  ;; \
                 (>= (length a) 3)
                 (= (car a) 120)) ;; x
            (let* ((y (progn (pop a) (pop a)))
                   (z (pop a)))
              (push (+ (* (h2i y) 16) (h2i z)) b))
          (push c b))))
    (as-string (as-octet-array (reverse b)))))

  ;; function to read a binary file
(defun slurp-binary-file (file)
  (let ((data (make-octet-array)))
    (with-open-file (in file :element-type 'octet)
      (do ((byte (read-byte in nil :eof) (read-byte in nil :eof)))
          ((or (null byte) (eql byte :eof)))
        (octet-array-extend byte data)))
    data))

;; function to read a text file
(defun slurp-text-file (filename)
  (with-open-file (stream filename)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))

;; function to write a binary file
(defun blurt-binary-file (file data)
  (with-open-file (out file :direction :output :if-exists :supersede)
    (write-sequence data out)))

;; copy a binary file
;(defun cp (src dst)
;  (ensure-directories-exist dst)
;  (blurt-binary-file dst (slurp-binary-file src)))

(defun cp (src dst)
  (ensure-directories-exist dst)
  #+allegro (sys:copy-file src dst :overwrite t :force t)
  #+ccl (ccl:copy-file src dst :if-exists :overwrite)
  ;; Not sure whether uiop:copy-file is exact replacement here.
  ;; Behavior may be diff when dst is dir.  
  ;; Can't tell how to fix this, since no idea what DST could be.
  #-(or allegro ccl)
  (if (uiop:directory-pathname-p (parse-namestring dst))
      (let* ((src-path (parse-namestring src))
             (src-name (pathname-name src-path))
             (src-type (pathname-type src-path))
             (dst (merge-pathnames (make-pathname :name src-name
                                                  :type src-type)
                                   (parse-namestring dst))))
        (uiop/stream:copy-file src dst))
      (uiop/stream:copy-file src dst)))

(defun mv (src dst)
  (ensure-directories-exist dst)
  (rename-file src dst))

;(defun hash-file (filename)
;  "Return a hash of contents of FILENAME."
;  (first (toolchain-command (format nil "md5sum ~A | cut -d ' ' -f 1" filename) :list-of-lines t)))

;; note that while you can use :output-file, Lisp still stupidly captures all the output into Lisp memory, so it doesnt fix
;; the problem when the output is huge.  Redirect it in the command instead
#+allegro
(defun toolchain-command (cmd &key (output-file nil) (list-of-lines nil))
  "Run a command, return values of stderr, stdout, exit status. When LIST-OF-LINES, return output as list of lines, otherwise as a single string."
  (dbug :os-utils "Toolchain cmd: ~S" cmd)
  (multiple-value-bind (output error-output exit-status)
    (dont-error
     (excl.osi:command-output cmd :output-file output-file :whole (not list-of-lines)))
    (dbug :os-utils "Exit status: ~A" exit-status)
    (when output
      (dbug :os-utils-deep "Output: ~S" output))
    (when error-output
      (dbug :os-utils "Err output: ~S" error-output))
    (values output error-output exit-status)))

;;;   2018-03-21 not supporting args in above allegro version, we dont use them and they are slightly fiddly in uiop
;;; They should probably go away to avoid cross-version confusion.  But really it'd be nice to reduce the numerous ways of running external programs, in this file.
#+(or ccl sbcl)
(defun toolchain-command (cmd)
  (uiop::run-program cmd :output :string :error-output :string))


(defun shell-command-with-timeout (timeout cmd)
  "Run cmd and return a list of its output, error and exit status (-1 if timed out)"
  (let ((out "")
        (err ""))
    (multiple-value-bind (stdout-stream stderr-stream pid)
        (excl:run-shell-command cmd :output :stream :wait nil :error-output :stream)
      (unwind-protect
          (mp:with-timeout (timeout
                            (dbug :os-utils "Killing process ~D for command ~S because it timed out after ~S seconds."
                                  pid cmd timeout)
                            (excl:run-shell-command (format nil "kill -9 ~D >/dev/null" pid))
                            (values out err -1))
             (dolines (line stdout-stream)
                (setf out (concatenate 'string out line)))
             (dolines (line stderr-stream)
                (setf err (concatenate 'string err line)))
             (values out err #+allegro (system:reap-os-subprocess :pid pid) #-allegro 0))
        (when stdout-stream (close stdout-stream))
        (when stderr-stream (close stderr-stream))))))

#| Simple examples :

(shell-command-with-timeout 10 "echo hi")

(shell-command-with-timeout 1 "sleep 10")

(shell-command-with-timeout 1 "echo hi; sleep 10")

|#


(defun cp-dir (src dst)
  "Copy a directory from SRC to DST."
  (toolchain-command (format nil "cp -a ~A ~A" (namestring src) (namestring dst))))

(defun make-file-executable (path)
  (toolchain-command (format nil "chmod a+x ~A" path))
  path)

(defun make-file-world-writable (path)
  (toolchain-command (format nil "chmod o+w ~A" path))
  path)

;; See the Allegro manual for why you need to use this in delib-tasks to avoid blocking all of Lisp
;; the toolchain-command stuff above does avoid blocking also, but binds all of the program outputs into
;; Lisp objects, which can be prohibitive.
;; This function will block until the cmd finishes, and optionally redirects stderr to stdout

(defvar *run-command-pid* nil)

#+allegro
(defun run-command (cmd &optional (redirect nil))
 (declare (special *delib-task* *delib-process* mp:*current-process*))
 (multiple-value-bind (s errs my-pid)
    (if redirect
        (excl::run-shell-command cmd :error-output :output :wait nil)
        (excl::run-shell-command cmd :wait nil))
  (declare (ignore errs s))
  (setf *run-command-pid* my-pid)
  ;; [mboldt:20150306] Track pid for killing delib task.
  (when (and *delib-task* (eql *delib-process* mp:*current-process*))
    (setf (pid *delib-task*) my-pid)
    (dbug :top "Set pid ~a on delib-task ~a." my-pid *delib-task*))
  (let ((my-status nil))
    (mp::process-wait "for run-shell-command to finish"
                      #'(lambda ()
                         (setq my-status
                          (or my-status
                             (sys:reap-os-subprocess
                              :pid my-pid :wait nil)))))
    (setf *run-command-pid* nil)
    (when (and *delib-task* (eql *delib-process* mp:*current-process*))
      (setf (pid *delib-task*) nil)
      (dbug :top "Cleared pid on delib-task ~a." my-pid *delib-task*))
    my-status)))

;; UIOP ver is a simple blocking run, that seems to be the way this was used
#+(or ccl sbcl)
(defun run-command (cmd &optional (redirect nil))
  (dbug :top "Running ~a" cmd)
  (multiple-value-bind (out err exitcode)
    (if redirect
        (uiop::run-program cmd :error-output :output :ignore-error-status T)
        (uiop::run-program cmd :ignore-error-status T))
    (declare (ignore out err))
    exitcode))

;;; -------------------------------------------------------------------------
;;; The current experiment dir will be mounted inside the docker container as /experiment
;;; You can try this as of April 22 2017 on condor-52 by doing:
;;; (docker-run "sift/binwalk" "/bin/bash -c 'ls /targets'" :remove-p nil :name "binwalk" :mounts '(("/scratch/cgc-data/vulns" "/vulns")("/scratch/cgc-data/targets" "/targets")))      ;; lists /targets - note the quotes
;;; If :remove nil, this leaves the docker container "started" but "disconnected"...so then to run another command/program in that docker
;;; you would use docker start/exec.
;;; Since we know we'll only be using a few diff dockers, we will probably have specialized wrappers for their usage.
;;; :mounts is a list of lists ((host-path container-path)...)  ;;  : chose lists not pairs for easier format iteration magic

(defun docker-run (imagename cmd &key (name nil) (mounts nil) (remove-p T) (options nil))
  (let ((cmdstr (format nil "docker run ~@[--name ~A~] ~@[~A~] ~:[~;--rm~] ~:{ -v=~A:~A~} ~A ~A"
                name
                options
                remove-p
                mounts
                imagename cmd)))
     (dbug :top "docker-run is ~S" cmdstr)
     (run-command cmdstr)
))

(defun docker-exec (name cmd)
  (let ((cmdstr (format nil "docker exec ~A ~A" name cmd)))
     (dbug :top "docker-exec is ~S" cmdstr)
     (run-command cmdstr)
))

(defun docker-kill (name)
  (let ((cmdstr (format nil "docker kill ~A" name)))
     (dbug :top "docker-kill is ~S" cmdstr)
     (run-command cmdstr)
))

;; This will stop the container and rm it, if :remove nil wasnt specified in docker-run
(defun docker-stop (name)
  (let ((cmdstr (format nil "docker stop ~A" name)))
     (dbug :top "docker-stop is ~S" cmdstr)
     (run-command cmdstr)
))


(defun docker-start (name)
  (let ((cmdstr (format nil "docker start ~A" name)))
     (dbug :top "docker-start is ~S" cmdstr)
     (run-command cmdstr)
))

(defmacro with-docker-stream ((stream imagename cmd &key (error-output :output) (name nil) (mounts nil) (remove-p T) (extra-args "") (use-image-name-for-container t)) &body body)
  (let ((use-name (gensym)))
    `(let ((cmdstr (format nil "docker run ~A ~:[~;--rm~] ~:{ -v=~A:~A~} ~A ~A ~A"
                           ;; MAD 2019-05-30 -- Allow option for not
                           ;; providing container name, causing docker
                           ;; to assign a random name.
                           (let ((,use-name (or ,name
                                               (when ,use-image-name-for-container
                                                 ,imagename))))
                             (if ,use-name
                                 (strcat "--name=" ,use-name)
                                 ""))
                           ,remove-p
                           ,mounts
                           ;; MAD 2019-05-30 -- Allow extra arguments to be
                           ;; passed as string if needed
                           ,extra-args
                           ,imagename ,cmd)))
       (dbug :top "with-docker-stream cmdstr is ~S" cmdstr)
       (with-external-program-stream (,stream cmdstr nil :error-output ,error-output) ,@body))))

(defmacro with-docker-exec-stream ((stream containername cmd &key (error-output :output)) &body body)
  `(let ((cmdstr (format nil "docker exec ~A ~A" ,containername ,cmd)))
        (dbug :top "with-docker-exec-stream cmdstr is ~S" cmdstr)
        (with-external-program-stream (,stream cmdstr nil :error-output ,error-output) ,@body)))

;;; -------------------------------------------------------------------------

;; alist helper functions
(defun alist-get (alist key)
  (cdr (assoc key alist :test #'equal)))

;; remove key
(defun alist-remove (alist key)
  (if alist
      (remove-if #'(lambda (conz) (equal (car conz) key)) alist)))

;; add or mutate key to have value
(defun alist-update (alist key value)
  (let ((conz (assoc key alist :test #'equal)))
    (if conz
        (progn
          (rplacd conz value) ;; cdr of this cons in alist mutated
          alist)
      (push (cons key value) alist)))) ;; alist not mutated

;; create new alist with key and value (no mutation)
(defun alist-update-new (alist key value)
  (alist-update (alist-remove alist key) key value))

;; add an item to the list associated with a key, making a new entry if it doesn't exist
(defun alist-push (alist key item)
  (let ((entry (assoc key alist :test #'equal)))
    (if entry
	(progn
	  (push item (cdr entry))  ; mutate the list value
	  alist)
	(cons (cons key (list item)) alist)))) ; add new entry

;;; ---------------------------------------
;;; Additions to FuzzBomb

(defun current-amp-quantum ()
  (declare (special *cur-quantum*))
  (update-time)
  *cur-quantum*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; file size
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod file-size ((filename string))
  "Return the size of the file FILENAME in bytes."
  ;; http://rosettacode.org/wiki/File_size#Common_Lisp
  (with-open-file (stream filename :direction :input :if-does-not-exist nil :element-type '(unsigned-byte 8))
    (if stream (file-length stream) 0)))

(defmethod file-size ((filepath pathname))
  (file-size (namestring filepath)))

(defmethod file-size ((file-list list))
  (loop for file in file-list summing (file-size file)))

(defun trim-whitespace (str)
  (string-trim '(#\Space #\Tab #\Newline) str))

(defun namestring-realpath (namestring)
  (trim-whitespace
   (uiop:run-program (format nil "realpath ~s" namestring) :output :string)))

(defun find-cmd-no-hidden (dir)
  ;; Learned that hard way that his fails miserably when there are '..' elements in the path!
  (format nil "find ~a -not -path \'*/\.*\'" (namestring-realpath dir)))

(defun find-cmd-c-files (dir)
  (format nil "find ~a -iregex \'.*\\.\\(c\\|cpp\\|h\\)$\'" (namestring-realpath dir))
  ;;(format nil "find ~a -iregex \'c$\'" (namestring-realpath dir)))
  )

(defun find-cmd-java-files (dir)
  (format nil "find ~a -iregex \'.*\\.java$\'" (namestring-realpath dir)))

(defun find-cmd (dir &key (args ""))
  (format nil "find ~a ~a" (namestring-realpath dir) args))

(declaim (ftype (function ((or pathname string)) (values string &optional)) native-namestring))
#+ccl
(defun native-namestring (pathname)
  (ccl:native-translated-namestring pathname))
#+sbcl
(defun native-namestring (pathname)
  (namestring pathname))

(defun uiop-file-namestring (namestring)
  "Wrapper around split-unix-namestring-directory-components which returns
   only the file-namestring (if namestring is a path to a file) or nil if
   namestring is a directory path."
  (multiple-value-bind (abs-or-rel dir-path last-comp no-dir-p)
      (uiop:split-unix-namestring-directory-components namestring)
    (declare (ignore abs-or-rel dir-path no-dir-p))
    last-comp))

(defun same-file-contents-p (namestring-1 namestring-2)
  "Use cmp to compare contents of files named by namestring-1 namestring-2."
  (multiple-value-bind (output-slurp error-slurp exit-code)
      (uiop:run-program (format nil "cmp -s ~a ~a" namestring-1 namestring-2)
                        :ignore-error-status t)
    (declare (ignore output-slurp error-slurp))
    (zerop exit-code)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; unix (posix) time, seconds since the epoch
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant +unix-epoch-offset+ 2208988800)

(defun get-unix-time ()
  (- (get-universal-time) +unix-epoch-offset+))

(defun get-unix-time-millisecs ()
  (* (get-unix-time) 1000))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun stable-set-difference (list1 list2 &key (test #'eql))
  "Return the elements of LIST1 that are not in LIST2, preserving the order of LIST1."
  (remove-if (lambda (x) (member x list2 :test test)) list1))
