;; $Id: excl-ccl.lisp 8451 2016-03-31 11:07:24Z musliner $

;; This is a very thin layer to make CCL's functions look like Allegro's EXCL pkg.
;; No fancy options are known to work except what we use already.
;; So far, 2/2014, the only thing demonstrated to work is starting Davinci in default mode.

;; ccl's run-program wants args to be a list of strings.  This is diff from ACL which handles a vector, list of strings, or single string with
;; space-separated args all correctly, afaict.

;; here we expect that the caller is leveraging the last option above, a single space-sep string, and we'll break that into a list of
;; no-space strings.  Heaven forfend you have quoted strings in there...  oh wait yeah we have to handle that.... nevermind, let's just
;; fix our callers.

#-ccl (error "This should only load in CCL")

(in-package :excl)

;; Dave's start on this before realizing he could hijack from acl-compat more easily.
;(defun run-shell-command (command &rest restargs)
;  (let* ((pos (position #\Space command))
;	 (programname (if pos (subseq command 0 pos) command))
;	 (args (if pos (subseq command pos) "")))
;    (apply #'ccl:run-program programname args restargs)))

;;;; hijacked mercilessly from ACL-COMPAT - EXCL

(defun run-shell-command (command
                          &key input (output *standard-output*) error-output separate-streams
                          if-input-does-not-exist if-output-exists
                          if-error-output-exists (wait t) environment show-window)
  (declare (ignore show-window))
  (let* ((program-and-arguments
          (split-sequence:split-sequence " " command :test #'string=))
         (program (car program-and-arguments))
         (arguments (cdr program-and-arguments)))
   (when environment
     #-unix (error "Don't know how to run program in an environment.")
     (setf arguments (append
                      (list "-i")
                      (loop for (name . value) in environment
                         collecting (concatenate 'string name "=" value))
                      (list program)
                      arguments))
     (setf program "env"))
       
;;; needed to handle command with no args- before it came up with nil, now empty string
;;; also : real acl excl default is to output to stdout and wait=t
   (let* ((process (ccl:run-program program (or arguments (list ""))
                                :input input
                                :if-input-does-not-exist
                                if-input-does-not-exist
                                :output output
                                :if-output-exists if-output-exists
                                :error error-output
                                :if-error-exists if-error-output-exists
                                :wait wait))
          (in-stream (ccl:external-process-input-stream process))
          (out-stream (ccl:external-process-output-stream process))
          (err-stream (ccl:external-process-error-stream process))
          (pid (ccl:external-process-id process)))
     (cond
       ;; one value: exit status
       (wait (nth-value 1 (ccl:external-process-status process)))
       ;; four values: i/o/e stream, pid
       (separate-streams
        (values (if (eql input :stream) in-stream nil)
                (if (eql output :stream) out-stream nil)
                (if (eql error-output :stream) err-stream nil)
                pid))
       ;; three values: normal stream, error stream, pid
       (t (let ((normal-stream
                 (cond ((and (eql input :stream) (eql output :stream))
				;;   2/2014 not sure why they get reversed, but they do!
     			(if (and (input-stream-p out-stream) (output-stream-p in-stream))
                        	(make-two-way-stream out-stream in-stream)
                        	(make-two-way-stream in-stream out-stream)))
                       ((eql input :stream) in-stream)
                       ((eql output :stream) out-stream)
                       (t nil)))
                (error-stream (if (eql error-output :stream) err-stream nil)))
            (values normal-stream error-stream pid)))))))

#|  ;; other crap that acl-compat defined and we may want eventually:

#+openmcl
(defun filesys-inode (path)
  (let ((checked-path (probe-file path)))
    (cond (checked-path
	   (ccl:rlet ((lstat :stat))
		     (ccl:with-cstrs ((str (namestring checked-path)))
				     (#_stat str lstat))
		     (ccl:pref lstat :stat.st_ino)))
	  (t (error "path ~s does not exist" path)))))

(defun cl-internal-real-time ()
  (round (/ (get-internal-real-time) 1000)))

(defun stream-input-fn (stream)
  stream)

(defun filesys-type (file-or-directory-name)
	(if (ccl:directory-pathname-p file-or-directory-name)
		:directory
		(if (probe-file file-or-directory-name)
			:file
			nil)))

(defmacro atomically (&body forms)
  `(ccl:without-interrupts ,@forms))

(defmacro without-package-locks (&body forms)
  `(progn ,@forms))

(define-condition stream-error (error)
  ((stream :initarg :stream
           :reader stream-error-stream)
   (action :initarg :action
           :initform nil
           :reader stream-error-action)
   (code :initarg :code
         :initform nil
         :reader stream-error-code)
   (identifier :initarg :identifier
               :initform nil
               :reader stream-error-identifier))
  (:report (lambda (condition stream)
             (format stream "A stream error occured (action=~A identifier=~A code=~A stream=~S)."
                     (stream-error-action condition)
                     (stream-error-identifier condition)
                     (stream-error-code condition)
                     (stream-error-stream condition)))))

(define-condition socket-error (stream-error)
  ()
  (:report (lambda (condition stream)
             (format stream "A socket error occured (action=~A identifier=~A code=~A stream=~S)."
                     (stream-error-action condition)
                     (stream-error-identifier condition)
                     (stream-error-code condition)
                     (stream-error-stream condition)))))



;! Need to figure out what to do here
(defun fasl-read (filename)
  (declare (ignore filename))
  (error "fasl-read not implemented for MCL.") )

(defun fasl-write (data stream opt)
  (declare (ignore data stream opt))
  (error "fasl-write not implemented for MCL.") )


(defmacro schedule-finalization (object function)
  `(ccl:terminate-when-unreachable ,object ,function))

|#
