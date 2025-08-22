;;; -------------------------------------------------------------------------
;;; collab.lisp
;;; - sockets/comm to talk w/ matchmaker C program (in RTS dir)
;;; that tells everyone where to find each other.
;;; - $Revision: 1.8 $
;;; -------------------------------------------------------------------------
;;; COPYRIGHT START
;;; COPYRIGHT END
;;; -------------------------------------------------------------------------

(cl:in-package :fuzzbomb)

;;;-------------------------------------------------------------------------

(debug-list-value :mm "Show information on matchmaker comms (collab.lisp).  [AMP]")

(pushnew :mm *debug-list*)

  ;; unlike regular musliner-tools dont-error this tries to disconnect from MM so will reconnect if that barfs during startup
(defmacro collab-dont-error (&body forms)
   `(catch 'trap-errors (handler-bind ((serious-condition #'(lambda (c) (format t "~&ERROR: Encountered serious-condition: ~A~%" c)
           #+allegro (top-level.debug::zoom *standard-output*)
           #+ccl (ccl:print-call-history)
	   #-(or allegro ccl) (uiop:print-backtrace)
           (format t "~&Disconnecting from MM if possible~%")
           (disconnect-from-matchmaker)
           (format t "~&Ignoring...carry on...~%") (throw 'trap-errors nil)))) ,@forms)))

;;;-------------------------------------------------------------------------
;;; based on collab.c matchmaker/RTS code in rts dir.
;;; - TODO: eventually this could fire up the MM if it is not already
;;; running!  That'd be nice....
;;;     - a triv thing would be to always fork it, and ignore it
;;;     if it dies b/c another one is already running...  give it a "-quiet" arg
;;; - of course this would only work if we are running this AMP on mm-host

(defun connect-to-matchmaker ()
  (dbug :mm "Connecting with matchmaker at host ~A port ~A" *matchmaker-host* *matchmaker-port*)
  (when *mm-sock*
    (dbug :mm "Already connected... ")
    (return-from connect-to-matchmaker))
  (collab-dont-error (setf *mm-sock* (reliably-make-socket :remote-host *matchmaker-host*
                               :remote-port *matchmaker-port*
                               :format :bivalent)))
  (when (not *mm-sock*)
        (dbug :top "WARNING: Failed to connect to MM; trying recursively.")
        (connect-to-matchmaker))
  #-ccl ;; acl-compat for ccl doesn't support chunking; this call is unimplemented and unnecessary.
  (ignore-errors                        ; will error if this is not a chunky socket stream
   (socket-control *mm-sock* :output-chunking nil :input-chunking nil))
  (dbug (:top :mm) "   Got connection to matchmaker.")
  )

;;;-------------------------------------------------------------------------
(defun register-with-matchmaker (&optional (name (name *self*)) (host (host *self*)) (port (port *self*)))
  (let (ack)

    (dbug :mm "Registering with matchmaker for ~A ~A ~A" name host port)
    (dbug :timeline ".BEGIN comms initialization.")
    (when (or (not (stringp name)) (not (stringp host)) (not (integerp port))) (error "Improper name, host, or port"))
    (connect-to-matchmaker)
    (send-string *mm-sock*
                 (format nil "REGISTER ~A ~A ~A" name host port))
    (setf ack (get-string *mm-sock*))
    (dbug :mm " Got ack from matchmaker = ~A" ack)
    ack
    ))

;;;-------------------------------------------------------------------------
(defun unregister-with-matchmaker ()
  (dbug :mm "Unregistering with matchmaker")
  (connect-to-matchmaker)
  (send-string *mm-sock*
               (format nil "FORGETME ~A" (name *self*)))
  (close *mm-sock*)
  (setf *mm-sock* nil)
  )

;;;-------------------------------------------------------------------------
(defun disconnect-from-matchmaker ()
 (when *mm-sock*
  (dbug :mm "Disconnecting from matchmaker")
  (dont-error (send-string *mm-sock* (format nil "BYE ~A" (name *self*))))
  (dont-error (close *mm-sock*))
  (setf *mm-sock* nil)
  ))

;;;-------------------------------------------------------------------------
(defun reset-matchmaker ()
  (dbug :mm "Resetting matchmaker")
  (connect-to-matchmaker)
  (send-string *mm-sock*
               (format nil "RESET"))
  (close *mm-sock*)
  (setf *mm-sock* nil)
  )

;;;-------------------------------------------------------------------------
;;; note we need to send the longname, including circa_basename and AMP.
;;; - this will return a list of (hostname port)

(defun request-collaborator-by-shortname (shortname)
  "Returns list of (hostname port) if successful, nil otherwise."
  (let ((name (make-full-name shortname))
        ack)

    (dbug :mm "Requesting collaborator information from matchmaker for ~A" name)
    (connect-to-matchmaker)
    (send-string *mm-sock* (format nil "REQUEST ~A" name))
    (setf ack (get-string *mm-sock*))
    (dbug (:top :mm) "    Got request response from matchmaker = ~A" ack)
    (when (string= "NOTFOUND" ack) (return-from request-collaborator-by-shortname nil))
    ;; turn ack into a list of symbols
;    (setf ack (read-from-string (format nil "(~A)" ack)))
    ;; if it is only one symbol long, == NOTFOUND
    ;; a successful lookup returns something like
    ;;      "COLLABORATOR MASTER-AMP-DAVE foom 8000"
;    (when (= (length ack) 4)
;      (destructuring-bind (collaborator name hostname port) ack
;        (declare (ignore collaborator name))
;        ;; convert hostname from symbol to string
;        (setf hostname (string-downcase (symbol-name hostname)))
;        (list hostname port)))
       (destructuring-bind (role name hostname port)
                    (uiop:split-string ack :separator " ")
         (declare (ignore role name))
         (list hostname (parse-integer port)))
))

;;;-------------------------------------------------------------------------
;;; this is NO LONGER like talk_to_matchmaker() in collab.c
;;; customized for roles/comm directions in CGC

(defun talk-to-matchmaker ()
  (let* ((vals t)
        (amps (remove *self* *amps*))
;       (num-dvms (count-if #'dvm-p amps))
;       (num-optimi (count-if #'optimus-p amps))
;       (num-fbs (count-if #'fuzzbomb-p amps))
        )
    (musliner:while (not (collab-dont-error (register-with-matchmaker))) (sleep 1))
    ;; first we reset port on all AMPs but us, so we know we havent
    ;; found out where they are yet.  Later fns will check port to see.
    (dolist (a amps)
      (setf (port a) nil))

                ;; DVMs need to know about all the FBs and Optimi...so they know who they have to call
   (cond ((dvm-p *self*)

        ;; wait until all the FBs and Opt are registered, so we know who to call
    (dolist (a (remove-if #'dvm-p amps))
        (musliner:while (not (port a))
          (dbug :top "Waiting for ~A to register with MM" a)
          (when (not vals) ;; if didnt hear from someone last round, sleep, then ask again
                (dbug :top "Disconnecting from MM and sleeping 1")
                (collab-dont-error (disconnect-from-matchmaker))
                (sleep 1))
          (setf vals (collab-dont-error (request-collaborator-by-shortname (shortname a))))
          (when vals
                (dbug :top "Got ~A registration at ~A" (shortname a) vals)
                (setf (host a) (first vals))
                (setf (port a) (second vals)))))

                (dbug :top "I'm a DVM, nothing more to do in talk-to-matchmaker"))
         (T

                ;; here we're an FB/Opt, and we need to
    ;; find out which AMPs have already registered
    (dolist (a amps)
      (setf vals (collab-dont-error (request-collaborator-by-shortname (shortname a))))
      (when vals
        (dbug :top "Got ~A registration at ~A" (shortname a) vals)
        (setf (host a) (first vals))
        (setf (port a) (second vals))))

        ))

    (disconnect-from-matchmaker)
))

;;;-------------------------------------------------------------------------
(defun wait-for-collaborators (&aux amp)
  (let (remote-sock
        remote-name
        (num-missing (count-if-not #'(lambda (a) (getassoc a (peer-sockets *self*))) (remove *self* *amps*))))
    (dbug :top "  Waiting for connections from ~A collaborators on port ~d..." num-missing (port *self*))
    (dotimes (n num-missing)
      (setf remote-sock (my-accept-connection (amp-sock *self*)))
      (setf remote-name (get-string remote-sock))
      (dbug :top " Got connection from ~A on local port ~A, remote port ~A" remote-name
            (local-port remote-sock) (remote-port remote-sock))
      (setf amp (find-amp remote-name))
      (when (not amp) ;; if the AMP is not found, possibly the listen socket died?  Try re-opening if so and retry wait-for-collab
        (dbug :top "ERROR, no amp found for get-string remote-name returnval ~A, re-opening listen socket" remote-name)
        (sleep 1)
        (initialize-collaborations)
        (return-from wait-for-collaborators)
        )
      (when (getassoc amp (peer-sockets *self*))
        (dbug :top "WARNING: ~A already in peer-sockets, cleaning up old connection" remote-name)
        (ignore-errors (close (getassoc amp (peer-sockets *self*))))
        (setf (peer-sockets *self*) (remove amp (peer-sockets *self*) :key #'first))
      )
      (setassoc amp remote-sock (peer-sockets *self*))
      ;; FIXME   DM says: "seems dumb that the connecting guy doesnt send his hostname,
      ;; i would think that ipaddr-to-hostname is expensive"
      (setf (host amp) (ipaddr-to-hostname (remote-host remote-sock)))
      (when (string= (host amp) "localhost") (setf (host amp) (host *self*))))
    
    (dbug :top "   All expected AMPs connected...")
    (dbug :timeline ".END comms initialization.")
    (dbug :amp-deep "(peer-sockets *self*): ~s~%" (peer-sockets *self*))
    ))

;;;-------------------------------------------------------------------------
;;; for all the AMPs that are already registered with MM, connect to them
;;; because they are waiting for us to do so.
;;;   For CGC: dont try to call any DVMs, it wont work

(defun connect-to-waiting-collaborators ()
  (let (remote-sock)

    (dbug :top "  Connecting to waiting collaborators...")
    (dolist (a (remove *self* *amps*))
      (setf remote-sock nil)
      (when (and (port a) (not (dvm-p a)))
        (dbug :mm "    Connecting to ~s at host ~s on port ~s..." (shortname a) (host a) (port a))
        ;(dbug :mm "(symbolp (host a)): ~s~%" (symbolp (host a)))
        (musliner:while (not remote-sock)
                (collab-dont-error (setf remote-sock (reliably-make-socket :remote-host (host a)
                                       :remote-port (port a)
                                       :format :bivalent)))
                (when (not remote-sock)
                        (dbug :top "WARNING: Failed to connect to ~s; trying again." (shortname a)))
           )
        #-ccl ;; acl-compat for ccl doesn't support chunking; this call is unimplemented and unnecessary.
        (ignore-errors (socket-control remote-sock :output-chunking nil :input-chunking nil))
        (send-string remote-sock (name *self*))
        (setassoc a remote-sock (peer-sockets *self*))
        (dbug :top "   Got connection to ~A at local port ~A" (shortname a) (local-port remote-sock))
        ))))

;;;-------------------------------------------------------------------------
;;; - open well-known socket, register w/ MM, and connect to all collaborators.
;;; - this works like the RTS's initialize_collaborations in collab.c
;;;     - the connection process is synchronized via single-threaded MM,
;;;     so when I call up, I see which of my collaborators have already
;;;     registered; those that have, I call up.  Those that havent
;;;     will call me when they register.
;;; - assumes *self* is bound to AMP object.

;;; Jan 28 2015 hacking this to be different for fuzzbomb, where the DVMs do not have well-known ports port-forwarded anymore
;;; so they have to do all the connecting outbound.
;;; So their algorithm is to repeatedly poll MM until they learn about all the FB and Optimus names, and then wait for them to connect.
;;; Conversely, FBs must get all the DVM registrations and connect to them.  This is likely easy b/c we now ensure, using
;;; require-dvms, that the DVMs are up.

(defun initialize-collaborations ()
  ;;(dbug :top "Initializing collaborations for ~A on port ~A" *self* (port *self*))
  (dbug :top "Initializing collaborations for ~A" *self*)
  (let ((port (port *self*)))
    (dbug :mm "Opening inbound socket for connections on port ~A" port)
    (setf (amp-sock *self*) (reliably-make-socket :connect :passive :reuse-address T
                                       :local-port port
                                       :format :bivalent
                                       #+ccl :auto-close #+ccl t)))

  ;; (dbug :top "Before inbound socket opened port = ~A" (port *self*))
  ;; (setf (port *self*) (local-port (amp-sock *self*)))
  (dbug :top "New inbound socket opened on port ~A" (port *self*))
  (talk-to-matchmaker)
        ;;   ok at this point, if i'm a DVM I've already waited until I know all the
        ;; FBs and Optimi to call, so the first call below should connect to them.  If i'm a FB or Optimus,
        ;; I should do the usual thing, but not call any DVMs I've learned about

  (connect-to-waiting-collaborators)

  (wait-for-collaborators)

  ;; note in collab.c the next thing is to send forgetme to MM.
  ;; but we wont do that until the RTS (and potentially anything
  ;; else we want to talk to) has connected.
  )
