(in-package :fuzzbomb)

;;
;; Extend CIRCA mission and phase for FB use (for now, so we can specialize methods on it).
;;
(defclass fb-mission (mission)
  ())

(defclass fb-mission-phase (mission-phase)
  ())

;;
;; Variables and their defaults for FB missions.
;;

(defvar *default-mission-name* "CGC-CQE")

;; Seed this with a default mission (in case init-mission isn't inovked)
(defvar *current-mission*
    (make-instance 'fb-mission :name *default-mission-name*))

;; (defvar *cur-phase* nil)

(defvar *default-phase-order* '(("fb-phase-0" 86400 nil)))


(defun init-time ()
  (setf *secs-per-quantum* (/ 1 INTERNAL-TIME-UNITS-PER-SECOND))
  (set-zero-time)
  (unless *cur-quantum*
    (setf *cur-quantum* *zero-time*))
  )

;;; Mission initialization will need to evolve to allow flexible specification of phases, phase durations,
;;;  and probably dynamic extension of phases over time
;;;   - phase-order must be a list of lists specifying the order of mission phases, where each contains the
;;;     phase name and its duration in minutes.
;;;   For now, each phase is instantiated as starting 1ms after the end of its preceeding phase.
;;;    FIXME: there's not yet an implemented mechanism to advance to next mission-phase when one ends
;;;    (For now we're ignoring the CIRCA 'quanta' concept, e.g. duration is converted/saved in secs)
(defun init-mission (&key (mission-name *default-mission-name*)
                          (phase-order *default-phase-order*)
                     &aux next-start-time)
  "Initialize the 'mission' for CRASH/FB, including definition of 'mission phases'"
  ;; [sfriedman:20120719.1350CST] First, do checks and initialize the globals.
  ;;  I do *not* like that these are stored as globals, but it's built into the FB/AMP structure right now.
  ;; (setf *phase-order* phase-order)
  (setf *phasename-order* (mapcar #'car phase-order))  ;; global var used by various AMP fcns
  (setf *current-mission* (make-instance 'fb-mission :name mission-name))
  (setf next-start-time *cur-quantum*)

  ;; [sfriedman:20120719.1347CST] Next, initialize the mission and the time conversion.
  (fb::dbug :amp "Initializing mission ~A with ~A phases." mission-name (length phase-order))
  (dolist (phase phase-order)
    ;; each "phase" entry is a list (<phase name> <duration> <app-names-list>)
    (let ((phase-name (first phase))
          (phase-duration (second phase))
          (phase-apps (third phase)))

      ;; Crap out if the mission phase doesn't have a legal duration.
      (unless (and (numberp phase-duration) (plusp phase-duration))
        (warn "Phase ~A has an invalid duration specified: ~A" phase-name phase-duration)
        (return-from init-mission *current-mission*))

      (let ((new-phase (add-phase-to-mission phase-name phase-apps phase-duration
                                             :mission *current-mission*
                                             :start-time next-start-time)))
        (setf next-start-time (+ 1 (start-time new-phase) (duration new-phase))))))

  ;; [sfriedman:20120719.1403CST] If we didn't make any phases, crap out.
  (unless (phases *current-mission*)
    (warn "At least one mission phase must be defined")
    (return-from init-mission nil))

  ;; It looks like we succeeded - set the globals (ick) accordingly.
  ;; (setf (phases *current-mission*) (reverse initialized-phases))
  (setq *cur-phase* (first (phases *current-mission*)))
  (fb::dbug :amp "Starting mission in phase ~A at zero time: ~A"
            (name *cur-phase*) (start-time *cur-phase*))
  *current-mission*)

(defun init-amp (&key (delib-mode *fb-delib-mode*) perf-prof-path)
  (declare (ignore perf-prof-path))
  ;; equate the cl-user and fb CIRCA baseport/name (needed for make-amp use)
  (setq *circa-baseport* *circa-baseport*
        *circa-basename* *circa-basename*)
  (setf *num-amps* 0)

  (setf *cur-quantum* *zero-time*)

  (setf *amps* nil)
  ;;(init-time)
  ;;(set-zero-time)

  ;;(dbug :amp-deep "FIXME!  circa-baseport: ~A " cl-user:*circa-baseport*)
  (setq *self* (make-amp "MASTER" :master-p T))
  (set-delib-mode delib-mode)
  (setf (mission *self*) *current-mission*)
  (setf *master-amp* *self*)            ; FB only has 1
  ;(clear-goals)                         ; Clear the goals.
  (setf (contracts *self*) nil)
  ;; [sfriedman:20140528.1240CST] TODO: initialize the perf prof appropriately.
  ;; (init-perf-prof perf-prof-path)
  )

(defun add-phase-to-mission (phase-name apps-running duration
                             &key (mission *current-mission*) start-time)
  (let* ((last-phase (car (last (phases mission))))
         (phase-start (or start-time
                          (when last-phase (+ (start-time last-phase) (duration last-phase) 1))
                          *cur-quantum*))
         (new-phase (make-instance 'fb-mission-phase
                      :name phase-name :mission mission
                      :duration (seconds-to-quanta duration)
                      :start-time phase-start
                      ;;:apps-running apps-running
                      )))
    (fb::dbug :amp "Phase ~A  start: ~A (~5,2F sec)  end: ~A (~5,2F sec)  apps: ~A"
              phase-name
              (start-time new-phase)
              (quanta-to-seconds (start-time new-phase))
              (+ (start-time new-phase) (duration new-phase))
              (quanta-to-seconds (+ (start-time new-phase) (duration new-phase)))
              apps-running)
    (setf (phases mission) (append (phases mission) (list new-phase)))
    new-phase))


(defun quanta-to-seconds (quanta)
  (* quanta *secs-per-quantum*))

(defun seconds-to-quanta (secs)
  (/ secs *secs-per-quantum*))

(defun quanta-to-minutes (quanta)
  (/ (quanta-to-seconds quanta) 60))

(defun minutes-to-quanta (mins)
  (seconds-to-quanta (* 60 mins)))
