;;; -------------------------------------------------------------------------
;;; perf-prof.lisp
;;; - manage CSM performance profiles
;;; - $Revision: 1.5 $
;;; -------------------------------------------------------------------------
;;; COPYRIGHT START
;;; COPYRIGHT END
;;; -------------------------------------------------------------------------

;;; performance profiles are kept as hash tables internally, accessed by
;;; relevant dimensions of config, consd into list.
;;; - currently only handles count of goals/threats as config characterization
;;; dimensions

;;; - currently this means that we only store a single number, the
;;; number of seconds taken to plan.  We could also include the likelihood
;;; of finding a plan.  However, for now we'll just say that this
;;; is always a constant for any particular performance profile.... say,
;;; 80% like what we did in MDP experiments.

;;; performance profiles can be domain-specific, or default to
;;; *default-perf-prof-file*

(in-package :fuzzbomb)
;;  [tzim 4/8/12] This is NOT the CIRCA/amp version (see jrye notes below)


;(defun init-perf-prof (path)
;  ;; [sfriedman:20120806.1602CST] We don't need a hashtable anymore.
;  ;; (setf *perf-prof-hash* (make-hash-table :size 100 :test #'equalp))
;
;  ;; Scraped this from the AMP's perf prob init.
;  (cond ((file-exists-p path)
;         (load path)
;         ;; (fb::dbug :amp-deep "Loading perf prof file: ~A" path)
;         )
;        (T (error "unable to find perf-prof file")))
;
;  ;; (cond ((musliner::file-exists-p (strcat *domain* "-" *perf-prof-suffix*))
;  ;;        (load (strcat *domain* "-" *perf-prof-suffix*))
;  ;;        (dbug :amp-deep "Loading perf prof file: ~A"
;  ;;              (strcat *domain* "-" *perf-prof-suffix*)))
;  ;;       ((musliner::file-exists-p *default-perf-prof-file*)
;  ;;        (load *default-perf-prof-file*))
;  ;;       (T (error "unable to find perf-prof file")))
;
;  ;;   This is what the perf-prof file looked
;  ;; like. Unfortunately, we don't have a sethash and I don't know
;  ;; where it came from. So, we'll just hack the perf-prof function
;  ;; below.
;
;  ;; At location (num-goals num-threats), hash the estimated
;  ;; seconds required for the task to complete... In CIRCA this was
;  ;; time to find a plan. In FB, this probably needs to be mapped to
;  ;; the specific task...
;  ;; (sethash '(0 0) *perf-prof-hash* 10.0)
;  ;; (setf *perf-prof-prob* 1.0)
;  )

(defun perf-prof (&key goals threats)
  (declare (type integer goals)  (type integer threats) (ignore goals threats))
  ;; (gethash (list goals threats) *perf-prof-hash*)
  ;;   Per comment above about not having a
  ;; sethash, just returning a constant.
  10.0
  )

;;; eventually this might lookup probability of getting an answer
;;; in the perf-prof time, but for now we assume all perf-profs
;;; are a single cut through the real full perf-prof at some percentage
;;; probability of getting a plan.  Set in perf-prof file.

;; [sfriedman:20121116.1012CST] Changing this to a method,
;; so we can have specialized perf-prof lookups for different task types.
(defmethod perf-prof-prob ((task t))
  *perf-prof-prob*)

(defmethod perf-prof-prob ((task null))
  *perf-prof-prob*)
