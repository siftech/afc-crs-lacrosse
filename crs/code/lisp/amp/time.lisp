;;; -------------------------------------------------------------------------
;;; time.lisp
;;; - handles AMP's perception of time (as delib quanta)
;;; - $Revision: 1.6 $
;;; -------------------------------------------------------------------------
;;; COPYRIGHT START
;;; COPYRIGHT END
;;; -------------------------------------------------------------------------

(cl:in-package :fuzzbomb)

;;; -------------------------------------------------------------------------
(defun set-zero-time ()
  (setf *zero-time* (get-internal-real-time))
  (setf *cur-quantum* 0))

;;; -------------------------------------------------------------------------
(defun tics-per-quantum ()
  (* *secs-per-quantum* INTERNAL-TIME-UNITS-PER-SECOND))

;;; -------------------------------------------------------------------------
;;; protected to run OK even if *zero-time* not set yet.

(defun update-time ()
  (setf *cur-quantum* 
	(if *zero-time*
		(round (- (get-internal-real-time) *zero-time*)
			(tics-per-quantum))
		0)
	))
