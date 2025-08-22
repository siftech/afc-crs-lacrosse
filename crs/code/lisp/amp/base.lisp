;;; -------------------------------------------------------------------------
;;; base.lisp
;;; - requires *circa-baseport* and *circa-basename*  for
;;;	derivation of all future names/ports.
;;; - stuff to handle deriving program names and ports
;;; 	from environment variables CIRCA_BASENAME and CIRCA_BASEPORT
;;;	is largely in comm.lisp.
;;; - $Revision: 1.5 $
;;; -------------------------------------------------------------------------
;;; COPYRIGHT START
;;; COPYRIGHT END
;;; -------------------------------------------------------------------------

(cl:in-package :fuzzbomb)

;;; -------------------------------------------------------------------------
;;; AMP's name is <name>-AMP-<basename>
;;; eg: MASTER-AMP-DAVE
(defun make-full-name (shortname)
  (format nil "~A-~A-~A" (string-upcase shortname) "AMP" *circa-basename*))

