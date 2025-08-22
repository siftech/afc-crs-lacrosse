;;; -------------------------------------------------------------------------
;;; patch-cand-class.lisp
;;;
;;; -------------------------------------------------------------------------
;;; COPYRIGHT START
;;; COPYRIGHT END
;;; -------------------------------------------------------------------------

(cl:in-package :fuzzbomb)

(defclass patch-cand (named-object)
  (
   (id :initarg :id :initform -1 :accessor id
       :documentation "Unique lax id."
       :type int)
   (cid ;;  :type string
        :accessor cid
        :documentation "Unique competition id assigned by aixcc infra when this is submitted.")
   (patch-file :initarg :patch-file :initform nil :accessor patch-file
               :documentation "String containing path to patch file."
               :type (or null string))
   (vuln-cand :initarg :vuln-cand :initform nil :accessor vuln-cand
              :documentation "Ptr to parent vuln-cand."
              :type (or null vuln-cand))
   (src-vuln-hyps :initarg :src-vuln-hyps :initform nil :accessor src-vuln-hyps
		  :documentation "Ptr to associated src-vun-hyps, which this attempted to patch (typically just one)."
		  :type list)
   (target :initarg :target :initform nil :accessor target
           :documentation "Ptr to the target and all of its state."
           :type (or null lacrosse-cp-target))
   (shared-data-dir :initarg :shared-data-dir :initform nil :accessor shared-data-dir
                    :documentation "String containing path to shared-data-dir."
                    ;;:type (or null string)
                    )
   (applied-p :initarg :applied-p :initform nil :accessor applied-p
              :documentation "Says whether the patch applied, according to internal testing.")
   (built-p :initarg :built-p :initform nil :accessor built-p
            :documentation "Says whether the patched code built, according to internal testing.")
   (fixed-pov-p :initarg :fixed-pov-p :initform nil :accessor fixed-pov-p
		:documentation "Says whether the patched and built code passed the PoV, according to internal testing.")
   (passed-functionality-p :initarg :passed-functionality-p :initform nil :accessor passed-functionality-p
			   :documentation "Says whether the patched and built code passed functionality tests, according to internal testing.")
   (passed-available-p :initarg :passed-available-p :initform nil :accessor passed-available-p
		       :documentation "Says whether the patch passed all available internal testing.")
   ;; submission status - this could be a mixin, but not now
   
   (submitted-p :initform nil :accessor submitted-p
                :documentation "Set to t after submission.")
   (rejected-p :initform nil :accessor rejected-p
               :documentation "Set to t when rejected.")
   (passed-p :initarg :passed-p :initform nil :accessor passed-p
               :documentation "Set to t when passed.")
   ;;   For now, python handles "accepted" state.
   ;;(accepted-p :initform nil :accessor accepted-p
   ;;            :documentation "Set to t when accepted.")
   (status :initform nil :accessor status
           :documentation "Raw status returned by aixcc infra OR :deadline-exceeded.")

   (gp-uuid :initarg gp-uuid :initform nil :accessor gp-uuid
            :documentation "String containing the gp-uuid from capi."
            ;;:type (or null string)
            )
   ))

(defmethod initialize-instance :after ((patch-cand patch-cand) &key)
  ;; Unless id is supplied to make-instance, use named-object name.
  ;; It's intended that only optimus assign the id.
  (when (and (numberp (id patch-cand))
             (= -1 (id patch-cand)))
    (setf (id patch-cand) (name patch-cand)))
  (add-object-for-lax-id patch-cand (id patch-cand))
  (let* ((shared-dir-base (if (vuln-cand patch-cand)
			      (shared-data-dir (vuln-cand patch-cand))
			      ;; FIXME this is a best-guess hack for the finals-- What to use when no vuln-cand?
			      (shared-path (target patch-cand))))
	 (shared-data-dir (format nil "~a/patch-cands/~6,'0d/" shared-dir-base (id patch-cand))))
    (setf (shared-data-dir patch-cand) shared-data-dir)
    (dbug :top "Initialized patch cand ~a with shared-data-dir: ~s" patch-cand (shared-data-dir patch-cand))
    (ensure-directories-exist shared-data-dir))
  )

(defmethod lp-string ((patch-cand patch-cand))
  (format nil "<PATCH-CAND: ~a  vcid: ~a: file: ~a>" (id patch-cand) (id (vuln-cand patch-cand)) (patch-file patch-cand)))

(defmethod ready-to-submit-p ((patch-cand patch-cand))
  "Does this patch-cand meet the min reqts for submission?
   This is the test for patches to be submitted immediately
   after arrival at optimus.  There is a different standard
   for delayed submission."
  (dbug :top "testing ready-to-submit-p: ~s" patch-cand)
  (cond
    ;;   Deadline is checked *right* before this is called, so not testing again.
    ;;((deadline-exceeded-p (target patch-cand))
    ;;      (dbug :top  "Deadline exceeded in ready-to-submit-p (patch-cand), dropping ~s ~a" (target patch-cand) (short-description (target patch-cand)))
    ;;      (setf (status patch-cand) :deadline-exceeded)
    ;;      nil)
    ((and
      ;; In semifinals we checked whether there was already a patch submitted for the vuln cand.
      ;; In finals, we expect just one patch to be created per vuln-cand.  
      (not (submitted-p patch-cand))
      (patch-file patch-cand)
      (not (rejected-p patch-cand))
      (passed-available-p patch-cand) ; Only submit patches that passed available tests
      (not (equiv-to-rejected-p patch-cand))
      ;; there must be no svh associated which is non-pov with a duplicate (the duplicate must be a pov-based vuln bc of the way we find duplicates)
      (not (some #'(lambda (svh)
		     (and (not (vuln-cand svh))
			  (duplicates svh)))
		 (src-vuln-hyps patch-cand)))
      ))
    (t nil)))

(defmethod ready-to-submit-immediately-p ((patch-cand patch-cand))
  "A pc is read for immediate submission if normal preconds are met
   *and* (it has a pov *or* lax is using :greedy *psp-submit-strategy*)."
  (and (ready-to-submit-p patch-cand)
       (or (eq :greedy *psp-submit-strategy*)
           (and (vuln-cand patch-cand)
                (passed-p (vuln-cand patch-cand))))))

(defmethod ready-to-submit-delayed-psp-p ((patch-cand patch-cand))
  (and (ready-to-submit-p patch-cand)
       (not (vuln-cand patch-cand))))

;; FIXME This mod for finals seems like a hack, but then again so does the original.
(defmethod equiv-to-rejected-p ((pc patch-cand))
  ;; *lax-final*
  (let ((retval (member pc (remove-if-not #'rejected-p (all-patch-cands *self*))
			:test #'same-patch-p)))
    (when retval
      (dbug :dupe "equiv-to-rejected-p: ~s same as ~s" pc (first retval)))
    retval))
  
(defmethod same-patch-p ((pc-1 patch-cand) (pc-2 patch-cand))
  (same-file-contents-p (patch-file pc-1) (patch-file pc-2)))

;;;(defun consider-submit-finals-patch (ctask)
;;;  (dbug :top "consider-submit-finals-patch")
;;;  ;;   Deadline is checked *right* before this is called, so not testing again.
;;;  (let ((patch-to-submit (find-if #'ready-to-submit-immediately-p (all-patch-cands ctask))))
;;;    (dbug :top "patch-to-submit: ~s" patch-to-submit)
;;;    (when patch-to-submit
;;;      (when (vuln-cand patch-to-submit)
;;;	(dbug :top "patch-to-submit has vuln-cand: ~s" (vuln-cand patch-to-submit))
;;;	(setf (submitted-patch (vuln-cand patch-to-submit)) patch-to-submit))
;;;      (when (src-vuln-hyps patch-to-submit)
;;;	(dbug :top "patch-to-submit has src-vuln-hyps: ~s" (src-vuln-hyps patch-to-submit))
;;;	(dolist (svh (src-vuln-hyps patch-to-submit))
;;;	  (dbug :top "svh ~s has vuln-cand ~s and duplicates ~s" svh (vuln-cand svh) (duplicates svh))
;;;	  (when (and (not (vuln-cand svh)) (duplicates svh))
;;;	    (dbug :top "WARNING: svh associated with submitted patch should not have duplicates if it has no vuln-cand (pov). This patch should not be in P_sp. Should not happen!"))))
;;;      (submit-patch patch-to-submit))))

(defun consider-submit-finals-patch (ctask)
  (dbug :top "consider-submit-finals-patch")
  (let ((pcs (cond ((eq :greedy *psp-submit-strategy*)
		    (all-patch-cands ctask))
		   (t (patch-cands-w-vcs ctask)))))
    (iter (for pc in pcs)
	  (when (ready-to-submit-immediately-p pc)
	    (when (vuln-cand pc)
	      (setf (submitted-patch (vuln-cand pc)) pc))
	    (when (src-vuln-hyps pc)
              (dbug :top "pc has src-vuln-hyps: ~s" (src-vuln-hyps pc))
              (dolist (svh (src-vuln-hyps pc))
		(dbug :top "svh ~s has vuln-cand ~s and duplicates ~s" svh (vuln-cand svh) (duplicates svh))
		(when (and (not (vuln-cand svh)) (duplicates svh))
		  (dbug :top "WARNING: svh associated with submitted patch should not have duplicates if it has no vuln-cand (pov). This patch should not be in P_sp. Should not happen!"))))
	    (submit-patch pc)))))
  
(defun submit-delayed-psps (ctask)
  (cond ((delta-ctask-p ctask)
         (submit-delayed-psps-delta ctask))
        (t
         (submit-delayed-psps-full ctask))))

(defun submit-delayed-psps-delta (ctask)
  (dbug :top "submit-delayed-psps-delta")
  (let ((sorted-psps (ready-psps-sorted ctask)))
    (dbug :top "sorted-psps: ~s" sorted-psps)
    (dbug :top "scores: ~s" (mapcar #'score sorted-psps))
    (cond ((zerop (num-patches-submitted ctask))
           (when sorted-psps
             (submit-patch (first sorted-psps))
             (let ((second-psp (second sorted-psps)))
               (when (and second-psp
                          (> (score second-psp) *high-confidence-psp-threshold*))
                 (submit-patch second-psp)))))
          (t nil))))

;;;(defun submit-delayed-psps-full (ctask)
;;;  (dbug :top "submit-delayed-psps-full")
;;;  (let* ((psps (iter (for pc in (all-patch-cands ctask))
;;;                 (when (ready-to-submit-delayed-psp-p pc)
;;;                   (collect pc))))
;;;         (sorted-psps (sort psps #'> :key #'score))
;;;         (num-to-submit (- (patch-submit-limit ctask) (num-patches-submitted ctask)))
;;;         (threshold (psp-threshold-score ctask)))
;;;    (dbug :top "submitting up to ~d psps" num-to-submit)
;;;    (dbug :top "sorted-psps: ~s" sorted-psps)
;;;    (dbug :top "scores: ~s" (mapcar #'score sorted-psps))
;;;    (iter (for pc in sorted-psps)
;;;      (for i from 1 to num-to-submit)
;;;      (when (> (score pc) threshold)
;;;        (submit-patch pc)))))

(defun submit-delayed-psps-full (ctask)
  (dbug :top "submit-delayed-psps-full")
  (let ((sorted-psps (ready-psps-sorted ctask))
        (num-pwps (num-patches-submitted ctask)))
    (dbug :top "  sorted-psps: ~s" sorted-psps)
    (dbug :top "  scores: ~s" (mapcar #'score sorted-psps))
    (dbug :top "  submitting mid-to-high-confidence psps.")
    (iter (for i from num-pwps to *mid-confidence-psp-limit*)
      (while sorted-psps)
      (while (> (score (first sorted-psps)) *default-psp-threshold-score*))
      (submit-patch (pop sorted-psps)))
    (dbug :top "  submitting only high-confidence psps.")
    (iter (for i from (max *mid-confidence-psp-limit* num-pwps) to *high-confidence-psp-limit*)
      (while sorted-psps)
      (while (> (score (first sorted-psps)) *high-confidence-psp-threshold*))
      (submit-patch (pop sorted-psps)))))

(defun ready-psps-sorted (ctask)
  (let ((psps (iter (for pc in (all-patch-cands ctask))
                (when (ready-to-submit-delayed-psp-p pc)
                  (collect pc)))))
    (sort psps #'> :key #'score)))

(defun submit-patch (pc)
  (let ((cmd-str (format nil "~a/afc-submit.py ~a --task_id ~a --patch ~a --lax_id ~a"
			     *lax-tools*
			     "patch"
                             (task-id (target pc))
			     (patch-file pc)
                             (id pc)
                             )))
        (dbug :top "Submitting patch to AFC: ~s" pc)
        (dbug :top "  cmd: ~s" cmd-str)
	;; FIXME: Once avail, we should only set this slot once submit cmd is known to have succeeded.
	(setf (submitted-p pc) t)
        (uiop:launch-program cmd-str :output *standard-output*
                                     :error-output :output
                                     :ignore-error-status t)))

(defmethod score ((pc patch-cand))
  (iter (for svh in (src-vuln-hyps pc))
    (maximize (score svh))))
    
;;;(defun iter-demo (start mid limit list &key (s1-lim 5) (s2-lim 15))
;;;  (iter (for i from start to mid)
;;;    (while list)
;;;    (while (> (first list) s1-lim))
;;;    (format t "stage 1: ~s ~s~%" i (pop list)))
;;;  (iter (for i from (max mid start) to limit)
;;;    (while list)
;;;    (while (> (first list) s2-lim))
;;;    (format t "stage 2: ~s ~s~%" i (pop list))))
