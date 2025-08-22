;;; -------------------------------------------------------------------------
;;; vuln-cand-class.lisp
;;; - target objects that AMP works on...
;;; -------------------------------------------------------------------------
;;; COPYRIGHT START
;;; COPYRIGHT END
;;; -------------------------------------------------------------------------

(cl:in-package :fuzzbomb)

(defclass vuln-cand (named-object)
  (
   (id :initarg :id :initform -1 :accessor id
       :documentation "Unique lax id."
       :type int)
   (cid ;;  :type string
       :accessor cid
       :documentation "Unique competition id assigned by aixcc infra when this is submitted.")
   (blob :initarg :blob :initform nil :accessor blob
         :documentation "Path to a blob containing a crashing input."
         :type (or null string))
   (blob-octets :accessor blob-octets
                :documentation "Contents of blob file as slurped by slurp-binary-file."
                :type (or null octet-array))
   (bic :initarg :bic :initform nil :accessor bic
        :documentation "String containing the hash of the BIC."
        :type (or null string))
   (cp-src
    ;;:type string
    :documentation "Path component distinguishing this repo."
    :initform nil
    :accessor cp-src)
   (files-in-commit :initarg :files-in-commit :initform nil :accessor files-in-commit
        :documentation "list of strings naming the files-in-commit of the BIC.")
   (lax2-reproduce-path
     :initarg :reproduce-path
     :initform nil
     :type (or null string)
     :accessor reproduce-path
     :documentation "The path to the output from AIxCC finals' infra/helper.py reproduce. Note that this probably has a bunch of junk in it; they run it with a PTY in raw mode, so it's using \\r\\n newlines, ANSI escape sequences, the full nine yards...")
   (sanitizer :initarg :sanitizer :initform nil :accessor sanitizer
                 :documentation "String containing the sanitizer."
;;; oh jeez, stop with the Fing type decls, this is lisp, lemme do what I want.
;;; In particular, the Fing IDs look like keywords, so just let em. sheesh...I aint got time to bleed

        ;;       :type (or null string)
                 )
   (engine :initarg :engine :initform nil :accessor engine
                 :documentation "String containing the engine."
        )
   (harness :initarg :harness :initform nil :accessor harness
               :documentation "String containing the harness."
;;; Again with the type decls..what, I gotta smack you?
;;             :type (or null string)
        )
   (harness-binary :initarg :harness-binary :initform nil :accessor harness-binary
               :documentation "String containing the harness-binary."
                   :type (or null string))

   ;; submission status - this could be a mixin, but not now
   (submitted-p :initarg :submitted-p :initform nil :accessor submitted-p
                :documentation "Set to t after submission.")
   (rejected-p :initarg :rejected-p :initform nil :accessor rejected-p
               :documentation "Set to t when rejected.")
   (passed-p :initarg :passed-p :initform nil :accessor passed-p
             :documentation "Set to t when passed.")
   ;;   For now, python handles "accepted" state.
   ;;(accepted-p :initform nil :accessor accepted-p
   ;;            :documentation "Set to t when accepted.")
   (status :initform nil :accessor status
           :documentation "Raw status returned by aixcc infra OR :deadline-exceeded.")

   (cpv-uuid :initarg cpv-uuid :initform nil :accessor cpv-uuid
             :documentation "String containing the cpv-uuid from capi.  Non-nil indicates acceptance."
             :type (or null string))
   (patch-cands :initarg :patch-cands :initform nil :accessor patch-cands
                :documentation "List of patch-cand objects."
                :type list)

   (submitted-patch :initarg :submitted-patch :initform nil :accessor submitted-patch
                    :documentation "The patch-cand object chosen for submission."
                    ;;:type patch-cand
                    )
   ;; NOT gp-ack ... look at the gp-response in the submitted-patch
   (target :initarg :target :initform nil :accessor target
           :documentation "Ptr to the target and all of its state."
           :type (or null lacrosse-cp-target))
   (shared-data-dir :initarg :shared-data-dir :initform nil :accessor shared-data-dir
               :documentation "String containing path to shared-data-dir."
               ;;:type (or null string)
               )
   ))

(defmethod blob-p ((vuln-cand vuln-cand))
  (not (not (blob vuln-cand))))

(defmethod bic-p ((vuln-cand vuln-cand))
  (not (not (bic vuln-cand))))

(defmethod initialize-instance :after ((vuln-cand vuln-cand) &key)
  ;; Unless id is supplied to make-instance, use named-object name.
  ;; It's intended that only optimus assign the id.
  (when (and (numberp (id vuln-cand))
             (= -1 (id vuln-cand)))
    (setf (id vuln-cand) (name vuln-cand)))
  (add-object-for-lax-id vuln-cand (id vuln-cand))
  (let ((shared-data-dir (format nil "~a/vuln-cands/~6,'0d/" (shared-path (target vuln-cand)) (id vuln-cand))))
    (setf (shared-data-dir vuln-cand) shared-data-dir)
    (ensure-directories-exist shared-data-dir)))

(defmethod print-object ((vuln-cand vuln-cand) str)
  (print-unreadable-object (vuln-cand str :type t)
    (format str "[~a] id: ~a  blob: ~a  sanitizer: ~a  bic: ~a"
            (name vuln-cand) (id vuln-cand) (blob vuln-cand) (sanitizer vuln-cand) (bic vuln-cand))))

(defmethod lp-string ((vuln-cand vuln-cand))
  (format nil "<VULN-CAND: ~a  blob: ~a  sanitizer: ~a  bic: ~a>"
          (id vuln-cand) (blob-p vuln-cand) (not (not (sanitizer vuln-cand))) (bic-p vuln-cand)))

;;(defmethod shared-blob-dir ((vuln-cand vuln-cand))
;;  (format nil "~a/" *experiment-dir*

(defmethod shared-bic-path-string ((vuln-cand vuln-cand))
  (format nil "~a/bic" (shared-data-dir vuln-cand)))

(defmethod write-shared-bic ((vuln-cand vuln-cand))
  (with-open-file (out (shared-bic-path-string vuln-cand) :direction :output :if-exists :supersede)
    (format out (bic vuln-cand))))

(defmethod set-blob-octets ((vuln-cand vuln-cand))
  "Set the blob-octets by reading the blog file."
  ;; Would make sense to this in an init method, but there are some ordering
  ;; dependencies that I'm not going to bother fixing cleaning up right now.  
  (setf (blob-octets vuln-cand) (slurp-binary-file (blob vuln-cand))))

(defmethod ready-to-submit-p ((vuln-cand vuln-cand))
  (cond ((submitted-p vuln-cand)
         nil)
        ;;   Deadline has just been checked by process-vuln-cand-msg; not checking here.
        ;;((deadline-exceeded-p (target vuln-cand))
        ;; (dbug :top  "Deadline exceeded in ready-to-submit-p (vuln-cand), dropping ~s ~a" (target vuln-cand) (short-description (target vuln-cand)))
        ;; (setf (status vuln-cand) :deadline-exceeded)
        ;; nil)
        (t t)))

  ;;(describe vuln-cand)
;;;  (let ((submitted-bics (submitted-bics (asc-target))))
;;;    (and (bic vuln-cand)
;;;         (sanitizer vuln-cand)
;;;         (harness-id vuln-cand)
;;;         (blob vuln-cand)
;;;         (not (submitted-p vuln-cand))
;;;         (not (member (bic vuln-cand) submitted-bics :test #'string-equal))
;;;         (not (rejected-p vuln-cand))
;;;         (not (equiv-to-rejected-p vuln-cand))
;;;         )))

(defmethod equiv-to-rejected-p ((vc vuln-cand))
  "Check bic and blob against previously rejected cands.
   Pretty crude, but will keep us from really dumb submits."
  (member vc (remove-if-not #'rejected-p (vuln-cands (target vc)))
          :test #'same-blob-p))

(defmethod same-blob-p ((vc-1 vuln-cand) (vc-2 vuln-cand))
  ;;(same-file-contents-p (blob vc-1) (blob vc-2))
  (equalp (blob-octets vc-1) (blob-octets vc-2))
  )

;;; This should be called only in scope of dont-error or some error handler,
;;; because it can raise an error.
(defun consider-submit-vc (target)
  (dbug :top "consider-submit-vc")
  (dbug :top "previously submitted bics: ~s" (submitted-bics target))
  ;;   FIXME this will likely submit *every* vc once, assuming pushes to vuln-cands list.
  ;;   Deadline has just been checked by caller; not checking here.
  (let ((vc-to-submit (find-if #'ready-to-submit-p (vuln-cands target))))
    (when (and vc-to-submit
               (not (submitted-p vc-to-submit)))
      (let ((cmd-str (format nil "~a/afc-submit.py ~a --testblob ~a --engine ~a --fuzzer ~a --sanitizer ~a --task_id ~a --lax_id ~a"
                             *lax-tools*
                             "pov"
                             (blob vc-to-submit)
                             (string-downcase (symbol-name (engine vc-to-submit)))
                             (harness vc-to-submit)
                             (string-downcase (symbol-name (sanitizer vc-to-submit)))
                             (task-id target)
                             (id vc-to-submit)
                             )))

        (dbug :top "submitting vc: ~s" vc-to-submit)
        (dbug :top "  cmd: ~s" cmd-str)
        (uiop:launch-program cmd-str :output *standard-output*
                                     :error-output :output
                                     :ignore-error-status t)
        (setf (submitted-p vc-to-submit) t)))))

(defmethod find-pc-by-id (id (vuln-cand vuln-cand))
  (find id (patch-cands vuln-cand) :key #'id :test #'=))

(defmethod has-passed-patch-p ((vuln-cand vuln-cand))
  (find-if #'passed-p (patch-cands vuln-cand)))

(defun submitted-bics (target)
  (iterate (for vc in (vuln-cands target))
    (when (and (bic vc)
               (submitted-p vc)
               (not (rejected-p vc)))
      (adjoining (bic vc) test #'string-equal))))

(defmethod spec-slots ((vc vuln-cand))
  '(id blob reproduce-path
    sanitizer engine harness
    submitted-p rejected-p passed-p))

(defmethod spec ((vc vuln-cand))
  (let ((slots (spec-slots vc)))
    (iterate (for slot in slots)
      (collect (list (intern (symbol-name slot) :keyword)
                     (maybe-unpack-slot-val (funcall slot vc)))))))

(defmethod new-vuln-cand-from-spec ((spec list) (target lax-ctask))
  "Given a list defining a vuln-cand,
   create a new target object and return it."
  ;; The only relevant elements of the spec are the cp_path and the id (if supplied).
  (dbug :target "new-vuln-cand-from-spec: ~s ~a" spec target)
  ;; avoid infinite method loop!
  (unless spec
    (error "nil is not a spec!"))
  (push `(:target ,target) spec)
  (let* ((vuln-cand-class 'vuln-cand)
         ;; flatten one level to turn pairs into keyword args.
         (make-inst-args (append (list vuln-cand-class)
                                 (musliner:flatten-one-level spec)))
        )
    (dbug :target "make-inst-args: ~s" make-inst-args)
    (let ((new-vuln-cand (apply #'make-instance make-inst-args)))
      new-vuln-cand)))
