;;; -------------------------------------------------------------------------
;;; lax-opentelemetry.lisp
;;; - functions for calling our scripts to send OpenTelemetry messages in the
;;;   AIxCC finals.
;;; -------------------------------------------------------------------------
;;; COPYRIGHT START
;;; COPYRIGHT END
;;; -------------------------------------------------------------------------

(cl:in-package :fuzzbomb)

(defgeneric format-for-telemetry (obj))

(defmethod format-for-telemetry ((obj t))
  ;; TODO: This should use the real serialization.
  (format nil "~s" obj))

(defun lax-action-category? (action-category)
  (member action-category '(:static-analysis :dynamic-analysis :fuzzing
                            :program-analysis :building :input-generation
                            :patch-generation :testing :scoring-submission)))

(defvar *required-telem-env-vars* '("OTEL_EXPORTER_OTLP_ENDPOINT"
                                    "OTEL_EXPORTER_OTLP_HEADERS"
                                    "OTEL_EXPORTER_OTLP_PROTOCOL")
  "List of env vars which must be and non-empty for telem.")

(defun init-telemetry ()
  ;; If env vars are not set, warn and turn off telemtry.
  (when (and *use-telemetry*
             (not (telem-vars-set-p)))
    (warn "*use-telemetry* was t, but env vars are not set.  Setting *use-telemetry* to nil.")
    (setf *use-telemetry* nil)))
    
(defun telem-vars-set-p ()
  (every #'(lambda (varname)
             (and (getenv varname)
                  (not (string-equal "" (getenv varname)))))
         *required-telem-env-vars*))

(defun send-telemetry-event (action-category action-name &optional (metadata nil metadata?))
  (cond ((not *use-telemetry*)
         (dbug :top "Since *use-telemetry* is nil, NOT SENDING ~s" `(send-telemetry-event ,action-category ,action-name ,metadata)))
        ;; else, *use-telemetry* is non-nil
        (t
         (dbug :top "~s" `(send-telemetry-event ,action-category ,action-name ,metadata))
         (dont-error
           (unless (lax-action-category? action-category)
             (dbug :top "~s: Invalid action category: ~s" 'send-telemetry-event action-category))
           (unless metadata?
             (dbug :top "~s: Missing metadata" 'send-telemetry-event))
           (unless (stringp action-name)
             (setf action-name (format-for-telemetry action-name)))
           (uiop:run-program
            (list
             "/lacrosse/code/tools/send-telemetry-event.py"
             (dashes-to-underscores (string-downcase (symbol-name action-category)))
             action-name
             action-name
             (cl-json:encode-json-alist-to-string metadata))
            :output t
            :error-output :output)))))
