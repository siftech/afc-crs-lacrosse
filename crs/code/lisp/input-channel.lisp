(cl:in-package :fuzzbomb)


(defclass input-channel ()
  (
   ;; required command line arguments, strings that should
   ;; be passed to the program itself.
   ;; I put this in the main class as it seems like most subclasses
   ;; make use of cli-args in some manner.
   (cli-args :initform nil :initarg :cli-args :accessor cli-args)
   ))

(defmethod cmdline-args ((c input-channel) args) 
  (format nil "~@[~A~] ~@[~A~]" (cli-args c) args))

;; The most basic input channel, no slots, just telling you it's standard input
(defclass stdin-channel (input-channel)
  ())

(defmethod cmdline-args ((c stdin-channel) args) 
  (format nil "~@[~A~] < ~A" (cli-args c) args))

;; We take in network io, presumbably some network io details might
;; appear in these slots
(defclass network-io-channel (input-channel)
  ())

;; For when the filename is specific to the program
(defclass named-file-channel (input-channel)
  (
   ;; This should refer to any file where reads from that file
   ;; are treated symbolically, including files passed in as arguments
   (filename :initform nil :initarg :filename :accessor filename)
   ))

;; For when file is taken in as input
;; Cli arguments to pass file in should be put in cli-args slot
(defclass file-channel (input-channel)
  ())

;;(defmethod cmdline-args ((a target) args) (cmdline-args (input-channel a) args))

(defun deser-input-channel (channel)
  "Gives back an input channel class instance from the serialized version in the target-msg."
  (let ((class-name (getassoc :type channel :proper t)))
    (apply #'make-instance
           (car class-name) (musliner:flatten-one-level (rest channel)))))
