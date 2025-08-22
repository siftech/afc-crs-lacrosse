;;;-*- Mode: Lisp; Package: VARIATES -*-

#| simple-header

Author: Gary King

DISCUSSION

|#
(in-package #:variates)

(defclass thread-safe-random-number-generator ()
  ())

;;; ---------------------------------------------------------------------------

(defmethod make-generator ((class list) (seed number) &key)
  (let ((class-name (find-matching-rng-class class)))
    (make-instance class-name :random-seed seed)))

;;; ---------------------------------------------------------------------------

(defun find-matching-rng-class (classes)
  (or (u:find-existing-subclass 'basic-random-number-generator classes)
      (u:define-class (u:simple-define-class-name classes)
        classes nil)))

;;; ---------------------------------------------------------------------------

(defmethod next-element :around ((rng thread-safe-random-number-generator))
  (declare (optimize speed))
  (u:without-interrupts (call-next-method)))



;;; ***************************************************************************
;;; *                              End of File                                *
;;; ***************************************************************************