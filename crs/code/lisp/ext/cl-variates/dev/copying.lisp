;;;-*- Mode: Lisp; Package: cl-variates -*-

#| simple-header

Author: Gary King

DISCUSSION

|#
(in-package #:cl-variates)

;;; ---------------------------------------------------------------------------

(metacopy:defcopy-methods basic-random-number-generator :set-all t)

(metacopy:defcopy-methods ran1-random-number-generator :copy-all t)

(metacopy:defcopy-methods ranq1-random-number-generator :copy-all t)

(metacopy:defcopy-methods random-number-generation-mixin :copy-all t)


;;; ***************************************************************************
;;; *                              End of File                                *
;;; ***************************************************************************