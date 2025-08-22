(in-package :common-lisp-user)

(defpackage :mp
  (:use :acl-compat.mp)
  (:export
   #:process-kill
   #:process-run-function
   #:wait-for-input-available
   #:with-timeout
   #:without-scheduling
   #:*current-process*
   #:*all-processes*
   )
  )
(push :mp *features*)
