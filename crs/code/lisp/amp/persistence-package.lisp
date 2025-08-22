(in-package :common-lisp-user)

(defpackage :persistence
  (:use :common-lisp)
  #+ccl
  (:import-from #:ccl #:standard-object-p)
  (:export "SAVE"
           "OBJECT-STRING"
           "RESTORE"
           "RESTORE-FROM-STRING")
  )
