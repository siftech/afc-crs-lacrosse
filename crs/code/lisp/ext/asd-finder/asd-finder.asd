;;; Smart Information Flow Technologies Copyright 2008 Unpublished work
;;;
(defpackage :asd-finder-asd
    (:use :common-lisp :asdf)
    )
(in-package :asd-finder-asd)

;;;
;;; The main system.
;;;
(defsystem :asd-finder
    :serial t
    :version "4.0.0"
    :components ((:file "package")
                 (:file "asd-finder")))

#+allegro
(defmethod perform :around ((op compile-op) (c (eql (asdf:find-component (asdf:find-system "asd-finder") "asd-finder"))))
  (let ((excl:*warn-on-nested-reader-conditionals* nil))
    (call-next-method)))


