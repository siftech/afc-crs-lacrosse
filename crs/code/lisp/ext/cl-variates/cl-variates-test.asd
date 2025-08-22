(in-package #:common-lisp-user)

(defpackage #:asdf-cl-variates-test (:use #:cl #:asdf))
(in-package #:asdf-cl-variates-test)

(defsystem cl-variates-test 
  :version "0.8.1"
  :author "Gary King <gwking@metabang.com>"
  :maintainer "Gary Warren King <gwking@metabang.com>"
  :licence "MIT Style License"
  :description "Portable Common Lisp Random Number Generation and more."
  :components ((:module 
                "tests"
                :components ((:file "package")
                             (:file "basic-tests" 
                                    :depends-on ("package")))))
  :depends-on (:cl-variates :lift))
