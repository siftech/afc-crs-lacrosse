(in-package #:common-lisp-user)

(defpackage #:asdf-cl-variates (:use #:cl #:asdf))
(in-package #:asdf-cl-variates)

;; try hard
;; (unless (find-system 'asdf-system-connections nil)
;;   (warn "The CL-Variates system would enjoy having ~
;; asdf-system-connections around. See 
;; http://www.cliki.net/asdf-system-connections for details and download
;; instructions."))
(when (find-system 'asdf-system-connections nil)
  (operate 'load-op 'asdf-system-connections))

(defsystem cl-variates 
  :version "0.9.0"
  :author "Gary King <gwking@metabang.com>"
  :maintainer "Gary Warren King <gwking@metabang.com>"
  :licence "MIT Style License"
  :description "Portable Common Lisp Random Number Generation."

  :components ((:static-file "COPYING")
               (:module 
                "dev"
                :components ((:file "package")
                             (:file "variates" 
                                    :depends-on ("package"))))
;               (:module 
;                "website"
;                :components ((:module 
;                              "source"
;                              :components ((:static-file "index.lml")))))
               )
  :in-order-to ((test-op (load-op :cl-variates-test)))
  :perform (test-op :after (op c)
                    (funcall
                     (intern (symbol-name '#:run-tests) :lift)
                     :config :generic))
  :depends-on ())

(defmethod operation-done-p 
           ((o test-op) (c (eql (find-system 'cl-variates))))
  (values nil))

#+asdf-system-connections
(asdf:defsystem-connection variates-and-metacopy
  :requires (cl-variates metacopy)
  :components ((:module "dev"
                        :components ((:file "copying")))))

