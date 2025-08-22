(in-package :common-lisp-user)

(defpackage :asd-finder
    (:use :common-lisp)
    (:export #:asd-finder)
    #+allegro
    (:import-from #:excl #:compile-re #:match-re))



