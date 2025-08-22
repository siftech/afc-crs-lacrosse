(in-package :common-lisp-user)

;;;   This package mimics the bits of Allegro's excl package necessary for CIRCA.

(unless (or (uiop:featurep :ccl)
            (uiop:featurep :sbcl))
  (error "EXCL portability not supported for this lisp (only for CCL or SBCL)."))

(defpackage :excl
  (:use :common-lisp)
  (:export
   #:run-shell-command
   ;; Not defined  
   ;; #:stream-terpri
   ;; #:stream-force-output
   )
  )

(push :excl *features*)
