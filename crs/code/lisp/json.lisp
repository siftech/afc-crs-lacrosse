;;; -------------------------------------------------------------------------
;;; COPYRIGHT START
;;; Copyright (c) 2019, Smart Information Flow Technologies (SIFT).  Developed with the sponsorship of the Office of Naval Research.
;;; COPYRIGHT END
;;; -------------------------------------------------------------------------

;;; -------------------------------------------------------------------------
;; This hacks in the prototype obj that cl-json wants to have in the json to declare what Lisp
;; class it should populate from a json string.
;; The problem is that it will only handle top-level objects, no nesting.
;; To handle nesting, we'd either have to convince the sender to do the prototype
;; inclusion or have a more complex way to insert them based on some recognition.
;; It might be easier to use the old stuff from CGC.  This is trickier and cooler,
;; for a single object level.

(defun read-json-file-into-class (filename classname)
  (let ((s (slurp-text-file filename))
        (json:*json-symbols-package* nil))
    (setf s (string-left-trim "{ 	" s))	;; trim off whitespace and the opening {
    (setf s (strcat "{\"prototype\": {\"lispPackage\": \"fuzzbomb\",
                              \"lispClass\": \"" classname "\"}," s))
    (json:with-decoder-simple-clos-semantics (json:decode-json-from-string s))))


;;; -------------------------------------------------------------------------
