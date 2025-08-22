(in-package :fuzzbomb)

(debug-list-value :sieve "Outputs for breathtaking viz.")

;;(pushnew :sieve *debug-list*)

(defun sieve-node (elt &key (shape "none") (color "white"))
  (dbug :sieve "NODE [[~A]] SHAPE ~A COLOR ~A" elt shape color))

(defun sieve-edge (src-elt targ-elt &key (label ""))
  (dbug :sieve "EDGE [[~A]] [[~A]] [[~A]]" src-elt label targ-elt))

(defun sieve-subgraph (elt)
  (dbug :sieve "SUBGRAPH [[~A]]" elt))

(defun sieve-member (member subgraph)
  (dbug :sieve "MEMBER [[~A]] [[~A]]" member subgraph))
