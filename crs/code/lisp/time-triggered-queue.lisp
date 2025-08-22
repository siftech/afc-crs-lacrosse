;;; -------------------------------------------------------------------------
;;; time-triggered-queue.lisp
;;; - really simple implementation of a sorted queue for short lists of
;;;   integer indexed payloads.
;;; -------------------------------------------------------------------------
;;; COPYRIGHT START
;;; COPYRIGHT END
;;; -------------------------------------------------------------------------

(cl:in-package :fuzzbomb)

(defclass tt-elt ()
  (
   (index :initarg :index :accessor index
        :type integer
        :documentation "Integer index for sorting/testing.")
   (payload :initarg :payload :accessor payload)))

(defmethod print-object ((e tt-elt) str)
  (print-unreadable-object (e str :type t)
    ;;(format str "[~d] ~s" (index e) (payload e))
    (format str "[~d]" (index e))
    ))

(defclass tt-queue ()
  (
   (elts :initform nil :initarg :elts :accessor elts
         :type list
         :documentation "List (for now) of sorted tt-elts.")))

(defmethod enqueue ((q tt-queue) (i integer) payload)
  (enqueue-elt q (make-instance 'tt-elt :index i :payload payload)))

(defmethod enqueue-elt ((q tt-queue) (e tt-elt))
  (push e (elts q))
  (sort (elts q) #'< :key #'index))
   
(defmethod first-index ((q tt-queue))
  (assert (not (empty-p q)))
  (index (first (elts q))))

(defmethod dequeue ((q tt-queue))
  (pop (elts q)))

(defmethod empty-p ((q tt-queue))
  (not (elts q)))
