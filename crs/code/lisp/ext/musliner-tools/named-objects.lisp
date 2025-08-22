;;; -------------------------------------------------------------------------
;;; named-objects.lisp
;;; - $Header: /projects/coordinators/CVS.r/ALL/code/ext/lisp-libs/musliner-tools/named-objects.lisp,v 1.2 2006/12/22 23:11:15 musliner Exp $
;;; -------------------------------------------------------------------------
;;; COPYRIGHT START
;;; COPYRIGHT END
;;; -------------------------------------------------------------------------


(in-package :musliner)

(defvar *musliner-readtable*
  (copy-readtable nil))

(defvar cl-user::*myreadtable*)

;;; in Allegro 8.2 the default readtable is read-only, so must do this... but actually
;;; only required if you load during clinit.cl, b/c after that they give you a writeable readtable
;;; themselves... strange but true...
(when (and (boundp 'cl-user::*myreadtable*)
           cl-user::*myreadtable*)
  ;; assuming this is the one you want...
  (setf *musliner-readtable* (copy-readtable cl-user::*myreadtable*)))

#+allegro
(tpl:setq-default *readtable* *musliner-readtable*)

(setf *readtable* *musliner-readtable*)

;;; -------------------------------------------------------------------------
;;; NAMED-OBJECT
;;; -------------------------------------------------------------------------
;;; Everything that has a name... an integer, useful for debugging
;;; lookup etc.  Note that these will get pushed onto a global list,
;;; so you need to use nuke-object whenever deleting one forever.
;;; Interesting buglet here.  We (alright, I) assumed that the initial
;;; value of the array would be NIL, because that's what it was in
;;; Allegro.  Well, not in CMUCL!  
(defun make-object-store (&optional (initial-size 1000))
  (make-array initial-size :initial-element nil :adjustable t :fill-pointer 0))

(defvar *objects* (make-object-store))



;;;-------------------------------------------------------------------------
;;;---------------------------------------------------------------------------
;;; I pulled the :name initarg, since this should only be handled by the
;;; initialize instance method and changed the name accessor to a reader,
;;; on the grounds that no one should be CHANGING the name of an object...
;;; [1997/08/13:goldman]
;;; I made NAMED-OBJECTS ****NOT**** be MY-CLASSes, on the grounds that we
;;; do not want to cache all named objects...  This could be wrong.
;;; [1999/03/08:goldman]
;;;---------------------------------------------------------------------------

(defclass named-object ()
     ((name :reader name))
  (:documentation "NAMED-OBJECT is actually a misnomer.  These
objects have NUMBERS (integers > 0) that are used to retrieve
them.  By and large, it seems that anything that's a NAMED-OBJECTS should
also be a BT-OBJECT (qv.).  The exception is state objects, because we don't
undo the refinement of abstract states.  If you have an object that's
only one of these two, you may have an incipient bug.")
  )

;;;---------------------------------------------------------------------------
;;; The following was automatically created when NAMED-OBJECT was a MY-CLASS.
;;; [1999/03/08:goldman]
;;;---------------------------------------------------------------------------

(declaim (inline named-object-p))
(defun named-object-p (x)
  (typep x 'named-object))


;;;---------------------------------------------------------------------------
;;; The following fixes a bug that can happen if we allow the user to
;;; assign object names him/herself. [1997/09/08:goldman]
;;;---------------------------------------------------------------------------


(defun previously-existing-object-p (name)
  (and (< name (fill-pointer *objects*))
       (aref *objects* name)))

;;;-------------------------------------------------------------------------
(defmethod print-object ((o named-object) (s stream))
  (format s "#<~A ~A>" (class-name (class-of o)) (name o)))

;;;-------------------------------------------------------------------------
(defmethod initialize-instance :before ((o named-object)
                                        &key name)
  (cond ((and name (previously-existing-object-p name))
         (error "Attempting to redefine object name ~A."
                name)
         )
        (name
         (assert (and (integerp name) (>= name 0)))
         (setf (slot-value o 'name) name)
         (cond ((>= name (array-dimension *objects* 0))
                (adjust-array *objects* (* name 2) :fill-pointer (1+ name)
                              :initial-element nil))
               ((> name (fill-pointer *objects*))
                (setf (fill-pointer *objects*) (1+ name))))
         (setf (aref *objects* name) o)
         )
        ;;else no name supplied
        (t
         (setf (slot-value o 'name)
           (fill-pointer *objects*))
         (vector-push-extend o *objects*))))

;;;-------------------------------------------------------------------------
(defun init-objects ()
  (setf *objects* (make-object-store)))

;;;-------------------------------------------------------------------------
(defun find-object (name)
  (aref *objects* name))

;;; this won't work like this, but you get the idea...
;;(defun find-instances (classname)
;;  (my-remove-if-not #'equal *objects* classname :key #'class-of))

;;;-------------------------------------------------------------------------
;;; This hack makes #F123 into a call (find-object 128)

(defun |#F-reader| (stream subchar arg)
  (declare (ignore subchar arg))
  (find-object (read stream t nil t)))

(set-dispatch-macro-character #\# #\F #'|#F-reader|)



;;;-------------------------------------------------------------------------
;;; really really kills the object... avoid memory leaks!

;;;  : 6/00 disabled this b/c we are mixing things up, esp in
;;;     :forward search.  When we backtrack over the creation of a state,
;;;     the edges-out are all getting nuked out of *objects* array, but
;;;     the state itself remains intact and keeps its own pointers to
;;;     edges out.  Then if/when we re-reach the state, it is found on
;;;     the list of states, retrieved, and the edges-out are not recreated
;;;     (the old ones are still there) but the edges are now inaccessible
;;;     via find-object, #F, and the daVinci click-an-edge interactions.

(defun nuke-object (o)
  (declare (ignore o))
;  (dbug :nuke "Nuking object ~A" (name o))
;  (setf (aref *objects* (name o)) nil)
)

