(in-package :persistence)

;;;   Using persistence w/ named-objects is
;;; tricky.  Probably best just not to!  But, it's possible, but
;;; considering the following issues is left as an exercise to the
;;; user: You could remove the "name" field from the things-to-save
;;; list, but then you'll have a named-object w an unbound "name" slot
;;; which doesn't appear in the musliner::*objects* list and you'll
;;; have to figure out how to deal with that.  If you decide to save/restore
;;; the name slot, then you'll have to figure out how to deal with that.

#-persist-named-objects
(defmethod things-to-save :before ((obj musliner:named-object))
  (error "Persisting named-objects is not allowed."))

#+persist-named-objects
(defmethod things-to-save :around ((obj musliner:named-object))
  (let ((retval (call-next-method)))
    (delete 'musliner:name retval))
  )
