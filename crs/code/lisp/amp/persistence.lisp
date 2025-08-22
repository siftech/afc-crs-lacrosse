;;; ancient history at bottom of file
(in-package :persistence)

(defvar *oid-counter* 0)
(defvar *symbol-table* nil)
(defvar *initial-table-size* 400000)

(declaim (special *obj-counter*))

(defun prin2 (obj &optional (output-stream *standard-output*))
  "Like prin1, but with a whitespace following."
  (prin1 obj output-stream)
  (write-char #\space output-stream))

(defmethod all-bound-slots (inst)
  ;; This is code from the browser.  Get the list of slots and values of an
  ;; object instance. The returned list is like this: (slot-name slot-value
  ;; slot-name slot-value ....)
  ;; [JimC 01/07/91 09:05] Modified to just return the bound slots.
  ;; [KyleN 04/12/93] Modified to call the things-to-save function,
  ;; this returned list is then called with the instance to collect the
  ;; values to save.
  (mapcan #'(lambda (name)
              (cond
                ;; no slots to save
                ((null name)
                 (list name))

                ;; slot to save
                ((and (standard-object-p inst)
                      (slot-exists-p inst name)
                      (slot-boundp inst name))
                 (list (cons name (slot-value inst name))))

                ;; call a function
                ((or (not (standard-object-p inst))
                     (not (slot-exists-p inst name)))
                 (list (cons name (funcall name inst))))
                ))
          (things-to-save inst)))

(defmethod set-all-bound-slots (inst slots)
  (mapc #'(lambda (slot)
            (let ((name (car slot))
                  (value (cdr slot)))
              (setf (slot-value inst name) value)))
        slots))

(defmethod things-to-save ((obj t))
  nil)

(defmethod things-to-save ((obj standard-object))
  (all-slot-names obj))

;; [pelican:20010116.0931CST] ACL 6.0 puts old clos internal
;; stuff in excl.  There may be another choice in aclmop package.
#+(and allegro-version>= (version>= 6))
(defmethod all-slot-names ((inst standard-object))
  (excl::object-instance-slot-names inst))

#+(and allegro-version>= (version>= 6))
(defun standard-object-p (thing)
  (excl::standard-instance-p thing))

;; [pelican:20010116.0932CST] This works for ACL 4 and ACL 5.
#+(and allegro-version<= (version<= 6))
(defmethod all-slot-names ((inst standard-object))
  (clos::object-instance-slot-names inst))

#+(and allegro-version<= (version<= 6))
(defun standard-object-p (thing)
  (clos::standard-class-p (class-of thing)))

;; for lucid
#+lcl4.1
(defmethod all-slot-names ((inst standard-object))
  (mapcar #'CLOS::SLOT-DEFINITION-NAME (CLOS:CLASS-SLOTS (class-of inst))))

#+lucid
(defun standard-object-p (thing)
  (SYSTEM:STANDARD-OBJECT-P thing))

#+pcl
;; I'm nervously modifying this  
(defmethod all-slot-names ((inst pcl:standard-object))
  (flet ((slot-name (slot) (slot-value slot 'pcl::name)))
    (mapcar #'slot-name (PCL:CLASS-SLOTS (class-of inst)))))

#+pcl
(defun standard-object-p (thing)
  (typep thing 'pcl:standard-object))

#+ccl
(defmethod all-slot-names ((inst standard-object))
    (mapcar #'ccl::slot-definition-name (ccl:class-slots (class-of inst))))

#+sbcl
(defun standard-object-p (thing)
  (typep thing 'cl:standard-object))

#+sbcl
(defmethod all-slot-names ((inst standard-object))
    (mapcar #'sb-mop:slot-definition-name (sb-mop:class-slots (class-of inst))))


;;;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;; This is the top level driver for save-all. This is the interface that
;;; everyone should use.
;;;   root :  the top level itom object
;;;   filename : a string that specifies the file in shich you want the object
;;;      base stored.
;;-;;(defun save (root filename &key (initial-table-size *initial-table-size*)
;;-;;                                (quiet nil))
;;-;;  (with-encoded-file (p-stream (merge-pathnames filename) :direction :output)
;;-;;    (let ((*oid-counter* 0)
;;-;;          (*symbol-table* nil)
;;-;;          (*package* (find-package #.(package-name *package*))))
;;-;;      (save-all root p-stream
;;-;;                :reset t
;;-;;                :initial-table-size initial-table-size
;;-;;                :quiet quiet)
;;-;;      (unless quiet
;;-;;        (format t "save wrote ~a objects.~%" *oid-counter*))))
;;-;;  root)

(defun object-string (root &key (initial-table-size *initial-table-size*)
                             (quiet nil))
  (let ((string-stream (make-string-output-stream)))
    (save-all root
              string-stream
              :reset t
              :initial-table-size initial-table-size
              :quiet quiet)
    (unless quiet
      (format t "save wrote ~a objects.~%" *oid-counter*))
    (get-output-stream-string string-stream)))

(defun build-save-table (root &key (initial-table-size *initial-table-size*))
  ;; creates a table of the objects that are referenced more than once
  ;; for each obj the symbol-table will hold the value -[num-of-references]
  (setf *symbol-table* (make-hash-table :size (max 64 initial-table-size)
                                        :test #'eq))
  (setf *oid-counter* 0)
  (setf *obj-counter* 0)
  (labels ((recurse (obj)
             ;;(format t "bst root: ~A~%" obj)
             (cond
               ;; first handle objects that don't require a lookup
               ((or (numberp root) (stringp root) (symbolp root) (null root))
                nil);; do nothing

               ((consp obj)
                (do ((l obj (when (consp l)
                              (cdr l))))
                    ((null l))
                  (if (consp l)
                      (recurse (car l))
                      (recurse l))))

               ;; now, handle objects, structures and vectors
               (t
                (let ((num (gethash obj *symbol-table*)))
                  (cond
                    (num
                     (let ((score (decf (gethash obj *symbol-table*))))
                       (when (= -2 score)
                         (incf *oid-counter*))))

                    ((things-to-save obj)
                     (setf (gethash obj *symbol-table*) -1)
                     (incf *obj-counter*)
                     (unless (equal (things-to-save obj) '(nil))
                       (recurse (all-bound-slots obj))))

                    ((or (eq (type-of obj) 'simple-vector)
                         (and (consp (type-of obj))
                              (eq (car (type-of obj)) 'simple-array)))
                     (setf (gethash obj *symbol-table*) -1)
                     (incf *obj-counter*)
                     (map nil
                          #'(lambda (item)
                              (recurse item))
                          obj))

                    (t
                     nil)))))))
    (recurse root)
    *symbol-table*))

;;;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;; This function accepts the top level root of an object instance tree,
;;; and writes out all objects and all their slots in such a way that they
;;; can be restored later. Call this with *oid-counter*=0 and
;;; *symbol-table*=nil.  The p-stream is the permanant file stream to which
;;; the object base is written.  CLOS Objects are written in 2 ways: a
;;; definition when they are first seen, and a reference on subsequent
;;; sightings.  Each object is assigned a unique integer OID. A symbol
;;; table is maintained for objects and OIDs, implemented as an assoc list
;;; where the key is the object. The object definitions and references in
;;; the saved file have this syntax, respectively:

;;;     (:%obj <oid> <class name>
;;;          <slot name> <value>
;;;          <slot name> <value>
;;;          <slot name> <value> .....
;;;          )
;;;
;;;     (:%obj-ref <oid>)
;;;
;;; [atkins 09/24/92] added cases below to strip out structures that might
;;; appear as slot values in the model. this was done because kyle's mixins
;;; in the schedule-ui subsystem had pointers to the lispview regions,
;;; which are not CLOS objects, but structures. i strip these out and set
;;; them to nil, since they are not really part of the model, but are
;;; values that are hung out there while scheduling.
;;; [KyleN 04/12/93] modified things to allow structures if the things-to-
;;; save and restore-object methods are defined for the struct.

(defun save-all (root p-stream &key (reset nil)
                                    (initial-table-size *initial-table-size*)
                                    (quiet nil))
  ;;(format t "current root: ~A~%" root)
  (when reset
    (setf *symbol-table* (build-save-table root :initial-table-size initial-table-size))
    (prin2 *oid-counter* p-stream)
    (setf *oid-counter* 0))

  (cond
     ;; Basic lisp types and nil are handled trivially
    ((or (numberp root) (stringp root) (symbolp root) (null root))
     (prin2 root p-stream))

     ;; it is a cons
    ((consp root)
     (write-string "(" p-stream)
     (do ((l root (when (consp l)
                    (cdr l))))
         ((null l))
       (if (consp l)
           (save-all (car l) p-stream)
           (progn
             (write-string " . " p-stream)
             (save-all l p-stream))))
     (write-string ")" p-stream))

    ;; otherwise it may be in the table
    (t
     (let ((oid (gethash root *symbol-table*)))
       ;;(format t "oid: ~A~%" oid)
       (cond
          ;; we've seen this one before, print a reference
        ((plusp oid)
          (write-string "(" p-stream)
          (prin2 :%obj-ref p-stream)
          (prin2 oid p-stream)
          (write-string ")" p-stream))

          ;; first occurence of a CLOS object
          ;; [KyleN 04/12/93] make sure the class can be found in the
          ;; current package and there are things to save for it.  if
          ;; not, we don't save it.
          ;; We have not seen this object before, so allocate an OID,
          ;; remember it in the symbol table, and print an object definition
         ((and (standard-object-p root)
               ;;(find-symbol (class-name (class-of root)))
               (symbolp (class-name (class-of root)))
               (things-to-save root))
          (write-string "(" p-stream)
          (prin2 :%obj p-stream)
          ;; if oid = -1 only one of these appears in the object being saved
          (if (= -1 oid)
              (prin2 0 p-stream)
              (progn
                (setf (gethash root *symbol-table*) (incf *oid-counter*))
                (prin2 *oid-counter* p-stream)))
          (prin2 (class-name (class-of root)) p-stream)
          (unless (equal (things-to-save root) '(nil))
            (save-all (all-bound-slots root) p-stream))
          (write-string ")" p-stream))

         ;; all other stuff, like structures are written out if there is a
         ;; things-to-save method defined for it, otherwise the type is printed
         ;; and the object isn't saved (the default things-to-save behavior)
         ((things-to-save root)
          (write-string "(" p-stream)
          (prin2 :%obj p-stream)
          (if (= -1 oid)
              (prin2 0 p-stream)
              (progn
                (setf (gethash root *symbol-table*) (incf *oid-counter*))
                (prin2 *oid-counter* p-stream)))
          (prin2 (class-name (class-of root)) p-stream)
          (save-all (all-bound-slots root) p-stream)
          (write-string ")" p-stream))

       ;; [MikeS 06/13/94] save the vector and create an object reference
      ((or (eq (type-of root) 'simple-vector)
           (and (consp (type-of root))
                (eq (car (type-of root)) 'simple-array)))
       (write-string "(" p-stream)
       (prin2 :%sv p-stream)
       (if (= -1 oid)
           (prin2 0 p-stream)
           (progn
             (setf (gethash root *symbol-table*) (incf *oid-counter*))
             (prin2 *oid-counter* p-stream)))
       (prin2 (length root) p-stream)
       (map nil
            #'(lambda (item)
                (save-all item p-stream))
            root)
       (write-string ")" p-stream))

       ;; if it doesn't satisfy one of our cases, dump the type to stdout
      (t
       (unless quiet
         (prin2 (type-of root))
         (prin2 nil p-stream))))))))

;;;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;; This is the top level driver for restore-all. This is the interface
;;; that everyone should use. The entire permanent object base file is
;;; read with one lisp read, since it is all one form. This restore list is
;;; what is operated upon during the restore process. This function returns
;;; a ptr to the top level itom object.  The input filename is a string
;;; that specifies the file from which you want the object base restored.
;;-;;(defun restore (filename)
;;-;;  (prog1
;;-;;      (multiple-value-bind (table-size r-list)
;;-;;          (let ((*package* (find-package #.(package-name *package*))))
;;-;;            (with-encoded-file (in-stream (merge-pathnames filename)
;;-;;                                          :direction :input)
;;-;;              (values (read in-stream) (read in-stream))))
;;-;;        (restore-all r-list :reset t :initial-table-size table-size))
;;-;;    (setf *symbol-table* nil)))
;;-;;

(defun restore-from-string (p-string)
  (let ((*package* (find-package #.(package-name *package*))))
    (multiple-value-bind (table-size index)
        (read-from-string p-string)
      (let* ((r-list (read-from-string p-string t nil :start index))
             (objs (restore-all r-list :reset t :initial-table-size table-size)))
        (setf *symbol-table* nil)
        objs))))

;;;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;; This function is the inverse of save-all. It takes an item (a list or
;;; sublist or individual lisp object). On the first call there is one top
;;; level definition that contains lots of other definition and reference
;;; specs. This returns the lisp or CLOS object that is at the top level.
;;; Hence this returns the top level itom object to the outside caller.
;;; Note that the symbol table is keyed by oid as opposed to by object in
;;; save-all.
(defun restore-all (item &key (reset nil) (initial-table-size *initial-table-size*))
  (when reset
    (setf *oid-counter* 0)
    (setf *symbol-table* (make-hash-table :size (max 64 initial-table-size))))
  (cond
    ;; Atomic lisp objects handled trivially. just return them.
    ((atom item)
     item)

    ;; handle the object definitions here
    ((eq (car item) :%obj)
     (let* ((oid (second item))
            (obj-class (third item))
            (obj-slots (fourth item))
            (obj nil))
       (unless (zerop oid)
         (setf (gethash oid *symbol-table*) nil))
       (setf obj (restore-object obj-class obj-slots :oid oid))
       obj))

    ;; handle object references here
    ((eq (car item) :%obj-ref)
     (let ((oid (second item)))
       (multiple-value-bind (obj found-p)
           (gethash oid *symbol-table*)
         (if found-p
             obj
             (error "OID ~s not found" oid)))))

    ;; [MikeS 06/13/94] handle simple-vectors here
    ;; maybe I should have been done via the things-to-save method?
    ((eq (car item) :%sv)
     (let ((oid (second item))
           (vector-length (third item))
           (content-list (cdddr item))
           (obj nil))
       (unless (zerop oid)
         (setf (gethash oid *symbol-table*) obj))
       (setf obj (make-array vector-length))
       (unless (zerop oid)
         (update-pending-list oid obj))
       (map-into obj #'restore-all content-list)
       obj))

    ;; handle conses here
    ((consp item)
     (do ((tail item (when (consp tail)
                       (cdr tail)))
          (new-list nil (if (consp tail)
                            (progn
                              (nconc new-list (list (restore-all (car tail)))))
                            (nconc new-list (restore-all tail)))))
         ((null tail) new-list)))))

(defmethod restore-object ((class t) things-saved &key oid)
  ;; [KyleN 04/12/93] this method will determine how an object of a
  ;; particular class is to be restored.  The default behavior is to
  ;; assume that the things saved for the object were slot values and call
  ;; make-instance with those values.
  ;; [KyleN 04/12/93] copied what was done in the earlier persistence
  ;; versions.
  ;; [MikeS 06/15/94] process the things-saved list: no restore-all on cars
  (let ((obj (allocate-instance (find-class class))))
    (update-pending-list oid obj)
    (set-all-bound-slots obj (restore-cdrs things-saved))
    obj))

(defun restore-cdrs (l)
  ;; [MikeS 06/15/94] several places we use (id . value) pairs - this restores
  ;; only the value (otherwise the recursion on cars of lists misses obj refs & defs)
  (mapcar #'(lambda (pair)
              (cons (car pair) (restore-all (cdr pair))))
          l))

(defun update-pending-list (oid obj)
  (multiple-value-bind (table-entry found-p)
      (gethash oid *symbol-table*)
    (declare (ignore table-entry))
    (when found-p
      (setf (gethash oid *symbol-table*) obj))))

;;;----------------------------------------------------------------------
;;; test data
;;;----------------------------------------------------------------------

(defclass class-one ()
  (
   (slot-a
    :initarg :a
    :accessor a)
   (slot-b
    :initarg :b
    :accessor b)))

(defclass class-two ()
  (
   (slot-a
    :initarg :a
    :accessor a)
   (slot-b
    :initarg :b
    :accessor b)))

;;; File Description:

;;;   [atkins 12/12/90 13:07] This file provides permanance for ITSE. It
;;;   writes out the object base to a file and reads it back in.

;;; History/Bugs/Notes:

;;;   [atkins 12/12/90 13:08] Created. Emailed to me from Jim C.
;;;
;;;   [JimC 01/07/91 13:34] Ported to lucid and cleaned up a bit.
;;;
;;;   [KyleN 04/08/93] Modifed for use with TMM.  Made it more
;;;   generic.  basically, for any given object there are some objects that
;;;   are pointed to by slots that don't have to be written out, e.g.
;;;   they are automatically generated by the application (ui objects,
;;;   internal constraints, etc.).  Furthermore, for a given object, the
;;;   things to be written out may not be slots at all but a list of
;;;   things (e.g. in tmm, list-points is not a slot but is a function
;;;   that return's a set of objects to be saved.).  Finally, certain
;;;   properties about an object may need to be saved that are not slots.
;;;   This is an  attempt to provide that kind of capability.
;;;
;;;   the method things-to-save will take an object and return a list of
;;;   functions/methods to
;;;   which the current object can be applied to collect the objects to be
;;;   saved.  the default behavior for things to save is to save nothing,
;;;   i.e. the object will not be saved (it may be autogen'd upon
;;;   restoration, for example).  If things-to-save returns '(()), (i.e. a
;;;   list containing nil, the object will be saved but no slots will be
;;;   saved for the object.
;;;
;;;   Upon reading the objects back in, when an object instance is to be
;;;   read back in, the method restore-object will be called with the
;;;   current object's class name (a symbol). the default behavior of this
;;;   method is to assume the things saved for the object were slots and
;;;   call make-instance with those slots.
;;;----------------------------------------------------------------------
