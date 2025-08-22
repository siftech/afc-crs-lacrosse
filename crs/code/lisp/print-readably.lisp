(cl:in-package :fuzzbomb)


(defun |#{-reader| (str char arg)
   (declare (ignore char arg))
     (let ((list (read-delimited-list #\} str t)))
       (let ((type (first list))
         (list (second list)))
         (let ((class (allocate-instance (find-class type))))
           (loop for i in list do
            (setf (slot-value class (car i)) (cdr i)))
           class))))

(set-dispatch-macro-character  #\# #\{ #'|#{-reader|)

(defun get-slots (object)
  ;; thanks to cl-prevalence
  #+openmcl
  (mapcar #'ccl:slot-definition-name
      (#-openmcl-native-threads ccl:class-instance-slots
       #+openmcl-native-threads ccl:class-slots
       (class-of object)))
  #+cmu
  (mapcar #'pcl:slot-definition-name (pcl:class-slots (class-of object)))
  #+sbcl
  (mapcar #'sb-mop:slot-definition-name (sb-mop:class-slots (class-of object)))
  #+lispworks
  (mapcar #'hcl:slot-definition-name (hcl:class-slots (class-of object)))
  #+allegro
  (mapcar #'mop:slot-definition-name (mop:class-slots (class-of object)))
  #+sbcl
  (mapcar #'sb-mop:slot-definition-name (sb-mop:class-slots (class-of object)))
  #+clisp
  (mapcar #'clos:slot-definition-name (clos:class-slots (class-of object)))
  #-(or openmcl cmu lispworks allegro sbcl clisp)
  (error "not yet implemented"))

;;;   Unfort, this doesn't handle circular refs
;;; and therefore should only be used for specific classes.
;;;(defmethod print-object ((object standard-object) stream)
;;;  (format stream "#{~s ~s}" (type-of object)
;;;      (loop for i in (get-slots object)
;;;         when (slot-boundp object i) collect (cons i (slot-value object i)))))


;; (defclass foo ()
;;   ((fooslot :initarg :fooslot :accessor fooslot)))

;; (defclass bar ()
;;   ((barslot :initarg :barslot :accessor barslot)))

;; (setf a (make-instance 'foo :fooslot 1))
;; (setf b (make-instance 'bar :barslot a))

;; (fooslot a)
;; (barslot b)

;; (setf s1 (format nil "~S" a))
;; (setf s2 (format nil "~S" b))

;; (setf a1 (read-from-string s1))
;; (setf b1 (read-from-string s2))

;; (fooslot a1)
;; (barslot b1)
;; (fooslot (barslot b1))
