;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; COPYRIGHT START
;;; COPYRIGHT END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package :fuzzbomb)
(eval-when (:compile-toplevel) (optimization-boilerplate))

;;; This makes the <classname>-p predicates for *all* future classes, not just subclasses of my-class (as the old model.lisp).
;;; This version, for CCL only at this point, does mostly the right thing, but CCL doesn't follow the standard
;;; that CLOS MOP fns should be in the CLOS: package.
#+ccl
(defmethod CCL::add-direct-subclass :after (superclass subclass)
  (declare (ignore superclass))
  ;; automatically defines classname-p class determination predicate
  (setf (symbol-function (intern (concatenate 'string
                                              (symbol-name (class-name subclass))
                                              "-P")
                                 :fuzzbomb))
        #'(lambda (x) (typep x (class-name subclass)))))
