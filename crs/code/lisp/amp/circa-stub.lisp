;;; -------------------------------------------------------------------------
;;; $Id: circa-stub.lisp 1814 2012-05-16 00:34:53Z tzimmerman $
;;; -------------------------------------------------------------------------
;;; COPYRIGHT START
;;; Copyright (c) 2012, Smart Information Flow Technologies (SIFT). Developed with the sponsorship of the Defense Advanced Research Projects Agency (DARPA) and Air Force Research Laboratory.  
;;; COPYRIGHT END
;;; -------------------------------------------------------------------------
;;; Stub fcns that allow FB to load only its needed CIRCA/AMP files

(in-package "CL-USER")
;; (eval-when (compile) (optimization-boilerplate))

;;;--- From planner.lisp 

(defun current-graph ()
  "Returns all states, not just the reachable ones.
For compatibility with Classic CIRCA."
 ;; *current-graph*)
  )

;;; -- From abstract-planner.lisp
;;;  Called in goal-class.lisp, this stubs out methods for abstract-state and feature-set objs
(defun necessarily-satisfies-p (obj1 obj2)
  (declare (ignore obj1 obj2)))

;;;  stubbing this csm method as a fcn, since FB won't call it anyway
;;;  : note we have some nasty overloading of this function here,
;;; using both feature lists and state objects as inputs to a function
;;; name "state-xxx".  This is gonna cause problems w/ the hybrid stuff,
;;; where features have to be compared differently.

;;(defgeneric state-possibly-satisfies-p (obj features)
;;  (:documentation
;;  "Does the OBJ possibly satisfy the FEATURES?"))

(defun state-possibly-satisfies-p (obj features)
  (declare (ignore obj features))
  )

;; [tzim] another defmethod defined here as fcn (unused in FB)
(defun find-applicable-transitions (x y z)
  (declare (ignore x y z)))


;; --- Fcns defined in trans-class.lisp
;;  [tzim] csm/trans-class is irrelevant to FB and requires several other 
;;     also irrelevant csm files.
;; Note: if trans-class is loaded in FB package, find-instance-by-name
;;   will clash with GBB fcn.

(defun find-trans-by-name (name &optional error-p)
  (declare (ignore name error-p)))

;;; [tzim] the above stubs out 3 trans-class methods
;;(defmethod find-trans-by-name ((name symbol) &optional (error-p t))
;;  (declare (ignore name error-p)))

;;(defmethod find-trans-by-name ((name string) &optional (error-p t))
;;  (find-instance-by-name 'trans (musliner::fix-dashes name) error-p))

;;(defmethod find-trans-by-name ((name symbol) &optional (error-p t))
;;  (find-instance-by-name 'trans (musliner::fix-dashes (symbol-name name)) error-p))


;;; Stub out some trans-class fcns called in state-class:
(defun ttf-p (x)
  (declare (ignore x)))

(defun delay (x)
  (declare (ignore x)))

(defun non-volitional-p (x)
   (declare (ignore x)))

(defun temporal-p (x)
  (declare (ignore x)))

(defun trans (x)
  (declare (ignore x)))

;; misc others...

(defun range-low (x)
  (declare (ignore x)))

(defun sink (x)
  (declare (ignore x)))

;;; [tzim] And this one is not found even in my CIRCA/amp load 
(defun loop-acceleration-on-p ())

;;; -------- end of stubs for fcns called from state-class.lisp ------

;;;  --- stubs for some fcns in goal-class.lisp

(defun reachable-states (&key (obj nil))
  (declare (ignore obj)))

;;; And this one is not found even in a CIRCA load ?
(defun get-goal-mode ())

;;; And this one is not found even in a CIRCA load ?
(defun check-goal-mode (m)
  (declare (ignore m)))

(defun assert-goal-mode (m)
  (declare (ignore m)))

;; [tzim 5/12/12] circa/csm/goal-class includes defclass declarations that use goal verification fcns that
;;  (unfortunately) reside in a larger RTA package and (unfortunately) are called with the rta:: package prefix.  
;;  The fcns are FB-irrelevant so here we just define a shell rta package and stick some stub fcns in it.
;;  Alternate: load the rta package as defined in circa/csm and all its files. Seems to depend on "iterate" package
;;  though, so may need to load that too.

(defpackage "RTA"
    (:use "COMMON-LISP"))

(in-package :rta)

(defun forall-always-verify (goal &optional model)
   (declare (ignore goal model)))

(defun forall-eventually-verify (goal &optional model)
   (declare (ignore goal model)))

(defun forall-eventually-forall-always-verify (goal &optional model)
   (declare (ignore goal model)))

(defun nil-verify (goal &optional model)
   (declare (ignore goal model)))

;;; put the following in all files...
;;; (eval-when (:compile-toplevel) (optimization-boilerplate))

(defvar cl-user::*compile-for-debug* NIL
  "When t, compile with higher safety and lower speed
   optimization.  NOTE: some source files do NOT yet notice this flag.")

(defvar cl-user::*circa-debug-safety* 3)
(defvar cl-user::*circa-debug-space* 0)
(defvar cl-user::*circa-debug-speed* 0)
(defvar cl-user::*circa-debug-debug* 3)

(defvar cl-user::*circa-perf-safety* 1)
(defvar cl-user::*circa-perf-space* 2)
(defvar cl-user::*circa-perf-speed* 3)
(defvar cl-user::*circa-perf-debug* 0)


(defmacro optimization-boilerplate ()
  `(eval-when (:compile-toplevel)
     ,(if cl-user::*compile-for-debug*
          `(declaim (optimize (safety ,cl-user::*circa-debug-safety*)
                              (space ,cl-user::*circa-debug-space*)
                              (speed ,cl-user::*circa-debug-speed*)
                              (debug ,cl-user::*circa-debug-debug*)))
          `(declaim (optimize (safety ,cl-user::*circa-perf-safety*)
                              (space ,cl-user::*circa-perf-space*)
                              (speed ,cl-user::*circa-perf-speed*)
                              (debug ,cl-user::*circa-perf-debug*))))))


