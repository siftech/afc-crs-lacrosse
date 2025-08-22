;; Define a macro for the *compile-for-debug* optimization settings
;; that we want at the head of each source file.

(in-package :musliner)

;; To use this, put the following at the head of each source file:
;; (eval-when (compile) (optimization-boilerplate))

(defmacro optimization-boilerplate ()
  (declare (special cl-user::*compile-for-debug*))
  (if (boundp 'cl-user::*compile-for-debug*)
      (if cl-user::*compile-for-debug*
          '(declaim (optimize (safety 3)  (space 0) (speed 0)(debug 3)))
        '(declaim (optimize (safety 1) (space 1) (speed 3) (debug 0))))
    (format t "*compile-for-debug* not bound.~%")))


