;;;-*- Mode: Lisp; Package: variates -*-

(in-package #:cl-variates)

;;; ---------------------------------------------------------------------------
;;; POSISSON
;;; ---------------------------------------------------------------------------

(defun poisson-random (generator mean)
  "Returns a sample from the Poisson distribution with mean `mean` using the random number generator `generator`."
  (poisson-random* generator (coerce mean 'double-float)))

;;; ---------------------------------------------------------------------------
;;; Poisson-random
;;; ---------------------------------------------------------------------------

;;; This code is from numerical recipe's but has been de-optimized.
;;; If we find that we're often calling it with the same test-mean, then
;;; we can investigate things like memoization or returning a function or...

(defun poisson-random* (generator test-mean)
  "Returns an integer valued floating point number that is a random
deviate drawn from a Poisson distribution of mean test-mean using the
generator as a source of uniform random deviates. The Poisson distribution
gives the probability of a the number m Poisson random processes ocuring in
a given interval of time."
  (declare (optimize (speed 3) (space 1) (safety 0) (debug 0)))
  (flet ((do-it (mean)
           (declare (type double-float mean))
           (let ((em 0.0d0)
                 (i 0.0d0)
                 (y 0.0d0)
                 (sq 0.0d0)
                 (alxm 0.0d0)
                 (g 0.0d0))
             (declare (type double-float em i y sq alxm g))
             (if (< mean 12.0d0)
               ;; use direct method
               (progn
                 (setf g (exp (- mean))
                       em -1.0d0
                       i 1.0d0)
                 (do ((done-once? nil t))
                     ((and done-once? (<= i g)))
                   (incf em)
                   (setf i (* i (next-element generator)))))
               ;; else, use rejection method
               (progn
                 (setf sq (sqrt (* 2.0d0 mean))
                       alxm (log mean)
                       g (- (* mean alxm) (gamma-ln (1+ mean))))
                 (do ((done-once-a? nil t))
                     ((and done-once-a? (<= (next-element generator) i)))
                   (do ((done-once-b? nil t))
                       ((and done-once-b? (>= em 0.0d0)))
                     ;(spy y em g)
                     (setf y (tan (* (next-element generator) 
                                     (coerce pi 'double-float)))
                           em (+ mean (* y sq))))
                   (setf em (float (floor em) 1.0d0)
                         i (* 0.9d0 (1+ (* y y)) 
                              (exp (- (* em alxm) (gamma-ln (1+ em)) g)))))))
             em)))
    (if (typep test-mean 'double-float)
      (do-it test-mean)
      (do-it (coerce test-mean 'double-float)))))
