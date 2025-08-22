;;; -*- Package: MUSLINER; Mode: LISP; Base: 10; Syntax: Common-Lisp; -*-
;;;-------------------------------------------------------------------------
;;; stochastic.lisp
;;; - generator classes for stochastic streams of various types.
;;; - $Header: /projects/coordinators/CVS.r/ALL/code/ext/lisp-libs/musliner-tools/stochastic.lisp,v 1.3 2007/03/12 19:18:05 musliner Exp $
;;; -------------------------------------------------------------------------
;;; COPYRIGHT START
;;; COPYRIGHT END
;;; -------------------------------------------------------------------------


(in-package :musliner)

;;;-------------------------------------------------------------------------
(defclass stochastic-stream ()
  ((seed :initform (get-random-state) :accessor seed)))

;;;-------------------------------------------------------------------------
;;; - probability of value being T is percent/base.
;;; - base defaults to 100;
(defclass probability (stochastic-stream)
  ((percent :initarg :percent :accessor percent)
   (base :initarg :base :initform 100 :accessor base)))

(defmethod print-object ((p probability) stream)
  (format stream "#<PROBABILITY ~A/~A~%" (percent p)(base p)))

;;;-------------------------------------------------------------------------
(defun percent-probability (p &optional (b 100))
  (make-instance 'probability :percent p :base b))

;;;-------------------------------------------------------------------------
(defmethod value ((p probability))
  (< (random (base p) (seed p)) (percent p)))

(defmethod value (p)
  (declare (ignore p))
  nil)
;;;-------------------------------------------------------------------------

;;; the oxymoronic distribution -- all mass is on the single value.
;;;
(defclass constant-distribution ()
  ((val :initarg :val :reader val)))

(defmethod print-object ((this constant-distribution) stream)
  (format stream "#<CONSTANT-DISTRIBUTION==~a>" (val this)))

(defun constant-distribution (v)
  (make-instance 'constant-distribution :val v))

(defmethod value ((this constant-distribution))
  (val this))

(defmethod lower-bound ((constant-distribution constant-distribution))
  (val constant-distribution))

(defmethod upper-bound ((constant-distribution constant-distribution))
  (val constant-distribution))


;;;-------------------------------------------------------------------------

;;; Generate deviates with equal likelihood on the set of integers [lo hi]
;;; n.b. this was formerly (< 5-15-03) called 'uniform-distribution'.

(defclass discrete-uniform-distribution (stochastic-stream)
  ((offset :initarg :offset :accessor offset)
   (maxval :initarg :maxval :accessor maxval)))

(defmethod print-object ((u discrete-uniform-distribution) stream)
  (format stream "#<DISCRETE-UNIFORM-DISTRIBUTION [~A ~A]>"
          (offset u)(+ (1- (maxval u)) (offset u))))

;;; Constructor for discrete uniform distributions.
;;; Returns an object used to generate uniform distribution values
;;; from low to high range, inclusive.
;;;
(defun DISCRETE-UNIFORM-DISTRIBUTION (low high)
  (unless (and (integerp low)
               (integerp high)
               (<= low high))
    (error
     "parameters to DISCRETE-UNIFORM-DISTRIBUTION must be non-decreasing integers"))
  (cond
   ((= low high)
    (constant-distribution low))
   (t
    (make-instance 'discrete-uniform-distribution
                   :offset low
                   :maxval (1+ (- high low))))))

;;; Generate a pseudorandom deviate from the distribution
;;;
(defmethod VALUE ((d discrete-uniform-distribution))
  (+ (offset d) (random (maxval d) (seed d))))

(defmethod lower-bound ((discrete-uniform-distribution discrete-uniform-distribution))
  (offset discrete-uniform-distribution))

(defmethod upper-bound ((discrete-uniform-distribution discrete-uniform-distribution))
  (+ (offset discrete-uniform-distribution)
     (maxval discrete-uniform-distribution)))

;;;-------------------------------------------------------------------------


;;; Generate deviates with equal likelihood on the half-open range [lo,hi)
;;;
(defclass uniform-distribution (stochastic-stream)
  ((offset :initarg :offset :accessor offset)
   (range :initarg :range :accessor range)))

(defmethod print-object ((u uniform-distribution) stream)
  (format stream "#<UNIFORM-DISTRIBUTION [~A ~A)>"
          (offset u) (+ (offset u) (range u))) )

;;; Returns an object used to generate uniform distribution values
;;; from low to high range, inclusive.
;;;
(defun UNIFORM-DISTRIBUTION (low high)
  (unless (and (realp low)
               (realp high)
               (<= low high))
    ;; no fair sneaking in complex numbers!
    (error "parameters to UNIFORM-DISTRIBUTION must be non-decreasing real numbers")
    )
  (cond
   ((= low high)
    (constant-distribution low))
   (t
    (make-instance 'uniform-distribution
                   :offset (float low)
                   :range (float (- high low))))))

;;; Generate a pseudorandom deviate from the distribution
;;;
(defmethod VALUE ((d uniform-distribution))
  (+ (offset d) (random (range d) (seed d))))

;;;-------------------------------------------------------------------------
;;; The conditional probability class has associated condition function
;;; in addition to probability percentage.  If condition function returns
;;; true, we use the random percentage to determine T/nil outcome.  Else nil.

(defclass conditional-probability (probability)
  ((cp-condition :initarg :cp-condition :accessor cp-condition)))

(defmethod value ((p conditional-probability))
  (if (funcall (cp-condition p))
        (call-next-method)
        nil))

;; VHA 10/12/04 Now prob-token-pairs is simply a list of the form like ((.1 a) (.9 b))
;; using this format, we can create a discrete-distribution from a postconds as-is
;; see initialize-instance :after method for transitions
;;;-------------------------------------------------------------------------
(defclass discrete-distribution (stochastic-stream)
  ((prob-token-pairs
    :initarg :prob-token-pairs
    :reader prob-token-pairs
    :documentation "A list of prob-token-pairs so that the sum of the probabilities is 1."
    )))

(defmethod initialize-instance :after ((dist discrete-distribution) &key)
  ;; check for well-formed prob-token-pairs
  (unless (prob-token-pairs-p (prob-token-pairs dist))
    (error "~a is not a well-formed prob-token-pairs list" (prob-token-pairs dist))))

;;; MP20041021 this is what I really want, but it does get messy...
;;;(defmethod print-object ((this discrete-distribution) stream)
;;;  (format stream "#<DISCRETE-DISTRIBUTION [")
;;;  (loop for prob-token-pair in (prob-token-pairs this)
;;;      do (format stream "~A with probability ~A  " (second prob-token-pair) (first prob-token-pair)))
;;;  (format stream "]>"))

(defmethod print-object ((this discrete-distribution) stream)
  (format stream "#<DISCRETE-DISTRIBUTION: ~%")
  (loop for prob-token-pair in (prob-token-pairs this)
      do (format stream "  ~A with probability ~A~%" (second prob-token-pair) (first prob-token-pair)))
  (format stream ">"))

;;;-------------------------------------------------------------------------
(defun prob-token-pairs-p (pairs)
  (and (consp pairs)
       (every #'(lambda (pair)
                  (let ((p (first pair)))
                    (and (numberp p)
                         (<= 0 p 1))))
              pairs)
        (= 1 (/ (fround (reduce #'+ pairs :key #'first) .0001) 10000))))

       ;;(= 1 (reduce #'+ pairs :key #'first))))

;;;-------------------------------------------------------------------------
(defun discrete-distribution (ptp)
  (make-instance 'discrete-distribution :prob-token-pairs ptp))

;;;-------------------------------------------------------------------------
(defmethod change-probs ((this discrete-distribution) (probs cons))
  (unless (and (every #'(lambda (x) (and (numberp x) (<= x 1) (<= 0 x))) probs)
                           (= 1 (reduce #'+ probs))
                           (= (length probs) (length (prob-token-pairs this)))
                           )
        (error "~A is not a list of numbers between 0 and 1 that sum up to 1, or its length is different~%" probs)
        )
  (loop for prob-token in (prob-token-pairs this)
          for prob in probs
          do (setf (first prob-token) prob)
                 )
  this
  )

;;;-------------------------------------------------------------------------
(defun equal-probabilities-list (n &key (precision 2))
  (let* ((precision-factor (expt 10 precision)))
    (multiple-value-bind (floor remainder)
        (floor precision-factor n)
      (let* ((initial-element (float (/ floor precision-factor)))
             (prob-list (make-list n :initial-element initial-element)))
        ;; when there's a remainder, peanut butter it over the start of the list
        (unless (zerop remainder)
          (loop for i from 0 upto (1- remainder)
              do (setf (nth i prob-list) (+ initial-element (float (/ precision-factor))))))
        prob-list))))

;;; This is a linear, non-optimal way to generate a sample of the discrete prob
;;; Binary search-based, faster methods exist
;;; VHA 10/12/2004 Added index-only optional argument. If true then returns the index
;;; of the element in the list, rather than the element itself
;;; Because of the optional argument, have to change the name to <> value
;;; FIXME MP20041021 Should create value method, too, for consistency or use &key

;;;-------------------------------------------------------------------------
(defmethod get-sample ((this discrete-distribution) &optional (index-only nil))
  (with-slots (prob-token-pairs) this
      (let* ((random-number (random 1.0 (seed this)))
             (temp 0)
             (index -1)
             )
        (loop for prob-token-pair in prob-token-pairs
            do (setf temp (+ temp (first prob-token-pair))
                     index (+ index 1))
            if (< random-number temp)
            return (if index-only index
                     (second prob-token-pair))
            finally (return (if index-only index
                              (second prob-token-pair)))))))

;;;-------------------------------------------------------------------------
(defmethod value ((dist discrete-distribution))
  (get-sample dist))

;;;-------------------------------------------------------------------------
;;; Method to perform stratified sampling on a discrete distribution
(defmethod s-sampling ((this discrete-distribution) (function symbol) (runs number))
  (float
   (reduce #'+ (prob-token-pairs this)
                   :key #'(lambda (pt)
                                        (if (= (first pt) 0)
                                                0
                                          (* (first pt)
                                                 (apply (symbol-function function) (list (second pt) runs))
                                          ))))))

;;; -------------------------------------------------------------------------
;;; Stochastic stream for normal distribution.
;;; krebsbac: Fri Jul 26 10:29:16 CDT 1996
;;;--------------------------------------------------------------------------
;;;
;;; Note that the technique we use for generation randoms in a
;;; normal distribution requires two seeds. We inherit one (seed)
;;; from stochastic-stream and add another (seed2).

(defclass normal-distribution (stochastic-stream)
  ((seed2 :initform (get-random-state) :accessor seed2)
   (mean   :initarg  :mean  :accessor mean)
   (std-dev :initarg  :std-dev  :accessor std-dev)))

;;; Returns an object used to generate continuous normal distribution values
;;; around the mean, in the range (-infinity infinity), with standard
;;; deviation std-dev (aka sigma).
;;; Currently generates values out to about 5 sigma in 100K trials.
(defun normal-distribution (mean std-dev)
  (make-instance 'normal-distribution
    :mean mean
    :std-dev std-dev))

;;; Generates a random value based on a normal distribution.
(defmethod value ((d normal-distribution))
  (let ((normal (generate-normal (seed d) (seed2 d))))
    ;;; Modify for specific mean (center) and std-dev (spread).
    (+ (mean d) (* normal (std-dev d)))))

;;; -------------------------------------------------------------------------
;;; Generate a normally-distributed random var with mean = 0 and
;;; std-dev = 1.
;;;--------------------------------------------------------------------------
;;; Generate a unit normal random number in the range (-inf inf)
;;; given good uniform random number generator that returns a value
;;; in the range [0, 1).
;;; The log function below is the natural log.
;;; Based on ratio-of-uniforms method from Ripley, Stochastic
;;; Simulation, 1987, page 229.
;;; Seed1 and Seed2 are lisp random-state objects, which are
;;; auto-modified when random is called on them.
(defun generate-normal (seed1 seed2)
  (let* ((b 0.857763884960706)
         (u (random 1.0 seed1))       ;seed1 struct modified
         (vrand (random 1.0 seed2))   ;seed2 struct modified
         (v (* b (- (* 2.0 vrand) 1.0)))
         (x (float (/ v u)))
         (z (/ (* x x) 4.0)))
    (cond ((or
            (< z (- 1.0 u))                       ; either this is true
            (and (not (> z (+ (/ 0.259 u) 0.35))) ; or this...
                 (not (> z (- (log u))))))        ; and this are both false
           x)                           ; it's a keeper
          (t (generate-normal seed1 seed2))))) ; try again with munged seeds

;         (cache-value x)
;         (update-max-min x)

;;;-------------------------------------------------------------------------
;;; this is just a normal distribution with the left side truncated to make
;;;     sure the value is always positive; we do this slightly nicely by
;;;     taking absolute value of return from the regular normal distribution.

(defclass positive-normal-distribution (normal-distribution) ())

(defun positive-normal-distribution (mean std-dev)
  (make-instance 'positive-normal-distribution
    :mean mean
    :std-dev std-dev))

(defmethod value ((d positive-normal-distribution))
  (let ((normal (generate-normal (seed d) (seed2 d))))
    (abs (+ (mean d) (* normal (std-dev d))))))

;;; -------------------------------------------------------------------------
;;; Stochastic stream for exponential distribution.
;;; krebsbac: Wed Jan 31 10:47:05 CST 2001
;;;--------------------------------------------------------------------------
;;;
;;; This very simple and fast logarithmic method for computing
;;; exponentially-distributed random numbers in the range [0, infinity)
;;; using uniform random numbers in the range [0, 1) is due to Knuth's Art
;;; of Computer Programming, Vol II (Seminumerical Methods). The mean is the
;;; "average value" of the distribution (usually called beta or mu in the
;;; literature), with its reciprocal, 1/mu, being the more familiar
;;; parameter (lambda) that defines the distribution. The exponential
;;; distribution is "memoryless" in the sense that the probability of the
;;; event occurring (lambda) remains constant over time.
;;; HLY added offset, so now we can get a shifted exponential distribution.

(defclass exponential-distribution (stochastic-stream)
  ((mean :initarg :mean  :accessor mean)
   (offset :initform 0 :initarg :offset :accessor offset)))

(defun exponential-distribution (mean &optional (offset 0))
  (make-instance 'exponential-distribution :mean mean :offset offset))

;;; Note that u=0 cannot be used, since (log 0) = -infinity
;;; In that rare case, generate a new exponential with the
;;; internal seed state changed.
(defun generate-exponential (seed mean)
  (let ((u (random 1.0 seed)))       ; will munge seed for next time
    (if (> u 0.0)
        (- (* mean (log u)))
        (generate-exponential seed mean))))  ;very seldom needed

;;; Generates a random value based on an exponential distribution.
(defmethod value ((d exponential-distribution))
  (let ((exp (generate-exponential (seed d) (- (mean d) (offset d)))))
    (+ exp (offset d))))

;;; FIXME?  random-sets (eg) probably shouldn't return t..
(defun distribution-p (distribution)
  (or (typep distribution 'stochastic-stream)
      (typep distribution 'constant-distribution)))

;;; Test Knuth stuff

;(setf *ed1* (exponential-distribution 10))
;
;(defun try-knuth (n)
;  (dotimes (i n)
;    (format t "~A~%" (value *ed1*))))

;;; -------------------------------------------------------------------------
;;; This is the slow, iterative way of generating exponential variates.
;;; Generate an exponentially-distributed random var.
;;; Note that it assumes lambda = .5 (mean = 2).
;;;--------------------------------------------------------------------------

;;; Generate a continuous exponential random number in the range (0,
;;; infinity) given good uniform random number generator that returns a
;;; value in the range [0, 1).
;;; Based on algorithm 3.7, (von Neumann) from Ripley, Stochastic
;;; Simulation, 1987, page 230.
;;; Seed1 and Seed2 are lisp random-state objects, which are
;;; auto-modified when random is called on them.

;(defun generate-exponential (seed1 seed2
;                           &optional (u nil) (uo nil) (u-star nil) (a 0.0))
;  (if (null u) (setf u (random 1.0 seed1)))            ;seed1 struct modified
;  (if (null u-star) (setf u-star (random 1.0 seed2)))

;  (if (< u u-star) (return (+ a uo)))
;  (setf u (random 1.0 seed1))

;  (cond ((< u u-star)
;        (generate-exponential seed1 seed2 u uo nil a))
;       (t
;        (generate-exponential seed1 seed2 nil uo nil (+ a 1.0)))
;       ))


;;; Testing

;(defun knuth (mu u)
;  (- (* mu (log u))))

;(defun knuth-iter (mu)
;    (dolist (u '(0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9))
;      (format t "~A  ~A  ~A  ~A~%" mu u (log u) (knuth mu u))))


;;;-------------------------------------------------------------------------
;;; A random-set is a set of objects and when you call (value) on it
;;; you get back one of the objects, randomly (and repeatably).

(defclass random-set (stochastic-stream)
  ((objects :initarg :objects :accessor objects)))

(defmethod print-object ((r random-set) stream)
  (format stream "#<RANDOM-SET ~A-~A>~%"
        (first (objects r)) (first (last (objects r)))))

(defmethod value ((r random-set))
  (nth (random (length (objects r))(seed r)) (objects r)))

(defun random-set (list-of-objs)
  (make-instance 'random-set :objects list-of-objs))

(defmethod random-subset ((choices random-set) N &key (test #'eq))
  "Returns a list of N unique items picked randomly from random set"
  (assert (>= (length (objects choices)) N) ()
        "There are not enough choices to give a random subset!")
  (let ((chosen nil))
  (while (< (length chosen) N)
        (pushnew (value choices) chosen :test test))
  chosen))



(defun test-random-sets (&aux r v1 v2 v1a v2a)
  (reset-randoms)
  (setf r (random-set '(a b c d)))
  (for (i 0 100 1) (push (value r) v1))
  (assert (member 'a v1) () "Random-set not producing all values")
  (assert (member 'b v1) () "Random-set not producing all values")
  (assert (member 'c v1) () "Random-set not producing all values")
  (assert (member 'd v1) () "Random-set not producing all values")

  (for (i 0 100 1) (push (value r) v2))
  (assert (not (equalp v1 v2)) () "Random-set not producing diff/new values")
  (reset-randoms)
  (setf r (random-set '(a b c d)))
  (for (i 0 100 1) (push (value r) v1a))
  (for (i 0 100 1) (push (value r) v2a))
  (assert (equalp v1 v1a) () "Random-set not reproducing same values after (reset-randoms)")
  (assert (equalp v2 v2a) () "Random-set not reproducing same values after (reset-randoms)")

  :OK
)

;;;-------------------------------------------------------------------------
(defvar *default-random-choice-stream* nil)

(defun reset-random-choice-stream ()
  (setf *default-random-choice-stream* nil))

(defun random-choice (arg &optional (stream *default-random-choice-stream*) (index-only nil))
  (when (not stream)
        (setf *default-random-choice-stream* (make-instance 'stochastic-stream))
        (setf stream *default-random-choice-stream*))
  (cond ((null arg) nil)
        (index-only (random (length arg) (seed stream)))
        (t (nth (random (length arg) (seed stream)) arg))))

(defun test-random-choice (&aux r v1 v2 v1a v2a)
  (reset-randoms)
  (setf r '(a b c d))
  (for (i 0 100 1) (push (random-choice r) v1))
  (assert (member 'a v1) () "Random-choice not producing all values")
  (assert (member 'b v1) () "Random-choice not producing all values")
  (assert (member 'c v1) () "Random-choice not producing all values")
  (assert (member 'd v1) () "Random-choice not producing all values")

  (for (i 0 100 1) (push (random-choice r) v2))
  (assert (not (equalp v1 v2)) () "Random-choice not producing diff/new values")
  (reset-randoms)
  (for (i 0 100 1) (push (random-choice r) v1a))
  (for (i 0 100 1) (push (random-choice r) v2a))
  (assert (equalp v1 v1a) () "Random-choice not reproducing same values after (reset-randoms)")
  (assert (equalp v2 v2a) () "Random-choice not reproducing same values after (reset-randoms)")

  :OK
)
;;;-------------------------------------------------------------------------

(defun randomize-list (l &aux choice (newl nil))
  (loop for i from 1 to (length l)
      do
        (setf choice (musliner::random-choice l))
        (push choice newl)
        (setf l (delete choice l)))
 newl)

