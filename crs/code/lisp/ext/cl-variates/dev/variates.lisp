;;;-*- Mode: Lisp; Package: variates -*-

(in-package #:cl-variates)

;;; ---------------------------------------------------------------------------
;;; uniform
;;; ---------------------------------------------------------------------------

(defun uniform-random (generator &optional (min 0.0d0) (max 1.0d0))
  "Returns a double-float pseudo random number between low (inclusive) 
and high (exclusive)."
  (let ((u (next-element generator)))
    (+ min (* u (- max min)))))

(defun integer-random (generator &optional (min 0) (max 1))
   "Returns a integer pseudo random number between low (inclusive) and high (inclusive)."
   (values (floor (uniform-random generator 
                                  (float min 1d0) (float (1+ max) 1d0)))))

(defun random-range (generator low high)
  "Returns a pseudo random number between low (inclusive) and high (exclusive or inclusive depending are arguments). If low and high are both integers (fixnums or bignums) this will return an integer, otherwise it will return a double-float."
  (if (and (integerp low)
           (integerp high))
    (values (integer-random generator low high))
    (values (uniform-random generator low high))))

(defun random-range-inclusive (generator low high)
  "Returns a pseudo random number between low and high (both inclusive). If low and high are both integers (fixnums or bignums) this will return an integer, otherwise it will return a double-float."
  (if (and (integerp low)
           (integerp high))
    (values (floor (uniform-random generator low (1+ high))))
    (values (uniform-random generator low high))))

(defun random-boolean (generator &optional (probability 0.5d0))
  "Returns T with probability given. Defaults to 0.5d0."
  (when (plusp probability)
    (< (next-element generator) probability)))

(defun random-sample-with-range (generator size low high)
  (loop repeat size collect
        (random-range generator low high)))

(defun random-element (generator sequence &key 
                       (start 0) (end (1- (length sequence))))
  "Returns a element selected from sequence uniformly at random. Start and end can be supplied to select an element from a subsequence of sequence."
  (if (or (and (consp sequence) (null sequence))
          (zerop (length sequence)))
    (values nil nil)
    (values
     (elt sequence (random-range generator start end))
     t)))

#+Alt
(defun random-element (generator sequence &key 
                       (start 0) (end (length sequence)))
  (if (or (and (consp sequence) (null sequence))
          (zerop (length sequence)))
    (values nil nil)
    (values
     (elt sequence (integer-random generator start end))
     t)))

;;; ---------------------------------------------------------------------------
;;; NORMAL
;;; ---------------------------------------------------------------------------

(defun normal-random (generator mean standard-deviation)
  "Returns a normally distributed double-float pseudo random number
with using 'mean' and 'standard-deviation'."
  (normal-random* generator mean standard-deviation))

;;; ---------------------------------------------------------------------------
;;; EXPONENTIAL
;;; ---------------------------------------------------------------------------

(defun exponential-random (generator &optional (rate 1.0d0))
    "Return a single element from the exponential distribution."
  (exponential-random* generator rate))


;;; ---------------------------------------------------------------------------
;;; classes and api
;;; ---------------------------------------------------------------------------

(defgeneric (setf random-seed) (random-seed random-number-generator)
  (:documentation "Sets the random seed of the generator. Two random generators with of the same class with the same seed will return the same sequence of random numbers."))

;;; ---------------------------------------------------------------------------

(defgeneric next-element (random-number-generator)
  (:documentation "Returns the next pseudo random number from a random number
generator (using the generator as output stream metaphor)."))

;;; ---------------------------------------------------------------------------

(defgeneric initialize-random-number-generator (random-number-generator)
  (:documentation "\(Internal\) \(Re\)initialize a random number generator. Called
when the generator is created and when the seed is changed."))

;;; ---------------------------------------------------------------------------

(defun make-random-number-generator (&optional (random-seed 42)
                                               (class 'ran1-random-number-generator))
  "Return a new random number generator of class `class' using `seed' as 
the initial seed." 
  (make-instance class :random-seed random-seed))

;;; ---------------------------------------------------------------------------

(defclass basic-random-number-generator ()
  ((random-seed :initarg :random-seed :reader random-seed))
  (:default-initargs 
    :random-seed 42)
  (:documentation "Root superclass for all random number generators."))

;;; ---------------------------------------------------------------------------

(defmethod (setf random-seed) ((random-seed number)
                               (rng basic-random-number-generator))
  (when (zerop random-seed)
    (error "Seed must never be zero."))
  (setf (slot-value rng 'random-seed) random-seed)
  (initialize-random-number-generator rng)
  (values (random-seed rng)))

;;; ---------------------------------------------------------------------------
;;; ran1-random-number-generator (from numerical recipes)
;;; ---------------------------------------------------------------------------

(defclass ran1-random-number-generator (basic-random-number-generator)
  ((internal-seed :initform 0.0d0 :reader internal-seed :initarg :internal-seed)
   (ia :initform 16807.0d0 :reader ia :initarg :ia)
   (im :initform 2.147483647d+9 :reader im :initarg :im)
   (am :initform 0.0d0 :reader am :initarg :am)
   (iq :initform 127773.0d0 :reader iq :initarg :iq)
   (ir :initform 2836.0d0 :reader ir :initarg :ir)
   (ntab :initform 32 :reader ntab :initarg :ntab)
   (ndiv :initform 0.0d0 :reader ndiv :initarg :ndiv)
   (rnmx :initform 0.0d0 :reader rnmx :initarg :rnmx)
   (iy :initform 0.0d0 :reader iy :initarg :iy)
   (iv :initform :unbound :reader iv :initarg :iv))
  (:documentation 
   "From Numerical Recipes in C:

'Minimal' random number generator of Park and Miller with Bayes-Durham
shuffle and added safeguards. Returns a uniform random deviate between 0.0 and
1.0 \(exclusive of the endpoint values\). It has a period of 2^31 - 1 
\(~ 2.1 x 10^9\)"))

(defmethod initialize-instance :after
    ((object ran1-random-number-generator) &key)
  (with-slots (im ntab) object
    (setf (slot-value object 'am) (/ 1.0d0 im)
          (slot-value object 'ndiv) (+ 1 (/ (- im 1) ntab))
          (slot-value object 'rnmx) (- 1.0d0 long-float-negative-epsilon)))
  (initialize-random-number-generator object))

(defmethod initialize-random-number-generator
    ((rng ran1-random-number-generator))
  (with-slots (ia im am iq ir ntab ndiv rnmx iy iv internal-seed)  
              rng
    (setf internal-seed (float (random-seed rng) 0d0))
    (setf internal-seed (mod (+ (* internal-seed 106) 1283) 6075)
          iy 0.0d0
          iv (make-array ntab))
    (do ((j (+ ntab 7) (1- j)))
        ((< j 0))
      (let ((k (ftruncate internal-seed iq)))
        (setf internal-seed (- (* ia (- internal-seed (* k iq))) (* ir k))))
      (if (< internal-seed 0) (incf internal-seed im))
      (if (< j ntab) (setf (svref iv j) internal-seed)))
    (setf iy (svref iv 0))))

;;; ---------------------------------------------------------------------------

(defmethod next-element ((rng ran1-random-number-generator))
  (declare (optimize speed))
  (with-slots (ia im am iq ir ndiv rnmx iv iy internal-seed) 
              rng
    (let ((k (ftruncate internal-seed iq)))
      (setf internal-seed (- (* ia (- internal-seed (* k iq))) (* ir k))))
    (when (< internal-seed 0d0) (setf internal-seed (+ internal-seed im)))
    (let ((j (floor iy ndiv)))
      (setf iy (svref iv j))
      (setf (svref iv j) internal-seed))
    ;; convert to number between 0.0 and 1.0
    (let ((temp (* am iy)))
      (if (> temp rnmx) rnmx temp))))


;;; ---------------------------------------------------------------------------
;;; ranq1-random-number-generator
;;; ---------------------------------------------------------------------------

(defclass ranq1-random-number-generator (basic-random-number-generator)
  ((current-value :initform 0d0 :reader current-value))
  (:documentation    "From Numerical Recipes in C:

This is a quick and dirty generator which 'is about as good as any
32-bit linear congruential generator. It is only about 1.7 times faster
than ran1 because of the extra divide we need. It also conses about
1.7 times less. I believe it has the same period as ran1."))

(defmethod initialize-instance :after ((object ranq1-random-number-generator) 
                                       &key)
  (initialize-random-number-generator object))

(defmethod initialize-random-number-generator 
    ((object ranq1-random-number-generator))
  (setf (slot-value object 'current-value) (random-seed object)))

(defmethod next-element ((rng ranq1-random-number-generator))
  (with-slots (current-value) rng
    (setf current-value
          (logand (+ (* current-value 1664525)
                     1013904223)
                  #xFFFFFFFF))
    (/ current-value #.(float (expt 2 32)))))

#+Test
(let ((g (make-random-number-generator 42 'ranq1-random-number-generator)))
  (delete-file "ccl:ranmt.dat")
  (variates::produce-random-bit-file 
   g "ccl:ranq1.dat" 1000000 25 #\linefeed))

;;; ---------------------------------------------------------------------------
;;; random-number-generation-mixin
;;; ---------------------------------------------------------------------------

(defclass random-number-generation-mixin ()
  ((random-number-generator 
    :initform nil
    :reader random-number-generator
    :initarg :random-number-generator)
   (random-number-generator-class
    :initform nil
    :reader random-number-generator-class
    :initarg :random-number-generator-class))
  (:default-initargs
    :random-number-generator-class 'ran1-random-number-generator
    :random-seed 42)
  (:documentation "Mixing this class into another class will make it appear to be a random number generator. The new class can be used as a generator in the various random number routines, has a random seed and so on. The mixin also provides two initargs: random-seed and random-number-generator-class. The former is self-explanitory. The latter specifies which random number generator class to use."))

(defmethod initialize-instance :after ((object random-number-generation-mixin)
                                       &key random-seed)
  (setf (slot-value object 'random-number-generator)
        (make-instance (random-number-generator-class object) 
          :random-seed random-seed)))

(defmethod next-element ((rng random-number-generation-mixin))
  (next-element (random-number-generator rng)))

(defgeneric random-seed (random-number-generator)
  (:documentation "Returns the original random seed of the generator."))

(defmethod random-seed ((rng random-number-generation-mixin))
  (random-seed (random-number-generator rng)))

(defmethod (setf random-seed) ((random-seed number)
                               (rng random-number-generation-mixin))
  (setf (random-seed (random-number-generator rng)) random-seed))



;;; ---------------------------------------------------------------------------
;;; The following functions take generators as arguments.  This makes testing
;;; easier.  
;;; --------------------------------------------------------------------------- 

(defun normal-random* (generator mean standard-deviation)
  "Gets a single value sampled from the normal distribution with mean `mean' and
standard devation `standard-deviation.' This uses the algorithm from Numerical
Recipes in C."
  ;; Note that this implementation throws away half of the Gaussian variates.
  ;; This is because we would have to store the other variate with the
  ;; generator, and that seems too cumbersome.
  (do* ((v1 (1- (* 2 (next-element generator))) 
            (1- (* 2 (next-element generator))))
        (v2 (1- (* 2 (next-element generator))) 
            (1- (* 2 (next-element generator))))
        (r (+ (* v1 v1) (* v2 v2)) (+ (* v1 v1) (* v2 v2))))
       ;; if (v1,v2) lie in the unit circle, return
       ;; v1*sqrt(-2.0*log(r)/r) and store v2*sqrt(-2.0*log(r)/r)
       ;; otherwise, try again
       ((< 0 r 1)
        #+ignore
        (setf *standard-normal-deviate-storage*
              (* v2 (sqrt (* -2.0 (/ (log r) r)))))
        (+ mean (* standard-deviation
                   (* v1 (sqrt (* -2.0 (/ (log r) r)))))))))

(defun exponential-random* (generator rate)
  (/ (- (log (next-element generator))) rate))


;; ---------------------------------------------------------------------------
;;; some utilities
;;; ---------------------------------------------------------------------------

(defvar *random-generator* (make-random-number-generator)
  "This variable takes the place of CL's *random-state*. It can be supplied as
a generator to all the functions in the variates package.")

(defgeneric shuffle-elements! (container &key generator times)
  (:documentation 
    "Destructively rearrange the elements of a container by performing 'times' swaps. If times is not specified, it will perform 2 times the container's size swaps."))

(defmethod shuffle-elements! ((sequence sequence) &key 
                              (generator *random-generator*) 
                              (times 0 times-supplied?))
  (let ((size (1- (length sequence))))
    (dotimes (i (if times-supplied? times (* 2 size)))
      (rotatef (elt sequence (integer-random generator 0 size))
               (elt sequence (integer-random generator 0 size)))))
  sequence)

;;; ---------------------------------------------------------------------------
;;; cl:random clone
;;; ---------------------------------------------------------------------------

(defun rand (n &optional (seed *random-generator*))
  "Simple standin for Common Lisp random function. [[?? remove, just type more]]"
  (etypecase n
    (double-float
     (uniform-random seed 0.0d0 n))
    ((or integer single-float) 
     (floor (uniform-random seed 0.0d0 (float n 1.0d0))))))

;;; ---------------------------------------------------------------------------
;;; coin flips
;;; ---------------------------------------------------------------------------

(defparameter *probability-of-heads* 0.5d0
  "The default probably used in calls to flip \(and therefore in calls to
binomial and geometric\).")

(defun flip (&optional (pr *probability-of-heads*) (heads t) (tails nil))
  "Flip a pseudo-random coin and return true if it comes up heads. The default
probably of heads for the coin is *probability-of-heads*."
  (if (< (rand 1.0d0) pr) heads tails))

;;; ---------------------------------------------------------------------------
;;; simple random variables
;;; ---------------------------------------------------------------------------

(defun binomial (n p)
  "Flip a coin \(using flip\) `n` times with probability `p` and return the number of heads." 
  (loop for i from 1 to n 
        summing (flip p 1 0)))

(defun geometric (p)
  "Returns a sample from the geometric distribution with probability `p`. I.e., it returns the number of flips it takes until a possibly biased coin \(using flip\) comes up heads."
  (loop for i from 1
        until (flip p)
        finally (return i)))

;;; ---------------------------------------------------------------------------
;;; more randomized algorithm stuff
;;; ---------------------------------------------------------------------------

(defun sample-sequence (seq &key key (pr *probability-of-heads*))
  "Returns a sub-sequence of `seq` with probability `pr` that each element of `seq` will be included. If a key function is supplied, then it is applied to each included element. [[?? Remove, use select-sample and/or sample-elements instead.]]"
  (unless key 
    (setf key #'identity))
  (flet ((f (x)
           (when (flip pr)
             (funcall key x))))
    (remove-if-not #'f seq)))

;;; ---------------------------------------------------------------------------

(defun select-sample (generator sample-size total-size)
  "Returns a bit vector of size total-size with exactly sample-size
bits set to 1. The one bits are selected uniformly at random. The algorithm
is attributed to Robert Floyd by Jon Bentley in More Programming Pearls." 
  (assert (<= sample-size total-size))
  (let ((set (make-array total-size :element-type 'bit
                         :initial-element 0)))
    (labels ((in-set-p (x)
               (= (sbit set x) 1))
             (put-in-set (x)
               (setf (sbit set x) 1))
             (sample-aux (elts total)
               (cond ((zerop elts)
                      nil)
                     (t
                      (sample-aux (1- elts) (1- total))
                      (let ((elt (integer-random generator 0 (1- total))))
                        (if (in-set-p elt)
                          (put-in-set (1- total))
                          (put-in-set elt)))))))
      (sample-aux sample-size total-size)
      set)))


;;; ---------------------------------------------------------------------------
;;; misc
;;; ---------------------------------------------------------------------------

(defun produce-random-bit-file (generator destination bits 
                                          &optional (line-length 25)
                                          (line-feed-char #\linefeed))
  ;; this is handy for using with the NIST test suite
  (with-open-file (s destination
                     :direction :output
                     :if-exists :error
                     :if-does-not-exist :create)
    (princ "  " s)
    (loop for i = 1 then (1+ i)
          repeat bits do
          (princ (if (random-boolean generator 0.5) 1 0) s)
          (when (zerop (mod i line-length)) 
            (princ line-feed-char s)
            (princ "  " s)))))



#| TESTS

(defun graph-random-numbers (count bins generator-function &rest args)
  (let ((generator (make-basic-random-number-generator)))
    (chart:histogram nil (loop repeat count collect (apply generator-function generator args)) :bins bins)))

(graph-random-numbers 1000 20 'uniform-random 0 10)
(graph-random-numbers 1000 20 'exponential-random 12.5)
(graph-random-numbers 1000 20 'normal-random 0 10)
(graph-random-numbers 1000 20 'poisson-random 12.5)

|#
