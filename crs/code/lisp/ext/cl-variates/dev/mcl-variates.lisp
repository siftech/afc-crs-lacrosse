;;;-*- Mode: Lisp; Package: VARIATES -*-

#| simple-header
Author: Gary King

DISCUSSION

Use MCL specific tricks to make a faster, less consy RNG

Need to figure out how to make ftruncate and floor not cons or at least cons less 
|#

(in-package #:variates)

(export '(ran1-fpc-random-number-generator))

;;; ---------------------------------------------------------------------------
;;; ran1-fpc-random-number-generator
;;; ---------------------------------------------------------------------------

(defclass ran1-fpc-random-number-generator (ran1-random-number-generator)
  ()
  (:documentation "The same generator as ran1-random-number-generator except
that we optimize the code using Randel Beer's MCL floating point compiler."))

;;; ---------------------------------------------------------------------------

(defmethod next-element ((rng ran1-fpc-random-number-generator))
  (declare (optimize (speed 3) (space 3) (debug 0)))
  (with-slots (ia im am iq ir ndiv rnmx iv iy internal-seed) 
              rng
    (declare (type double-float ia im am iq ir ndiv iy)
             (type simple-vector iv)) 
    (let ((k (float (%floor-1 internal-seed iq) 0d0)))
      (ccl:%set-double! internal-seed (- (* ia (- internal-seed (* k iq))) (* ir k))))
    (when (< internal-seed 0d0)
      (ccl:%set-double! internal-seed (+ internal-seed im)))
    (let ((j (%floor-1 iy ndiv)))
      (setf iy (svref iv j))
      (setf (svref iv j) internal-seed))
    ;; convert to number between 0.0 and 1.0
    (let ((temp (ccl:%double-inline (* am iy))))
      (if (> temp rnmx) rnmx temp))))


#+TEst
(let ((ia 16807d0)
      (k 8512d0)
      (internal-seed 1.087673513d+9)
      (iq 127773d0)
      (ir 2836d0))
  (print (- (* ia (- internal-seed (* k iq))) (* ir k)))
  (print (ccl:%double-inline (- (* ia (- internal-seed (* k iq))) (* ir k))))
  (values))

#+Debug
(defmethod next-element ((rng ran1-fpc-random-number-generator))
  (declare (optimize (speed 3) (space 3) (debug 0)))
  (with-slots (ia im am iq ir ndiv rnmx iv iy internal-seed) 
              rng
    (declare (type double-float ia im am iq ir ndiv iy)
             (type simple-vector iv)) 
    (let ((k (float (%floor-1 internal-seed iq) 0d0)))
      (spy k ia internal-seed iq ir)
      (ccl:%set-double! internal-seed (- (* ia (- internal-seed (* k iq))) (* ir k)))
      (spy internal-seed))
    (when (< internal-seed 0d0)
      (ccl:%set-double! internal-seed (+ internal-seed im))
      (spy internal-seed))
    (let ((j (%floor-1 iy ndiv)))
      (setf iy (svref iv j))
      (spy iy ndiv j)
      (setf (svref iv j) internal-seed))
    ;; convert to number between 0.0 and 1.0
    (let ((temp (ccl:%double-inline (* am iy))))
      (spy temp)
      (if (> temp rnmx) rnmx temp))))

;;; ---------------------------------------------------------------------------

(defun %floor-1 (number divisor)
  (ccl:with-temp-double-floats (temp!)
    (ccl:%set-double! temp! (/ number divisor))
    (ccl::%unary-truncate temp!)))

#+Test
(let ((x 0))
  (time
   (progn
     (loop repeat 10000 do
           (loop for y in '(2.1 3.4 7.8 100.4 123231231231231.1) do
                 (setf x (floor y))))
     x)))

#+Test
(let ((x 0))
  (time
   (progn
     (loop repeat 10000 do
           (loop for y in '(2.1 3.4 7.8 100.4 123231231231231.1) 
                 for z in '(6.7 1.2 0.454 123.2 34635.9) do
                 (setf x (floor y z))))
     x)))

#+Test
(let ((x 0))
  (time
   (progn
     (loop repeat 10000 do
           (loop for y in '(2.1 3.4 7.8 100.4 123231231231231.1) 
                 for z in '(6.7 1.2 0.454 123.2 34635.9) do
                 (setf x (%floor-1 y z))))
     x)))

;;; ---------------------------------------------------------------------------

(let ((ccl:*warn-if-redefine* nil))
  (defun uniform-random (generator min max)
    (check-type min double-float)
    (check-type max double-float)
    (let ((u (next-element generator)))
      (ccl:%double-inline (+ min (* u (- max min)))))))


#+Test
(let ((g1 (make-random-number-generator 10 'ran1-random-number-generator))
      (g2 (make-random-number-generator 13 'ran1-fpc-random-number-generator)))
  (time (uniform-random g1 0.0 1.0))
  (time (uniform-random g2 0.0 1.0)))


#+Test
(let ((g1 (make-random-number-generator 10 'ran1-random-number-generator))
      (g2 (make-random-number-generator 13 'ran1-fpc-random-number-generator)))
  (time (next-element g1))
  (time (next-element g2)))

#+Test
(let ((g1 (make-random-number-generator 10 'ran1-random-number-generator))
      (g2 (make-random-number-generator 13 'ran1-fpc-random-number-generator))
      (count 100000))
  (time (loop repeat count do (next-element g1)))
  (time (loop repeat count do (next-element g2))))

