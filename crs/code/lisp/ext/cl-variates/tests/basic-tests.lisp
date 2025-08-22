(in-package #:cl-variates-test)

(deftestsuite cl-variates-test ()
  ((state (make-random-number-generator))))

(addtest (cl-variates-test) 
  setting-random-seed
  (setf (random-seed state) 44)
  (ensure-same (random-seed state) 44))

(addtest (cl-variates-test)
  random-seed
  (let (a b)
    (setf (random-seed state) 10
          a (loop for i from 1 to 10 collect
                  (random-range state 0 10))
          (random-seed state) 10
          b (loop for i from 1 to 10 collect
                  (random-range state 0 10)))
    (ensure-same a b)))

(defun test-uniform-random* ()
  ;; Already tested the uniform generator, so just test the range stuff.
  (let* ((g (make-random-number-generator 1)))
    (loop repeat 1000 for v = (uniform-random g -5d0 +5d0)
       minimize v into min
       maximize v into max
       finally (return (values min max)))))

(addtest (cl-variates-test)
  uniform-random-star
  (ensure-same
   (test-uniform-random*)
   (values -4.998793695121442 4.988718372764399)))

(addtest normal-random-star
  (let ((g (make-random-number-generator 1)))
    (ensure-same
     (cl-variates::normal-random* g 0.0 1.0)
     0.9142027287650025)))

(addtest (cl-variates-test)
  test-select-sample
  (ensure-same #*11111 (select-sample *random-generator* 5 5))
  (ensure-same #*00000 (select-sample *random-generator* 0 5))
  (ensure-same 1 (count 1 (select-sample *random-generator* 1 100))))

#+test
(defun test-normal-random* (&optional (seed 3))
  (let* ((g  (make-random-number-generator seed)))
    (loop with times = 10000 and mean = 0.0 and standard-deviation = 5.0
          with a = (make-array times)
       repeat times
          for i from 0
       for v = (normal-random* g mean standard-deviation)
       do (setf (aref a i) v)
          finally
          (format t "~&MEAN=~a SD=~a~%"
                  (clasp::mean a) (clasp::standard-deviation a)))))


#+TEST
(defvar test-hash-table (make-hash-table :test #'eql))

#+TEST
(defun test-normal-random-1 (count interval mu sigma)
  (clrhash test-hash-table)
  (utils:docount (count)
    (let ((value (* (truncate (normal-random mu sigma) interval) interval)))
      (if (numberp (gethash value test-hash-table))
        (incf (gethash value test-hash-table))
        (setf (gethash value test-hash-table) 0))))
  test-hash-table)

#+TEST
(defun test-normal-random (samples mu sigma)
  
  (plotter:histogram-stuff (loop repeat samples
                                 collect (normal-random mu sigma))
                           :bucket-width 10))
                                    
#+test
(defun test-exponential-random* ()
  (let* ((mg (make-meta-generator 3))
         (g  (funcall mg)))
    (loop with times = 1000 and rate = 5.0 with a = (make-array times)
       repeat times
       for i from 0
       for v = (exponential-random* g rate)
       do (setf (aref a i) v)
       sum v into s
       minimize v into min
       maximize v into max
       finally (spy (/ s times) min max
                    (user:kolmogorov-smirnov
                     a #'(lambda (x) (- 1.0 (exp (- (* rate x))))))))
    (dolist (rate '(0.1 0.2 0.5 1.0 1.5))
      (format t "~&Rate = ~f~%" rate)
      (dotimes (i 5)
        (format t "   ~6f~%" (exponential-random* g rate))))))




#+Ignore
(deftestsuite random-range (variates)
  (:test 
   (let ((nums (loop repeat 9000 collect (random-range *random-seed* -1 1))))
     (spy (loop for x from -1 to 1 collect
           (count x nums))))))


;;; ---------------------------------------------------------------------------
;;; select-sample
;;; ---------------------------------------------------------------------------

(deftestsuite test-select-sample (cl-variates-test)
  ())

(addtest (test-select-sample) 
  test-number-of-bits-1
  (let* ((sample-count 200)
         (sample-size 12)
         (total-size 20)
         (samples (collect-select-sample-samples 
                   state sample-count sample-size total-size)))
    (ensure-same (reduce #'+ (summarize-select-sample-counts samples))
                 (* sample-count sample-size))))

(addtest (test-select-sample) 
  test-number-of-bits-2
  (let* ((sample-count 200)
         (sample-size 2)
         (total-size 7)
         (samples (collect-select-sample-samples 
                   state sample-count sample-size total-size)))
    (ensure-same (reduce #'+ (summarize-select-sample-counts samples))
                 (* sample-count sample-size))))

#+Ignore
(addtest (test-select-sample) 
  test-equal-bits
  (let* ((sample-count 2000)
         (sample-size 2)
         (total-size 20)
         (samples (collect-select-sample-samples 
                   state sample-count sample-size total-size))
         (summaries (summarize-select-sample-counts samples))
         (std (sqrt (variance summaries)))
         (expected (float (/ (* sample-count sample-size) total-size))))
    (loop for bit = 0 then (1+ bit)
          for sum in summaries do
          (ensure (<= (- expected (* 2 std)) sum (+ expected (* 2 std)))
                       :report "Position ~A had ~D ones, which is not between ~,2F and ~,2F" 
                       :args (bit sum (- expected (* 2 std)) (+ expected (* 2 std)))))))

(defun mean (list)
  (let ((sum 0) (count 0))
    (loop for x in list do
          (incf sum x)
          (incf count))
    (float (/ sum count))))

(defun variance (list)
  (let ((m (mean list))
        (sum 0)
        (count 0))
    (loop for x in list do
          (let ((diff (- x m)))
            (incf sum (* diff diff))
            (incf count)))
    (float (/ sum count))))

(defun collect-select-sample-samples (state count sample-size total-size)
  (loop repeat count collect 
        (variates:select-sample state sample-size total-size)))

(defun summarize-select-sample-counts (bvs)
  (flet ((count-bits (bvs pos)
           (count-if (lambda (bv) (= 1 (aref bv pos))) bvs)))
    (loop for p from 0 to (1- (length (first bvs))) collect
          (count-bits bvs p))))

;;; ---------------------------------------------------------------------------
;;; integer-random
;;; ---------------------------------------------------------------------------

(deftestsuite test-integer-random (cl-variates-test)
  ())

(addtest (test-integer-random)
  test-1
  (ensure-same (integer-random state 0 0) 0))

(addtest (test-integer-random)
  test-2
  (let* ((min -2)
         (max 2)
         (lots (loop repeat 100 collect (integer-random state min max))))
    (loop for i from min to max do
          (ensure (member i lots)
                       :report "Integer-random did not generate ~D" 
                       :arguments (i)))))


#|
(defvar *test-rng* nil)

(defvar *threads* nil)

(defun make-rng-test-process (name)
  (make-thread name (lambda ()
                      (loop do
                            (next-element *test-rng*)))))

(defun test-multi-process-rng (count &key (class 'ran1-random-number-generator))
  (setf *test-rng* (make-random-number-generator 1 class))
  (setf *threads* 
        (loop repeat count 
              for i from 1 collect
              (make-rng-test-process (format nil "RNG-~D" i)))))

(defun stop-multi-process-rng ()
  (loop for thread in *threads* do (destroy-thread thread))
  (setf *threads* nil))

#+Test
(u::test-multi-process-rng 4)
#+Test
(u::test-multi-process-rng 
 4 
 :class '(ran1-random-number-generator thread-safe-random-number-generator))
#+Test
(stop-multi-process-rng)


|#