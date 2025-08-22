(in-package #:common-lisp-user)

(defpackage #:cl-variates
  (:documentation "The variates package provides portable random number generation as well as numerous distributions.") 
  (:use #:common-lisp)
  (:nicknames #:variates)
  (:export
   #:basic-random-number-generator
   #:ran1-random-number-generator
   #:ranq1-random-number-generator
   #:copyable-mixin-placeholder
   #:random-number-generation-mixin
   #:random-number-generator-class
   
   #:uniform-random 
   #:normal-random
   #:exponential-random
   #:integer-random
   #:random-boolean
   #:random-element
   #:poisson-random
   #:random-range
   #:random-range-inclusive
   #:random-number-generator
   #:random-seed
   #:random-element
   #:make-random-number-generator
   #:next-element
   #:*random-generator*
   #:rand
   #:*probability-of-heads*
   #:flip
   #:binomial
   #:geometric
   #:sample-sequence
   #:select-sample
   #:map-unique-indexes
   #:shuffle-elements!))

