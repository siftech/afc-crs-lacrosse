;;; -*- lisp -*- system definition

(defsystem :iterate
    :description "Jonathan Amsterdam's iterator/gatherer/accumulator facility"
    :in-order-to ((test-op (test-op "iterate/tests")))
    :components ((:file "package")
                 (:file "iterate" :depends-on ("package"))))

(defsystem :iterate/pg
    :depends-on (:iterate pg)           ; Eric Marsden's pg.lisp
    :pathname ""
    :components ((:file "iterate-pg")))

(defsystem :iterate/tests
    :depends-on (:iterate #+sbcl sb-rt #-sbcl :rt)
    :pathname ""
    :components ((:file "iterate-test")))

(defmethod asdf:perform ((op asdf:test-op) (c (eql (find-system ':iterate/tests))))
  (funcall (intern "DO-TESTS" (find-package #+sbcl "SB-RT"
                                            #-sbcl "REGRESSION-TEST"))))

(defmethod asdf:perform :after ((o asdf:load-op) (c (eql (find-system ':iterate))))
  (provide '#:iterate))

;;; arch-tag: "b8bc9675-313c-11d8-abb9-000c76244c24"
