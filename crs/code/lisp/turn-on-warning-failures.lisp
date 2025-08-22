(setf asdf:*compile-file-failure-behaviour* :error)   ;; already done at top of load.lisp, but for good measure
(setf asdf:*compile-file-warnings-behaviour* :error)    ;; this is really the purpose, getting very picky
