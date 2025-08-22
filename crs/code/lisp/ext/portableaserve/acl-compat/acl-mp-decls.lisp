(in-package :acl-compat.mp)


(declaim (ftype (function
                 ;; whostate seconds function &rest args
                 (string integer function &rest t)
                          (values t))
                process-wait-with-timout))
