;;; -------------------------------------------------------------------------
;;; $Id: package.lisp 1612 2014-05-08 21:58:52Z sfriedman $
;;; -------------------------------------------------------------------------
;;; COPYRIGHT START
;;; Copyright (c) 2012, Smart Information Flow Technologies (SIFT). Developed with the sponsorship of the Defense Advanced Research Projects Agency (DARPA) and Air Force Research Laboratory.  
;;; -------------------------------------------------------------------------

(defpackage :fuzzbomb
  (:nicknames :fb)
  (:shadowing-import-from :alexandria "WITH-GENSYMS")
  (:shadowing-import-from :bordeaux-threads "ALL-THREADS" "CURRENT-THREAD" "THREAD-NAME")
  (:import-from :acl-compat.excl #:run-shell-command)
  (:use :common-lisp
   :iterate
        :musliner
   #+allegro :socket ;; in CCL this is acl-compat sockets and they suck so dont use them
   #+sbcl :acl-compat.socket
   )
  ;; Prefer symbols from iterate package over musliner package for these.
  (:shadowing-import-from :iterate #:for)
  (:shadowing-import-from :iterate #:while)
  ;; the following vomitrocious interference with import is required because -- ugh --
  ;; acl-compat.socket smashes the sb-bsd-sockets package.
  #+sbcl
  (:shadow #:local-port #:local-host #:remote-port #:remote-host)
  )
