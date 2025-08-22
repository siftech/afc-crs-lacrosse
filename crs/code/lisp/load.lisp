;;; -------------------------------------------------------------------------
;;; $Id: load.lisp 2011 2014-05-08 18:53:57Z sfriedman $
;;; -------------------------------------------------------------------------
;;; COPYRIGHT START
;;; Copyright (c) 2012, Smart Information Flow Technologies (SIFT). Developed with the sponsorship of the Defense Advanced Research Projects Agency (DARPA) and Air Force Research Laboratory.  
;;; COPYRIGHT END
;;; -------------------------------------------------------------------------
(in-package :common-lisp-user)

(defvar *fb-code-directory* (make-pathname :directory (pathname-directory *load-truename*)))

;;; no idea how to tell what version this will give us...
; (require :asdf)
;; the following is wrong: should be testing to verify that the compiled file is newer.  

#-(or ccl sbcl allegro)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (error "Need to fix loading ASDF for this lisp implementation"))
(let* ((asdf-dir (merge-pathnames (make-pathname :directory '(:relative "ext" "asdf"))
                                  *fb-code-directory*))
       (asdf-fasl-dir (ensure-directories-exist (merge-pathnames (make-pathname :directory `(:relative #+ccl "ccl-fasl" #+sbcl "sbcl-fasl" #+allegro "allegro-fasl"))
                                                                 asdf-dir)))
       (asdf-fasl-file (merge-pathnames (make-pathname :name "asdf"
                                                       :type "fasl")
                                        asdf-fasl-dir))
       (asdf-lisp-file (merge-pathnames (make-pathname :name "asdf" :type "lisp") asdf-dir)))
  (unless (probe-file asdf-lisp-file)
    (error "ASDF source missing!"))
  (flet ((compile-asdf ()
           (compile-file asdf-lisp-file :output-file asdf-fasl-file)))
    (unless (and (probe-file asdf-fasl-file) (>= (file-write-date asdf-fasl-file) (file-write-date asdf-lisp-file)))
      (compile-asdf))
    (unless (probe-file asdf-fasl-file)
      (error "Failed to compile ASDF for this lisp"))
    (catch 'compiled
      (handler-bind
          (#+sbcl(SB-FASL::INVALID-FASL-VERSION
                   #'(lambda (c) (declare (ignore c))
                       (and (compile-asdf)
                            (load asdf-fasl-file)
                            (throw 'compiled t)))))
        (load asdf-fasl-file)))))

(format T "Loading NEO-Fuzz~%")

(setf asdf:*compile-file-failure-behaviour* #+allegro :warn #-allegro :error)
;;(setf asdf:*compile-file-warnings-behaviour* :error)  ;; can't enable this yet, too many issues w/ external pkgs

;;; Memory config stolen from CIRCA CSM
;;; The quantum parameter controls the step size (and thus minimum size) for
;;; a new/old space allocation. The number is the number of 8KB pages allocated.
#+allegro
(setf (sys:gsgc-parameter :quantum) 3200) ; new/old space increase in multiples of 25MB
#+allegro
(setf (sys:gsgc-parameter :generation-spread) 8)
;;; Allocate 300MB bytes for both new and old space.
;;#+allegro
;;(sys:resize-areas :new (* 300 1024 1024) :old (* 300 1024 1024))

#+allegro
(setq excl::*warn-smp-usage* nil)       ;; Note we only run this in non-SMP lisps, so stop whining about without-scheduling macro

(format t "*fb-code-directory* is ~A~%" *fb-code-directory*)

(defvar *fb-instance* nil "Name of this Fuzzbomb instance")

(defvar *compile-for-debug* t)          ; this should be T until delivery  

;;; put fasls in platform specific subdirectory, under the cur dir
#-asdf3
(asdf:enable-asdf-binary-locations-compatibility)

#+asdf3
(asdf:initialize-output-translations `(:output-translations
                                       (t (:root :**/ :implementation :*.*.*))
                                       :ignore-inherited-configuration))

(defvar *ext-directory* (merge-pathnames (make-pathname :directory '(:relative "ext")) *fb-code-directory*))
(push *ext-directory* asdf:*central-registry*)

(defvar *asd-finder-directory* (merge-pathnames (make-pathname :directory '(:relative "asd-finder")) *ext-directory*))
(push *asd-finder-directory* asdf:*central-registry*)
(format t "*asd-finder-directory* is ~A~%" *asd-finder-directory*)

(asdf:load-system "asd-finder")

;; Now add any packages in this directory (or subdir) to the central
;; registry so that dependencies are found without drama.
(setf asdf:*central-registry*
      (union (asd-finder:asd-finder (list *fb-code-directory*)
                                :prune-names (list ".svn" "cfg"  "targets")
                                :verbose t)
         asdf:*central-registry* :test 'equal))

(defvar *load-forces-recompile* nil
  "If nil, the fuzzbuster load-system in load.lisp does not force recompilation.
   Defaults to nil, but can be set before loading load.lisp."
)

(asdf:load-system :cl-ppcre :force *load-forces-recompile*)

(let #+ccl ((asdf:*compile-file-failure-behaviour* :warn))
  #-ccl ()
  (asdf:load-system :ironclad))

;;;   To use the advice-profiler, provided in the CCL
;;; distro, arrange for the package to appear in the asdf:*central-registry*
;;; and add :ccl-advice-profiler to the *features* list.
;;; (The easiest way to put it on the asdf:*central-registry* list is to copy
;;; the advice-profiler directory from the CCL distribution to your local code/lisp/ext
;;; directory.
;;;(pushnew :ccl-advice-profiler *features*)
#+ccl-advice-profiler
(asdf:load-system :profiler)

;;;   To use the sam profiler, found on
;;; github at https://github.com/eugeneia/sam, arrange for the package
;;; to appear in the asdf:*central-registry* and add :ccl-sam-profiler
;;; to the *features* list.
;;;(pushnew :ccl-sam-profiler *features*)
#+ccl-sam-profiler
(asdf:load-system :sam)


(asdf:load-system :fuzzbomb :force *load-forces-recompile*)


;; And switch the fuzzbomb package so that we don't need to prefix
;; anything with the package name.

(in-package :fuzzbomb)

;; Make sure we have our environment set up.
(fb-init-env)

(defun |#F-reader| (stream subchar arg)
  (declare (ignore subchar arg))
  (find-object (read stream t nil t)))

(set-dispatch-macro-character #\# #\F #'|#F-reader|)

(format t "Finished loading :fuzzbomb~%")
(format t "Fuzzbomb is in the :fuzzbomb package.~%")
(format t "You can make the symbols available with:~%")
(format t "(use-package :fb) or (in-package :fb) or :pa :fb~%")


;; (format t "Oh OK, I'll just do it for you!~%")
;;(tpl:do-command "pa" :fb)
#+allegro
(tpl:setq-default top-level::*package* (find-package :fb))

;;; The idea is that we have some standard default *debug-list* items,
;;; including :top (in there by default) and then you can add in ones
;;; for modules you're working on in your local.lisp, which does not
;;; live in svn.
;;; If you want to see everything, (pushnew :all *debug-list*)
(pushnew :top *debug-list*)
(pushnew :amp *debug-list*)
(pushnew :reload *debug-list*)
;;(pushnew :sieve *debug-list*)
;; (pushnew :irm *debug-list*)
;; (pushnew :c3po *debug-list*)
;; (pushnew :exgen *debug-list*)
;; (pushnew :mft *debug-list*)
;; (pushnew :fuzz-wrapper *debug-list*)
;; (pushnew :patcher *debug-list*)
(pushnew :mr *debug-list*)

;;
;;(setf *debug-prefix-function* #'musliner::flag-debug-prefix)
(setf *debug-prefix-function* #'(lambda (keys)
                                  (format nil "~&;; [~A] ~A"
                                          (musliner::tstamp nil)
                                          (musliner::flag-debug-prefix keys))))

(setf *program-name* "FB:")

#+nil
(progn
 (dbug :top "------ SVN version info:")
 (run-command "svn info")       ;; show what version we've just compiled
)
