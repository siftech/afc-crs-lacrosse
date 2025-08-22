;;; -------------------------------------------------------------------------
;;; domains.lisp
;;; - stuff to handle loading diff domains into AMP
;;; - we've mandated the file naming scheme, to make
;;;     life easier on coordination w/ RTS etc...
;;; - for domain X you always have
;;;             - X-amp.lisp    ;; stuff for AMP mostly
;;;             - X-csm.lisp    ;; stuff for CSM only (and RTS to parse_trans)
;;; - $Revision: 1.6 $
;;; -------------------------------------------------------------------------
;;; COPYRIGHT START
;;; COPYRIGHT END
;;; -------------------------------------------------------------------------

(cl:in-package :fuzzbomb)

;;; Lightweight domain object.  For now, just a path to the csm file
;;; and a domain name.
(defclass amp-domain ()
  ((name
    :accessor name
    :initarg :name
    :documentation "Name of the domain as string.  Must match filenames (<name>-csm.lisp, <name>-amp.lisp).")
   (csm-file-path
    :accessor csm-file-path
    :initarg :csm-file-path
    :documentation "Physical path to the csm file associated with this domain.")))

(defmethod initialize-instance :after ((domain amp-domain) &key csm-file-path)
  "Translate any logical pathname to physical pathname for consistency and ease of manipulation."
  (setf (slot-value domain 'csm-file-path) (translate-logical-pathname csm-file-path)))

(defmethod print-object ((domain amp-domain) stream)
  (when *print-readably*
    (error 'print-not-readable :object domain))
  (print-unreadable-object (domain stream :type t)
    (format stream "~s" (name domain))))

(defmethod domain-directory ((domain amp-domain))
  "Return physical path to directory containing files defining the domain."
  (pathname-directory (csm-file-path domain)))

(defmethod amp-file-path ((domain amp-domain))
  "Return physical path to domain's amp file."
  (merge-pathnames (format nil "~a-amp.lisp" (name domain))
                   (directory-namestring (csm-file-path domain))))

;;;-------------------------------------------------------------------------
(#+sbcl locally
 #+sbcl (declare (sb-ext:muffle-conditions style-warning))
 ;; above handles SBCL not liking the deprecated use of &optional and &key
 ;; in the same lambda list.  
 #-sbcl progn
 (defun load-domain (&optional (name nil) &key (dir nil))
   "Load a domain.  If name is specified, it must match a pair of
   files <name>-csm.lisp and <name>-amp.lisp defining the domain.
   If dir is specified, that directory will be searched.  Otherwise,
   the cwd will be searched.  If name is not specified, check for
   a command line arg.  Otherwise, search the indicated directory for
   possibilities and ask the user to choose among them.
   Returns an amp-domain object."
   (dbug :delib-trace "load-domain called with ~A" name)
   (cond (name
          (load-domain-1 name :dir dir))
         (t
          (load-domain-1 (user-picks-domain :dir dir))))))

;;;-------------------------------------------------------------------------
;;; this helper (internal) function expects a string w/ the prefix part
;;; of the  domain name, and it just loads the right files w/ that root.

(defmethod load-domain-1 ((name string) &key dir)
  "Create amp-domain object and call the load-domain-1 method for amp-domain class.
   dir may be supplied to indicate directory to search.  Otherwise, search cwd."
  (let* ((domain-directory (cond (dir dir)
                                 (t (namestring *default-pathname-defaults*))))
         (csm-file-path (merge-pathnames (format nil "~a-csm.lisp" name)
                                         domain-directory)))
    (dbug :delib-trace "loading domain from dir: ~A" domain-directory)
    (load-domain-1 (make-instance 'amp-domain
                     :name name
                     :csm-file-path csm-file-path))))

(defmethod load-domain-1 ((domain amp-domain) &key &allow-other-keys)
  "Load the domain files.  Set *domain* to the domain name."
  (dbug :delib "Loading domain ~A" (name domain))
  (setf *domain* (name domain))
  ; (dbug :delib-trace "Loading csm: ~A" (csm-file-path domain))
  ; (load (csm-file-path domain))
  (dbug :delib-trace "Loading amp: ~A" (amp-file-path domain))
  (load (amp-file-path domain))
  (dbug :delib-trace "Done loading domain"))

;;;-------------------------------------------------------------------------
(defun user-picks-domain (&key (dir nil))
  "Search current directory (or dir, if supplied) for domain
   definition files and ask the user to choose one."
  (let ((domains (find-conformant-domains :dir dir))
        (d nil)
        index )
    (musliner:while (not d)
      (format t "Select which domain to run:~%")
      (musliner:for (i 0 (length domains) 1)
           (format t "     ~A : ~A~%" i
                   (name (nth i domains))))
      (format t "Enter index: ")
      (setf index (read-from-string (read-line)))
      (cond ((and (numberp index)
                  (< index (length domains)))
             (setf d (nth index domains)))
            (T (format t "Invalid index, try again~%"))))
    d))

;;;-------------------------------------------------------------------------
(defun find-conformant-domains (&key (dir nil))
  "Looks in the current directory (or dir, if supplied) to find all domains that
   have both a -csm.lisp and -amp.lisp file defined."
  (let* ((csm-files (cond
                     (dir (directory (merge-pathnames "*-csm.lisp" dir)))
                     (t (directory "*-csm.lisp"))))
         ;; mapcan/list idiom to ignore nils
         (domains (mapcan #'(lambda (path)
                              (let* ((pathname-name (pathname-name path)) ; removes directory and suffix
                                     (domain-name (subseq pathname-name 0 (- (length pathname-name) 4))) ; removes "-csm"
                                     (amp-file-path (merge-pathnames (format nil "~a-amp.lisp" domain-name)
                                                                     (directory-namestring path))))

                                (when (file-exists-p amp-file-path)
                                  (list
                                   (make-instance 'amp-domain
                                     :name domain-name
                                     :csm-file-path path)))))
                          csm-files)))
    domains))

;;;-------------------------------------------------------------------------
