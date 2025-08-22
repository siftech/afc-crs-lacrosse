(in-package :asd-finder)

(defun file-directory-p (path)
  (directory-exists-p path))

;;;---------------------------------------------------------------------------
;;; The following lifted from CL-FAD (http://www.cliki.net/cl-fad),
;;; which is licensed with the BSD license.  "Most of the code was
;;; written by Peter Seibel for his book Practical Common Lisp."
;;; CL-FAD is maintained by Edi Weitz.
;;;  I could have used CL-FAD, but I wanted this to be a small library
;;; that was loaded early, and so it should have minimal external
;;; dependencies.
;;;---------------------------------------------------------------------------

(defun directory-exists-p (pathspec)
  "Checks whether the file named by the pathname designator PATHSPEC
exists and if it is a directory.  Returns its truename if this is the
case, NIL otherwise.  The truename is returned in directory form as if
by PATHNAME-AS-DIRECTORY."
  #+allegro
  (and (excl:probe-directory pathspec)
       (pathname-as-directory (truename pathspec)))
  #+lispworks
  (and (lw:file-directory-p pathspec)
       (pathname-as-directory (truename pathspec)))
  #+ccl
  (ccl:directoryp pathspec)
  #+clisp
  (ignore-errors (ext:probe-directory pathspec))
  #-(or :allegro :lispworks :ccl clisp)
  (let ((result (probe-file pathspec)))
    (and result
         (directory-pathname-p result)
         result)))

(defun directory-pathname-p (pathspec)
  "Returns NIL if PATHSPEC \(a pathname designator) does not designate
a directory, PATHSPEC otherwise.  It is irrelevant whether file or
directory designated by PATHSPEC does actually exist."
  (and
    (not (component-present-p (pathname-name pathspec)))
    (not (component-present-p (pathname-type pathspec)))
    pathspec))

(defun component-present-p (value)
  "Helper function for DIRECTORY-PATHNAME-P which checks whether VALUE
is neither NIL nor the keyword :UNSPECIFIC."
  (and value (not (eql value :unspecific))))

(defun pathname-as-directory (pathspec)
  "Converts the non-wild pathname designator PATHSPEC to directory
form."
  (let ((pathname (pathname pathspec)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (cond ((not (directory-pathname-p pathspec))
           (make-pathname :directory (append (or (pathname-directory pathname)
                                                 (list :relative))
                                             (list (file-namestring pathname)))
                          :name nil
                          :type nil
                          :defaults pathname))
          (t pathname))))

;;;---------------------------------------------------------------------------
;;; End of material from CL-FAD
;;;---------------------------------------------------------------------------
(defmacro define-constant (name value &optional doc)
  `(defconstant ,name (cond ((and (boundp ',name) (equalp (symbol-value ',name) ,value))
                             (symbol-value ',name))
                            ((boundp ',name)
                             (progn
                               (cerror "Accept new value" "Redefining constant ~s" ',name)
                               ,value))
                            (t ,value))
     ,@(when doc (list doc))))

(define-constant +vcs-names+
  ;; lifted from ASDF...  
  '(".bzr" ".cdv"
    ;; "~.dep" "~.dot" "~.nib" "~.plst" ; we don't support ack wildcards
    ".git" ".hg" ".pc" ".svn" "CVS" "RCS" "SCCS" "_darcs"
    "_sgbak" "autom4te.cache" "cover_db" "_build"
    "debian"))

(defun asd-finder (openlist &key prune verbose prune-names)
  "OPENLIST is a directory or list of directories.
Will search for and return a list of pathnames under the entries
in openlist for asdf system definition files.  Useful for managing
system loading when you are working on more than one lisp system.
   The PRUNE keyword argument can be used to hide some directories
from the asd-finder search.  The prune argument should be a list of
pathnames or namestrings.
   The PRUNE-NAMES keyword argument takes a list of strings.  These will
be matched against directory names.
   ASD-FINDER will do its best to skip over directory names that correspond
to directories created by version control systems.  If you create a directory
called .git or .svn in your code filesystem, you deserve whatever happens
to you!"
  ;; for convenience, we will accept a pathname instead of just a
  ;; list...
  (unless (listp openlist)
    (setf openlist (list openlist)))
  (unless (listp prune)
    (setf prune (list prune)))
  (flet ((promote-pathname (path)
           "Take a pathname that names a directory and return a new pathname with
the corresponding :directory slot value.  This is necessary to do recursive
exploration of a file tree."
           (pathname-as-directory path)))
    (let ((prune-paths
           (loop for prune-me in prune
               as path = (translate-logical-pathname
                          (if (pathnamep prune-me)
                              prune-me
                            (parse-namestring prune-me)))
               collect (promote-pathname path))))
      (flet ((prune-name-p (path)
               (let ((dirname (first (last (pathname-directory path)))))
                 (loop for name in (append prune-names +vcs-names+)
                     if (string= name dirname)
                     return t
                     finally (return nil)))))
        (locally (declare (optimize (speed 3) (safety 1) (debug 1)))
          (loop while openlist
                with acc
                   ;; kill logical pathnames, because the cross-implementation
                   ;; differences are so intractable.  
                as path = (translate-logical-pathname (pop openlist))
                
                unless #+clisp (ext:probe-directory path)
                       #-clisp (probe-file path)
                  do (error "No such pathname: ~a" path)
                when (file-directory-p path)
                  do (setf path (promote-pathname path))
                  and unless (or (member path prune-paths :test #'equalp)
                                 (prune-name-p path))
                        do (let ((pat (merge-pathnames (make-pathname
                                                        :name :wild
                                                        :type "asd")
                                                       path)))
                               (when (directory pat)
                                 (when verbose
                                   (format t "~&Found ASDF directory ~S~%" path))
                                 (push path acc)))
                             (setf openlist
                                   (append (loop for child in (directory (merge-pathnames (make-pathname :name :wild :type :wild) path)
                                                                         #+ccl #+ccl
                                                                         :directories t)
                                                 when (file-directory-p child)
                                                   collect (promote-pathname child))
                                           openlist))
                finally (return acc)))))))
