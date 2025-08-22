;;; -*- Package: MUSLINER; Mode: LISP; Base: 10; Syntax: Common-Lisp; -*-
;;; -------------------------------------------------------------------------
;;; musliner-tools.lisp
;;; - general purpose enhancements to lisp environment
;;; - $Header: /projects/coordinators/CVS.r/ALL/code/ext/lisp-libs/musliner-tools/musliner-tools.lisp,v 1.13 2007/05/28 04:48:12 carciofi Exp $
;;; -------------------------------------------------------------------------
;;; COPYRIGHT START
;;; COPYRIGHT END
;;; -------------------------------------------------------------------------

(in-package :musliner)

#+cmu
(defun compile-file-if-needed (filename &key output-file force-compile verbose)
  (let ((pname (parse-namestring filename)))
    ;; if it lacks extension, add one.
    (unless (pathname-type pname)
      (setq pname (make-pathname :defaults pname :type "LISP")))
    ;; verify source exists
    (unless (probe-file pname)
      (error "Cannot find the source file ~s" pname))
    (unless output-file
      (setq output-file
            (make-pathname :defaults pname
                           :type "FASL"
                           ; <> (c:backend-fasl-file-type c:*backend*)
                           )))
    (when (or force-compile
              (not (probe-file output-file))
              (< (file-write-date output-file)
                 (file-write-date pname)))
      (when verbose
        (format t "; ~s last written ~d but~%" pname (file-write-date pname))
        (format t "; ~s last written ~d -> recompile~%" output-file
        (file-write-date output-file)))
      (compile-file pname))))

;;; **********************************************************************
;;; setup ops5 startup macro

;;(defmacro ops5 ()
;;  (require 'ops)
;;  (in-package :ops)
;;  (setf top-level:*prompt* "<ops ~d> "))

;;; **********************************************************************
;;; setup Allegro-dependent search paths according to what machine we are on.
;;; - try to load fasl or lisp version from a subdirectory named after the type
;;; of CPU we are running on, thus allowing us to keep multiple, device
;;; dependent .fasl files around in subdirectories.

;; (defvar *cpu* nil)

;; (cond ((member :SPARC *features*)
;;        (setf *cpu* "sparc"))
;;       ((member :X86 *features*)
;;        (setf *cpu* "x86"))
;;       ((member :MIPS *features*)
;;         (setf *cpu* "mips"))
;;       ((member :M68K *features*)
;;         (setf *cpu* "m68k"))
;;       ((member :TI *features*)
;;         (setf *cpu* "ti"))
;;       (T
;;         (format t "WARNING:   tools.lisp could not determine CPU type for enhanced load")))

#+ALLEGRO-V4.2
(setf system:*load-search-list*
      (list
        (list 'newest
                (make-pathname :directory *cpu* :type "fasl")
                (make-pathname :directory "." :type "fasl")
                ;(make-pathname :directory "." :type "cl")
                (make-pathname :directory "." :type "lisp"))
;       (list 'newest
;               (make-pathname :directory "~musliner/src/lisp" :type "fasl")
;               (make-pathname :directory "~musliner/src/lisp" :type "lisp"))
         excl::*library-code-fasl-pathname*
         excl::*library-code-cl-pathname*))

;; [pelican:20011114.1002CST] This is probably wrong for lots of other
;; configs, too.
#+(and allegro (version<= 5 0))
(setf system:*require-search-list* system:*load-search-list*)

#+(and allegro (version>= 5 0))
(setf system:*require-search-list*
  (append system:*require-search-list* system:*load-search-list*))



;;; **********************************************************************
;;; - string used by default debug prefix to label each debug printout
(defvar *program-name* nil)

(defvar *indent-level* 0)

(defvar *debug-list* (list :top)
  "This flag variable is used to determine which DBUG statements
are actually printed to the terminal.")

(defvar *output-list* nil
  "This flag variable is used to control which DBUG statements
output to files.")

(defvar *output-file* nil
  "The value of this variable should be the STREAM to which you
want DBUG output sent.")

(defun default-debug-prefix (keys)
  (declare (ignore keys))
  *program-name*)
(defun flag-debug-prefix (keys)
  (strcat
   (cond ((consp keys) (symbol-name (first keys)))
         (t (symbol-name keys)))
   ":"))

;;; This variable points to a function that returns a string used as the
;;;     prefix to the debug output.
;;; - Usually used to print out program module name, possibly also a simulation
;;;     time, iteration count, etc.  Stuff you want to see every time a debug
;;;     statement is printed.

(defvar *debug-prefix-function* #'default-debug-prefix)

;;;-------------------------------------------------------------------------
;;; Modified dbug that has special flag :out used to also write out
;;; to the *output-file* same information as is printed to screen.  If you
;;; only want output to file and not stdout, dont include :out in *debug-list*.
;;; - Also, generalized to have an *output-list* of flags that are output to
;;;     the file, so you can mix and match which dbug statements are stored
;;;     to a file and/or printed.  Cool!
;;; [1997/08/20:goldman] Altered to permit multiple flags...
#|
(defun dbug (flags format-string &rest rest-args)
  (when (symbolp flags)
    (setf flags (list flags)))
  (let (string printed-debug-out printed-file-out)
    (dolist (flag flags)

      (when (and (not printed-debug-out) (member flag *debug-list*))
        (setf string
          (apply #'format nil (strcat "~A " format-string "~%")
                 (funcall *debug-prefix-function*) rest-args))
        (setf printed-debug-out t)
        (format t "~A" string))
      (when (and (not printed-file-out) (member flag *output-list*))
        (if (not string)
            (setf string
                  (apply #'format nil (strcat "~A " format-string "~%")
                         (funcall *debug-prefix-function*) rest-args)))
        (setf printed-file-out t)
        (format *output-file* "~A" string)))))
|#

;;;---------------------------------------------------------------------------
;;; dbug should be changed to be a macro because we don't want it to
;;; evaluate its arguments unless the flags are there....
;;;   12/5/02 new feature: if :all is a feature, print out all dbugs.
;;; [2001/10/25:goldman]
;;;---------------------------------------------------------------------------

(defmacro dbug (flags format-string &rest rest-args)
  ;; [2007/03/14:JCC] Create uninterned symbols for bindings to make
  ;; macro safe. I also moved logic for dealing with list of flags so
  ;; this happens at expansion time.
  (when (symbolp flags)
    (setf flags (list flags)))
  (let ((all-in-dbug-var (gensym))
        (all-in-output-var (gensym))
        (print-debug-out-var (gensym))
        (print-file-out-var (gensym)))
    `(let ((*print-pretty* t)
           (,all-in-dbug-var (member :all *debug-list*))
           (,all-in-output-var (member :all *output-list*))
           (,print-debug-out-var (intersection ',flags *debug-list* :test #'eq))
           (,print-file-out-var (intersection ',flags *output-list* :test #'eq)))
       (when (or ,all-in-dbug-var ,all-in-output-var ,print-debug-out-var ,print-file-out-var)
         (let ((string
                (funcall #'format nil ,(strcat "~A " format-string "~%")
                         (funcall *debug-prefix-function* ',flags) ,@rest-args)))
           (when (or ,all-in-dbug-var ,print-debug-out-var)
           ;;; added the ~& because other stuff might leave you in the middle of a line.  
             (format t "~&~A" string))
           (when (or ,all-in-output-var ,print-file-out-var)
             (format *output-file* "~A" string))))
       (values))))

(defun debug-on (&rest flags)
  (if (null flags) (pprint *debug-list*)
    (loop for flag in flags
        do (pushnew flag *debug-list*))))

(defun debug-off (&rest flags)
  (if (null flags) (setf *debug-list* nil)
    (setf *debug-list*
      (loop for flag in *debug-list*
          unless (member flag flags)
          collect flag))))

;;(defun dbug (flag format-string &rest rest-args)
;;  (when (member flag *debug-list*)
;;        (apply #'format t (strcat "~A: " format-string "~%")
;;                        *program-name* rest-args))
;;  (when (member flag *output-list*)
;;        (apply #'format *output-file* (strcat format-string "~%") rest-args)))


;;; -------------------------------------------------------------------------
(defmacro when-dbug (key &body b)
  `(when (member ,key *debug-list*) ,@b))

(defmacro if-dbug (key if-block else-block)
  `(if (member ,key *debug-list*) ,if-block ,else-block))

(defmacro dbug-when (key &body b)
  `(when (member ,key *debug-list*) ,@b))

(defmacro unless-dbug (key &body b)
  `(unless (or (member ,key *debug-list*)
               (member :all *debug-list*))
  ,@b))

(defmacro dbug-unless (key &body b)
  `(unless-dbug ,key ,@b))

;;; -------------------------------------------------------------------------
;;; this give facility to have a dbug appear only once for a given keyword...

(defvar *used-debug-list* nil
        "After a dbug-once for a keyword is printed once, it gets put on this list and will never be printed again.
        Note the time it *is* printed it is really just turned into a (dbug :top...), so if you omit :top from
        *debug-list* it will never happen.")

(defmacro dbug-once (key &body b)
  `(when (not (member ,key *used-debug-list*)) (dbug :top ,@b) (push ,key  *used-debug-list*)))

;;; -------------------------------------------------------------------------

(defvar *debug-list-values* nil
  "List of possible keywords to put in *debug-list*.  Add new entries using (debug-list-value). [CIRCA]")

(defun debug-list-value (keyword docstring)
  "Declare the keyword arg as a valid possible entry in *debug-list*
        (or *output-list*), and assign docstring to describe its function."
;  (when (find keyword *debug-list-values*)
;       (format t "Warning: ~s is already declared as a possible debug-list value~%" keyword))
  (pushnew keyword *debug-list-values*)
  #-SBCL  ;; sbcl warns on this; ccl just ignores it; acl stashes it in a excl field.
  (setf (documentation keyword 'constant) docstring)
  )


(debug-list-value :all
  "Causes all (dbug) calls to turn on... max verbose!")

;;; **********************************************************************
(defmacro perror (format-string &rest allargs)
  `(progn
        (format t ";; ~A: ERROR -- " *program-name*)
        (format t ,format-string ,@allargs)
        (format t "~%")))

;;; **********************************************************************
(defun positive (a) (> a 0))
(defun negative (a) (< a 0))

;;; **********************************************************************
(defun beep () (format t "***** BEEP ***** ~%"))

;;; **********************************************************************
;;; Function ls
;;; - lists directory (default current).

(defun ls ( &optional (pathname "."))
  ;(setf pathname (make-pathname :directory pathname))
  (dolist (filename (directory pathname))
    (format t "~A~%" filename)))

#+allegro
(defun mkdir (dirname)
  (excl:make-directory dirname))
#+cmu
(defun mkdir (dirname)
  (port:mkdir dirname))

#+allegro
(defun getpid () (excl::getpid))
#+cmu
(defun getpid () (unix:unix-getpid))

#+allegro
(defun command-line-argument (index)
  (sys:command-line-argument index))
#+cmu
(defun command-line-argument (index)
  (aref extensions:*command-line-strings* index))


;;; **********************************************************************
;;; Macro sh
;;; - runs shell command
#+allegro
(defun sh (cmd) (run-shell-command cmd))
;;;not sure how to run a shell command in cmucl yet.  Seems like
;;; run-program would probably do it, but I'm not sure.  

;;; **********************************************************************
;;; pwd
;;; - prints current (working) directory

#+(and allegro-version>= (version>= 5))
 (defun pwd ()
  *default-pathname-defaults*)
#-(and allegro-version>= (version>= 5))
 (defun pwd ()
  (UIOP/OS:GETCWD))

;;; **********************************************************************
;;; Macro cd
;;; - changes current (working) directory

#+(and allegro-version>= (version>= 5))
 (defun cd (dir)
   (if (pathnamep dir)
      (setf *default-pathname-defaults* dir)
    (setf *default-pathname-defaults* (make-pathname :directory dir))))
#+cmu
(import 'port:chdir)
#-(and allegro-version>= (version>= 5))
 (defun cd (dir)
  (uiop/os:chdir dir))

(defvar *directories* nil)
(defun pushd (d)
  (push (pwd) *directories*)
  (cd d))
(defun popd ()
  (cd (pop *directories*)))
;;; **********************************************************************
;;; Macro while
;;; - execs body as long as pred returns non-nil value.
;;; - returns nil

(when (not (fboundp 'while))
 (defmacro while (pred &body body)
  `(loop (unless ,pred (return nil)) ,@body)))

;;;***********************************************************************
;;; Macro for
;;; (for (variable start-value end-value increment) S-expression*)
;;; A 'for' loop like a normal language would have.

(defmacro for (arg-list &body body)
"(for (variable start-value end-value increment) S-expression*)
        - A 'for' loop like a normal language would have (  tools)."

  (let  ((var (first arg-list))
         (start (second arg-list))
         (end (third arg-list))
         (inc (fourth arg-list)))
        (append (list 'do (list (list var start (list '+ var inc)))
                        (list (list '= var end)))
                body)))

;;;***********************************************************************

(defmacro setappend (a b) `(setf ,a (append ,a ,b)))

;;;***********************************************************************
;;; Function rank-and-choose

;;;(defvar *ranked-list* nil)
;;;(defvar *sorted-list* nil)

(defun rank-and-choose (rank-function choose-function arglist &rest rest-args)
  "(rank-and-choose rank-function choose-function arglist &rest rest-args)
        - Applies choose-function to select a single member of arglist
          based on rankings given by rank-function (with rest-args) (  tools).
        - example: (rank-and-choose #'first #'max '((5 foo) (6 boo) (1 roo)))."

  (if (null arglist)
      ;(warn "Rank-and-choose called with null list.")
        nil
    (let* ((ranked-list (apply #'my-mapcar
                                (list* rank-function arglist rest-args)))
           (bestrank (apply choose-function ranked-list)))
      (values (nth (position bestrank ranked-list) arglist) bestrank))))

;;(defun rank-and-choose (rank-function choose-function arglist &rest rest-args)
;;  "(rank-and-choose rank-function choose-function arglist &rest rest-args)
;;        - Applies choose-function to select a single member of arglist
;;          based on rankings given by rank-function (with rest-args) (  tools).
;;        - example: (rank-and-choose #'first #'max '((5 foo) (6 boo) (1 roo)))."
;;
;;  (let  ((ranked-list (apply #'my-mapcar
;;                              (list* rank-function arglist rest-args))))
;;        (nth (position (apply choose-function ranked-list) ranked-list)
;;             arglist)))
;;

;;;-----------------------------------------------------------------
;;; like rank and choose above, but uses a full sort (avoiding things like
;;;     512 element limitation of #'min if used in above.  Also, allows us
;;;     to specify if want the top 'bandwidth' elements returned, not just 1
(defun band-rank-and-choose (rank-function sort-function bandwidth
                                           arglist &rest rest-args)
  (let* ((return-val nil)
         (ranked-list (apply #'my-mapcar
                               (list* rank-function arglist rest-args)))
         ;; need to copy-list so ranked-list remains undisturbed.
         (sorted-list (sort (copy-list ranked-list) sort-function)))
    (dolist (el (subseq sorted-list 0 (min (length sorted-list) bandwidth)))
      (setappend return-val (list (nth (position el ranked-list) arglist))))
    return-val))

;;;-----------------------------------------------------------------
(defun unique-p (list &key (test #'eq) &aux (return-val t))
  "(unique arg-list &key (test #'eq))
        - returns T if every element is unique according to pairwise application
          of test [test should return T if 2 arguments are not unique] ( )."
  (dolist (element1 list)
        (dolist (element2 (rest (member element1 list)))
                (when (apply test (list element1 element2))
                        (setf return-val nil)
                        (return))))
  return-val)

;;;***********************************************************************
;;; - note we do not expect anyone to ever pass in a non-nil second arg.

(defun permute (choices &optional (chosen nil))
  "(permute choices)
        - returns list of all ordered permutation of choices list ( )."
  (let  ((return-val nil))
                ;; if only 1 choice left, return full permutation formed from
                ;; that choice and others already chosen.
        (cond ((= (length choices) 1)
                (setf return-val (list (cons (first choices) chosen))))
                ;; else, make all choices at this stage, appending returns from
                ;; recursive calls.
              (t (dolist (choice choices)
                        (setappend return-val
                           (permute (remove choice choices)
                                             (cons choice chosen))))))
        return-val))

;;;------------------------------------------------------------------------
;;; takes in a list of lists of choices; returns a list consisting of
;;; all possible ordered combinations of individual choices.
;;; (set-choose '((a b) (c d))) --> ((C A) (D A) (C B) (D B))
;;; - note set-choose reverses order of output lists.

(defun set-choose (lists &key (preserve-order nil))
  (let ((results nil))
    (loop for list in lists
       do (setf results
                (if results
                    (loop for r in results
                       append (loop for v in list collect (cons v r)))
                    (mapcar #'list list))))
    (cond (preserve-order
           (mapcar #'reverse results))
          (t
           results))))

;;;------------------------------------------------------------------------
;;;
(defun choose-unique-set (choices count &key (test #'eq))
  "Returns a list of count unique items picked randomly from choices"
  (assert (>= (length choices) count) ()
    "There are not enough choices to give a random subset!")
  (let ((chosen nil))
    (loop while (< (length chosen) count)
        for choice = (random-choice choices)
        do (pushnew choice chosen :test test))
    chosen))


;;;***********************************************************************
;;; - note we do not expect anyone to ever pass in a non-nil third arg.

(defun choose (choices count &optional (chosen nil))
  "(choose choices count)
        - returns list of all possible unordered lists with 'count' elements
          taken from 'choices' list (no duplication of elements) ( )."
  (let  ((return-val nil))
                ;; if have chosen enough, return full permutation.
        (cond ((= (length chosen) count)
                (setf return-val (list chosen)))
                ;; else, make all choices at this stage, appending returns from
                ;; recursive calls.
              (t (dolist (choice choices)
                        (setappend return-val
                           (choose (rest (member choice choices))
                                   count (cons choice chosen))))))
        return-val))

;;;---------------------------------------------------------------------------
;;; produces a sequence with count copies of value in it.

(defun dupe (value count)
  (cond ((or (not (integerp count))
             (< count 0))
         (error "Bad count argument."))
        ((> count 0)
         (cons value (dupe value (1- count))))
        (t
         nil)))

;;;***********************************************************************
;;; - note we do not expect anyone to ever pass in a non-nil third arg.

(defun dupe-choose (choices count &optional (chosen nil))
  "(dupe-choose choices count)
        - returns list of all possible ordered lists with 'count' elements
          taken from 'choices' list, including duplication of elements ( )."
  (let  ((return-val nil))
                ;; if have chosen enough, return full permutation.
        (cond ((= (length chosen) count)
                (setf return-val (list chosen)))
                ;; else, make all choices at this stage, appending returns from
                ;; recursive calls.
              (t (dolist (choice choices)
                        (setappend return-val
                           (dupe-choose choices count (cons choice chosen))))))
        return-val))

;;;***********************************************************************
(defun factorial (n)
  (cond ((zerop n) 1)
        (t (* n (factorial (1- n))))))

(defun combinations (n k)
  "n choose k."
  (/ (factorial n) (* (factorial k)(factorial (- n k)))))

;;;***********************************************************************
;;; Function random-choice
;;; - returns randomly selected element of arg list
;;; Now defined in stochastic.lisp

;(defun random-choice (arg)
;  (cond ((null arg) nil)
;        (T (nth (random (length arg)) arg))))

;;;------------------------------------------------------------------------
(defun average (&rest args)
  (/ (apply #'+ args) (length args)))

;;;***********************************************************************
;;;;; from umass-extended-lisp.lisp
;;(defmacro strcat (&rest strings)
;;  "strcat {simple-string}*
;;      Concatenates all the strings together.  Each argument must be
;;      acceptable to the STRING function."
;;
;;  `(concatenate 'simple-string
;;                ;; Make sure that each argument is a string.
;;                ,@(mapcar #'(lambda (x)
;;                              (if (stringp x) x `(string ,x)))
;;                          strings)))

;;; this is the function version; can be used w/ apply.
#-ALLEGRO-V4.2
(defun strcat (&rest strings)
  "strcat {simple-string}*
        Concatenates all the strings together.  Each argument must be
        acceptable to the STRING function."

  (apply #'concatenate (cons 'simple-string
                ;; Make sure that each argument is a string.
                (mapcar #'(lambda (x)
                              (if (stringp x) x (string x)))
                          (remove-if-not #'identity strings)))))

;;;***********************************************************************
(defmacro null-string (s)
  `(string= "" ,s))

;;;-------------------------------------------------------------------------

(defun dottify-alist (alist)
  "(dottify-alist list)
        - Converts non-dotted alist to dotted form ( ).
        - ex: (dottify-alist '((a b) (c d))) --> ((a . b) (c . d))"
  (mapcar #'(lambda (pair) (cons (first pair) (second pair))) alist))

;;;-------------------------------------------------------------------------

(defun trim-suffix (c s &key (test #'eq) &aux pos)
  "(trim-suffix C S)
   - Trims off end of sequence S, starting with first instance of item C ( ).
        Returns all of S if no C present."
  (if (setf pos (position c s :test test))
        (subseq s 0 pos)
        s))

(defun trim-prefix (c s &key (test #'eq) &aux pos)
  "(trim-prefix c s)
   - Trims off beginning of sequence S, up to and including first instance
        of item C ( ).  Returns nil if no C in S."
  (if (setf pos (position c s :test test))
        (subseq s (1+ pos) (length s))
        nil))

;;;-------------------------------------------------------------------------
;;; returns a suffix of the arg sequence with all prefix elements for
;;; which fn returns non-nil trimmed off.  Seq. is assumed to be ordered so
;;; that as soon as one element fails to satisfy fn, all rest will.
;;; Ex; (trim-sequence-prefix '(1 2 3 4 5 6) #'(lambda (x) (< x 3)))
;;;     --> (3 4 5 6)

(defun trim-sequence-prefix (seq fn)
  (cond ((null seq) nil)
        ((funcall fn (first seq)) (trim-sequence-prefix (rest seq) fn))
        (t seq)))

;;;***********************************************************************
;;; Macro setvar
;;; - declares global var and sets to the value.

;;; [2007/03/14:JCC] Isn't this the same as defparameter?
(defmacro setvar (var value &optional doc)
  `(progn
        (defvar ,var ,value ,doc)
        (setf ,var ,value)))

;;;***********************************************************************
;;; Macro getassoc
;;; - returns element of alist associated w/ key.

(defmacro getassoc (key alist
                        &key (test (quote #'eq))
                        (proper nil))
  "Returns element of alist associated w/ key.
By default, operates on  -ALISTS, where the
value associated with the key is the second element,
rather than being made up of dotted pairs.  If the
PROPER keyword argument is T, will operate on alists
of dotted pairs."
  ;; added the let-binding to maximize common code and
  ;; also to permit later addition of error-checking,
  ;; if desired  
  ;; [2007/03/14:JCC] Create uninterned symbol for binding to make macro safe.
  (let ((cell-var (gensym)))
    `(let ((,cell-var (assoc ,key ,alist :test ,test)))
       ,(if (constantp proper)
            (if proper
                `(cdr ,cell-var)
                `(second ,cell-var))
            `(if ,proper
                (cdr ,cell-var)
                (second ,cell-var))))))

;;;----------------------------------------------------------------------------
;;; increments the value associated w/ key in alist.
;;; - sets to one if key has no assoc value yet.

(defmacro incfassoc (key alist &optional (incr 1) &key (test (quote #'eq)))
  ;; [2007/03/14:JCC] Create uninterned symbol for binding to make macro safe.
  (let ((var (gensym)))
    `(let ((,var (assoc ,key ,alist :test ,test)))
       (if ,var (incf (second ,var) ,incr)
         (setf ,alist (cons (list ,key 1) ,alist))))))

;;;---------------------------------------------------------------------------
;;; prints a "." every time you pass value that is mod milestone.

(defmacro milestone-incf (counter &optional (incr 1) (milestone 100) (stream t))
  `(prog1 (incf ,counter ,incr)
          (when (zerop (mod ,counter ,milestone)) (format ,stream "."))))

;;;***********************************************************************
;;; Macro setassoc
;;; - deletes any element associated w/ key in alist, then
;;; adds an entry associating key w/ new-element.
;;; - note it must evaluate the new element before deleting old element, in case
;;;     new depends on old.

(defmacro setassoc (key new-element alist &key (test (quote #'eq)))
  ;; [2007/03/14:JCC] Create uninterned symbols for bindings to make macro safe.
  (let ((new-element-var (gensym))
        (old-assoc-var (gensym)))
    `(let* ((,new-element-var ,new-element)
            (,old-assoc-var (assoc ,key ,alist :test ,test)))

       (if ,old-assoc-var (setf ,alist (delete ,old-assoc-var ,alist)))
       (push (list ,key ,new-element-var) ,alist))))

;;;----------------------------------------------------------------------------
;;; - deletes any element associated w/ key in alist, then
;;; adds an entry associating key w/ new-element.
;;; new-element is treated as an atom, and new associated element will be list.

(defmacro setlassoc (key new-element alist)
  ;; [2007/03/14:JCC] Create uninterned symbols for bindings to make macro safe.
  (let ((new-element-var (gensym))
        (old-assoc-var (gensym)))
    `(let* ((,new-element-var ,new-element)
            (,old-assoc-var (assoc ,key ,alist)))

       (if ,old-assoc-var (setf ,alist (delete ,old-assoc-var ,alist)))
       (push (list ,key (list ,new-element-var)) ,alist))))

;;;----------------------------------------------------------------------------
;;; adds an element to the value associated in alist with key.  Note
;;; new-element is treated as an atom, and new associated element will be list.

(defmacro addlassoc (key new-element alist &key (test (quote #'eq)))
  ;; [2007/03/14:JCC] Create uninterned symbol for binding to make macro safe.
  (let ((old-assoc-var (gensym)))
    `(let* ((,old-assoc-var (assoc ,key ,alist :test ,test)) old-value)

       (cond (,old-assoc-var
              (setf old-value (second ,old-assoc-var))
                  ;;   this seems really inefficient
              (setf ,alist (remove ,old-assoc-var ,alist))
              (push (list ,key (append old-value (list ,new-element))) ,alist)
              )
             (t (push (list ,key (list ,new-element)) ,alist))))))
;;(defmacro addlassoc (key new-element alist)
;;  `(let* ((old-assoc (assoc ,key ,alist)) old-value)
;;
;;        (cond (old-assoc
;;                (setf old-value (second old-assoc))
;;                (setf ,alist (remove old-assoc ,alist))
;;                (push (list ,key (append old-value (list ,new-element))) ,alist)
;;                )
;;              (T (push (list ,key (list ,new-element)) ,alist)))))
;;
;;;***********************************************************************
;;; Macro *=
;;; - simple macro to ease multiplication.

(defmacro *= (location increment)
  `(setf ,location (* ,location ,increment)))

;;;***********************************************************************
;;; Macro +=
;;; - simple macro to ease incrementing

(defmacro += (location increment)
  `(incf ,location ,increment))

;;;***********************************************************************
;;; Macro -=
;;; - simple macro to ease decrementing

(defmacro -= (location increment)
  `(decf ,location ,increment))

;;;-------------------------------------------------------------------------
;;; this is like the C function for ++.... ie, if you want to get the
;;; increment operator to happen after you use the value of the thing
;;; you are incrementing (e.g., in C you do something like array[i++]

(defmacro postincf (loc &optional (incr 1))
  `(prog1 ,loc (incf ,loc ,incr)))

;;;-------------------------------------------------------------------------
(defmacro maxf (a b)
  "Sets the location A to the maximum of the value of A or B"
  `(setf ,a (max ,a ,b)))

;;; **********************************************************************
(defvar *last-file-loaded* nil)
;;; **********************************************************************
;;; Macro !
;;; - reloads the last file loaded

#-SCLE
(defmacro ! () (load *last-file-loaded*))

;;; **********************************************************************
;;; Macro !!
;;; - repeats the last command  (specific to Allegro)

#+(and (not SCLE) allegro)
(defmacro !! () (eval +))


;;; **********************************************************************
;;; Macro --
;;; - decrements argument by one

(defmacro -- (foo) `(decf ,foo))

;;; **********************************************************************
(defun my-remove-if (function arglist &rest rest-args)
  "(my-remove-if predicate arglist &rest rest-args)

        Returns list of elements of arglist for which predicate returns nil
        when applied to a list of the element plus rest-args (  tools)."

  (let  ((return-val nil))
        (dolist (element arglist)
                (if (not (apply function (list* element rest-args)))
                    (setf return-val (append return-val (list element)))))
        return-val))


;;; **********************************************************************
(defun my-remove-if-not (function arglist &rest rest-args)
  "(my-remove-if-not predicate arglist &rest rest-args)

        Returns list of elements of arglist for which predicate returns non-nil
        when applied to a list of the element plus rest-args (  tools)."

  (let  ((return-val nil))
        (dolist (element arglist)
                (if (apply function (list* element rest-args))
                    (setf return-val (append return-val (list element)))))
        return-val))

;;;---------------------------------------------------------------------------
;;; This function from Peter Norvig's text...
;;; copyright (c) 1991 Peter Norvig
;;;---------------------------------------------------------------------------
(defun find-all (item sequence &rest keyword-args
                 &key (test #'eql) test-not &allow-other-keys)
  "Find all those elements of sequence that match item,
  according to the keywords.  Doesn't alter sequence."
  (if test-not
      (apply #'remove item sequence
             :test-not (complement test-not) keyword-args)
      (apply #'remove item sequence
             :test (complement test) keyword-args)))

;;; **********************************************************************
(defun my-every (function arglist &rest rest-args)
  "(my-every predicate arglist &rest rest-args)

        Predicate is applied to a list made of element 0 of arglist
        and rest-args, then element 1 and rest-args, etc.  Returns T if every
        predicate returns non-nil value, else nil (  tools)."

  (let  ((return-val t))
        (dolist (element arglist)
                (when (null (apply function (list* element rest-args)))
                        (setf return-val nil)
                        (return)))
        return-val))

;;; **********************************************************************
(defun any (function arglist &rest rest-args)
  "(any predicate arglist &rest rest-args)

        Predicate is applied to a list made of element 0 of arglist
        and rest-args, then element 1 and rest-args, etc.  Returns element when
        predicate returns first non-nil value, or nil if end of arglist is
        reached (  tools)."

  (let  ((return-val nil))
        (dolist (element arglist)
                (when (apply function (list* element rest-args))
                        (setf return-val element)
                        (return)))
        return-val))

;;; **********************************************************************
(defun my-mapcar (function arglist &rest rest-args)
  "(my-mapcar function arglist &rest rest-args)

        Returns list made up of return from function applied to a list made of
        element 0 of arglist and rest-args, then element 1 and rest-args, etc.
        (  tools)."
  (mapcar #'(lambda (e) (apply function e rest-args)) arglist))

;;;-----------------------------------------------------------------
(defun map-pairs (fn arg-list)
  "(map-pairs fn arg-list)
        - returns a list made up of results of fn called on each ordered
                pair of elements in arg-list.
        - E.g., (map-pairs #'+ '(1 2 3 4)) --> (3 5 7)   (  tools)."

  (let ((return-val nil))
    (for (i 0 (1- (length arg-list)) 1)
          (setappend return-val
                (list (funcall fn (nth i arg-list) (nth (1+ i) arg-list)))))
    return-val))

;;;-----------------------------------------------------------------

(defun map-by-twos (fn arg-list)
  "(map-by-twos fn arg-list)
        - returns a list made up of results of fn called on each disjoint
                ordered pair of elements in arg-list.
        - E.g., (map-by-twos #'+ '(1 2 3 4)) --> (3 7)   (  tools)."

  (cond ((null arg-list) nil)
        (t (cons (funcall fn (first arg-list) (second arg-list))
                 (map-by-twos fn (rest (rest arg-list)))))))

;;; Rewrote this extensively, because CMULISP discovered that
;;; (quote rest-args) was always true (since it's just a verbose way of
;;; writing 'rest-args)....  
(defun map-stride (function arglist offset stride &rest rest-args)
  "(map-stride function list offset stride &rest rest-args)

        Applies function to the elements of arglist which are stride-apart,
starting with element # offset, returning list made up of cons'd results of
the function, or the original element if function was not run (see also
map-odd and map-even) (  tools)."

  (decf stride)                         ;convert to base zero, I think....
  (loop for element in arglist
        with index of-type fixnum = offset
        if (= index 0)
          do (setf index stride)
          and collect (apply function element rest-args)
        else ;(not (= index 0))
        do (decf index)
        and collect element))

(defmacro map-even (function arglist &rest rest-args)
"(map-even function list offset stride &rest rest-args)
        See map-stride"

  `(map-stride ,function ,arglist 0 2 ,@rest-args))

(defmacro map-odd (function arglist &rest rest-args)
"(map-odd function list offset stride &rest rest-args)
        See map-stride"

  `(map-stride ,function ,arglist 1 2 ,@rest-args))

;;;-------------------------------------------------------------------------
;;; split takes in a list and function (that itself takes as args two of the
;;;     list elements).  Split walks down the list, consing up a new
;;;     list of sublists, where each sublist consists of the neighboring
;;;     original entries for which the function, when applied pairwise, returns
;;;     non-nil.
;;;
;;; Ex: (split '(1 2 3 4 5 8 10 11 98 99) #'(lambda (x y) (> (- y x) 1)))
;;;             --> ((1) (2) (3) (4) (5 8 10) (11 98) (99))
;;; NOTE: not currently protected against lists of < 2 elements.

(defun split (arglist fn)
  (let ((map (map-pairs fn arglist))
        (result (list (list (pop arglist)))))

  (dolist (m map)
        (cond (m (setappend (first (last result)) (list (pop arglist))))
              (t (setappend result (list (list (pop arglist)))))))
  result))


;;; **********************************************************************
(defun sum-mapcar (function arglist)
"(sum-mapcar function arglist)
 - Returns sum of values of function applied to each element of arglist ( )."
  (let ((sum 0))

  (dolist (el arglist) (incf sum (funcall function el)))
  sum))

;;;-------------------------------------------------------------------------
;;; Maps the function over the arglist, returning an assoc list in which each
;;; arglist element is associated w/ its function call result.

(defun mapassoc (fn arglist)
  (cond ((null arglist) nil)
        (t (cons (list (first arglist) (funcall fn (first arglist)))
                 (mapassoc fn (rest arglist))))))

;;; **********************************************************************
(defun docfun (function) (documentation function 'function))
(defun docvar (function) (documentation function 'variable))

;;;***************************************************************************
;;; Function variable-p
;;; - returns T if sym is a symbol starting with ?

(defun variable-p (sym)
  (and (symbolp sym)
       (equal (char (symbol-name sym) 0) #\?)))

;;; ***************************************************************************
;;; brings all elements out to atom level.

(defun delistify (l &aux j)
"(delistify arg)

        Brings all atoms in argument out to top level of list (  tools).
                e.g., (delistify '(a (b (c)))) --> (a b c)
     aka: flatten"

  (cond ((null l) nil)
        ((atom l) l)
        (t (setf j (delistify (first l)))
           (if (atom j)
                (cons j (delistify (rest l)))
                (append j (delistify (rest l)))))))

;;;------------------------------------------------------------------------
(defun ensure-list-if-atom (atom-or-list)
  "If arg is a list, return it.  If arg is an atom, return fresh list containing it."
  (etypecase atom-or-list
    (list atom-or-list)
    (atom (list atom-or-list))))


;;;------------------------------------------------------------------------
;; brings all elements up one level in list hierarchy (except atoms already
;; at top level).

(defun flatten-one-level (l &optional accum)
  (cond ((null l) accum)
        ((atom l) (cons l accum))
        ((null (first l)) (flatten-one-level (rest l) accum))
        ((atom (first l))
          (cons (first l)
                 (flatten-one-level (rest l) accum)))
        (t (cons (first (first l))
                 (flatten-one-level (cons (rest (first l)) (rest l)) accum)))))


;;; ***************************************************************************
;; (defmacro my-compile (file)
;;   `(compile-file-if-needed ,file :output-file (make-pathname :name ,file
;;                                                         :directory *cpu*
;;                                                         :type #+allegro "fasl"
;;                                                         #+(and cmu x86)
;;                                                         "x86f")))

#+TI
(defmacro my-compile (file)
  `(compile-file ,file :output-file
                (make-pathname :name ,file :directory *cpu* :type "fasl")))

;;; this version does not match the comments above: it tries to load the
;;; **********************************************************************
;;; Function package-call
;;; - allows us to make a function call given the package in a variable, so
;;;     we dont depend on fixed previous locations of functions we shadow:
;;; - store their old packages in vbles, then use package-call to get to the
;;;     original versions.
;
;(defun package-call (package function &rest args)
;  (eval (read-from-string (format nil
;               "(apply #'~A:~A '~S)" package function args))))
;
;;;; **********************************************************************
;
;(defvar *old-load-package* (package-name (symbol-package 'load)))


;(shadow 'load)
;
;;;; **********************************************************************
;;;; Function load
;
;(defun load (filename)
;  (setf *last-file-loaded* filename)
;  (let* ((faslname (make-pathname
;                               :directory *cpu*
;                               :name filename
;                               :type "fasl"))
;        (lispname (make-pathname
;                               :directory *cpu*
;                               :name filename
;                               :type "lisp")))
;
;       (cond ((probe-file faslname)
;               (package-call *old-load-package* 'load faslname))
;             ((probe-file lispname)
;               (package-call *old-load-package* 'load lispname))
;             (T
;               (package-call *old-load-package* 'load filename)))))
;
;
;;;; **********************************************************************
;(defvar *old-require-package* (package-name (symbol-package 'require)))
;
;;(shadow 'require)
;
;;;; **********************************************************************
;;;; Function require
;
;(defun my-require (package &optional (filename nil))
;  (if (not filename) (setf filename (string-downcase (symbol-name package))))
;  (let* ((faslname (make-pathname
;                               :directory *cpu*
;                               :name filename
;                               :type "fasl"))
;        (lispname (make-pathname
;                               :directory *cpu*
;                               :name filename
;                               :type "lisp")))
;
;       (cond ((probe-file faslname)
;               (format t "calling require with cpu/*.fasl~%")
;               (package-call *old-require-package* 'require package faslname))
;             ((probe-file lispname)
;               (format t "calling require with cpu/*.lisp~%")
;               (package-call *old-require-package* 'require package lispname))
;             (T
;               (format t "calling require with orig filename~%")
;               (package-call *old-require-package* 'require package filename)))))
;

(defun stringify (s)
  (format nil "~S" s))

(defun pl (list)
  (dolist (item list)
         (format t "~A~%" item)))


;;;***************************************************************************
;;; shorthand way to find source file.
#+allegro
(defun sf (obj)
   (source-file obj))

;;;***************************************************************************
(defmethod lp ((l cons))
  (mapc #'lp l))
  ;(dolist (i l) (format t "~A~%" i)))

(defmethod lp ((s string))
  (format t "~A " s))

(defmethod lp ((n number))
  (format t "~A " n))

(defmethod lp ((s symbol))
  (format t "~A " s))

;;;***************************************************************************
;;; alternative version of long list printer (mnemonic: print list)
;; (defun pl (arg)
;;   (format t "( ")
;;   (dolist (a arg) (format t "~A " a))
;;   (format t ")~%"))

;;;-------------------------------------------------------------------------
;;; useful to set Allegro debug parameters for a deep dive into details.

#+ALLEGRO
(defun deep ()
  (setf TOP-LEVEL:*ZOOM-PRINT-LENGTH* 10)
  (setf TOP-LEVEL:*ZOOM-PRINT-LEVEL* 8)
  (setf TOP-LEVEL:*PRINT-LEVEL* 10)
  (setf TOP-LEVEL:*PRINT-LENGTH* 100)
  (setf INSPECT::*INSPECT-LENGTH* 30))

;;;---------------------------------------------------------------------------
#+cmu
(defun my-load (file)
  (compile-file-if-needed file)
  (load file))

;;;---------------------------------------------------------------------------
;;; This lets you do :MY-LOAD (or :ML) as an Allegro command.
;;;---------------------------------------------------------------------------
#+allegro
(defun do-my-load (arg)
  (my-load (symbol-name arg)))

#+allegro
(top-level:alias ("ml" :case-sensitive) (arg)
  (do-my-load arg)
  )

#+allegro
(top-level:alias ("my-load" :case-sensitive) (arg)
  (do-my-load arg))

;;;---------------------------------------------------------------------------
(defun file-exists-p (filename)
  "This will return true if there is a file named FILENAME and if it
is a file.  If you want a function that will return true if the
FILENAME corresponds to a file OR DIRECTORY, then you should use
CL:PROBE-FILE instead."
  (let ((stream-or-nil (open filename :if-does-not-exist nil)))
    (when stream-or-nil
      (close stream-or-nil))))

;;;---------------------------------------------------------------------------
;;; destructively modifies and
;;; returns a string in which every occurrence of oldch character is
;;; replaced by newch.

(defun string-subst! (str oldch newch)
  (for (i 0 (length str) 1)
        (setf (char str i)
                (if (char= (char str i) oldch)
                    newch
                   (char str i))))
  str)

(defun fix-spaces (str)
  (substitute #\_ #\space str))


;;; -------------------------------------------------------------------------
;;; replaces all the dashes in a string with underscores, so that action
;;; names etc are all using underscores, for compatibility w/ the
;;; names in C code in RTS.

(defun fix-dashes (str)
  (substitute #\_ #\- str))

;;;---------------------------------------------------------------------------
;;; These are the memoization functions from Norvig, p270++
;;;; Code from Paradigms of AI Programming
;;;; Copyright (c) 1991 Peter Norvig
;;;; The Memoization facility:

(defmacro defun-memo (fn args &body body)
  "Define a memoized function."
  `(memoize (defun ,fn ,args . ,body)))

(defun memo (fn &key (key #'first) (test #'eql) name)
  "Return a memo-function of fn."
  (let ((table (make-hash-table :test test)))
    (setf (get name 'memo) table)
    #'(lambda (&rest args)
        (let ((k (funcall key args)))
          (multiple-value-bind (val found-p)
              (gethash k table)
            (if found-p val
                (setf (gethash k table) (apply fn args))))))))


;; this one has a hack to note/count when we hit cache, to see if working.
;(defun memo (fn &key (key #'first) (test #'eql) name)
;  "Return a memo-function of fn."
;  (let ((table (make-hash-table :test test)))
;    (setf (get name 'memo) table)
;    #'(lambda (&rest args)
;        (let ((k (funcall key args)))
;          (multiple-value-bind (val found-p)
;              (gethash k table)
;            (if found-p (progn (incf a)(format t "HIT MEMO CACHE!~%") val)
;                (setf (gethash k table) (apply fn args))))))))

(defun memoize (fn-name &key (key #'first) (test #'eql))
  "Replace fn-name's global definition with a memoized version."
  (clear-memoize fn-name)
  (setf (symbol-function fn-name)
        (memo (symbol-function fn-name)
              :name fn-name :key key :test test)))

(defun clear-memoize (fn-name)
  "Clear the hash table from a memo function."
  (let ((table (get fn-name 'memo)))
    (when table (clrhash table))))

;;;---------------------------------------------------------------------------
;;; helper macro to make it clear when you're setting value of a hash entry.
;;;

(defmacro sethash (key table value)
  `(setf (gethash ,key ,table) ,value))

(defmacro setdelete (thing set &rest all)
  `(setf ,set (delete ,thing ,set ,@all)))
;;;---------------------------------------------------------------------------
;;; helper to retrieve list of all the keys. Weird you cannot do this natively.

(defun hash-keys (hash)
  (let ((keys nil))
    (maphash #'(lambda (k v)
                 (declare (ignore v))
                 (push k keys))
             hash)
    keys))

(defun hash-values (hash)
  (let ((vals nil))
    (maphash #'(lambda (k v)
                 (declare (ignore k))
                 (push v vals))
             hash)
    vals))

(defun format-hash (hash)
  (maphash #'(lambda (k v)
               (format t "~a ~a~%" k v))
           hash))

;;;---------------------------------------------------------------------------
;;; helper to do what maphash should do ... collect the results of calling
;;; function on each of the (hash-key hash-entry) pairs.

(defun my-maphash (fn hash)
  (let ((result nil))
    (maphash #'(lambda (k v)
                 (push (funcall fn k v) result))
             hash)
    result))


;;;---------------------------------------------------------------------------
;;; Since Lisp's intersection function (cltl2 p 429) is not reqd to retain
;;; any ordering, we need this one... it returns intersection elements in
;;; the same order they appear in seq1.

(defun ordered-intersection (seq1 seq2 &key (test #'eq))
  "Return the intersection of seq1 and seq2, in order of appearance in seq1"
 (loop for e in seq1
   when (member e seq2 :test test)
     collect e))

(defun any-duplicate-p (seq &key (test #'eq))
  (loop for sublist on seq
      thereis (member (car sublist) (cdr sublist) :test test)))

(defun set-equal (list-1 list-2 &key (key #'identity) (test #'eql))
  "Returns t, when list-1 and list-2 contain the same elements."
  (null (set-exclusive-or list-1 list-2 :key key :test test)))


;;;-------------------------------------------------------------------------
;;; HOSTNAME
;;;-------------------------------------------------------------------------
(defun hostname ()
 (or
  #+allegro (excl.osi:gethostname)
  #+cmu (port:getenv "HOSTNAME")
  #+ccl (ccl:getenv "HOSTNAME")
  #+asdf3 (uiop:getenv "HOSTNAME")
  (error "Do not know how to get host name on this lisp.")))

;;;---------------------------------------------------------------------------
;;; Restricted var macros

;;; (define-restricted-var name '(:one :two))
;;; defines a variable called name and a set-name function
;;; (where any leading or trailing asterisks have been removed)
;;; and a check-name function (again, no leading or trailing *s)
;;; and an assert-name function (no trailing or leading *s).
;;; assert-name asserts that the value is in the range for the
;;; restricted var (causing a runtime error if the value is
;;; out of bounds).  check-name returns t if the value is
;;; in the accepted range, nil otherwise.

;;; default value is the first in the valid values list

;;; FIXME check-name would best be defined as a macro, but that would
;;; create ordering problems for source code compilation, which
;;; we are not very good at maintaining...

;;; FIXME not properly using test arg yet (in member calls)

(defun no-stars-symbol-name (symbol)
  (string-trim "*" (symbol-name symbol)))

(defun rv-symbol-cat (prefix symbol)
  (let ((sym-name (no-stars-symbol-name symbol)))
    (intern (strcat prefix sym-name))))

(defun rv-setter-sym (symbol)
  (rv-symbol-cat "SET-" symbol))

(defun rv-checker-sym (symbol)
  (rv-symbol-cat "CHECK-" symbol))

(defun rv-asserter-sym (symbol)
  (rv-symbol-cat "ASSERT-" symbol))

(defmacro define-restricted-var (name valid-values-list
                                 &optional
                                 (docstring nil docstring-suppliedp)
                                 (test 'eq))
  ;; [2007/03/14:JCC] Check types of args.  Macro would otherwise be malformed.
  (assert (symbolp name))
  (assert (listp valid-values-list))
  (assert (symbolp test))
  (when docstring-suppliedp
    (assert (stringp docstring)))

  (let* ((asserter-sym (rv-asserter-sym name))
         (checker-sym (rv-checker-sym name))
         (setter-sym (rv-setter-sym name))
         (docstring (if docstring-suppliedp
                        docstring
                      (format nil "~S" valid-values-list))))
    `(progn
       (defvar ,name ,(car valid-values-list)
         ,docstring
         )

       (defun ,asserter-sym (value)
         (assert (member value ',valid-values-list))
         value)

       (defun ,checker-sym (value)
         (member value ',valid-values-list))

       (defun ,setter-sym (&optional (value nil value-suppliedp))
         (cond
          ((eq value :help)
            (format t "Please choose a value for ~A from the following list:~%~A~%" (symbol-name ',name) ,docstring))
          ((not value-suppliedp)
           (loop
               with input-value
               do (format t "Please choose a value for ~A from the following list:~%~A~%Or choose :quit to exit unchanged.~%" (symbol-name ',name) ,docstring)
                  (setf input-value (read))
               if (,test input-value :quit)
               do (format t "~A is still ~S.  Leaving unchanged.~%" (symbol-name ',name) ,name)
                  (return-from ,setter-sym ,name)
               else if (,checker-sym input-value)
               do (setf ,name input-value)
                  (return-from ,setter-sym ,name)
               else do (format t "~S is not a valid value for ~A.  Please reenter.~%" input-value (symbol-name ',name))))
          ((,checker-sym value)
           (setf ,name value))
          (t
           (format t "~S is not a valid value for ~A.  Leaving unchanged as ~S.~%" value (symbol-name ',name) ,name))))
       )))



;;;---------------------------------------------------------------------------

(defun tstamp (&optional (stream t))
  "prints a timestamp string to the stream"

  (multiple-value-bind (second minute hour date month year) (get-decoded-time)
    (format stream "~D/~D/~D ~D:~2,'0D:~2,'0D" month date year hour minute second)))

;;;---------------------------------------------------------------------------
;;; stupid allegro sys:command-line-argument barfs if there is no arg in a
;;; posn, so this one just returns nil if not.
;;; - note the count is from 1, but the posn is zero indexed.  hence >

(defun get-command-line-argument (posn)
  #+allegro
  (if (> (sys:command-line-argument-count) posn)
      (sys:command-line-argument posn))
  #+cmu
  (if (> (length extensions:*command-line-words*) posn)
      (nth posn extensions:*command-line-words*)
      nil)
  #-(or allegro cmu)
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (error "Don't know how to read command-line arguments for this Lisp dialect, ignoring ~S." posn))
  )

;;;---------------------------------------------------------------------------
(defun singleton-p (lst)
  (and (consp lst)
       (not (cdr lst))))

;;;---------------------------------------------------------------------------
;;; takes in a low or upcase string with or without the beginning colon
;;; and returns keyword if it exists, otherwise returns nil.
;;; - also works on symbols, with or without :.

(defmethod find-keyword ((str string))
  (find-symbol
        (string-upcase
                (if (equal (char str 0) #\:)
                    (string-left-trim ":" str)
                    str))
        (find-package 'keyword)))

(defmethod find-keyword ((key symbol))
  (if (keywordp key)
        key
        (find-keyword (symbol-name key))))
;;;---------------------------------------------------------------------------

#+allegro
(defmacro profile (&rest all)
  ;; [2007/03/14:JCC] Create uninterned symbol for binding to make macro safe.
  (let ((result-var (gensym)))
    `(let (,result-var)
       (prof:with-profiling (:type :time) (setf ,result-var (progn ,@all)))
       (prof:show-call-graph)
       ,result-var)))

;;;---------------------------------------------------------------------------

;(defmacro abort-on-error (&body forms)
;   `(handler-bind ((error #'abort))
;      ,@forms))

;; This version can take a bunch of forms and will just return nil if any of them pukes...it wont keep going after the puking expr on to the next one
;; Same as ignore-errors but not silent about it!
;(defmacro dont-error (&body forms)
;   `(catch 'trap-errors (handler-bind ((serious-condition #'(lambda (c) (format t "~&ERROR: Encountered serious-condition: ~A~%" c) (format t "~&Ignoring...carry on...~%") (throw 'trap-errors nil)))) ,@forms)))

;;; FIXME: could replace with IGNORE-ERRORS, which might let us handle the backtracing
;;; better.  
(defmacro dont-error (&body forms)
  `(catch 'trap-errors
     (handler-bind ((serious-condition #'(lambda (c) (format t "~&ERROR: Encountered serious-condition: ~A~%" c)
                                           #+allegro (top-level.debug::zoom *standard-output*)
                                           ;; REMOVING backtrace for LAX, trying to avoid ccl socket error bug  
                                           #-allegro (uiop:print-backtrace)
                                           (format t "~&Ignoring...carry on...~%")
                                           (throw 'trap-errors nil))))
       ,@forms)))

;;; YES ERROR! (uncomment this to override above defn.)
;;;(defmacro dont-error (&body forms)
;;;  `(progn ,@forms))

;(dont-error (error "foo") (format t "hi"))
;
;(defun a ()
;  (dont-error (error "foo") (format t "1"))
;  (format t "2")
;  (dont-error (format t "3") (error "boo"))
;  (format t "2")
;)

;(defmacro dont-error (&body forms)
;   `(handler-bind ((condition #'(lambda (c) (format t "~&Encountered condition: ~A~%Ignoring...carry on...~%" c))))
;      ,@forms))
;
;(dont-error (error "foo") (format t "hi"))

;;;---------------------------------------------------------------------------
(provide 'tools)
