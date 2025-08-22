;;; -------------------------------------------------------------------------
;;; COPYRIGHT START
;;; COPYRIGHT END
;;; -------------------------------------------------------------------------

(in-package :fuzzbomb)
(eval-when (:compile-toplevel) (optimization-boilerplate))

;;; -------------------------------------------------------------------------
(debug-list-value :bf
  "The main debugging keyword for NIST Bug Framework code.")
(debug-list-value :bf-deep
  "Shows details of the inner thoughts on BF.")

;;; -------------------------------------------------------------------------
#|
;;; Excerpts from "The Bugs Framework (BF)..." paper, 2016 IEEE Conf Software Quality, Reliability and Security
;;; by Bojanova, Black, Yesha, and Wu

The BF comprises four main areas: causes, attributes, consequences and sites of
bugs The causes and consequences are well represented with a directed graph.
Causes include implementation mistakes, conditions, preceding weaknesses and
circumstances that bring about the fault. Some of the causes are nested
hierarchically.

;;; Above suggests using classes for above; can get hierarchy and auto-draw graph if want.

The identifying or distinguishing attributes are the next general area.
Each attribute is an enumeration of possible values.

;;; Above suggests using either just types or defenum

Lists of attributes also open
the opportunity to more formally define and reason about them.
Note that the attributes describe an event, not the site in code
that gives rise to the event

A site is a location in code where a weakness might be For
instance, every buffer access in a C program is a site where
buffer overflow might occur if the code is buggy. In other words,
sites for a weakness are places that must be checked for that
weakness.
|#

;; Top-level bug class, from which more-specific subclasses of bug will inherit various slots.
;; named-object makes each of these have a unique integer identifier 'name'.
;; :metaclass instance-collector makes it collect a list of member instances, including members of subclasses.
;;	- but note subclasses *must also* specify the :metaclass instance-collector since the metaclass compatibility stuff
;;	either isnt working or I don't understand enough (or both)
(defclass bug (named-object)
  (
   (stringname :initform "" :initarg :stringname :accessor stringname)
   (causes :initform nil :initarg :causes :accessor causes) ;;:type (list bug-cause)
;; we initially had just a list of attributes, but then cannot say which is which...so now there are more-specific slots, below
;;   (attributes :initform nil :initarg :attributes :accessor attributes :type (list bug-attribute))
   (consequences :initform nil :initarg :consequences :accessor consequences ) ;;:type (list bug-consequence))
   (site :initform nil :initarg :site :accessor site)  ;; :type bug-site)
   (result :initform nil :initarg :result :accessor result) ;; :type exploitable-result
   )
  (:metaclass instance-collector)
)

;;;-------------------------------------------------------------------------
;;; Cause instances can be linked together to form a directed graph.  They can also be hierarchical.
;;; So we're using classes for the hierarchy and leads-to links for graph structure.
;;;-------------------------------------------------------------------------

(defclass bug-cause ()
  (
   (leads-to :initform nil :initarg :leads-to :accessor leads-to ;;:type (or (list 'bug-cause) nil)
		)
   )
)

;; So far, it looks like these could just be deftypes instead of full classes... tbd

(defclass data-exceeds-array (bug-cause) ())
(defclass array-too-small (data-exceeds-array) ())
(defclass too-much-data (data-exceeds-array) ())

(defclass programmer-error (bug-cause) ())
(defclass input-not-checked-properly (programmer-error) ())
(defclass input-not-checked-at-all (programmer-error) ())

(defclass no-null-termination (bug-cause) ())

(defclass wrong-index (bug-cause) ())	;; aka pointer-out-of-range

(defclass incorrect-conversion (bug-cause) ())

(defclass incorrect-calculation (bug-cause) ())
(defclass missing-factor (incorrect-calculation) ())
(defclass off-by-one (incorrect-calculation) ())
(defclass incorrect-argument (incorrect-calculation) ())

;;;-------------------------------------------------------------------------
;; While neither SIFT's initial encoding of CVE-2015-7848 nor Paul's edited version listed the consequence as bof-then-DOS, I think that's actually
;; right for this example:
;; the overflow leads to a memcpy trying to read unallocated memory.
;; This also echoes Paul's emailed comments about chains of bugs/flaws.
;; Anyways, the point is, this suggests that at least some categories of bugs are themselves also of class bug-consequence.  So yay for multiple inheritance.
;;;-------------------------------------------------------------------------
;;; FIXME havent fleshed out consequences at all yet.

(defclass bug-consequence ()
  (
   (leads-to :initform nil :initarg :leads-to :accessor leads-to ;;:type (or (list 'bug-consequence) nil)
	)
   )
)

;;;-------------------------------------------------------------------------
;;; This has some first thoughts about how a site might be more-precisely defined.
;;;-------------------------------------------------------------------------
(defclass bug-site ()
  ((pc :initform nil :initarg :pc :accessor pc ;;:type '(or int nil)
	:documentation "the program counter, from gdb on crash")
   (source-file :initform nil :initarg :source-file :accessor source-file) ;; :type source-file)
   (source-line :initform nil :initarg :source-line :accessor source-line) ;; :type source-line)
   (source-char :initform nil :initarg :source-char :accessor source-char) ;; :type source-char :documentation "the starting char of the actual bug site") ;; this could be hard to figure out
   (backtrace :initform nil :initarg :backtrace :accessor backtrace ;;:type '(or string nil)
              :documentation "from gdb, if a crash occurs")
   (fn-name :initform nil :initarg :fn-name :accessor fn-name ;;:type '(or string nil)
            :documentation "Name of the function where crash occurred.")
   )
)

;;;-------------------------------------------------------------------------
;;;-------------------------------------------------------------------------
;;; NOTE still considering whether these should be deftype/defenum so can just have them as a value,
;;; or whether they need to be classes so that we can store more info with the instances of them.
;;; To the degree that categorization power is the point of the BF, they should just be types/symbols.  But
;;; the more we need to have separated-out data/substantiation behind the categories, the more we need classes here.
;;; Also, as we started to make instances, we found that if the attribute types are not full classes, we cannot easily separate out
;;; which, eg, is the bug-language vs. the bug-entry-point, so now those are slots of bug classes.
;;; Finally, note that Allegro CL seems to mostly ignore slot type specifications, at least during setfing, so these aren't
;;; enforced (yet).  We have not investigated enforcement, yet.  SBCL is typically more stringent with type specs.
;;;-------------------------------------------------------------------------
(deftype bug-attribute () '(or bof-attribute inj-attribute cif-attribute fop-attribute))

(deftype unknown-attribute () '(or any dont-care unknown))

;;;-------------------------------------------------------------------------
;;; BOF mostly fleshed out
;;;-------------------------------------------------------------------------
(deftype bof-attribute () '(or bug-access bug-boundary bug-location bug-magnitude bug-data-size bug-reach))
(deftype bug-access () '(member read write))
(deftype bug-boundary () '(member above below))
(deftype bug-location () '(member heap stack bss data code))
(deftype bug-magnitude () '(member small moderate large))	;; NOTE inconsistency b/w BOF in original paper ("far") and FOP ("large") in google doc
(deftype bug-data-size () '(member little some huge))
(deftype bug-reach () '(member continuous discrete))

(defclass bof (bug bug-consequence)
 ((class-stringname :initform "Buffer Overflow" :reader class-stringname :allocation :class)
  (class-shortname :initform "BOF" :reader class-shortname :allocation :class)
  (class-level :initform "low" :reader class-level :allocation :class)
  (access :initarg :access :accessor access :type (or 'bug-access 'unknown-attribute))
  (boundary :initarg :boundary :accessor boundary :type (or 'bug-boundary 'unknown-attribute))
  (location :initarg :location :accessor location :type (or 'bug-location 'unknown-attribute))
  (magnitude :initarg :magnitude :accessor magnitude :type (or 'bug-magnitude 'unknown-attribute))
  (data-size :initarg :data-size :accessor data-size :type (or 'bug-data-size 'unknown-attribute))
  (reach :initarg :reach :accessor reach :type 'bug-reach)
 )
  (:metaclass instance-collector))

;;;-------------------------------------------------------------------------
;;; ARC not fleshed out
;;;-------------------------------------------------------------------------


;;;-------------------------------------------------------------------------
;;; INJ not fleshed out
;;;-------------------------------------------------------------------------

(deftype inj-attribute () '(or bug-language bug-special-element bug-entry-point))
(deftype bug-language () '(member database-query regular-expression command markup script))
(deftype bug-special-element () '(member query-elements header-separators scripting-elements format-parameters path-traversals wildcards metacharacters))
(deftype bug-entry-point () '(member data-entry-field scripting-tag markup-tag function-call-parameter procedure-call-argument))

;;;-------------------------------------------------------------------------
;;; CIF not fleshed out
;;;-------------------------------------------------------------------------

(deftype cif-attribute () '(or bug-interaction bug-number bug-unit bug-actor))
(deftype bug-interaction () '(member authentication-attempt book checkout register initiate))
(deftype bug-number () '(member single unique specified-number))	;; Note the last could really be an integer >1 spec
(deftype bug-unit () '(member time-interval event user))
(deftype bug-actor () '(member user part-of-program-logic automated-process))

;;;-------------------------------------------------------------------------
;;; TODO: FOP seems to have vanished from the BF website.
;;; FOP, more complete
;;;-------------------------------------------------------------------------
(deftype fop-attribute () '(or bug-reset-fault bug-operand-error bug-magnitude bug-excursion))	;; FIXME add commented-out ones below once they are fixed
(deftype bug-result-fault () '(member overflow underflow undefined loss-of-precision truncation distortion))
;; NOTE: These operators are in terms of C operators (technically, it is language specific, but we'll assume C and assembly)
(deftype bug-operation () '(member + - * / < > ^ & |\|| <= >= == << >> += -= *= /= &= |\|=| ^= = <<= >>= explicit-conversion argument-passing))
(deftype bug-operand-error () '(member value-exceeds-type inadequate-type domain-error range-error)) ;; FIXME NOTE range-error not defined in FOP google doc
;;(deftype bug-type () '(member int double long int...			;; FIXME skipping for now long encoding of the casting-pairs
(deftype bug-excursion () '(member continuous discrete))



;;;-------------------------------------------------------------------------

(defclass arc (bug bug-consequence)
  ((class-stringname :initform "Arithmetic or Conversion Fault" :reader class-stringname :allocation :class)
   (class-shortname :initform "ARC" :reader class-shortname :allocation :class)
   (class-level :initform "low" :reader class-level :allocation :class)

   (result-fault :initarg :result-fault :accessor result-fault :type (or 'bug-result-fault 'unknown-attribute))
   (operation :initarg :operation :accessor operation :type (or 'bug-operation 'unknown-attribute))
   (operand-error :initarg :operand-error :accessor operand-error :type (or 'bug-operand-error 'unknown-attribute))
   ;; The types field doesn't yet have a good type for it.
   (types :initarg :types :accessor types) ;; FIXME: :type (or 'bug-types 'unknown-attribute))
   (data-size :initarg :data-size :accessor data-size :type (or 'bug-data-size 'unknown-attribute))
   (excursion :initarg :excursion :accessor excursion :type (or 'bug-reach 'unknown-attribute))
   )
  (:metaclass instance-collector))


;;;-------------------------------------------------------------------------

;; TODO: FOP seems to have vanished form the BF website..... I don't think
;; this is valid anymore. -pkeller 11-26-2019
(defclass fop (bug)
 ((class-stringname :initform "Faulty Operation" :reader class-stringname :allocation :class)
  (class-shortname :initform "FOP" :reader class-shortname :allocation :class)
  (class-level :initform "low" :reader class-level :allocation :class)
  (result-fault :initarg :result-fault :accessor result-fault :type (or 'bug-result-fault 'unknown-attribute))
  (operation :initarg :operation :accessor operation)  ;; :type (or 'bug-operation 'unknown-attribute))
  (operand-error :initarg :operand-error :accessor operand-error :type (or 'bug-operand-error 'unknown-attribute))
  (types :initarg :types :accessor types) ;; :type (or (list 'bug-type) 'unknown-attribute)
  (excursion :initarg :excursion :accessor excursion :type (or 'bug-excursion 'unknown-attribute))
  (magnitude :initarg :magnitude :accessor magnitude :type (or 'bug-magnitude 'unknown-attribute))	;; copied from BOF, see doc
 )
  (:metaclass instance-collector))

;;;-------------------------------------------------------------------------
;;; A few disconnected-from-source instances, to flex the framework and see how it fits;
;;; a real bug found by ISABEL will be more specific than this, we expect.

#|
(in-package :fb)

(defvar CVE-2016-4291 (make-instance 'fop
   :stringname "CVE-2016-4291"
   :result-fault 'overflow
   :operation "*"
   :operand-error 'range-error
   ;;:types (list 'int 'int)
   :causes (list (make-instance 'input-not-checked-at-all))
   ;;; FIXME :consequence
))


(defvar CVE-2015-7848 (make-instance 'fop
   :stringname "CVE-2015-7848"
   :result-fault 'overflow
   :operation "--"
   :operand-error 'domain-error
   ;;:types (list 'u_short)
   :causes (list (make-instance 'programmer-error))
   ;;; FIXME :consequence  (make-instance 'bof :... :leads-to (make-instance 'dos...)
))

;; And a bit of code to test instance-collector functionality

(format t "list of all bugs: ~A~%" (find-all-instances 'bug))
(format t "list of fops: ~A~%" (find-all-instances 'fop))
(format t "list of bofs: ~A~%" (find-all-instances 'bof))


|#

;;;-------------------------------------------------------------------------
;;; As we think about each of the attributes more, it might be appropriate to keep provenance for each one, noting
;;; what analysis tool/result supports the value of the attribute.
;;;-------------------------------------------------------------------------
(defclass analysis-result (named-object)
 ((timestamp :initform nil :initarg :timestamp :accessor timestamp :documentation "when the analysis was run")
  (returnval :initarg :returnval :accessor returnval :documentation "analysis tool return val")
 ))

;;;-------------------------------------------------------------------------
(defclass exploitable-result (analysis-result)
  (;; These slots are for the 'exploitable' command
   (classification :initform nil :initarg :classification :accessor classification)
   (hash :initform nil :initarg :hash :accessor hash)
   (short-description :initform nil :initarg :short-description :accessor short-description)
   (description :initform nil :initarg :description :accessor description)
   (explanation :initform nil :initarg :explanation :accessor explanation)
   (other-tags :initform nil :initarg :other-tags :accessor other-tags)

   ;; These slots are for the 'fuzzy_pov_stack_search' command.
   ;; Values can be (in order from least likely to most):
   ;; :normal-exit, :undetectable, :coincidence, :very-unlikely, :unlikely,
   ;; :indeterminate, :suspicious, :likely, :very-likely
   (fpss-classification :initform nil :initarg :fpss-classification :accessor fpss-classification)
 ))

(defparameter *sample-exploitable-result*
  (make-instance 'exploitable-result
                 :TIMESTAMP "3/27/2024 10:11:57"
                 ;; :RETURNVAL: #<Unbound>
                 :CLASSIFICATION "PROBABLY_NOT_EXPLOITABLE"
                 :HASH "82b89b80ad2de93a031cae4602c73096.82b89b80ad2de93a031cae4602c73096"
                 :SHORT-DESCRIPTION "DivideByZero"
                 :DESCRIPTION "Divide by Zero"
                 :EXPLANATION "The target crashed while performing a divide by zero. It is generally difficult to leverage these types of errors to gain control of the processor."
                 :OTHER-TAGS "FloatingPointException (18/23)"
                 :FPSS-CLASSIFICATION NIL))

(defparameter *sample-bug-site*
  (let ((site (make-instance 'bug-site)))
    (setf (pc site) "0x000055555555491d")
    (setf (source-file site) "heap_overflow_01_stdin.c")
    (setf (source-line site) "30")
    (setf (fn-name site) "main")
    site))

(defparameter *sample-bug*
  (make-instance 'bug
                 :site *sample-bug-site*
                 :result *sample-exploitable-result*))
