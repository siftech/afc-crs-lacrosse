;;; -------------------------------------------------------------------------
;;; utils.lisp
;;; -------------------------------------------------------------------------

(cl:in-package :fuzzbomb)

;;; General utils used w/in the fuzzbomb amp.

(defun eval-all-from-string (string)
  (let ((stream (make-string-input-stream string))
        a)
    (loop while (setf a (read stream nil))
       collecting (eval a))))

;;; ------------------------------------------------------------
;;; Helpers

(defun summarize-string-list (strings &key (length 50))
  "Returns a string summarizing the contents of STRINGS"
  (format nil "~{~a~^, ~}" (mapcar #'(lambda (s) (summarize-string s :length length)) strings)))

(defun summarize-string (str &key length)
  "Returns a summary of STR. The summary includes the length and a quoted version of the string. The quoted version of the string is optionally limited to LENGTH characters."

  (let (summary)
    (when str
      (let ((shortened-str (printable-string str)))
        (when (and length
                   (> length 4)
                   (> (length shortened-str) length))
          (let* ((prefix-length (ceiling (/ (- length 3) 2)))
                 (suffix-length (- length prefix-length 3)))
            (setf shortened-str
              (concatenate 'string
                (subseq shortened-str 0 prefix-length)
                "..."
                (subseq shortened-str (- (length shortened-str) suffix-length))))))
        ;; (string-maxlength shortened-str length)))
        (setf summary (format nil "[~A] \"~A\""
                              (length str)
                              shortened-str
                              ))))
    summary
    ))

(defun pathname-looks-like-c-source-p (pathname)
  (cl-ppcre:scan ".c$|.C$|.cpp|.CPP" (namestring pathname)))

(defun python-boolean-to-lisp (boolean-string)
  "Returns t if string is true, ignoring case."
  (when (string-equal "true" boolean-string)
    t))

(defun powerset (s)
  (if s (mapcan (lambda (x) (list (cons (car s) x) x))
		(powerset (cdr s)))
      '(())))

(defun size-of-powerset (s)
  (expt 2 (list-length s)))

(defun dashes-to-underscores (str)
  "Replace all the dashes in a string with underscores."
  (substitute #\_ #\- str))

(defun maybe-unpack-slot-val (val)
  (cond ((typep val 'standard-object) ; For CLOS instances, try to call spec on them if defined
	 (when (compute-applicable-methods #'spec (list val))
	   (spec val)))
	((proper-list-p val) ; For lists, unpack the members
	 (mapcar #'maybe-unpack-slot-val val))
	(t val))) ; For others, return as-is

(defun proper-list-p (x)
  (loop while (consp x) do (setf x (cdr x))
	finally (return (null x))))

(defun load-json-from-file (path)
  (let* ((contents (uiop:read-file-string path))
	 (decoded (cl-json:decode-json-from-string contents)))
    ;; (dbug :top "decoded: ~a ~s" (type-of decoded) decoded)
    decoded))

(defmethod to-json ((lst list))
  (if (proper-list-p lst)
      (mapcar #'to-json lst)
      lst))

(defmethod to-json ((other t))
  other)

(defun write-to-json-file (obj path)
  (let ((json-obj (to-json obj)))
    ;; (dbug :top "json-obj: ~s" json-obj)
    (ensure-directories-exist path)
    (with-open-file (out path
			 :direction :output
			 :if-exists :supersede
			 :if-does-not-exist :create
			 :external-format :utf-8)
      (cl-json:encode-json json-obj out))))

(defun flip (odds)
  "Return T with probability ODDS (a float between 0 and 1), else NIL."
  (< (random 1.0) odds))

(defun generate-tempfile-name (&key (prefix "tempfile") (suffix ".txt"))
  (let ((timestamp (get-universal-time))
	(rand1 (random 1000000))
	(rand2 (random 1000000)))
    (format nil "~A-~A-~A~A~A" prefix timestamp rand1 rand2 suffix)))

;; to-json above was not sophistcated enough
(defmethod msg-to-json-friendly ((lst list))
  ;; Two cases to worry about here: (1) arrays, and (2) dicts
  ;; dicts will look like dave-style plists
  ;; arrays will be everything else, and can just stay the same
  (cond ((assoc-p lst)
	 (assoc-to-json lst))
	(t
	 (mapcar #'msg-to-json-friendly lst))))

(defmethod msg-to-json-friendly ((other t))
  other)

(defun assoc-to-json (alst)
  (mapcar #'(lambda (pair)
	      (cons (first pair)
		    (msg-to-json-friendly (second pair))))
	  alst))

(defun assoc-p (lst)
  (every #'(lambda (item)
	     (and (listp item)
		  (= (length item) 2)
		  (keywordp (first item))))
	 lst))
