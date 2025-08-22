;;; Creates CLOS classes and deserializes JSON into them, based on a a JSON schema.
;;;
;;; Everything here works by crawling through the schema as a graph. When creating classes with
;;; DEFINE-CLASSES-FOR-JSON-SCHEMA, WALK-SCHEMA is used to find all the internal subschemas (that
;;; we care about), and create classes for those that are objects. DESER then assumes these classes
;;; exist when walking through the schema to convert the value into CLOS objects. Use JSON-FROM-PATH
;;; to laod the JSON files, or otherwise ensure that arrays are deserialized to vectors.
(cl:in-package :fuzzbomb)

(defun assoc-value (key alist &optional default)
  (let ((val (assoc key alist)))
    (if val (cdr val) default)))

(defun assoc-value-permissive (key alist &optional default)
  "assoc-value, but returns nil if alist is not a list"
  (and (listp alist) (assoc-value key alist default)))



(defun char-kebabcase (ch)
  (cond
    ((find ch " /")    "-")
    ((find ch ".-")    (coerce (list ch) 'string))
    ((lower-case-p ch) (string-upcase (coerce (list ch) 'string)))
    ((upper-case-p ch) (format nil "-~a" ch))
    (t                 (format nil "-~a" (char-code ch)))))

(defun string-kebabcase (str)
  "Turns a string (possibly a JSON pointer) to a somewhat reasonable Lisp name as a symbol."
  (cond
    (str (apply #'concatenate 'string (mapcar #'char-kebabcase (coerce str 'list))))
    (t   nil)))



(defun walk-schema (cb schema &optional (reverse-pointer-chunks nil))
  "Calls the callback function for every nested subschema, including the schema itself."

  ; Properties whose values are schemas.
  (loop
    for property in '(:additional-items :additional-properties :contains :then :else)
    if (and (listp schema) (assoc property schema))
    do (walk-schema cb (assoc-value property schema) (cons property reverse-pointer-chunks)))

  ; items can either be a schema or an array of schemas.
  (alexandria:if-let (items (assoc-value-permissive :items schema))
    (cond
      ((vectorp items)
       (loop
         for item across items
         for i from 0
         do (walk-schema cb item (cons i (cons :items reverse-pointer-chunks)))))
      (t
       (walk-schema cb items (cons :items reverse-pointer-chunks)))))

  ; Properties whose values are arrays of schemas.
  (loop
    for property in '(:any-of :one-of)
    for subschema in (assoc-value-permissive property schema)
    for i from 0
    do (walk-schema cb subschema (cons i (cons property reverse-pointer-chunks))))

  ; Properties whose values are objects whose values are schemas.
  (loop
    for property in '(:definitions :properties :pattern-properties)
    do (loop
         for pair in (assoc-value-permissive property schema)
         do (walk-schema cb (cdr pair) (cons (car pair) (cons property reverse-pointer-chunks)))))

  ; Actually run the callback.
  (funcall cb schema reverse-pointer-chunks))

(defun list-subschemas (schema)
  "Returns an alist whose keys are chunked JSON pointers (see json-pointer-to-chunks) and whose
   values are subschemas of the given JSON schema (including the schema itself)."
  (let ((lst nil))
    (walk-schema #'(lambda (s p) (push (cons (reverse p) s) lst)) schema)
    lst))

(defun json-schema-infer-type (schema root-schema)
  "Returns the type expected for a schema, e.g. :STRING or :INTEGER."
  (let ((given-type (assoc-value :type schema))
        (ref-pointer (assoc-value :$ref schema)))
    (cond
      (ref-pointer
       (json-schema-infer-type (follow-json-pointer root-schema ref-pointer) root-schema))
      ((and (vectorp given-type) (not (stringp given-type)) (eql (length given-type) 0))
       nil)
      ((and (vectorp given-type) (not (stringp given-type)))
       (intern (string-upcase (elt given-type 0)) :keyword))
      (given-type
       (intern (string-upcase given-type) :keyword))
      ((eq nil schema)
       nil)
      (t
       (warn "cannot json-schema-infer-type of ~a~%" schema)))))

(defun json-schema-type-name (schema pointer &key (prefix ""))
  (declare (ignore schema))	;;; FIXME is this really OK to ignore?
  "Creates a reasonable class name for the type created from a JSON schema."
  (intern
    (string-upcase
      (uiop:strcat prefix
        ;;(let ((chunks (json-pointer-to-chunks pointer)))
          (cond
            ; ((and (eq (length chunks) 2) (eq (car chunks) :definitions)) (string (cadr chunks)))
            ((and (string-equal pointer "") (string-equal prefix ""))
             "root")
            ((and (> (length pointer) 0) (eq (elt pointer 0) #\#))
             (string-kebabcase (subseq pointer 1)))
            (t
             (string-kebabcase pointer))))))) ;;)

(defun json-pointer-from-chunks (chunks)
  "Joins chunks into a JSON pointer."
  (let ((pointer ""))
    (loop
      for chunk in chunks
      do (setf pointer (uiop:strcat pointer "/" (string-downcase (string chunk)))))
    pointer))

(defun json-pointer-to-chunks (pointer)
  "Breaks a JSON pointer into a list of chunks. Each chunk is either a keyword symbol or a number.
   This allows using the chunks as indices of a vector or keys of an alist."
  (loop
    for chunk in (uiop:split-string pointer :separator "/")
    if (not (or (string-equal chunk "") (string-equal chunk "#")))
    collect (or (parse-integer chunk :junk-allowed t)
                (intern (string-kebabcase chunk) :keyword))))

(defun booleanp (x)
  "Returns whether the given value is a well-formed boolean, i.e. t or nil."
  (or (eq x nil) (eq x t)))

(defun trivial-schema (schema root-schema)
  "Returns whether the schema is \"trivial\", i.e. would be represented by plain values rather than
   CLOS objects."
  (or
    (booleanp schema)
    (assoc :$ref schema)
    (assoc :any-of schema)
    (assoc :enum schema)
    (member (json-schema-infer-type schema root-schema) '(:array :boolean :integer :string))))

(defun make-slots-for (schema &key (prefix ""))
  "Creates the slots for the JSON schema classes."
  (alexandria:if-let (properties (assoc-value :properties schema))
    (loop
      for pair in properties
      for name = (car pair)
      for name-symbol = (intern (string-upcase (uiop:strcat prefix (string name))))
      collect `(,name-symbol :initarg ,name :accessor ,name-symbol))))

(defun make-single-schema-defclass (pointer schema root-schema &key (prefix ""))
  "Makes a DEFCLASS form for a JSON schema."
  (when (not (trivial-schema schema root-schema))
    (let ((name (json-schema-type-name schema (json-pointer-from-chunks pointer) :prefix prefix)))
      `(defclass ,name ()
         ,(make-slots-for schema :prefix prefix)))))

(defun make-schema-defclasses (schema &key (prefix ""))
  "Makes DEFCLASS forms for all nested JSON schemas."
  (loop
    for (pointer . subschema) in (list-subschemas schema)
    for form = (make-single-schema-defclass pointer subschema schema :prefix prefix)
    when form
    collect form))

(defun define-classes-for-json-schema (schema &key (prefix ""))
  "Defines classes for all nested JSON schemas."
  (loop
    for form in (make-schema-defclasses schema :prefix prefix)
    do (eval form)))


;;; "Runtime" functions
(define-condition deser-failure () ((expected :initarg :expected) (json :initarg :json)))
(defmethod print-object ((err deser-failure) out)
  (with-slots (expected json) err
    (format out "#<DESER-FAILURE ~s ~s>" expected json)))
(defun error-deser (expected json)
  "Signals an error in deserialization."
  (error 'deser-failure :expected expected :json json))

(defun follow-json-pointer-chunk (json chunk)
  (cond
    ((numberp chunk) (elt json chunk))
    (t               (assoc-value chunk json))))
(defun follow-json-pointer (json pointer)
  "Accesses an inner value with a JSON pointer."
  (loop
    for chunk in (json-pointer-to-chunks pointer)
    do (setf json (follow-json-pointer-chunk json chunk)))
  json)

(defun json-from-path (path)
  "Loads a JSON file from the given path, with the settings preferred by json-schema-to-clos."
  (json:bind-custom-vars (:array-type 'vector)
    (with-open-file (s path)
      (cl-json:decode-json s))))

(defun deser-properties (json root-schema properties required pointer &key (prefix ""))
  "The main workhorse of object deserialization."
  (loop
    for (name . subschema) in properties
    if (or (find name required) (assoc name json))
    collect (let ((value (deser-value (assoc-value name json) root-schema subschema
                                      (uiop:strcat pointer "/" (string-downcase (string name)))
                                      :prefix prefix)))
              (list name value))))

(defun deser-array (json root-schema schema pointer &key (prefix ""))
  "Deserializes an array."
  (when (not (vectorp json))
    (error-deser schema json))
  (let ((items (assoc-value :items schema))
        (ptr (format nil "~a/items" pointer)))
    (loop
      for value across json
      for i from 0
      collect (deser-value value root-schema items ptr :prefix prefix))))

(defun deser-from-pred (pred ty json)
  "Deserializes to the current value if it passes a predicate; otherwise fails."
  (cond 
    ((funcall pred json) json)
    (t (error-deser ty json))))

(defun deser-object (json root-schema schema pointer &key (prefix ""))
  "Deserializes an object."
  (let ((properties            (assoc-value :properties schema))
        (pattern-properties    (assoc-value :pattern-properties schema))
        (additional-properties (assoc-value :additional-properties schema))
        (required              (assoc-value :required schema))
        (type-name             (json-schema-type-name schema pointer :prefix prefix)))
    (cond
      ((and (member pattern-properties (list nil :false)) (member additional-properties (list nil :false)))
       (apply #'make-instance type-name
         (apply #'nconc
           (deser-properties json root-schema properties required (uiop:strcat pointer "/properties")
             :prefix prefix))))
      (t
        (warn "TODO: deser-object with pattern-properties/additional-properties")
        ; These are probably pretty good indicators that the value is a map, so fall back to
        ; returning the alist?
        json))))

(defun deser-value (json root-schema schema pointer &key (prefix ""))
  "Deserializes a value from JSON corresponding to the given location in the JSON schema."   
  (cond
    ((assoc-value-permissive :$ref schema)
     (let ((ptr (assoc-value :$ref schema)))
       (deser-value json root-schema (follow-json-pointer root-schema ptr) ptr :prefix prefix)))
    ((assoc-value-permissive :any-of schema)
     ; Try each of the subschemas, erroring out if we get to the end without any of them matching.
     (block any-of-body
       (let* ((subschemas (assoc-value :any-of schema))
              (len (length subschemas)))
         (loop
           for i from 0 to (1- len)
           do (block any-of-loop
             (let ((subschema (elt subschemas i))
                   (ptr (format nil "~a/~a" pointer i)))
               (return-from any-of-body
                 (handler-case (deser-value json root-schema subschema ptr :prefix prefix)
                   (deser-failure (c)
                     (declare (ignore c))
                     (return-from any-of-loop)))))))
         (error-deser schema json))))
    ((assoc-value-permissive :enum schema) json)
    (t
     (let* ((ty (json-schema-infer-type schema root-schema)))
       (case ty
         ((nil)     json)
         (:array    (deser-array json root-schema schema pointer :prefix prefix))
         (:boolean  (deser-from-pred #'booleanp schema json))
         (:integer  (deser-from-pred #'integerp schema json))
         (:object   (deser-object json root-schema schema pointer :prefix prefix))
         (:string   (deser-from-pred #'stringp schema json))
         (otherwise (error "~s is not a valid type (from json-schema-infer-type)" ty)))))))

(defun deser (json root-schema &key (prefix ""))
  "Deserializes a JSON value to objects already defined by DEFINE-CLASSES-FOR-JSON-SCHEMA."
  (deser-value json root-schema root-schema "" :prefix prefix))



(defun test-json-schema-to-clos (schema file &key (prefix ""))
  (let* ((manifest-schema (json-from-path (format nil "json-schema-to-clos/schemas/~a" schema)))
         (manifest-json   (json-from-path (format nil "json-schema-to-clos/examples/~a" file))))
    (loop
      for form in (make-schema-defclasses manifest-schema :prefix prefix)
      do (pprint form)
      do (format t "~%")
      do (finish-output *standard-output*))
    (define-classes-for-json-schema manifest-schema :prefix prefix)
    (deser manifest-json manifest-schema :prefix prefix)))
