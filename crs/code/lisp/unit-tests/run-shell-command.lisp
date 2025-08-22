(defpackage #:run-shell-command-tests
  (:nicknames #:rsc-tests)
  (:import-from #:excl #:run-shell-command)
  (:use #:common-lisp #:iterate #:fiveam)
  )

(in-package #:rsc-tests)

(defparameter +trim-me+
  (list #\newline #\return #\space))

(def-suite* test-run-shell-command)

;;; Synchronous
;;; command:
;;; - string

(test string-arg
      (uiop:with-temporary-file (:stream s :pathname path)
        ;; somewhat unfortunately, I have to test with :supersede here because WITH-TEMPORARY-FILE creates the
        ;; temporary file. Not sure that :if-output-exists and :if-error-output-exists should default to :ERROR
        ;; if the argument is a stream. Should check against ACL.  
        (let ((res (multiple-value-list (run-shell-command "echo foo" :output s :if-output-exists :supersede))))
          (is (= (length res) 1))
          (is (= (first res) 0)))
        :close-stream
        (uiop:with-input-file (s path)
          (is (string-equal "foo" (string-trim +trim-me+ (read-line s)))))))

;;; - vector

(test vector-arg
      (uiop:with-temporary-file (:stream s :pathname path)
        ;; somewhat unfortunately, I have to test with :supersede here because WITH-TEMPORARY-FILE creates the
        ;; temporary file. Not sure that :if-output-exists and :if-error-output-exists should default to :ERROR
        ;; if the argument is a stream. Should check against ACL.  
        (let ((res (multiple-value-list (run-shell-command (vector "echo" "foo") :output s :if-output-exists :supersede
                                                           :wait t))))
          (is (= (length res) 1))
          (is (= (first res) 0)))
        :close-stream
        (uiop:with-input-file (s path)
          (is (string-equal "foo" (string-trim +trim-me+ (read-line s)))))))

(test vector-arg-coerce
      (uiop:with-temporary-file (:stream s :pathname path)
        ;; somewhat unfortunately, I have to test with :supersede here because WITH-TEMPORARY-FILE creates the
        ;; temporary file. Not sure that :if-output-exists and :if-error-output-exists should default to :ERROR
        ;; if the argument is a stream. Should check against ACL.  
        (let ((res (multiple-value-list (run-shell-command (vector "/bin/echo" '#:foo "bar") :output s :if-output-exists :supersede
                                                           :wait t))))
          (is (= (length res) 1))
          (is (= (first res) 0)))
        :close-stream
        (uiop:with-input-file (s path)
          (is (string-equal "foo bar" (string-trim +trim-me+ (read-line s)))))))

;;;   - test coercion to string from symbol.  Numbers will not work. Test pathname?
;;; input arg
;;; output arg
;;; error output arg
;;; job success
;;; job failure
(test string-arg-fail
      (let ((res (multiple-value-list (run-shell-command "false" :output nil))))
        (is (= (length res) 1))
        (is (= (first res) 1))))

(test vector-arg-fail
      (let ((res (multiple-value-list (run-shell-command (vector "/usr/bin/false")))))
        (is (= (length res) 1))
        (is (= (first res) 1))))

(test vector-arg-fail
      (let ((res (multiple-value-list (run-shell-command (vector "/usr/bin/true")))))
        (is (= (length res) 1))
        (is (= (first res) 0))))


;;; with and without environment
(test shell-env
      (let* ((envar-sym (gensym))
             (envar-string (symbol-name envar-sym)))
        (setf (uiop:getenv envar-string)
              "yes")
        (uiop:with-temporary-file (:stream s :pathname path)
          (let ((exit-code
                  (excl:run-shell-command
                   (format nil "if [ \"${~a}\" = \"yes\" ] ; then echo 'yes'; else echo 'no'; fi" envar-string)
                   :output s :if-output-exists :supersede)))
            (is (= exit-code 0)))
          :close-stream
          (uiop:with-input-file (s path)
            (is (string-equal "yes" (string-trim +trim-me+ (read-line s))))))
        (uiop:with-temporary-file (:stream s :pathname path)
          (let ((exit-code
                  (excl:run-shell-command
                   (format nil "if [ \"${~a}\" = \"yes\" ] ; then echo 'yes'; else echo 'no'; fi" envar-string)
                   :environment `((,envar-string "no"))
                   :output s :if-output-exists :supersede)))
            (is (= exit-code 0)))
          :close-stream
          (uiop:with-input-file (s path)
            (is (string-equal "no" (string-trim +trim-me+ (read-line s))))))
        ;; check exit code
        (let ((exit-code
                (excl:run-shell-command
                 (format nil "if [ \"${~a}\" = \"yes\" ] ; then exit 0; else exit 1; fi" envar-string))))
          (is (= exit-code 0)))
        (let ((exit-code
                (excl:run-shell-command
                 (format nil "if [ \"${~a}\" = \"yes\" ] ; then exit 0; else exit 1; fi" envar-string)
                 :environment `((,envar-string "no")))))
          (is (= exit-code 1)))))



;;; Asynchronous
