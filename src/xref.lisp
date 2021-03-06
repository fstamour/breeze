
(uiop:define-package #:breeze.xref
  (:documentation "Cross-reference and introspection")
  (:mix #:breeze.definition :cl #:breeze.utils #:alexandria)
  (:import-from #:breeze.test
                #:test-body
                #:*test*)
  (:export
   #:package-test
   #:calls-who
   #:test-calls-who
   #:tested-by
   #:test-case
   #:tests-by-package
   ;; Utilities
   #:find-packages-by-prefix
   #:find-packages-by-regex
   ;; Symbol inspection
   #:generic-method-p
   #:specialp
   #:macrop
   #:simple-function-p
   #:classp))

(in-package #:breeze.xref)

(defun package-test (package)
  "Find all tests defined in a pacakge."
  (let ((package (find-package package)))
    (loop
       :for test-name :being :the :hash-key :of breeze.test:*test*
       :using (hash-value test-definition)
       :for test-package = (second test-definition)
       :when (eq test-package package)
       :collect test-name)))

(defun tests-by-package ()
  "Return a hash-table of tests keyed by package."
  (cl-hash-util:collecting-hash-table (:mode :append)
    (loop
       :for test-name :being :the :hash-key :of breeze.test:*test*
       :using (hash-value test-definition)
       :for package = (second test-definition)
       :do (cl-hash-util:collect package test-definition))))

(defun calls-who (function-name)
  "Take a function name and returns a list of all the functions it calls."
  (uiop:while-collecting (collect)
      (walk-car
       (function-body function-name)
       (lambda (el)
         (when (function-body el)
           (collect el))))))

(defun test-calls-who (test-name)
  "Take a test name and return a list of all the functions it calls."
  (let ((function (make-hash-table)))
    (walk-car
     (test-body test-name)
     #'(lambda (el)
	 (when (and (symbolp el) (fboundp el)) ;;(function-body el)
	   (setf (gethash el function) t))))
    (hash-table-keys function)))

(defun tested-by (function-name)
  "Return a list of all the tests that calls FUNCTION-NAME."
  (loop :for test-name :being :the :hash-key :of *test*
     :when (member function-name (test-calls-who test-name))
     :collect test-name))

(defun test-case (function-name)
  "List all the test-cases"
  (let ((case-set (make-hash-table :test 'equal)))
    (loop :for test-name :being :the :hash-key :of *test*
          :for test-body = (test-body test-name)
          :do (walk-list
               test-body
               #'(lambda (list)
                   (when (eq function-name (car list))
                     (setf (gethash list case-set) t)))
               #'(lambda (node)
                   (if (listp node)
                       (not (eq 'quote (car node)))
                       t))))
    (hash-table-keys case-set)))

(defun function-without-test (package)
  ""
  (let ((function-called (make-hash-table)))
    ;; For each tests
    (loop :for test-name :being :the :hash-key :of *test*
       ;; For each function called by this test.
       :do (loop :for function-name :in (test-calls-who test-name)
	      ;; accumulate
              :do (setf (gethash function-name function-called) t)))
    ;; For each functions
    (loop :for function-name :being :the :hash-key :of *function*
       ;; Is it called in any test?
       :unless (gethash function-name function-called)
       :collect function-name)))

;; TODO
#+todo ;; It's done, but not tested nor used
(defun list-function (&optional (package *package*))
  "List all the functions, optionally filter by package"
  (loop :for function-name :being :the :hash-key :of *function*
        :when (if package
                  (eq package (symbol-package function-name))
                  t)
          :collect function-name))

;; TODO
#+todo ;; Look at parse-smth in cover.lisp
(defun function-without-documentation (&optional (package *package*)))

(defun find-packages-by-prefix (prefix)
  "Find all packages whose name starts with the given prefix (case insensitive by default)."
  (loop
     :with prefix = (string-downcase prefix)
     :for package :in (list-all-packages)
     :when (starts-with-subseq prefix
			       (string-downcase
				(package-name package)))
     :collect package))

(defun find-packages-by-regex (regex &optional (case-insensitive-p t))
  "Find all packages whose name match the regex (case insensitive by default)."
  (loop
     :with scanner = (cl-ppcre:create-scanner regex :case-insensitive-mode
					      case-insensitive-p)
     :for package :in (list-all-packages)
     :when (cl-ppcre:scan scanner
			  (string-downcase
			   (package-name package)))
     :collect package))

(defun generic-method-p (symbol)
  "Returns T if SYMBOL designates a generic method"
  (and (fboundp symbol)
       (subtypep
	(type-of (fdefinition symbol))
	'standard-generic-function)))

(defun specialp (symbol)
  "Return true if SYMBOL is a special variable."
  (and (symbolp symbol)
       (or (boundp symbol)
	   (eval `(let (,symbol)
		    (declare (ignorable ,symbol))
		    (boundp ',symbol))))))

(defun macrop (symbol)
  "Return true if SYMBOL designate a macro."
  (and (symbolp symbol)
       (macro-function symbol)))

(defun simple-function-p (symbol)
  "Return true if SYMBOL is a function that is nor a macro nor a generic function."
  (and (fboundp symbol)
       (not (generic-method-p symbol))
       (not (macrop symbol))))

(defun classp (symbol)
  "Return true if SYMBOL designate a class."
  (find-class symbol nil))
