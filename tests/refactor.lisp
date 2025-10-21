(cl:in-package #:common-lisp-user)

(uiop:define-package #:breeze.test.refactor
    (:use :cl #:breeze.refactor)
  ;; Importing non-exported symbols of the "package under test"
  (:import-from #:breeze.refactor
                #:suggest-package-definition)
  ;; Things needed to define new commands
  (:import-from #:breeze.command
                #:define-command
                #:insert
                #:choose
                #:outer-node)
  (:import-from #:breeze.test.command
                #:drive-command)
  (:import-from #:breeze.string
                #:remove-indentation
                #:split-by-newline)
  (:import-from #:breeze.indirection
                #:with-simple-indirections)
  (:import-from #:parachute
                #:define-test
                #:define-test+run
                #:is
                #:true
                #:false
                #:of-type))

(in-package #:breeze.test.refactor)

(defparameter *directory* "./")


;;; Testing suggestions

;; TODO...


;;; Test refactoring commands

;; TODO this probably belongs in "tests/commands.lisp"
(define-test+run "All commands must be exported"
  (let ((commands (remove-if (lambda (symbol)
                               (or (search ".TEST." (package-name (symbol-package symbol)))
                                   (breeze.xref:externalp symbol)))
                             (breeze.command:list-all-commands))))
    (false commands "The following commands are not exported:~%~{  - ~S~%~}" commands)))

(defun missing-tests ()
  (set-difference
   ;; List all commands
   (breeze.command:list-all-commands)
   ;; List relevant tests
   (remove-if
    (lambda (test)
      (member test '(breeze.thread:breeze-kill-worker-threads)))
    (remove-if-not #'symbolp
                   (mapcar #'parachute:name
                           (parachute:package-tests '#:breeze.test.refactor
                                                    ;; *package*
                                                    ))))
   :key #'symbol-name
   :test #'string=))

#++ ;; TODO #A re-enable this test
(define-test+run "All commands must be tested"
  (let ((commands (missing-tests)))
    (false commands
           "The following commands don't have a corresponding test:~%~{  - ~S~%~}"
           commands)))




(defun common-trace-asserts (command-name trace expected-length)
  "Generic assertions for the output of drive-command."
  (check-type command-name symbol)
  (of-type 'list trace
           "Drive-command should return a list.")
  (false (caar trace) "The first input should always be nil.")
  (is equal '(nil ("done")) (alexandria:lastcar trace)
      "The command should be done.")
  (is = expected-length (length trace)
      "The command ~a was expected to have a trace of length ~d, got ~d instead."
      command-name expected-length (length trace)))



(define-test insert-asdf
  (let* ((trace (drive-command #'insert-asdf
                               :inputs '("a" "b" "c")
                               :context '())))
    (common-trace-asserts 'insert-asdf trace 8)
    (destructuring-bind (input request) (first trace)
      (false input)
      (is string= "read-string" (first request))
      (is string= "Name of the system: " (second request))
      (false (third request)))
    (destructuring-bind (input request) (second trace)
      (is string= "a" input)
      (is string= "read-string" (first request))
      (is string= "Author: " (second request))
      (is string= "" (third request)))
    (destructuring-bind (input request) (third trace)
      (is string= "b" input)
      (is string= "read-string" (first request))
      (is string= "Licence name: " (second request))
      (false (third request)))
    (destructuring-bind (input request) (fourth trace)
      (is string= "c" input)
      (is string= "insert" (first request))
      (is equal
          '("(cl:in-package #:cl)"
            ""
            "")
          (split-by-newline (second request))))
    (destructuring-bind (input request) (fifth trace)
      (false input)
      (is string= "insert" (first request))
      (is equal
          '("(defpackage #:a.asd"
            " (:use :cl :asdf))"
            ""
            "")
          (split-by-newline (second request))))
    (destructuring-bind (input request) (sixth trace)
      (false input)
      (is string= "insert" (first request))
      (is equal
          '("(in-package #:a.asd)"
            ""
            "")
          (split-by-newline (second request))))
    (destructuring-bind (input request) (seventh trace)
      (false input)
      (is string= "insert" (first request))
      (is equal
          '("(asdf:defsystem #:a"
            "  :description \"\""
            "  :version \"0.0.1\""
            "  :author \"b\""
            "  :licence \"c\""
            "  :depends-on ()"
            "  ;; :pathname \"src\""
            "  ;; :serial t"
            "  :components"
            "    (#+(or) (:file \"todo\"))"
            "  ;; in order to test this system, load the test system"
            "  :in-order-to ((test-op (load-op a/test)))"
            "  ;; this tells asdf what to execute to run the tests"
            "  :perform"
            "  (test-op (o c)"
            "           (uiop:symbol-call"
            "            'a.test 'run-tests))"
            "  )"
              "")

          (split-by-newline (second request))))))

(define-test+run insert-breeze-define-command
  (let* ((trace (drive-command #'insert-breeze-define-command
                               :inputs '("rmrf")
                               :context '())))
    (common-trace-asserts 'insert-breeze-define-command trace 3)
    (destructuring-bind (input request) (first trace)
      (declare (ignorable input))
      (is string= "read-string" (first request))
      (is string= "Name of the command (symbol): " (second request))
      (is string= nil (third request)))
    (destructuring-bind (input request) (second trace)
      (declare (ignorable input))
      (is equal '"rmrf" input) ;; TODO the linter should warn me about the extraneous quote
      (is string= "insert" (first request))
      (is equal
          '("(define-command rmrf ()"
            "  \"Rmrf.\""
            "  )")
          (split-by-newline (second request))))))

(define-test+run insert-defun
  (let* ((trace (drive-command #'insert-defun
                               :inputs '("real-fun" "a &optional b")
                               :context '())))
    (common-trace-asserts 'insert-defun trace 6)
    (destructuring-bind (input request) (first trace)
      (declare (ignorable input))
      (is string= "insert" (first request))
      (is string= "(defun " (second request)))
    (destructuring-bind (input request) (second trace)
      (declare (ignorable input))
      (is string= "read-string" (first request))
      (is string= "Name: " (second request))
      (is string= nil (third request)))
    (destructuring-bind (input request) (third trace)
      (is equal '"real-fun" input)
      (is string= "insert" (first request))
      (is string= "real-fun " (second request)))
    (destructuring-bind (input request) (fourth trace)
      (declare (ignorable input))
      (is string= "read-string" (first request))
      (is string= "Enter the arguments: " (second request))
      (is string= nil (third request)))
    (destructuring-bind (input request) (fifth trace)
      (is equal '"a &optional b" input)
      (is string= "insert" (first request))
      (is equal
          '("(a &optional b))"
            "")
          (split-by-newline (second request))))))

(define-test insert-defvar
  (let* ((trace (drive-command #'insert-defvar
                               :inputs '("var" "42" "This is a nice var")
                               :context '())))
    (common-trace-asserts 'insert-defvar trace 8)
    (destructuring-bind (input request) (first trace)
      (declare (ignorable input))
      (is string= "insert" (first request))
      (is string= "(defvar " (second request)))
    (destructuring-bind (input request) (second trace)
      (declare (ignorable input))
      (is string= "read-string" (first request))
      (is string= "Name: " (second request))
      (is string= nil (third request)))
    (destructuring-bind (input request) (third trace)
      (is equal '"var" input)
      (is string= "insert" (first request))
      (is string= "*var* " (second request)))
    (destructuring-bind (input request) (fourth trace)
      (declare (ignorable input))
      (is string= "read-string" (first request))
      (is string= "Initial value: " (second request))
      (is string= nil (third request)))
    (destructuring-bind (input request) (fifth trace)
      (is equal '"42" input)
      (is string= "insert" (first request))
      (is equal
          '("42"
            "")
          (split-by-newline (second request))))
    (destructuring-bind (input request) (sixth trace)
      (declare (ignorable input))
      (is string= "read-string" (first request))
      (is string= "Documentation string " (second request))
      (is string= nil (third request)))
    (destructuring-bind (input request) (seventh trace)
      (is equal '"This is a nice var" input)
      (is string= "insert" (first request))
      (is string= "\"This is a nice var\")" (second request)))))

(define-test insert-defclass
  (let* ((trace (drive-command #'insert-defclass
                               :inputs '("klass")
                               :context '())))
    (common-trace-asserts 'insert-defclass trace 3)
    (destructuring-bind (input request) (first trace)
      (declare (ignorable input))
      (is string= "read-string" (first request))
      (is string= "Name of the class: " (second request))
      (is string= nil (third request)))
    (destructuring-bind (input request) (second trace)
      (is equal '"klass" input)
      (is string= "insert" (first request))
      (is equal
          '("(defclass klass ()"
            "  ((slot"
            "    :initform nil"
            "    :initarg :slot"
            "    :accessor klass-slot))"
            "  (:documentation \"\"))")
          (split-by-newline (second request))))))

(define-test+run insert-defmacro
  (let* ((trace (drive-command #'insert-defmacro
                               :inputs '("mac" "(x) &body body")
                               :context '())))
    (common-trace-asserts 'insert-defmacro trace 6)
    (destructuring-bind (input request) (first trace)
      (declare (ignorable input))
      (is string= "insert" (first request))
      (is string= "(defmacro " (second request)))
    (destructuring-bind (input request) (second trace)
      (declare (ignorable input))
      (is string= "read-string" (first request))
      (is string= "Name: " (second request))
      (is string= nil (third request)))
    (destructuring-bind (input request) (third trace)
      (is equal '"mac" input)
      (is string= "insert" (first request))
      (is string= "mac " (second request)))
    (destructuring-bind (input request) (fourth trace)
      (declare (ignorable input))
      (is string= "read-string" (first request))
      (is string= "Enter the arguments: " (second request))
      (is string= nil (third request)))
    (destructuring-bind (input request) (fifth trace)
      (is equal '"(x) &body body" input)
      (is string= "insert" (first request))
      (is equal
          '("(x) &body body))"
            "")
          (split-by-newline (second request))))))

(define-test+run insert-defgeneric
  (let* ((trace (drive-command #'insert-defgeneric
                               :inputs '("gen")
                               :context '())))
    (common-trace-asserts 'insert-defgeneric trace 3)
    (destructuring-bind (input request) (first trace)
      (declare (ignorable input))
      (is string= "read-string" (first request))
      (is string= "Name of the generic function: " (second request))
      (is string= nil (third request)))
    (destructuring-bind (input request) (second trace)
      (is equal '"gen" input)
      (is string= "insert" (first request))
      (is equal
          '("(defgeneric gen ()"
            "  (:documentation \"\")"
            "  #++(:method-combination + #++ :most-specific-last)"
            "  (:method () ()))")
          (split-by-newline (second request))))))

(define-test insert-defmethod
  (let* ((trace (drive-command #'insert-defmethod
                               :inputs '("frob")
                               :context '())))
    (common-trace-asserts 'insert-defmethod trace 3)
    (destructuring-bind (input request) (first trace)
      (declare (ignorable input))
      (is string= "read-string" (first request))
      (is string= "Name of the method: " (second request))
      (is string= nil (third request)))
    (destructuring-bind (input request) (second trace)
      (is equal '"frob" input)
      (is string= "insert" (first request))
      (is equal
          '("(defmethod frob ()"
            "  )")
          (split-by-newline (second request))))))


(define-test+run insert-defparameter
  (let* ((trace (drive-command #'insert-defparameter
                               :inputs '("param" "\"meh\""
                                         "This is a meh variable")
                               :context '())))
    (common-trace-asserts 'insert-defparameter trace 8)
    (destructuring-bind (input request) (first trace)
      (false input)
      (is string= "insert" (first request))
      (is string= "(defparameter " (second request)))
    (destructuring-bind (input request) (second trace)
      (false input)
      (is string= "read-string" (first request))
      (is string= "Name: " (second request))
      (false (third request)))
    (destructuring-bind (input request) (third trace)
      (is string= "param" input)
      (is string= "insert" (first request))
      (is string= "*param* " (second request)))
    (destructuring-bind (input request) (fourth trace)
      (false input)
      (is string= "read-string" (first request))
      (is string= "Initial value: " (second request))
      (false (third request)))
    (destructuring-bind (input request) (fifth trace)
      (is string= "\"meh\"" input)
      (is string= "insert" (first request))
      (is equal
          '("\"meh\""
            "")
          (split-by-newline (second request))))
    (destructuring-bind (input request) (sixth trace)
      (false input)
      (is string= "read-string" (first request))
      (is string= "Documentation string " (second request))
      (false (third request)))
    (destructuring-bind (input request) (seventh trace)
      (is string= "This is a meh variable" input)
      (is string= "insert" (first request))
      (is string= "\"This is a meh variable\")" (second request)))))

(define-test insert-handler-bind-form
  (let* ((trace (drive-command #'insert-handler-bind-form
                               :inputs 'nil
                               :context '())))
    (common-trace-asserts 'insert-handler-bind-form trace 2)
    (destructuring-bind (input request) (first trace)
      (false input)
      (is string= "insert" (first request))
      (is equal
          '("(handler-bind"
            "  ((error (lambda (condition)"
            "    (describe condition *debug-io*))))"
            "  (frobnicate))")
          (split-by-newline (second request))))))

(define-test insert-handler-case-form
  (let* ((trace (drive-command #'insert-handler-case-form
                               :inputs 'nil
                               :context '())))
    (common-trace-asserts 'insert-handler-case-form trace 2)
    (destructuring-bind (input request) (first trace)
      (false input)
      (is string= "insert" (first request))
      (is equal
          '("(handler-case"
            "  (frobnicate)"
            "  (error (condition)"
            "    (describe condition *debug-io*)))")
          (split-by-newline (second request))))))

#++
(define-test insert-in-package-cl-user
  (let* ((trace (drive-command #'insert-in-package-cl-user
                               :inputs 'nil
                               :context '())))
    (common-trace-asserts 'insert-in-package-cl-user trace 2)
    (destructuring-bind (input request) (first trace)
      (false input)
      (is string= "insert" (first request))
      (is string= "(cl:in-package #:cl-user)" (second request)))))

(define-test insert-lambda
  (let* ((trace (drive-command #'insert-lambda
                               :inputs 'nil
                               :context '())))
    (common-trace-asserts 'insert-lambda trace 2)
    (destructuring-bind (input request) (first trace)
      (false input)
      (is string= "insert" (first request))
      (is string= "(lambda ())" (second request)))))

(define-test insert-loop-clause-for-hash
  (let* ((trace (drive-command #'insert-loop-clause-for-hash
                               :inputs '("key" "*ht*" "v")
                               :context '())))
    (common-trace-asserts 'insert-loop-clause-for-hash trace 8)
    (destructuring-bind (input request) (first trace)
      (false input)
      (is string= "insert" (first request))
      (is string= " :for " (second request)))
    (destructuring-bind (input request) (second trace)
      (false input)
      (is string= "read-string" (first request))
      (is string= "Enter the variable name for the key: " (second request))
      (false (third request)))
    (destructuring-bind (input request) (third trace)
      (is string= "key" input)
      (is string= "insert" (first request))
      (is string= "key :being :the :hash-key :of " (second request)))
    (destructuring-bind (input request) (fourth trace)
      (false input)
      (is string= "read-string" (first request))
      (is string= "Enter the variable name for the hash-table: " (second request))
      (false (third request)))
    (destructuring-bind (input request) (fifth trace)
      (is string= "*ht*" input)
      (is string= "insert" (first request))
      (is string= "*ht* :using (hash-value " (second request)))
    (destructuring-bind (input request) (sixth trace)
      (false input)
      (is string= "read-string" (first request))
      (is string= "Enter the variable name for the value: " (second request))
      (false (third request)))
    (destructuring-bind (input request) (seventh trace)
      (is string= "v" input)
      (is string= "insert" (first request))
      (is string= "v)" (second request)))))

(define-test insert-loop-clause-for-in-list
  (let* ((trace (drive-command #'insert-loop-clause-for-in-list
                               :inputs '("el" "list")
                               :context '())))
    (common-trace-asserts 'insert-loop-clause-for-in-list trace 6)
    (destructuring-bind (input request) (first trace)
      (false input)
      (is string= "insert" (first request))
      (is string= " :for " (second request)))
    (destructuring-bind (input request) (second trace)
      (false input)
      (is string= "read-string" (first request))
      (is string= "Enter the variable name for the iterator: " (second request))
      (false (third request)))
    (destructuring-bind (input request) (third trace)
      (is string= "el" input)
      (is string= "insert" (first request))
      (is string= "el :in " (second request)))
    (destructuring-bind (input request) (fourth trace)
      (false input)
      (is string= "read-string" (first request))
      (is string= "Enter the the list to iterate on: " (second request))
      (false (third request)))
    (destructuring-bind (input request) (fifth trace)
      (is string= "list" input)
      (is string= "insert" (first request))
      (is string= "list" (second request)))))

(define-test insert-loop-clause-for-on-list
  (let* ((trace (drive-command #'insert-loop-clause-for-on-list
                               :inputs '("rest" "list")
                               :context '())))
    (common-trace-asserts 'insert-loop-clause-for-on-list trace 6)
    (destructuring-bind (input request) (first trace)
      (false input)
      (is string= "insert" (first request))
      (is string= " :for " (second request)))
    (destructuring-bind (input request) (second trace)
      (false input)
      (is string= "read-string" (first request))
      (is string= "Enter the variable name for the iterator: " (second request))
      (false (third request)))
    (destructuring-bind (input request) (third trace)
      (is string= "rest" input)
      (is string= "insert" (first request))
      (is string= "rest :on " (second request)))
    (destructuring-bind (input request) (fourth trace)
      (false input)
      (is string= "read-string" (first request))
      (is string= "Enter the the list to iterate on: " (second request))
      (false (third request)))
    (destructuring-bind (input request) (fifth trace)
      (is string= "list" input)
      (is string= "insert" (first request))
      (is string= "list" (second request)))))

(define-test insert-print-unreadable-object-boilerplate
  (let* ((trace (drive-command #'insert-print-unreadable-object-boilerplate
                               :inputs '("node" "node")
                               :context '())))
    (common-trace-asserts 'insert-print-unreadable-object-boilerplate trace 4)
    (destructuring-bind (input request) (first trace)
      (false input)
      (is string= "read-string" (first request))
      (is string= "Name of the object (parameter name of the method): " (second request))
      (false (third request)))
    (destructuring-bind (input request) (second trace)
      (is string= "node" input)
      (is string= "read-string" (first request))
      (is string= "Type of the object: " (second request))
      (false (third request)))
    (destructuring-bind (input request) (third trace)
      (is string= "node" input)
      (is string= "insert" (first request))
      (is equal
          '("(defmethod print-object ((node node) stream)"
            "  (print-unreadable-object"
            "      (node stream :type t :identity nil)"
            "    (format stream \"~s\" (node-something node))))")
          (split-by-newline (second request))))))

;; TODO
;; (define-test+run insert-parachute-define-test)



;;; TODO

(defun call-with-fake-file (fn content
                            &key
                              (buffer-name "fake-file.lisp")
                              (buffer-file-name buffer-name)
                              (point 1)
                              (point-min 1)
                              (point-max (1+ (length content))))
  (let* ((breeze.workspace:*workspace* (make-instance 'breeze.workspace:workspace))
         (breeze.command::*command* (breeze.command::make-command-handler
                                     fn
                                     (list
                                      :buffer-string content
                                      :buffer-name buffer-name
                                      :buffer-file-name buffer-file-name
                                      :point point
                                      :point-min point-min
                                      :point-max point-max))))
    (funcall fn)))


(defun test-suggest-package-definition (&rest args)
  (apply #'call-with-fake-file #'suggest-package-definition args))

(defun should-suggest-to-insert-package-definition-when (when content)
  (loop :for point :from 1 #| TODO 0 |# :upto (1+ #| TODO remove 1+ |# (length content))
        :do (is eq 'breeze.package-commands:insert-defpackage (test-suggest-package-definition content :point point)
                "Should suggest to insert a package definition when the buffer ~a (point = ~d)."
                when point)))

(define-test+run suggest-package-definition
  (should-suggest-to-insert-package-definition-when
   "is empty"
   "")
  (should-suggest-to-insert-package-definition-when
   "contains only whitespaces"
   "  ")
  (should-suggest-to-insert-package-definition-when
   "contains only line comments"
   "; a line comment ")
  (should-suggest-to-insert-package-definition-when
   "contains only block comments"
   "#| a block comment |#")
  (should-suggest-to-insert-package-definition-when
   "contains only block comments, even if it's not closed properly"
   "#| a block comment |#"))

;;; maybe-ask-to-load-system
;;; check-in-package
;;; compute-suggestions


;;; Testing quickfix... This is nice, but quickfix regroups pretty
;;; much everything, so it is be better to test smaller parts
;;; individually.

(defun test-quickfix (buffer-name pre post &key (inputs (list "")))
  "Helper function to test the quickfix command. The PRE and POST
arguments represents the content of the buffer, the point is where the
strings get concatenated."
  (let ((buffer-filename (namestring (merge-pathnames buffer-name *directory*)))
        (point (length pre))
        (buffer-string (concatenate 'string pre post)))
    (drive-command 'quickfix
                   :context (list :buffer-name buffer-name
                                  :buffer-file-name buffer-filename
                                  :buffer-string buffer-string
                                  :point point
                                  :point-min 0
                                  :point-max (length buffer-string))
                   :inputs inputs)))

#++
(test-quickfix "blah.lisp" "" ""
               :inputs '("Insert a defpackage form." "foo"))


#++
(test-quickfix "blah.lisp" "  " "  "
               :inputs '("Insert a defpackage form." "foo"))


(defun expect-suggestions (&rest expected-suggested-commands)
  `((nil ("choose" "Choose a command: " ,(mapcar (alexandria:compose #'second
                                                                     #'command-description)
                                                 expected-suggested-commands)))
    ("" ("message" "\"\" is not a valid choice"))
    (() ("done"))))


#++
(define-test "quickfix: mapcar"
  ;; TODO Make less brittle assertions, like "Insert lambda form."
  ;; must be suggested, but don't fail the test if there are other
  ;; suggestions.
  (is equalp
      (expect-suggestions 'insert-lambda)
      (test-quickfix
       "mapcar.lisp"
       "(mapcar " ")")))

#++
(define-test "quickfix: suggest inserting lambda"
  ;; TODO Make less brittle assertions, like "Insert lambda form."
  ;; must be suggested, but don't fail the test if there are other
  ;; suggestions.
  (is equalp
      (expect-suggestions 'insert-lambda)
      (test-quickfix
       "mapcar.lisp"
       `("(" (or
              apply
              complement
              ;; map
              ;; map-into
              mapc
              mapcan
              mapcar
              mapcon
              mapl
              maplist
              reduce
              ;; set-dispatch-macro-character
              ;; set-macro-character
              ;; set-pprint-dispatch
              ;; shared-initialize
              funcall)
             (cursor " ")
             ")"))))


#+(or)
(buffer-string
 (alexandria:plist-hash-table
  '(:buffer-string "asdf")))

#++ ;; TODO modify a define-package/defpackage form to add 1
;; import-from "clause". In this case alexandria:when-let
(let* ((input
        "(uiop:define-package #:package
    (:use #:cl)
  (:use-reexport #:breeze.parser #:breeze.pattern)
  ;; Category A
  (:export
   #:a
   #:b
   #:c)
  ;; Category B
  (:export
   #:d
   #:e
   #:f))")
       (expected-output
        "(uiop:define-package #:package
    (:use #:cl)
  (:use-reexport #:breeze.parser #:breeze.pattern)
  ;; Category A
  (:export
   #:a
   #:b
   #:c)
  ;; Category B
  (:export
   #:d
   #:e
   #:f)
  (:import-from #:alexandria
                #:when-let))")
       (state (read-))))

#++ ;; TODO
(define-test "simple format fixes"
    (let* ((trace (drive-command #'fix-formatting
                                 :inputs '()
                                 :context '())))
      (common-trace-asserts 'fix-formatting trace 4)

      ;; TODO
      (destructuring-bind (input request) (first trace)
        (false input)
        (is string= "read-string" (first request))
        (is string= "Name of the object (parameter name of the method): " (second request))
        (false (third request)))
      (destructuring-bind (input request) (second trace)
        (is string= "node" input)
        (is string= "read-string" (first request))
        (is string= "Type of the object: " (second request))
        (false (third request)))
      (destructuring-bind (input request) (third trace)
        (is string= "node" input)
        (is string= "insert" (first request))
        (is equal
            '("(defmethod print-object ((node node) stream)"
              "  (print-unreadable-object"
              "      (node stream :type t :identity nil)"
              "    (format stream \"~s\" (node-something node))))")
            (split-by-newline (second request))))))



;;; Completion at point

;; Should suggest valid type specifier in check-type, etypecase, etc.
;;
;; Should suggest symbols that are not imported, and automatically
;; import them (e.g. add them to the right defpackage.
