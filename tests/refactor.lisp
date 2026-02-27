;; TODO test quickinsert

#|
quickfix-1 - current top (or root?) only 1
quickfix-current-sexp - look outward, upto the root
quickfix-buffer
quickfix-all - all files

dont Toggle header line
|#

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
  (:import-from #:breeze.generics
                #:eqv)
  (:import-from #:breeze.workspace
                #:*workspace*
                #:make-workspace)
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
  (is = expected-length (length trace)
      "The command ~a was expected to have a trace of length ~d, got ~d instead."
      command-name expected-length (length trace)))

(defun expect-done (trace)
  (is equal '(:request ("done") :response nil) (first trace)
      "The command should be done.")
  (false (rest trace) "Some trace left to assert..."))

(defun expect-buffer-string (trace-item &optional (expected-buffer-string ""))
  (destructuring-bind (&key request response) trace-item
    (is string= expected-buffer-string response
        "Expected buffer string ~s but got ~s"
        expected-buffer-string response)
    (is equal '("buffer-string") request
        ;; TODO add a nice description for when the test fails...
        )))

(defun expect-read-string (trace-item prompt
                           expected-response
                           &key initial-input
                             history)
  (destructuring-bind (&key request response) trace-item
    (let ((expected-request (list "read-string"
                                  ;; expected prompt
                                  prompt
                                  :initial-input initial-input
                                  :history history)))
      (is equal expected-request request
          "Expected request ~%  ~s~%but got~%  ~s."
          expected-request request))
    (is equal expected-response response
        "Expected the response~%~s~% to (\"read-string\" ~s ...), but got~%~s."
        expected-response prompt response)))

(defun expect-insert* (trace-item &key lines saving-excursion-p)
  (destructuring-bind (&key request response) trace-item
    (false response
           "An \"insert\"'s response should be nil, got ~s."
           response)
    (is eqv (if saving-excursion-p
                '("insert-saving-excursion" :_)
                '("insert" :_))
        request)
    (let* ((lines-got (split-by-newline (second request)))
           (mismatch (mismatch lines lines-got :test #'string=)))
      (is equal lines lines-got
          "Line-by-line comparison:~%~{~s~%~}<--- mismatch here --->~%~{~s~^~%~}"
          (when mismatch
            (loop
              :for el :in lines
              :for i :below mismatch :collect el))
          (when mismatch
            (loop
              :for expected-line :in lines
              :for line-got :in lines-got
              :for i :from 0
              :when (<= mismatch i)
                :collect (if (string= expected-line line-got)
                             line-got
                             (cons expected-line line-got))))))))
(defun expect-insert (trace-item &rest lines)
  (expect-insert* trace-item :lines lines))

(defun expect-insert-saving-excursion (trace-item &rest lines)
  (expect-insert* trace-item :lines lines :saving-excursion-p t))



(define-test+run insert-asdf
  :time-limit 0.1
  (let ((trace (drive-command #'insert-asdf
                              :inputs '("a" "b" "c")
                              :context '())))
    (common-trace-asserts 'insert-asdf trace 6)
    (expect-buffer-string (pop trace))
    (expect-read-string
     (pop trace)
     "Name of the system: " "a"
     :history "breeze-#<function insert-asdf>")
    (expect-read-string
     (pop trace)
     "Author: " "b"
     :initial-input "" ; TODO this is not a useful
                                        ; initial-input... might as well be nil
     :history "breeze-#<function insert-asdf>--author")
    (expect-read-string
     (pop trace)
     "Licence name: " "c"
     :history "breeze-#<function insert-asdf>--licence")
    (expect-insert
     (pop trace)
     "(asdf:defsystem #:a"
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
     ;; TODO this last parenthesis should not be on its own line
     "  )"
     "")
    (expect-done trace)))

(define-test+run insert-breeze-define-command
  :time-limit 0.1
  (let ((trace (drive-command #'insert-breeze-define-command
                              :inputs '("rmrf")
                              :context '())))
    (common-trace-asserts 'insert-breeze-define-command trace 4)
    (expect-buffer-string (pop trace))
    (expect-read-string
     (pop trace)
     "Name of the command (symbol): "
     "rmrf"
     :history "breeze-#<function insert-breeze-define-command>")
    (expect-insert
     (pop trace)
     "(define-command rmrf ()"
     "  \"Rmrf.\""
     "  )")
    (expect-done trace)))

(define-test+run insert-defun
  :time-limit 0.1
  (let ((trace (drive-command #'insert-defun
                              :inputs '("real-fun" "a &optional b")
                              :context '())))
    (common-trace-asserts 'insert-defun trace 8)
    (expect-buffer-string (pop trace))
    (expect-insert (pop trace) "(defun ")
    (expect-insert-saving-excursion (pop trace) ")")
    (expect-read-string (pop trace) "Name: " "real-fun"
                        :history "breeze-#<function insert-defun>")
    (expect-insert (pop trace) "real-fun ")
    (expect-read-string
     (pop trace)
     "Enter the arguments: "
     "a &optional b"
     :history "breeze-#<function insert-defun>")
    (expect-insert (pop trace) "(a &optional b)" "  ")
    (expect-done trace)))

(define-test+run insert-defvar
  :time-limit 0.1
  (let ((trace (drive-command #'insert-defvar
                              :inputs '("var" "42" "This is a nice var")
                              :context '())))
    (common-trace-asserts 'insert-defvar trace 10)
    (expect-buffer-string (pop trace))
    (expect-insert (pop trace) "(defvar ")
    (expect-insert-saving-excursion (pop trace) ")")
    (expect-read-string (pop trace) "Name: " "var"
                        :history "breeze-#<function insert-defvar>")
    (expect-insert (pop trace) "*var* ")
    (expect-read-string (pop trace)
                        "Initial value: "
                        "42"
                        :history "breeze-#<function insert-defvar>")
    (expect-insert (pop trace) "42" "")
    (expect-read-string (pop trace)
                        "Documentation string "
                        "This is a nice var"
                        :history "breeze-#<function insert-defvar>")
    (expect-insert (pop trace) "\"This is a nice var\"")
    (expect-done trace)))

(define-test+run insert-defclass
  :time-limit 0.1
  (let* ((*workspace* (make-workspace))
         (trace (drive-command #'insert-defclass
                               :inputs '("klass" "sloot" "")
                               :context '(:buffer-name "insert-defclass-test.lisp"
                                          :point 0))))
    (common-trace-asserts 'insert-defclass trace 15)
    #| TODO instead of "imperatively" poping trace item from the list of trace, I would like to collect all the assertions into a "model" of what the trace should be.
    That way it would be much much easier to detect drift between the model and the actual trace. ;
    Similar to how I print the "mismatch" in "expect-insert*" ;
    |#
    (expect-buffer-string (pop trace))
    (expect-insert (pop trace) "(defclass")
    (expect-insert-saving-excursion (pop trace) ")")
    (expect-read-string (pop trace)
                        "Name of the class: "
                        "klass"
                        :history "breeze-#<function insert-defclass>")
    (expect-insert (pop trace) " klass ()" "  (")
    (expect-insert-saving-excursion (pop trace) ")")
;;; first insert-class-slot
    (expect-buffer-string (pop trace))
    (expect-read-string (pop trace)
                        "Name of the slot: "
                        "sloot"
                        :history "breeze-#<function insert-defclass>--slot")
    (expect-insert (pop trace)
                   "(sloot"
                   "    :initform nil"
                   "    :initarg :sloot"
                   "    :accessor sloot"
                   "    :documentation \"\")")
;;; second insert-class-slot
    (expect-buffer-string (pop trace))
    (expect-read-string (pop trace)
                        "Name of the slot: "
                        ""
                        :history "breeze-#<function insert-defclass>--slot")
    ;; TODO expect (goto-char 1)
    (pop trace)
    (expect-insert-saving-excursion (pop trace) "\")")
    (expect-insert (pop trace) "" "  (:documentation \"")
    (expect-done trace)))

(define-test+run insert-defmacro
  :time-limit 0.1
  (let ((trace (drive-command #'insert-defmacro
                              :inputs '("mac" "(x) &body body")
                              :context '())))
    (common-trace-asserts 'insert-defmacro trace 8)
    (expect-buffer-string (pop trace))
    (expect-insert (pop trace) "(defmacro ")
    (expect-insert-saving-excursion (pop trace) ")")
    (expect-read-string (pop trace) "Name: " "mac"
                        :history "breeze-#<function insert-defmacro>")
    (expect-insert (pop trace) "mac ")
    (expect-read-string
     (pop trace)
     "Enter the arguments: "
     "(x) &body body"
     :history "breeze-#<function insert-defmacro>")
    (expect-insert (pop trace) "(x) &body body)" "  ")
    (expect-done trace)))

(define-test+run insert-defgeneric
  :time-limit 0.1
  (let ((trace (drive-command #'insert-defgeneric
                              :inputs '("gen")
                              :context '())))
    (common-trace-asserts 'insert-defgeneric trace 4)
    (expect-buffer-string (pop trace))
    (expect-read-string (pop trace) "Name of the generic function: " "gen"
                        :history "breeze-#<function insert-defgeneric>")
    (expect-insert
     (pop trace)
     "(defgeneric gen ()"
     "  (:documentation \"\")"
     "  #++(:method-combination + #++ :most-specific-last)"
     "  (:method () ()))")
    (expect-done trace)))

(define-test+run insert-defmethod
  :time-limit 0.1
  (let ((trace (drive-command #'insert-defmethod
                              :inputs '("frob")
                              :context '())))
    (common-trace-asserts 'insert-defmethod trace 4)
    (expect-buffer-string (pop trace))
    (expect-read-string (pop trace) "Name of the method: " "frob"
                        :history "breeze-#<function insert-defmethod>")
    (expect-insert
     (pop trace)
     "(defmethod frob ()"
     "  )")
    (expect-done trace)))


(define-test+run insert-defparameter
  :time-limit 0.1
  (let ((trace (drive-command #'insert-defparameter
                              :inputs '("param" "\"meh\""
                                        "This is a meh variable")
                              :context '())))
    (common-trace-asserts 'insert-defparameter trace 10)
    (expect-buffer-string (pop trace))
    (expect-insert (pop trace) "(defparameter ")
    (expect-insert-saving-excursion (pop trace) ")")
    (expect-read-string (pop trace) "Name: " "param"
                        :history "breeze-#<function insert-defparameter>")
    (expect-insert (pop trace) "*param* ")
    (expect-read-string (pop trace)
                        "Initial value: "
                        "\"meh\""
                        :history "breeze-#<function insert-defparameter>")
    (expect-insert (pop trace) "\"meh\"" "")
    (expect-read-string (pop trace)
                        "Documentation string "
                        "This is a meh variable"
                        :history "breeze-#<function insert-defparameter>")
    (expect-insert (pop trace) "\"This is a meh variable\"")
    (expect-done trace)))

(define-test+run insert-handler-bind-form
  :time-limit 0.1
  (let ((trace (drive-command #'insert-handler-bind-form
                              :inputs 'nil
                              :context '())))
    (common-trace-asserts 'insert-handler-bind-form trace 3)
    (expect-buffer-string (pop trace))
    (expect-insert
     (pop trace)
     "(handler-bind"
     "  ((error (lambda (condition)"
     "    (describe condition *debug-io*))))"
     "  (frobnicate))")
    (expect-done trace)))

(define-test+run insert-handler-case-form
  :time-limit 0.1
  (let ((trace (drive-command #'insert-handler-case-form
                              :inputs 'nil
                              :context '())))
    (common-trace-asserts 'insert-handler-case-form trace 3)
    (expect-buffer-string (pop trace))
    (expect-insert
     (pop trace)
     "(handler-case"
     "  (frobnicate)"
     "  (error (condition)"
     "    (describe condition *debug-io*)))")
    (expect-done trace)))

#++
(define-test insert-in-package-cl-user
  :time-limit 0.1
  (let ((trace (drive-command #'insert-in-package-cl-user
                              :inputs 'nil
                              :context '())))
    (common-trace-asserts 'insert-in-package-cl-user trace 2)
    (destructuring-bind (&key request response) (pop trace)
      (false response)
      (is string= "insert" (first request))
      (is string= "(cl:in-package #:cl-user)" (second request)))))

(define-test+run insert-lambda
  :time-limit 0.1
  (let ((trace (drive-command #'insert-lambda
                              :inputs 'nil
                              :context '())))
    (common-trace-asserts 'insert-lambda trace 4)
    (expect-buffer-string (pop trace))
    (expect-insert (pop trace) "(lambda (")
    (expect-insert-saving-excursion (pop trace) "))")
    (expect-done trace)))

(define-test+run insert-loop-clause-for-hash
  :time-limit 0.1
  (let ((trace (drive-command #'insert-loop-clause-for-hash
                              :inputs '("key" "*ht*" "v")
                              :context '())))
    (common-trace-asserts 'insert-loop-clause-for-hash trace 9)
    (expect-buffer-string (pop trace))
    (expect-insert (pop trace) " :for ")
    (expect-read-string (pop trace)
                        "Enter the variable name for the key: "
                        "key"
                        :history "breeze-#<function insert-loop-clause-for-hash>")
    (expect-insert (pop trace) "key :being :the :hash-key :of ")
    (expect-read-string (pop trace)
                        "Enter the variable name for the hash-table: "
                        "*ht*"
                        :history "breeze-#<function insert-loop-clause-for-hash>")
    (expect-insert (pop trace) "*ht* :using (hash-value ")
    (expect-read-string (pop trace)
                        "Enter the variable name for the value: "
                        "v"
                        :history "breeze-#<function insert-loop-clause-for-hash>")
    (expect-insert (pop trace) "v)")
    (expect-done trace)))

(define-test+run insert-loop-clause-for-in-list
  :time-limit 0.1
  (let ((trace (drive-command #'insert-loop-clause-for-in-list
                              :inputs '("el" "list")
                              :context '())))
    (common-trace-asserts 'insert-loop-clause-for-in-list trace 7)
    (expect-buffer-string (pop trace))
    (expect-insert (pop trace) " :for ")
    (expect-read-string (pop trace)
                        "Enter the variable name for the iterator: "
                        "el"
                        :history "breeze-#<function insert-loop-clause-for-in-list>")
    (expect-insert (pop trace) "el :in ")
    (expect-read-string (pop trace)
                        "Enter the the list to iterate on: "
                        "list"
                        :history "breeze-#<function insert-loop-clause-for-in-list>")
    (expect-insert (pop trace) "list")
    (expect-done trace)))

(define-test+run insert-loop-clause-for-on-list
  :time-limit 0.1
  (let ((trace (drive-command #'insert-loop-clause-for-on-list
                              :inputs '("rest" "list")
                              :context '())))
    (common-trace-asserts 'insert-loop-clause-for-on-list trace 7)
    (expect-buffer-string (pop trace))
    (expect-insert (pop trace) " :for ")
    (expect-read-string (pop trace)
                        "Enter the variable name for the iterator: "
                        "rest"
                        :history "breeze-#<function insert-loop-clause-for-on-list>")
    (expect-insert (pop trace) "rest :on ")
    (expect-read-string (pop trace)
                        "Enter the the list to iterate on: "
                        "list"
                        :history "breeze-#<function insert-loop-clause-for-on-list>")
    (expect-insert (pop trace) "list")
    (expect-done trace)))

(define-test+run insert-print-unreadable-object-boilerplate
  :time-limit 0.1
  (let ((trace (drive-command #'insert-print-unreadable-object-boilerplate
                              :inputs '("node" "node")
                              :context '())))
    (common-trace-asserts 'insert-print-unreadable-object-boilerplate trace 5)
    (expect-buffer-string (pop trace))
    (expect-read-string (pop trace)
                        "Name of the object (parameter name of the method): "
                        "node"
                        :history "breeze-#<function insert-print-unreadable-object-boilerplate>")
    (expect-read-string (pop trace)
                        "Type of the object: "
                        "node"
                        :history "breeze-#<function insert-print-unreadable-object-boilerplate>")
    (expect-insert
     (pop trace)
     "(defmethod print-object ((node node) stream)"
     "  (print-unreadable-object"
     "      (node stream :type t :identity nil)"
     "    (format stream \"~s\" (node-something node))))")
    (expect-done trace)))

;; TODO
;; (define-test+run run insert-parachute-define-test+run TODO

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
        :do (is eq 'breeze.package-commands:insert-defpackage
                (test-suggest-package-definition content :point point)
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
(define-test+run "quickfix: mapcar"
  ;; TODO Make less brittle assertions, like "Insert lambda form."
  ;; must be suggested, but don't fail the test if there are other
  ;; suggestions.
  (is equalp
      (expect-suggestions 'insert-lambda)
      (test-quickfix
       "mapcar.lisp"
       "(mapcar " ")")))

#++
(define-test+run "quickfix: suggest inserting lambda"
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
(define-test+run "simple format fixes"
  (let* ((trace (drive-command #'fix-formatting
                               :inputs '()
                               :context '())))
    (common-trace-asserts 'fix-formatting trace 4)

    ;; TODO
    (destructuring-bind (&key request response) (pop trace)
      (false response)
      (is string= "read-string" (first request))
      (is string= "Name of the object (parameter name of the method): " (second request))
      (false (third request)))
    (destructuring-bind (&key request response) (pop trace)
      (is string= "node" response)
      (is string= "read-string" (first request))
      (is string= "Type of the object: " (second request))
      (false (third request)))
    (destructuring-bind (&key request response) (pop trace)
      (is string= "node" response)
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
