(cl:in-package #:common-lisp-user)

(uiop:define-package #:breeze.test.refactor
    (:use :cl #:breeze.refactor)
  ;; Importing non-exported symbols of the "package under test"
  (:import-from #:breeze.refactor)
  ;; Things needed to "drive" a command
  (:import-from #:breeze.command
                #:start-command
                #:cancel-command
                #:continue-command
                #:context-plist-to-hash-table
                #:*command*
                #:donep)
  ;; Things needed to define new commands
  (:import-from #:breeze.command
                #:define-command
                #:insert
                #:choose
                #:outer-node)
  (:import-from #:breeze.test.command
                #:drive-command)
  (:import-from #:breeze.utils
                #:remove-indentation)
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

(define-test "All commands must be exported"
  (let ((commands (remove-if #'breeze.xref:externalp (breeze.refactor::all-commands))))
    (false commands "The following commands are not exported:~%~{  - ~S~%~}" commands)))

(defun missing-tests ()
  (set-difference
   ;; List all commands
   (breeze.refactor::all-commands)
   ;; List relevant tests
   (remove-if-not #'symbolp
                  (mapcar #'parachute:name (parachute:package-tests *package*)))
   :key #'symbol-name
   :test #'string=))

(define-test "All commands must be tested"
  (let ((commands (missing-tests)))
    (false commands
           "The following commands don't have a corresponding test:~%~{  - ~S~%~}"
           commands)))


;;; Oh yiisss! In this "page", I create a command that can generate
;;; tests _interactively_ for a command.
;;;
;;; TODO (WIP) be able to update existing tests (currently, I
;;; re-generate them manually, which will get very annoying fast)

(defun insert-assert-request (expected i)
  "Helper function to split an assertion into multiple _if_ there's a
newline in the expected result."
  (if expected
      (if (position #\Newline expected)
          (progn
            (insert "~%        (is equal")
            (insert "~%            '(")
            (loop :for line :in (str:split #\Newline expected)
                  :for i :from 0
                  :unless (zerop i)
                    :do (insert "~%             ")
                  :do (insert "~s" line))
            (insert ")")
            (insert "~%            (str:split #\\Newline (~:R request)))" i))
          (insert "~%        (is string= ~s (~:R request))" expected i))
      (insert "~%        (false (~:R request))" i)))

;; TODO I sorely need something more declarative for those kinds of
;; snippets... Which is why I'm working so much on having good tests
;; for the snippets in the first place!
#++
(define-command insert-test ()
  "Insert a missing test!"
  (augment-context-by-parsing-the-buffer (breeze.command:context*))

  (breeze.refactor::let+ctx (outer-node
                             (outer-node-car (when (breeze.syntax-tree:list-node-p outer-node)
                                               (breeze.syntax-tree::node-first outer-node))))
    (when (breeze.syntax-tree::list-car-symbol= outer-node 'define-test+run)
      ;; TODO Maybe make a command "replace-form" or "replace-car"
      (breeze.command:replace-region
       (breeze.syntax-tree:node-start outer-node-car)
       (breeze.syntax-tree:node-end outer-node-car)
       "define-test")
      (breeze.command:return-from-command))
    (let* ((name
             (or
              ;; Get from the context, or...
              (when (breeze.syntax-tree::list-car-symbol= outer-node 'define-test)
                (breeze.syntax-tree:node-content
                 (second (breeze.syntax-tree:node-content outer-node))))
              ;; Ask the user
              (choose "Name of the command (symbol): "
                      (or (missing-tests)
                          (breeze.refactor::all-commands)))))
           (symbol (etypecase name
                     (symbol name)
                     (string (read-from-string name))))
           (trace (drive-command symbol
                                 :inputs '()
                                 :context '()
                                 :ask-for-missing-input-p t))
           (inputs (remove-if #'null (mapcar #'first trace)))
           (*print-case* :downcase))
      ;; We got the name from the current top-level form, which means we
      ;; want to insert the snippet elsewhere.
      (when (symbolp name)
        ;; TODO a "go to position" command would be better lol
        (breeze.command:insert-at (breeze.syntax-tree:node-end outer-node) "~%~%"))
      (insert "(define-test+run ~(~a~)" symbol)
      (insert "~%    (let* ((trace (drive-command #'~(~a~)" symbol)
      (insert "~%                                 :inputs '~s" inputs)
      (insert "~%                                 :context '())))")
      (insert "~%      (common-trace-asserts '~(~a~) trace ~d)"
              symbol (length trace))
      (loop
        :for (input request) :in (butlast trace)
        :for i :from 1
        :do
           (insert "~%      (destructuring-bind (input request) (~:R trace)" i)
           (insert "~%        ")
           (if input
               (insert "(is string= ~s input)" input)
               (insert "(false input)")
               ;; (insert "(declare (ignore input))")
               )
           (loop
             :for part :in request
             :for j :from 1
             :do (insert-assert-request part j))
           (insert ")"))
      (insert "))"))))



;; This is emacs lisps to add a binding to the command "insert-test"
;; defined just above:
#+elisp
(progn
  (defun breeze--insert-test ()
    (interactive)
    (breeze-run-command "breeze.test.refactor::insert-test"))
  (define-key breeze-minor-mode-map (kbd "C-,") #'breeze--insert-test))



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
      (false (third request)))
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
          (str:split #\Newline (second request))))
    (destructuring-bind (input request) (fifth trace)
      (false input)
      (is string= "insert" (first request))
      (is equal
          '("(defpackage #:a.asd"
            " (:use :cl :asdf))"
            ""
            "")
          (str:split #\Newline (second request))))
    (destructuring-bind (input request) (sixth trace)
      (false input)
      (is string= "insert" (first request))
      (is equal
          '("(in-package #:a.asd)"
            ""
            "")
          (str:split #\Newline (second request))))
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
            "  :serial t"
            "  :components"
            "    (#+(or) (:file \"todo\")))"
            "")
          (str:split #\Newline (second request))))))

(define-test+run insert-breeze-define-command
    (let* ((trace (drive-command #'insert-breeze-define-command
                                 :inputs '("rmrf")
                                 :context '())))
      (common-trace-asserts 'insert-breeze-define-command trace 3)
      (destructuring-bind (input request) (first trace)
        (is string= "read-string" (first request))
        (is string= "Name of the command (symbol): " (second request))
        (is string= nil (third request)))
      (destructuring-bind (input request) (second trace)
        (is equal '"rmrf" input)
        (is string= "insert" (first request))
        (is equal
            '("(define-command rmrf ()"
              "  \"Rmrf.\""
              "  )")
            (str:split #\Newline (second request))))))

(define-test+run insert-defun
    (let* ((trace (drive-command #'insert-defun
                                 :inputs '("real-fun" "a &optional b")
                                 :context '())))
      (common-trace-asserts 'insert-defun trace 7)
      (destructuring-bind (input request) (first trace)
        (is string= "insert" (first request))
        (is string= "(defun " (second request)))
      (destructuring-bind (input request) (second trace)
        (is string= "read-string" (first request))
        (is string= "Name: " (second request))
        (is string= nil (third request)))
      (destructuring-bind (input request) (third trace)
        (is equal '"real-fun" input)
        (is string= "insert" (first request))
        (is string= "real-fun (" (second request)))
      (destructuring-bind (input request) (fourth trace)
        (is string= "read-string" (first request))
        (is string= "Enter the arguments: " (second request))
        (is string= nil (third request)))
      (destructuring-bind (input request) (fifth trace)
        (is equal '"a &optional b" input)
        (is string= "insert" (first request))
        (is equal
            '("a &optional b)"
              ")")
            (str:split #\Newline (second request))))
      (destructuring-bind (input request) (sixth trace)
        (is string= "backward-char" (first request))
        (is string= nil (second request)))))


(define-test insert-defvar
  (let* ((trace (drive-command #'insert-defvar
                               :inputs '("var" "42" "This is a nice var")
                               :context '())))
    (common-trace-asserts 'insert-defvar trace 8)
    (destructuring-bind (input request) (first trace)
      (is string= "insert" (first request))
      (is string= "(defvar " (second request)))
    (destructuring-bind (input request) (second trace)
      (is string= "read-string" (first request))
      (is string= "Name: " (second request))
      (is string= nil (third request)))
    (destructuring-bind (input request) (third trace)
      (is equal '"var" input)
      (is string= "insert" (first request))
      (is string= "*var* " (second request)))
    (destructuring-bind (input request) (fourth trace)
      (is string= "read-string" (first request))
      (is string= "Initial value: " (second request))
      (is string= nil (third request)))
    (destructuring-bind (input request) (fifth trace)
      (is equal '"42" input)
      (is string= "insert" (first request))
      (is equal
          '("42"
            "")
          (str:split #\Newline (second request))))
    (destructuring-bind (input request) (sixth trace)
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
          (str:split #\Newline (second request))))))

(define-test insert-defmacro
  (let* ((trace (drive-command #'insert-defmacro
                               :inputs '("mac" "(x) &body body")
                               :context '())))
    (common-trace-asserts 'insert-defmacro trace 7)
    (destructuring-bind (input request) (first trace)
      (is string= "insert" (first request))
      (is string= "(defmacro " (second request)))
    (destructuring-bind (input request) (second trace)
      (is string= "read-string" (first request))
      (is string= "Name: " (second request))
      (is string= nil (third request)))
    (destructuring-bind (input request) (third trace)
      (is equal '"mac" input)
      (is string= "insert" (first request))
      (is string= "mac (" (second request)))
    (destructuring-bind (input request) (fourth trace)
      (is string= "read-string" (first request))
      (is string= "Enter the arguments: " (second request))
      (is string= nil (third request)))
    (destructuring-bind (input request) (fifth trace)
      (is equal '"(x) &body body" input)
      (is string= "insert" (first request))
      (is equal
          '("(x) &body body)"
            ")")
          (str:split #\Newline (second request))))
    (destructuring-bind (input request) (sixth trace)
      (is string= "backward-char" (first request))
      (is string= nil (second request)))))

(define-test+run insert-defgeneric
    (let* ((trace (drive-command #'insert-defgeneric
                                 :inputs '("gen")
                                 :context '())))
      (common-trace-asserts 'insert-defgeneric trace 3)
      (destructuring-bind (input request) (first trace)
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
            (str:split #\Newline (second request))))))

(define-test insert-defmethod
  (let* ((trace (drive-command #'insert-defmethod
                               :inputs '("frob")
                               :context '())))
    (common-trace-asserts 'insert-defmethod trace 3)
    (destructuring-bind (input request) (first trace)
      (is string= "read-string" (first request))
      (is string= "Name of the method: " (second request))
      (is string= nil (third request)))
    (destructuring-bind (input request) (second trace)
      (is equal '"frob" input)
      (is string= "insert" (first request))
      (is equal
          '("(defmethod frob ()"
            "  )")
          (str:split #\Newline (second request))))))

;; TODO Variants: *insert-defpackage/cl-user-prefix*
;; TODO infer-project-name
;; TODO infer-is-test-file
;; TODO infer-package-name-from-file
(define-test+run insert-defpackage
  (let* ((trace (drive-command #'insert-defpackage
                               :inputs '("pkg")
                               :context '())))

    (common-trace-asserts 'insert-defpackage trace 4)
    (destructuring-bind (input request) (first trace)
      (false input)
      (is string= "read-string" (first request))
      (is string= "Name of the package: " (second request))
      (false (third request)))
    (destructuring-bind (input request) (second trace)
      (is string= "pkg" input)
      (is string= "insert" (first request))
      (is equal "(defpackage " (second request)))
    (destructuring-bind (input request) (third trace)
      (false input)
      (is string= "insert" (first request))
      (is equal
          '("#:pkg"
            "  (:documentation \"\")"
            "  (:use #:cl))"
            ""
            "(in-package #:pkg)")
          (str:split #\Newline (second request))))))


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
            (str:split #\Newline (second request))))
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
            "  ((error #'(lambda (condition)"
            "    (describe condition *debug-io*))))"
            "  (frobnicate))")
          (str:split #\Newline (second request))))))

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
          (str:split #\Newline (second request))))))

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
      (is string= "#'(lambda ())" (second request)))))

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
          (str:split #\Newline (second request))))))

;; TODO
(define-test+run insert-parachute-define-test)



;;; TODO

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
  (let ((buffer-file-name (namestring (merge-pathnames buffer-name *directory*)))
        (point (length pre))
        (buffer-string (concatenate 'string pre post)))
    (drive-command 'quickfix
                   :context (list :buffer-name buffer-name
                                  :buffer-file-name buffer-file-name
                                  :buffer-string buffer-string
                                  :point point
                                  :point-min 0
                                  :point-max (length buffer-string))
                   :inputs inputs)))

#++
(test-quickfix "blah.lisp" "" ""
               :inputs '("Insert a defpackage form."))

#++
(test-quickfix "blah.lisp" "  " "  "
               :inputs '("Insert a defpackage form."))


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


#+ (or)
(context-buffer-string
 (alexandria:plist-hash-table
  '(:buffer-string "asdf")))
