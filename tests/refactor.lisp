;;;; !!!! TODO !!!! make the asserts easier to read/less brittle


(cl:in-package #:common-lisp-user)

(uiop:define-package #:breeze.test.refactor
  (:use :cl #:breeze.refactor)
  ;; Importing non-exported symbols of the "package under test"
  (:import-from #:breeze.refactor
                #:augment-context-by-parsing-the-buffer)
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
                #:choose)
  (:import-from #:breeze.test.command
                #:drive-command)
  (:import-from #:breeze.reader
                #:node-content
                #:parse-string
                #:unparse-to-string

                ;; Types of node
                #:skipped-node
                #:symbol-node
                #:read-eval-node
                #:character-node
                #:list-node
                #:function-node

                ;; Type predicates
                #:skipped-node-p
                #:symbol-node-p
                #:read-eval-node-p
                #:character-node-p
                #:list-node-p
                #:function-node-p)
  (:import-from #:breeze.utils
                #:remove-indentation)
  (:import-from #:parachute
                #:define-test
                #:define-test+run
                #:is
                #:true
                #:false))

(in-package #:breeze.test.refactor)

(defparameter *directory* "./")



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


;;; Oh yiisss

;; TODO be able to update existing tests
(define-command insert-test ()
  "Insert a missing test!"
  (let* ((name (choose "Name of the command (symbol): "
                       ;; (breeze.refactor::all-commands)
                       (missing-tests)
                       ))
         (symbol (find-symbol name (find-package :breeze.refactor)))
         (output (drive-command symbol
                                :inputs '()
                                :context '()
                                :ask-for-missing-input-p t))
         (inputs (remove-if #'null (mapcar #'first output)))
         (*print-case* :downcase))
    (insert
     (remove-indentation
      "
       (define-test+run ~(~a~)
         (is equal
             '~s
             (drive-command #'~(~a~)
                            :inputs '~s
                            :context '())))")
     name
     output

     name
     inputs)))


;; This is emacs lisps to add a binding to the command "insert-test"
;; defined just above:
#++
(progn
  (defun breeze--insert-test ()
    (interactive)
    (breeze-run-command "breeze.test.refactor::insert-test"))
  (define-key breeze-minor-mode-map (kbd "C-,") #'breeze--insert-test))



(define-test insert-asdf
  (is equal
      '((nil ("read-string" "Name of the system: " nil))
        ("a" ("read-string" "Author: " nil))
        ("b" ("read-string" "Licence name: " nil))
        ("c"
         ("insert" "(cl:in-package #:cl)

"))
        (nil
         ("insert" "(defpackage #:a.asd
 (:use :cl :asdf))

"))
        (nil
         ("insert" "(in-package #:a.asd)

"))
        (nil
         ("insert" "(asdf:defsystem #:a
  :description \"\"
  :version \"0.0.1\"
  :author \"b\"
  :licence \"c\"
  :depends-on ()
  ;; :pathname \"src\"
  :serial t
    :components
    (#+(or) (:file \"todo\")))
"))
        (nil ("done")))
      (drive-command #'insert-asdf
                     :inputs '("a" "b" "c")
                     :context '())))

(define-test insert-breeze-define-command
  (is equal
      '((nil ("read-string" "Name of the command (symbol): " nil))
        ("rmrf"
         ("insert" "(define-command rmrf ()
  \"Rmrf.\"
  )"))
        (nil ("done")))
      (drive-command #'insert-breeze-define-command
                     :inputs '("rmrf")
                     :context '())))

(define-test insert-defun
  (is equal
      '((nil ("insert" "(defun ")) (nil ("read-string" "Name: " nil))
        ("real-fun" ("insert" "real-fun ("))
        (nil ("read-string" "Enter the arguments: " nil))
        ("a &optional b"
         ("insert" "a &optional b)
)"))
        (nil ("backward-char" nil)) (nil ("done")))
      (drive-command #'insert-defun
                     :inputs '("real-fun" "a &optional b")
                     :context '())))


(define-test insert-defvar
  (is equal
      '((nil ("insert" "(defvar ")) (nil ("read-string" "Name: " nil))
        ("var" ("insert" "*var* ")) (nil ("read-string" "Initial value: " nil))
        ("42"
         ("insert" "42
"))
        (nil ("read-string" "Documentation string " nil))
        ("This is a nice var" ("insert" "\"This is a nice var\")"))
        (nil ("done")))
      (drive-command #'insert-defvar
                     :inputs '("var" "42" "This is a nice var")
                     :context '())))


(define-test insert-defclass
  (is equal
      '((nil ("read-string" "Name of the class: " nil))
        ("k"
         ("insert" "(defclass k ()
  ((slot
    :initform nil
    :initarg :slot
    :accessor k-slot))
  (:documentation \"\"))"))
        (nil ("done")))
      (drive-command #'insert-defclass
                     :inputs '("k")
                     :context '())))


(define-test insert-defmacro
  (is equal
      '((nil ("insert" "(defmacro ")) (nil ("read-string" "Name: " nil))
        ("mac" ("insert" "mac ("))
        (nil ("read-string" "Enter the arguments: " nil))
        ("(x) &boby body"
         ("insert" "(x) &boby body)
)"))
        (nil ("backward-char" nil)) (nil ("done")))
      (drive-command #'insert-defmacro
                     :inputs '("mac" "(x) &boby body")
                     :context '())))


(define-test insert-defgeneric
  (is equal
      '((nil ("read-string" "Name of the generic: " nil))
        ("gen"
         ("insert" "(defgeneric gen ()
  (:documentation \"\")
  (:method () ()))"))
        (nil ("done")))
      (drive-command #'insert-defgeneric
                     :inputs '("gen")
                     :context '())))

;; TODO Variants: *insert-defpackage/cl-user-prefix*
;; TODO infer-project-name
;; TODO infer-is-test-file
;; TODO infer-package-name-from-file
(define-test+run insert-defpackage
  (is equal
      '((nil ("read-string" "Name of the package: " nil))
        ("pkg"
         ("insert" "(defpackage #:pkg
  (:documentation \"\")
  (:use #:cl))

(in-package #:pkg)"))
        (nil ("done")))
      (drive-command #'insert-defpackage
                     :inputs '("pkg")
                     :context '())))

(define-test insert-defparameter
  (is equal
      '((nil ("insert" "(defparameter ")) (nil ("read-string" "Name: " nil))
        ("param" ("insert" "*param* "))
        (nil ("read-string" "Initial value: " nil))
        ("\"meh\""
         ("insert" "\"meh\"
"))
        (nil ("read-string" "Documentation string " nil))
        ("This a meh variable" ("insert" "\"This a meh variable\")"))
        (nil ("done")))
      (drive-command #'insert-defparameter
                     :inputs '("param" "\"meh\"" "This a meh variable")
                     :context '())))

(define-test insert-handler-bind-form
  (is equal
      '((nil
         ("insert" "(handler-bind
  ((error #'(lambda (condition)
    (describe condition *debug-io*))))
  (frobnicate))"))
        (nil ("done")))
      (drive-command #'insert-handler-bind-form)))

(define-test insert-handler-case-form
  (is equal
      '((nil
         ("insert" "(handler-case
  (frobnicate)
  (error (condition)
    (describe condition *debug-io*)))"))
        (nil ("done")))
      (drive-command #'insert-handler-case-form)))

(define-test insert-in-package-cl-user
  (is equal
      '((nil ("insert" "(cl:in-package #:cl-user)")) (nil ("done")))
      (drive-command #'insert-in-package-cl-user
                     :inputs 'nil
                     :context '())))

(define-test insert-lambda
  (is equal
      '((nil ("insert" "#'(lambda ())")) (nil ("done")))
      (drive-command #'insert-lambda
                     :inputs '()
                     :context '())))

(define-test insert-loop-clause-for-hash
  (is equal
      '((nil ("insert" " :for "))
        (nil ("read-string" "Enter the variable name for the key: " nil))
        ("key" ("insert" "key :being :the :hash-key :of "))
        (nil
         ("read-string" "Enter the variable name for the hash-table: " nil))
        ("*ht*" ("insert" "*ht* :using (hash-value "))
        (nil ("read-string" "Enter the variable name for the value: " nil))
        ("v" ("insert" "v)")) (nil ("done")))
      (drive-command #'insert-loop-clause-for-hash
                     :inputs '("key" "*ht*" "v")
                     :context '())))

(define-test insert-loop-clause-for-in-list
  (is equal
      '((nil ("insert" " :for "))
        (nil ("read-string" "Enter the variable name for the iterator: " nil))
        ("el" ("insert" "el :in "))
        (nil ("read-string" "Enter the the list to iterate on: " nil))
        ("list" ("insert" "list")) (nil ("done")))
      (drive-command #'insert-loop-clause-for-in-list
                     :inputs '("el" "list")
                     :context '())))


(define-test insert-loop-clause-for-on-list
  (is equal
      '((nil ("insert" " :for "))
        (nil ("read-string" "Enter the variable name for the iterator: " nil))
        ("rest" ("insert" "rest :on "))
        (nil ("read-string" "Enter the the list to iterate on: " nil))
        ("list" ("insert" "list")) (nil ("done")))
      (drive-command #'insert-loop-clause-for-on-list
                     :inputs '("rest" "list")
                     :context '())))

(define-test insert-print-unreadable-object-boilerplate
  (is equal
      '((nil
         ("read-string" "Name of the object (parameter name of the method): "
          nil))
        ("node" ("read-string" "Type of the object: " nil))
        ("node"
         ("insert" "(defmethod print-object ((node node) stream)
  (print-unreadable-object
      (node stream :type t :identity nil)
    (format stream \"~s\" (node-something node))))"))
        (nil ("done")))
      (drive-command #'insert-print-unreadable-object-boilerplate
                     :inputs '("node" "node")
                     :context '())))


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


(defun expect-suggestions (&rest expected-suggested-commands)
  `((nil ("choose" "Choose a command: " ,(mapcar (alexandria:compose #'second
                                                                     #'command-description)
                                                 expected-suggested-commands)))
    ("" ("message" "\"\" is not a valid choice"))
    (() ("done"))))


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
