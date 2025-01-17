;; a.k.a. `code assists`, `code actions``

(in-package #:common-lisp-user)

(uiop:define-package #:breeze.refactor
    (:documentation "Snippets and refactoring commands")
  (:use #:cl #:breeze.command #++ #:breeze.reader)
  (:import-from
   #:alexandria
   #:ends-with-subseq
   #:if-let
   #:symbolicate
   #:lastcar)
  (:import-from
   #:breeze.string
   #:ensure-circumfix)
  (:import-from
   #:breeze.utils
   #:symbol-package-qualified-name
   #:before-last
   #:find-version-control-root)
  (:import-from
   #:breeze.indirection
   #:indirect)
  (:export
   #:command-description
   ;; Simple transformation commands
   #:insert-breeze-define-command
   #:insert-parachute-define-test
   #:insert-loop-clause-for-on-list
   #:insert-loop-clause-for-in-list
   #:insert-loop-clause-for-hash
   #:insert-handler-bind-form
   #:insert-handler-case-form
   #:insert-defvar
   #:insert-defparameter
   #:insert-defconstant
   #:insert-define-constant
   #:insert-defun-shaped
   #:insert-defun
   #:insert-defmacro
   #:insert-defpackage
   #:insert-in-package-cl-user
   #:insert-asdf
   #:insert-defclass
   #:insert-defgeneric
   #:insert-defmethod
   #:insert-print-unreadable-object-boilerplate
   #:insert-make-load-form-boilerplate
   #:insert-lambda
   ;; Other commands
   #:quickfix
   #| WIP |# #++ #:other-file))

(in-package #:breeze.refactor)

#++
(define-node-form-predicates (uiop:define-package))


;;; Insertion commands

;; Dogfood'ing to the max!
(define-command insert-breeze-define-command ()
  "Insert a breeze:define-command form."
  (let ((name (read-string "Name of the command (symbol): ")))
    (insert
     "(define-command ~a ()~
    ~%  \"~@(~a~).\"~
    ~%  )"
     name
     (substitute #\Space #\- name))))

#++ ;; snippet draft:
`(define-command (:the symbol ?name) () \n
   (fmt "\"~@(~a~).\"" ?name))

(define-command insert-handler-case-form ()
  "Insert handler case form."
  (insert
   "(handler-case~
  ~%  (frobnicate)~
  ~%  (error (condition)~
  ~%    (describe condition *debug-io*)))"))

(define-command insert-handler-bind-form ()
  "Insert handler bind form."
  (insert
   "(handler-bind~
  ~%  ((error #'(lambda (condition)~
  ~%    (describe condition *debug-io*))))~
  ~%  (frobnicate))"))

(define-command insert-loop-clause-for-on-list ()
  "Insert a loop clause to iterate on a list."
  (insert " :for ")
  (read-string-then-insert
   "Enter the variable name for the iterator: " "~a :on ")
  (read-string-then-insert
   "Enter the the list to iterate on: " "~a"))

(define-command insert-loop-clause-for-in-list ()
  "Insert a loop clause to iterate in a list."
  (insert " :for ")
  (read-string-then-insert
   "Enter the variable name for the iterator: " "~a :in ")
  (read-string-then-insert
   "Enter the the list to iterate on: " "~a"))

(define-command insert-loop-clause-for-hash ()
  "Insert a loop clause to iterate on a hash-table."
  (insert " :for ")
  (read-string-then-insert
   "Enter the variable name for the key: "
   "~a :being :the :hash-key :of ")
  (read-string-then-insert
   "Enter the variable name for the hash-table: "
   "~a :using (hash-value ")
  (read-string-then-insert
   "Enter the variable name for the value: "
   "~a)"))

(defun insert-defvar-shaped (form-name &optional circumfix)
  "Start a command to insert a form that has the same shape as a
defvar."
  (insert "(~a " form-name)
  (let ((name (read-string "Name: ")))
    (insert (if circumfix
                (ensure-circumfix circumfix name)
                name)))
  (read-string-then-insert "Initial value: " " ~a~%")
  (read-string-then-insert "Documentation string " "\"~a\")"))

(define-command insert-defvar ()
  "Insert a defvar form."
  (insert-defvar-shaped "defvar" "*"))

(define-command insert-defparameter ()
  "Insert a defparameter form."
  (insert-defvar-shaped "defparameter" "*"))

(define-command insert-defconstant ()
  "Insert a defconstant form."
  (insert-defvar-shaped "defconstant" "+"))

;; TODO Add "alexandria" when the symbol is not interned
;;      ^^^ that should go in "refactor.lisp"
(define-command insert-define-constant ()
  "Insert a alexandria:define-constant form."
  (insert-defvar-shaped "define-constant"))


(defun insert-defun-shaped (form-name)
  "Start a command to insert a form that has the same shape as a
defun."
  (insert "(~a " form-name)
  (read-string-then-insert "Name: " "~a (")
  (read-string-then-insert
   ;; Who needs to loop...?
   "Enter the arguments: " "~a)~%)"))

(define-command insert-defun ()
  "Insert a defun form."
  (insert-defun-shaped "defun"))

(define-command insert-defmacro ()
  "Insert a defmacro form."
  (insert-defun-shaped "defmacro"))

;; TODO Move to config?
;; TODO This might change per-project... We could try to infer it?
(defparameter *insert-defpackage/cl-user-prefix* nil
  "Whether to include (in-package #:cl-user) before a defpackage form.")

(defun directory-name (pathname)
  (lastcar (pathname-directory pathname)))

(defun infer-project-name (path)
  "Try to infer the name of the project from the PATH."
  ;; Infer project-name by location .git folder
  (when path
    (if-let ((vc-root (indirect (find-version-control-root path))))
      (directory-name vc-root))))

(defun infer-is-test-file (path)
  "Try to infer if a file is part of the tests."
  (when path
    (member
     (directory-name path)
     '("test" "tests" "t")
     :test #'string-equal)))

(defun infer-package-name-from-file (file-pathname)
  "Given a FILE-PATHNAME, infer a proper package name."
  (when file-pathname
    (let ((project (infer-project-name file-pathname))
          (test (when (infer-is-test-file file-pathname)
                  "test"))
          (name (pathname-name file-pathname)))
      (format nil "~{~a~^.~}"
              (remove-if #'null (list project test name))))))

#+ (or)
(trace
 infer-project-name
 infer-is-test-file
 infer-package-name-from-file)

(define-command insert-defpackage ()
  "Insert a defpackage form."
  (let ((package-name
          (read-string
           "Name of the package: "
           (infer-package-name-from-file (buffer-file-name)))))
    (when *insert-defpackage/cl-user-prefix*
      (insert
       "(cl:in-package #:cl-user)~%~%~"))
    (if nil ; TODO
        (insert "(uiop:define-package ")
        (insert "(defpackage "))
    ;; TODO don't insert the (in-package ...) if it already exists
    (insert
     "#:~a~
    ~%  (:documentation \"\")~
    ~%  (:use #:cl))~
    ~%~
    ~%(in-package #:~a)"
     package-name package-name)))

(define-command insert-local-nicknames ()
  "Insert local nicknames."
  (insert
   "(:local-nicknames ~{~a~^~%~})"
   (loop :for name = (read-string "Name of the package to alias: ")
         :while (plusp (length name))
         :for alias = (read-string "Alias of the package: ")
         :collect (format nil "(#:~a #:~a)" alias name))))

(define-command insert-in-package-cl-user ()
  "Insert (cl:in-package #:cl-user)"
  (insert "(cl:in-package #:cl-user)"))

;; TODO insert-let (need to loop probably)

(define-command insert-asdf ()
  "Insert an asdf system definition form."
  (let ((system-name (read-string "Name of the system: "))
        ;; TODO Add *default-author*
        (author (read-string "Author: "))
        ;; TODO Add default license from config
        (licence (read-string "Licence name: ")))
    (insert "(cl:in-package #:cl)~%~%")
    ;; TODO don't insert a defpackage if it already exists
    (insert "(defpackage #:~a.asd~% (:use :cl :asdf))~%~%"
            system-name)
    (insert "(in-package #:~a.asd)~%~%" system-name)
    (insert "(asdf:defsystem #:~a~%~{  ~a~%~}"
            system-name
            `(":description \"\""
              ":version \"0.0.1\""
              ,(format nil ":author \"~a\"" author)
              ,(format nil ":licence \"~a\"" licence)
              ":depends-on ()"
              ";; :pathname \"src\""
              ":serial t"
              ":components"
              "  (#+(or) (:file \"todo\")))"))))



;; TODO How could I re-use an hypothetical "insert-slot" command?
(define-command insert-defclass ()
  "Insert a defclass form."
  (read-string-then-insert
   "Name of the class: "
   "(defclass ~a ()~
   ~%  ((slot~
   ~%    :initform nil~
   ~%    :initarg :slot~
   ~%    :accessor ~@*~a-slot))~
   ~%  (:documentation \"\"))"))

(define-command insert-defgeneric ()
  "Insert a defgeneric form."
  (let ((name (read-string
               "Name of the generic function: ")))
    (insert
     "(defgeneric ~a ()~
     ~%  (:documentation \"\")~
     ~%  #++(:method-combination + #++ :most-specific-last)~
     ~%  (:method () ()))"
     name)))

(define-command insert-defmethod ()
  "Insert a defmethod form."
  (let ((name (read-string
               "Name of the method: ")))
    (insert
     "(defmethod ~a ()~
    ~%  )"
     name)))

(define-command insert-print-unreadable-object-boilerplate ()
  "Insert a print-object method form."
  (let ((name (read-string
               "Name of the object (parameter name of the method): "))
        (type (read-string
               "Type of the object: ")))
    (insert
     "(defmethod print-object ((~a ~a) stream)~
          ~%  (print-unreadable-object~
          ~%      (~a stream :type t :identity nil)~
          ~%    (format stream \"~~s\" (~a-something ~a))))"
     name type
     name
     type name)))

(define-command insert-make-load-form-boilerplate ()
  "Insert a make-load-form method form."
  (let ((name (read-string
               "Name of the object (parameter name of the method): "))
        (type (read-string
               "Type of the object: "))
        (slots (read-string
                "Slots of the object: ")))
    (insert
     "(defmethod make-load-form ((~A ~A) &optional environment)~
          ~%  (make-load-form-saving-slots ~A~
          ~%                              :slot-names '(~{~A~^ ~})~
          ~%                              :environment environment))"
     name type
     name
     (list slots))))

(define-command insert-lambda ()
  "Insert a lambda form."
  (insert "#'(lambda ())"))

;; TODO quick-insert (format *debug-io* "~&")

(define-command insert-parachute-define-test ()
  "Insert a parachute:define-test form"
  (insert "(define-test+run ")
  (read-string-then-insert "Name of the test: "
                           "~a~%)~%"))


;;;

;; TODO "Move the current form into the nearest parent \"let\" form."
;; TODO "Add or update defpackage to \"import-from\" the symbol at point."


;;; Quickfix

;; TODO move to command.lisp
(defgeneric describe-command (command)
  (:documentation "Give a user-friendly description for a command.")
  (:method ((command symbol))
    (symbol-package-qualified-name command)))

;; TODO move to command.lisp
(defun command-docstring (function)
  "Return the function's docstring, signals an error if it's nil."
  (let ((doc (documentation function 'function)))
    (unless doc
      (error
       "Function ~s does not have a documentation string.~
                  Is it defined?"
       function))
    doc))

(defun command-description (function)
  "Return a list of 2 elements: (FUNCTION its-docstring)"
  (list function (command-docstring function)))

(defparameter *commands-applicable-at-toplevel*
  '(insert-asdf
    insert-defun
    insert-defmacro
    insert-defpackage
    insert-in-package-cl-user
    insert-defvar
    insert-defparameter
    insert-defclass
    insert-defgeneric
    insert-defmethod
    insert-print-unreadable-object-boilerplate
    ;; not specfic to cl
    insert-breeze-define-command
    insert-parachute-define-test))

(defparameter *commands-applicable-in-a-loop-form*
  '(insert-loop-clause-for-in-list
    insert-loop-clause-for-on-list
    insert-loop-clause-for-hash))

;; That's some Java-level variable name
(defparameter *commands-applicable-inside-another-form-or-at-toplevel*
  '(insert-handler-bind-form
    insert-handler-case-form
    insert-lambda))

(defun validate-nearest-in-package (nodes outer-node)
  "Find the lastest \"in-package\" form, test if the packages can be
found."
  (let* ((previous-in-package-form
           (find-nearest-sibling-in-package-form nodes (or outer-node
                                                           (point)))))
    (when previous-in-package-form
      (let* ((package-designator (in-package-node-package
                                  previous-in-package-form))
             (package (find-package package-designator)))
        (when (null package)
          package-designator)))))



(defmacro let+ctx (bindings &body body)
  "Helper macro, to ease the access to the current commands' context."
  `(let*
       (,@ (loop :for binding :in bindings
                 :if (listp binding)
                   ;; acts like a regular "let*"
                   :collect binding
                 :else
                   :collect `(,binding (context-get (context*) ',binding))))
     ,@body))


#+ (or)
(let+ctx ((x 42)
          nodes position path))

(defparameter *qf* nil
  "Data from the latest quickfix invocation.
For debugging purposes ONLY.")

(defun sanitize-list-of-commands (commands)
  ;; Some methods returns lists, some just a symbol.
  ;; We flatten that to just a list of symbols.
  (setf commands
        (alexandria:flatten
         (copy-seq
          (alexandria:ensure-list commands))))

  ;; Fallback to suggesting _all_ commands.
  (unless commands
    (setf commands (copy-seq (list-all-commands))))

  ;; Deduplicate commands
  (setf commands (remove-duplicates commands))

  ;; Augment the commands with their descriptions.
  (setf commands (mapcar #'command-description commands))

  ;; Save some information for debugging
  (setf *qf* `((:context . ,(context*))
               (:commands . ,commands)))
  ;; Returns the list of applicable commands
  commands)

(defun shortcircuit (x)
  (throw 'shortcircuit x))

(defun suggest-defpackage ()
  "When the buffer is empty, or only contains comments and whitespaces."
  (let+ctx (nodes)
    (cond
      ((null nodes) 'insert-defpackage)
      ((and nodes
            (nodes-emptyp nodes))
       ;; TODO Add a configuration to decide whether to shortcircuit or
       ;; not. Because suggesting to insert a "defpackage" form when in
       ;; an empty file is pretty much just my personal preference.
       #++
       (shortcircuit 'insert-defpackage)
       'insert-defpackage))))

(defun suggest-system-definition ()
  "When in an .asd file"
  (when (ends-with-subseq ".asd" (buffer-name)
                          :test #'string-equal)
    'insert-asdf))

(defun suggest-lambda ()
  "When inside a higher-order function, like mapcar."
  (let+ctx (inner-node)
    ;; TODO Use breeze.cl:higher-order-function-p
    ;; TODO Must extract the symbol from the parent-node first
    ;; Higher-order functions
    (when (and inner-node
               (mapcar-form-p inner-node))
      (shortcircuit 'insert-lambda))))

(defun suggest-loop-clauses ()
  "When inside a loop form."
  (let+ctx (inner-node)
    (when (and inner-node
               (loop-form-p inner-node))
      (shortcircuit *commands-applicable-in-a-loop-form*))))

(defun suggest-defpackage-clauses ()
  "When inside a defpackage form."
  (let+ctx (inner-node)
    (when (and inner-node
               (or (defpackage-form-p inner-node)
                   (uiop/package--define-package-form-p inner-node)))
      (shortcircuit 'insert-local-nicknames))))

(defun suggest-other ()
  "Otherwise"
  (let+ctx (point outer-node)
    (append *commands-applicable-at-toplevel*
            *commands-applicable-inside-another-form-or-at-toplevel*
            *commands-applicable-in-a-loop-form*)
    #++(if
        ;; if "at top-level"
        (and outer-node
             (or
              ;; in-between forms
              (null outer-node)
              ;; just at the start or end of a form
              (= point (node-start outer-node))
              (= point (node-end outer-node))
              ;; inside a comment (or a form disabled by a
              ;; feature-expression)
              #++
              (typep outer-node
                     'breeze.reader:skipped-node)))
        *commands-applicable-at-toplevel*
        *commands-applicable-inside-another-form-or-at-toplevel*)))


(defun compute-suggestions ()
  "Given the current commands' context, suggests an appropriate list of
commands that the user might want to run."
  (alexandria:ensure-list
   (catch 'shortcircuit
     (mapcar #'funcall
             '(suggest-defpackage
               suggest-system-definition
               suggest-lambda
               suggest-loop-clauses
               suggest-defpackage-clauses
               suggest-other)))))



;; I made this command mostly for manually testing replace-region
(define-command delete-parent-form
    ()
  "Given the context, suggest some applicable commands."
  (augment-context-by-parsing-the-buffer (context*))
  (let+ctx (parent-node)
    ;; Save some information for debugging
    (setf *qf* `((:context . ,(context*))))
    (if (and parent-node
             (not (listp parent-node)))
        (destructuring-bind (from . to)
            (node-source parent-node) ;; TODO undefined function:
          (replace-region from to ""))
        (message "No parent node at point."))))

(defun check-in-package ()
  "Make sure the previous in-package form desginates a package that can
be found. If it's not the case (e.g. because the user forgot to define
a package and/or evaluate the form that defines the package) they show
a message and stop the current command."
  (let+ctx (nodes
            outer-node
            ;; Check if the closes defpackage was evaluated once
            (invalid-in-package
             (and nodes
                  outer-node
                  (validate-nearest-in-package nodes outer-node))))
    (when invalid-in-package
      (message "The nearest in-package form designates a package that doesn't exists: ~s"
               invalid-in-package)
      (return-from-command))))


(defun maybe-ask-to-load-system ()
  (if-let ((file-name (buffer-file-name)))
    (multiple-value-bind (status system)
        (breeze.asdf:loadedp (buffer-file-name))
      (when (eq :not-loaded status)
        (when (ask-y-or-n-p "The current file is part of the system \"~a\", but has not been loaded yet. Do you want to load it now? (y/n) "
                            (asdf:component-name system))
          (message "Loading system \"~a\"..." system)
          (asdf:load-system system)
          (message "System \"~a\" successfully loaded." system)
          (return-from-command))))))

(define-command quickfix ()
  "Given the context, suggest some applicable commands."
  (ignore-errors (maybe-ask-to-load-system))
  (augment-context-by-parsing-the-buffer (context*))
  (check-in-package)
  ;; TODO try to fix only the "current" block and/or iterate
  (multiple-value-bind (fixed fixed-anything-p)
      (breeze.analysis:fix :buffer-string (buffer-string)
                           :point-max (point-max))
    (if fixed-anything-p
        (replace-region (point-min) (point-max) fixed)
        (let* (;; Compute the applicable commands
               (commands (sanitize-list-of-commands (compute-suggestions)))
               ;; TODO What if there are no suggestions?
               ;; Ask the user to choose a command
               (choice (choose "Choose a command: "
                               (mapcar #'second commands)))
               (command-function (car (find choice commands
                                            :key #'second
                                            :test #'string=))))
          (if command-function
              (funcall command-function)
              (message "~s is not a valid choice" choice)))))
  #++
  (message "Failed to parse..."))


#+nil
(quickfix :buffer-string "   " :point 3)



#++ ;; TODO
(define-command move-to-tests ())

(define-command other-file ()
  "Find the alternative file for the current file."
  (message (buffer-file-name)))

;; 1. generate dirs from "vc-root" '("src" "t" "test" "tests")
;; 2. find in which directory is the current file
;; 3. find which alternative directory exists
;; 4. find which alternative file exists

;; On second thought, the "find test directory"/"find test files" should be part of the "workspace".

#++
(if-let (buffer-file-name))
#++
(if-let ((vc-root (indirect (find-version-control-root path))))
  (directory-name vc-root))
