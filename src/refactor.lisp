;; a.k.a. `code assists`, `code actions``

(in-package #:common-lisp-user)

(uiop:define-package #:breeze.refactor
    (:documentation "Snippets and refactoring commands")
  (:use #:cl #:breeze.command #:breeze.analysis)
  (:import-from #:alexandria
                #:ends-with-subseq
                #:if-let
                #:when-let
                #:symbolicate
                #:lastcar)
  (:import-from #:breeze.string
                #:symbol-package-qualified-name
                #:symbol-starts-with
                #:trim-whitespace
                #:ensure-circumfix
                #:ensure-circumfixes)
  (:import-from #:breeze.utils
                #:before-last)
  (:import-from #:breeze.indirection
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
   #:insert-setf-defun
   #:insert-defmacro
   #:insert-defpackage
   #:insert-in-package-cl-user
   #:insert-asdf
   #:insert-defclass
   #:insert-class-slot
   #:insert-defgeneric
   #:insert-defmethod
   #:insert-print-unreadable-object-boilerplate
   #:insert-make-load-form-boilerplate
   #:insert-lambda
   ;; Other commands
   #:quickfix
   #| WIP commands |#
   #++ #:other-file
   #:completions-at-point))

(in-package #:breeze.refactor)

#++
(define-node-form-predicates (uiop:define-package))

(defun normalize-docstring (string)
  "Try to normalize docstring entered by the user into something that can
be inserted correctly into a buffer."
  (ensure-circumfix "\"" (trim-whitespace string)))

(defun normalize-lambda-list (string)
  "Try to normalize lambda-list entered by the user into something that can
be inserted correctly into a buffer."
  (ensure-circumfix "(" (trim-whitespace string) ")"))


;;; Insertion commands

;; Dogfood'ing to the max!
(define-command insert-breeze-define-command ()
  "Insert a breeze:define-command form."
  (declare (context :top-level))
  (let ((name (read-string "Name of the command (symbol): ")))
    (insert
     "(define-command ~a ()~
    ~%  \"~@(~a~).\"~
    ~%  )"
     name
     (substitute #\Space #\- name))))

#++ ;; TODO snippet draft:
`(define-command (:the symbol ?name) () \n
   (fmt "\"~@(~a~).\"" ?name))

(define-command insert-handler-case-form ()
  "Insert handler case form."
  (declare (context :expression))
  (insert
   "(handler-case~
  ~%  (frobnicate)~
  ~%  (error (condition)~
  ~%    (describe condition *debug-io*)))"))

(define-command insert-handler-bind-form ()
  "Insert handler bind form."
  (declare (context :expression))
  (insert
   "(handler-bind~
  ~%  ((error (lambda (condition)~
  ~%    (describe condition *debug-io*))))~
  ~%  (frobnicate))"))

(define-command insert-loop-clause-for-on-list ()
  "Insert a loop clause to iterate on a list."
  (declare (context cl:loop))
  (insert " :for ")
  (read-string-then-insert
   "Enter the variable name for the iterator: " "~a :on ")
  (read-string-then-insert
   "Enter the the list to iterate on: " "~a"))

(define-command insert-loop-clause-for-in-list ()
  "Insert a loop clause to iterate in a list."
  (declare (context cl:loop))
  (insert " :for ")
  (read-string-then-insert
   "Enter the variable name for the iterator: " "~a :in ")
  (read-string-then-insert
   "Enter the the list to iterate on: " "~a"))

(define-command insert-loop-clause-for-hash ()
  "Insert a loop clause to iterate on a hash-table."
  (declare (context cl:loop))
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
  (read-string-then-insert "Name: " "~a "
                           (lambda (name)
                             (ensure-circumfix circumfix name)))
  (read-string-then-insert "Initial value: " "~a~%")
  (read-string-then-insert "Documentation string " "~a)"
                           #'normalize-docstring))

(define-command insert-defvar ()
  "Insert a defvar form."
  (declare (context :top-level))
  (insert-defvar-shaped "defvar" "*"))

(define-command insert-defparameter ()
  "Insert a defparameter form."
  (declare (context :top-level))
  (insert-defvar-shaped "defparameter" "*"))

(define-command insert-defconstant ()
  "Insert a defconstant form."
  (declare (context :top-level))
  (insert-defvar-shaped "defconstant" "+"))

;; TODO Add "alexandria" when the symbol is not interned
(define-command insert-define-constant ()
  "Insert a alexandria:define-constant form."
  (declare (context :top-level))
  (insert-defvar-shaped "define-constant"))

(defun insert-defun-shaped (form-name &optional
                                        (name-callback #'identity)
                                        (arguments-callback #'identity))
  "Start a command to insert a form that has the same shape as a
defun."
  (insert "(~a " form-name)
  (read-string-then-insert "Name: " "~a " name-callback)
  (read-string-then-insert
   ;; Who needs to loop...?
   "Enter the arguments: " "~a)~%"
   (alexandria:compose #'normalize-lambda-list arguments-callback))
  #++ ;; TODO
  (read-string-then-insert
   "Documentation string: " "  ~a)"
   #'normalize-docstring))

(define-command insert-defun ()
  "Insert a defun form."
  (declare (context :top-level))
  (insert-defun-shaped "defun"))

(define-command insert-setf-defun ()
  "Insert a setf function form e.g. (defun (setf ...) ...)"
  (declare (context :top-level))
  (insert-defun-shaped
   "defun"
   (lambda (name)
     (ensure-circumfixes '("(" "setf" " ") name ")"))
   (lambda (arguments)
     (if (string= "" arguments)
         "(new-value)"
         (ensure-circumfixes '("(" "new-value" " ")
                             (trim-whitespace arguments)
                             ")")))))

(define-command insert-defmacro ()
  "Insert a defmacro form."
  (declare (context :top-level))
  (insert-defun-shaped "defmacro"))

(define-command insert-defpackage ()
  "Insert a defpackage form."
  (declare (context :top-level))
  (let ((package-name
          (read-string
           "Name of the package: "
           (infer-package-name-from-file (current-buffer-filename)))))
    (when (in-package-cl-user-p)
      (insert
       "(cl:in-package #:cl-user)~%~%"))
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
  (declare (context (:child-of :package-definition))) ; TODO
  (insert
   "(:local-nicknames ~{~a~^~%~})"
   (loop :for name = (read-string "Name of the package to alias: ")
         :while (plusp (length name))
         :for alias = (read-string "Alias of the package: ")
         :collect (format nil "(#:~a #:~a)" alias name))))

(define-command insert-in-package-cl-user ()
  "Insert (cl:in-package #:cl-user)"
  (declare (context :top-level))
  (insert "(cl:in-package #:cl-user)"))

;; TODO insert-let (need to loop probably)

(define-command insert-asdf ()
  "Insert an asdf system definition form."
  (declare (context :top-level
                    :extension ".asd"))
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



;; TODO How could I re-use an hypothetical "add-slot" command?
(define-command insert-defclass ()
  "Insert a defclass form."
  (declare (context :top-level))
  (read-string-then-insert
   "Name of the class: "
   "(defclass ~a ()~
   ~%  ((slot~
   ~%    :initform nil~
   ~%    :initarg :slot~
   ~%    :accessor ~@*~a-slot))~
   ~%  (:documentation \"\"))"))

(define-command insert-class-slot ()
  "Insert a defclass slot form."
  (declare (context cl:defclass :slot-specifier))
  (read-string-then-insert
   "Name of the slot: "
   "~%  (~a~
   ~%    :initform nil~
   ~%    :initarg :~@*~a~
   ~%    :accessor ~@*~a~
   ~%    :documentation \"\")"))

(define-command insert-defgeneric ()
  "Insert a defgeneric form."
  (declare (context :top-level))
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
  (declare (context :top-level))
  (let ((name (read-string
               "Name of the method: ")))
    (insert
     "(defmethod ~a ()~
    ~%  )"
     name)))

(define-command insert-print-unreadable-object-boilerplate ()
  "Insert a print-object method form."
  (declare (context :top-level))
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
  (declare (context :top-level))
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
  (declare (context :expression))
  (insert "(lambda ())"))

;; TODO quick-insert (format *debug-io* "~&")

;; TODO move into a +parachute.lisp file
(define-command insert-parachute-define-test ()
  "Insert a parachute:define-test form"
  (declare (context :top-level))
  (insert "(define-test+run ")
  (read-string-then-insert "Name of the test: "
                           "~a)~%"))


;;;

;; TODO "Move the current form into the nearest parent \"let\" form."
;; TODO "Add or update defpackage to \"import-from\" the symbol at point."


;;; Quickfix

(defun commands-applicable-at-toplevel ()
  "Create a list of all the commands applicable in the TOP-LEVEL context."
  (remove-if-not (lambda (command)
                   (eq (get command 'context) :top-level))
                 (list-all-commands)))

(defun commands-applicable-in-a-loop-form ()
  "Create a list of all the commands applicable directly under a \"loop\" clause."
  (remove-if-not (lambda (command)
                   (eq (get command 'context) 'cl:loop))
                 (list-all-commands)))

;; That's some Java-level variable name
(defun commands-applicable-in-expression-context ()
  "Create a list of all the commands applicable where an expression would be expected.

TODO maybe find a better nomenclature?"
  (remove-if-not (lambda (command)
                   (eq (get command 'context) :expression))
                 (list-all-commands)))



;; TODO use a node-iterator instead
;; TODO this is _very_ similar to breeze.analysis::warn-undefined-in-package
;; TODO see breeze.analysis::check-in-package...
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

(defun suggest-package-definition ()
  "When the buffer is empty, or only contains comments and whitespaces."
  (let* ((buffer (current-buffer))
         (node-iterator (node-iterator buffer)))
    (when (and node-iterator
               (every #'whitespace-or-comment-node-p (root-vector node-iterator)))
     ;; TODO Add a configuration to decide whether to shortcircuit or
     ;; not. Because suggesting to insert a "defpackage" form when in
     ;; an empty file is pretty much just my personal preference.
     #++
     (shortcircuit 'insert-defpackage)
     'insert-defpackage)))

(defun suggest-system-definition ()
  "When in an .asd file"
  (when (ends-with-subseq ".asd" (current-buffer-name)
                          :test #'string-equal)
    'insert-asdf))

(defun suggest-lambda ()
  "When inside a higher-order function, like mapcar."
  (let* ((buffer (current-buffer))
         (node-iterator (node-iterator buffer)))
    ;; TODO Use breeze.cl:higher-order-function-p
    ;; Higher-order functions
    (when (and node-iterator
               (child-of-mapcar-node-p node-iterator))
      (shortcircuit 'insert-lambda))))

(defun suggest-loop-clauses ()
  "When inside a loop form."
  nil #++  ;; TODO
  (when (and inner-node
             (loop-form-p inner-node))
    (shortcircuit (commands-applicable-in-a-loop-form))))

(defun suggest-package-definition-clauses ()
  "When inside a package definition form."
  nil #++  ;; TODO
  (let+ctx (inner-node)
    (when (and inner-node
               (or (defpackage-form-p inner-node)
                   (uiop/package--define-package-form-p inner-node)))
      (shortcircuit 'insert-local-nicknames))))

(defun suggest-other ()
  "Otherwise"
  ;; TODO
  (list-all-commands)
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
      (commands-applicable-at-toplevel)
      (commands-applicable-in-expression-context)))


(defun compute-suggestions ()
  "Given the current commands' context, suggests an appropriate list of
commands that the user might want to run."
  (alexandria:ensure-list
   (catch 'shortcircuit
     (mapcar #'funcall
             '(suggest-package-definition
               suggest-system-definition
               suggest-lambda
               suggest-loop-clauses
               suggest-package-definition-clauses
               suggest-other)))))



(defun maybe-ask-to-load-system ()
  (when-let ((filename (current-buffer-filename)))
    (multiple-value-bind (status system)
        (breeze.asdf:loadedp filename)
      (when (eq :not-loaded status)
        (when (ask-y-or-n-p "The current file is part of the system \"~a\", but has not been loaded yet. Do you want to load it now? (y/n) "
                            (asdf:component-name system))
          (message "Loading system \"~a\"..." system)
          (asdf:load-system system)
          (message "System \"~a\" successfully loaded." system)
          (return-from-command))))))

(define-command quickfix ()
  "Given the context, suggest some applicable commands."
  (maybe-ask-to-load-system)
  #++ (check-in-package)
  ;; TODO this currently only fix the first issue it finds
  ;; TODO cache the linter issues (in the *workspace*'s buffer)
  ;; TODO try to fix only the "current" block and/or iterate
  (let ((fixes (breeze.analysis:fix-buffer (current-buffer))))
    (if fixes
        (let* ((fix (first fixes))
               (node (value (breeze.analysis::target-node fix)))
               (replacement (breeze.analysis::replacement fix)))
          (replace-region (start node) (end node)
                          (or replacement "")))
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
              (message "~s is not a valid choice" choice))))))

#|

TODO there's some different kind of "quickfixes":

- quickfixes are "obviously" the right thing to do, you don't have to ask
- code actions are contextual commands
- contextual inserts (add a binding to a let)
- contextual changes (move an expression into a let, change let to let*)
  - contextual "inverts" (e.g. =x= → =(not x)=
- non-contextual snippets
- contextual delete (remove a binding in a let)
- other: if point is on a (trace ...), suggest to untrace something,
  to comment or remove the form, to replace trace by untrace, or add
  the equivalent untrace next to it.
- contextual move up/down, out/into (e.g. slots in defclass)

|#


#+nil
(quickfix :buffer-string "   " :point 3)


;;; Test files

#++ ;; TODO
(define-command move-to-tests ())

(define-command other-file ()
  "Find the alternative file for the current file."
  (message (buffer-file-name)))

#++
(when path
    (if-let ((vc-root (indirect (find-version-control-root path))))
      (mapcar)))

#++
(let ((vc-root (find-version-control-root (breeze.utils:breeze-relative-pathname "."))))
  (mapcar (lambda (directory)
            (merge-pathnames directory vc-root))
          '("" "src/" "source/" "sources/")))

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



;;; Completion-at-point



(define-command completions-at-point ()
  ""
  (let ((node-iterator (node-iterator)))
    (break)
    (return-value-from-command '("asfd" "qwer" "uiop"))))


;;; Emacs header-line

;; "this won't introduce any latency /s"
#++
(progn
  (define-command header-line ()
    "Compute a string to show in emacs' header-line."
    (return-value-from-command
     (or (handler-case
             (format nil "~a ~a" (current-point)
                     (let ((node-iterator (current-node-iterator)))
                       (if node-iterator
                           (let* ((state (breeze.lossless-reader:state node-iterator))
                                  (node (breeze.iterator:value node-iterator)))
                             (breeze.lossless-reader:node-content state node))
                           "NODE-ITERATOR is nil")))
           (error (condition) (apply #'format nil (simple-condition-format-control condition)
                                     (simple-condition-format-arguments condition))))
         "An error occured when calling breeze-header-line")))
  (export 'header-line))
