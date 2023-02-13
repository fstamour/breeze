;; a.k.a. `code assists`, `code actions``

(in-package #:common-lisp-user)

(uiop:define-package #:breeze.refactor
    (:documentation "Snippets and refactoring commands")
  (:use #:cl #:breeze.command #:breeze.reader)
  (:import-from
   #:alexandria
   #:ends-with-subseq
   #:if-let
   #:symbolicate
   #:lastcar)
  (:import-from
   #:breeze.utils
   #:symbol-package-qualified-name
   #:before-last
   #:find-version-control-root)
  (:export
   #:command-description
   ;; Simple transformation commands
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
   #:insert-lambda
   ;; Other commands
   #:quickfix))

(in-package #:breeze.refactor)


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

(defun insert-defvar-shaped (form-name)
  "Start a command to insert a form that has the same shape as a
defvar."
  (insert "(~a " form-name)
  ;; TODO Check if name is surrounded by "*"
  (read-string-then-insert "Name: " "*~a* ")
  (read-string-then-insert "Initial value: " "~a~%")
  (read-string-then-insert "Documentation string " "\"~a\")"))

(define-command insert-defvar ()
  "Insert a defvar form."
  (insert-defvar-shaped "defvar"))

(define-command insert-defparameter ()
  "Insert a defparameter form."
  (insert-defvar-shaped "defparameter"))

(define-command insert-defconstant ()
  "Insert a defconstant form."
  (insert-defvar-shaped "defconstant"))

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
   "Enter the arguments: " "~a)~%)")
  (backward-char))

(define-command insert-defun ()
  "Insert a defun form."
  (insert-defun-shaped "defun"))

(define-command insert-defmacro ()
  "Insert a defmacro form."
  (insert-defun-shaped "defmacro"))

(defparameter *insert-defpackage/cl-user-prefix* nil
  "Whether to include (in-package #:cl-user) before a defpackage form.")

(defun directory-name (pathname)
  (lastcar (pathname-directory pathname)))

(defun infer-project-name (path)
  "Try to infer the name of the project from the PATH."
  ;; Infer project-name by location .git folder
  (when path
    (if-let ((vc-root (find-version-control-root path)))
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
           (infer-package-name-from-file buffer-file-name))))
    (when *insert-defpackage/cl-user-prefix*
      (insert
       "(cl:in-package #:cl-user)~%~%~"))
    (insert
     "(defpackage #:~a~
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
              "  :components
    (#+(or) (:file \"todo\")))"))))



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
  (read-string-then-insert
   "Name of the generic: "
   "(defgeneric ~a ()~
   ~%  (:documentation \"\")~
   ~%  (:method () ()))"))

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


(define-command insert-lambda ()
  "Insert a lambda form."
  (insert "#'(lambda ())"))

;; TODO quick-insert (format *debug-io* "~&")


;;;

;; TODO "Move the current form into the nearest parent \"let\" form."
;; TODO "Add or update defpackage to \"import-from\" the symbol at point."


;;; Quickfix

(defgeneric describe-command (command)
  (:documentation "Give a user-friendly description for a command.")
  (:method ((command symbol))
    (symbol-package-qualified-name command)))

(defun command-description (function)
  "Return a list of 2 elements: (FUNCTION its-docstring)"
  (let ((doc (documentation function 'function)))
    (unless doc
      (error
       "Function ~s does not have a documentation string.~
                  Is it defined?"
       function))
    (list function doc)))

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
    insert-print-unreadable-object-boilerplate
    insert-breeze-define-command))

(defparameter *commands-applicable-in-a-loop-form*
  '(insert-loop-clause-for-in-list
    insert-loop-clause-for-on-list
    insert-loop-clause-for-hash))

;; That's some Java-level variable name
(defparameter *commands-applicable-inside-another-form-or-at-toplevel*
  '(insert-handler-bind-form
    insert-handler-case-form
    insert-lambda))


(defparameter *qf* nil
  "Data from the latest quickfix invocation.
For debugging purposes ONLY.")

#++
(let* ((context (cdr (assoc :context *qf*)))
       (inner-node (gethash 'BREEZE.COMMAND::INNER-NODE context))
       (nodes (gethash 'BREEZE.COMMAND::NODEs context)))
  ;; context
  ;; (mapcar-form-p inner-node)
  (values nodes
          (nodes-emptyp nodes))
  ;; (node-symbol= 'uiop:define-package inner-node)
  )

#+ (or)
(let* ((*standard-output* *debug-io*)
       (nodes )
       (path )
       (outer-node )
       (parent-node )
       (inner-node ))
  (loop :for (node . index) :in path
        :for i :from 0
        :do (format t "~%=== Path part #~d, index ~d ===~%~s"
                    i index node))
  (format t "~%innore-node source: ~d-~d"
          (node-start inner-node)
          (node-end inner-node))
  (format t "~%unparsed inner-node: ~s"
          (breeze.reader:unparse-to-string inner-node))
  (format t "~%nearest in-package: ~a" (find-nearest-in-package-form nodes outer-node))
  (format t "~%parent node: ~a" parent-node))

;; (cdr (assoc :context *qf*))


(defun validate-nearest-in-package (nodes outer-node)
  (let* ((previous-in-package-form
           (find-nearest-sibling-in-package-form nodes outer-node)))
    (when previous-in-package-form
      (let* ((package-designator (in-package-node-package
                                  previous-in-package-form))
             (package (find-package package-designator)))
        (when (null package)
          package-designator)))))

(defmacro let+ctx (bindings &body body)
  `(let*
       (,@ (loop :for binding :in bindings
                 :if (listp binding)
                   :collect binding
                 :else
                   :collect `(,binding (context-get (command-context*) ',binding))))
     ,@body))


#+ (or)
(let+ctx ((x 42)
          nodes position path))

(defun compute-suggestions (&aux commands)
  "Compute the list of applicable commands given the current context."
  (let+ctx (nodes
            point
            path
            outer-node
            inner-node
            inner-node-index
            parent-node)
    (declare (ignorable path inner-node-index parent-node))
    (labels ((append-commands (cmds)
               (setf commands (append cmds commands)))
             (push-command (fn)
               (push fn commands))
             (push-command* (&rest fns)
               (mapcar #'push-command fns)))
      (cond
        ((ends-with-subseq ".asd" (context-buffer-name*)
                           :test #'string-equal)
         (push-command 'insert-asdf))
        ;; When the buffer is empty, or only contains comments and
        ;; whitespaces.
        ((nodes-emptyp nodes)
         (push-command* 'insert-defpackage))
        ;; TODO Use breeze.cl:higher-order-function-p
        ;; TODO Must extract the symbol from the parent-node first
        ;; Higher-order functions
        ((and (mapcar-form-p inner-node)
              ;; TODO Find the position inside the form
              )
         (push-command 'insert-lambda))
        ((or
          ;; in-between forms
          (null outer-node)
          ;; just at the start or end of a form
          (= point (node-start outer-node))
          (= point (node-end outer-node))
          ;; inside a comment (or a form disabled by a
          ;; feature-expression)
          (typep outer-node
                 'breeze.reader:skipped-node))
         (append-commands *commands-applicable-at-toplevel*)
         (append-commands
          *commands-applicable-inside-another-form-or-at-toplevel*))
        ;; Loop form
        ((or
          ;; (loop-form-p parent-node)
          (loop-form-p inner-node))
         (append-commands *commands-applicable-in-a-loop-form*))
        ;; Defpackage form
        ((or (defpackage-form-p inner-node)
             (uiop/package--define-package-form-p inner-node))
         (push-command 'insert-local-nicknames))
        (t
         (append-commands
          *commands-applicable-inside-another-form-or-at-toplevel*)))))

  ;; Deduplicate commands
  (setf commands (remove-duplicates commands))

  ;; Augment the commands with their descriptions.
  (setf commands (mapcar #'command-description commands))

  ;; Save some information for debugging
  (setf *qf* `((:context . ,(command-context*))
               (:commands . ,commands)))

  ;; Returns the list of applicable commands
  commands)


;; I made this command mostly for manually testing replace-region
(define-command delete-parent-form
    ()
  "Given the context, suggest some applicable commands."
  (augment-context-by-parsing-the-buffer (command-context*))
  (let+ctx (parent-node)
    ;; Save some information for debugging
    (setf *qf* `((:context . ,(command-context*))))
    (if (and parent-node
             (not (listp parent-node)))
        (destructuring-bind (from . to)
            (node-source parent-node)
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
            (invalid-in-package (validate-nearest-in-package nodes outer-node)))
    (when invalid-in-package
      (message "The nearest in-package form designates a package that doesn't exists: ~s"        invalid-in-package)
      (return-from-command))))

(define-command quickfix ()
  "Given the context, suggest some applicable commands."
  #++ (message "swank::*emacs-connection* ~s
swank::*send-counter* ~s" swank::*emacs-connection*
swank::*send-counter*)
  (multiple-value-bind (status system)
      (breeze.asdf:loadedp (context-buffer-file-name*))
    (when (eq :not-loaded status)
      (when (ask-y-or-n-p "The current file is part of the system \"~a\", but has not been loaded yet. Do you want to load it now? (y/n) "
                          (asdf:component-name system))
        (message "Loading system \"~a\"..." system)
        (asdf:load-system system))
      (message "System \"~a\" successfully loaded." system)
      ;; TODO Remove
      (return-from-command)))
  (if (augment-context-by-parsing-the-buffer (command-context*))
      (progn
        (check-in-package)
        (let* (;; Compute the applicable commands
               (commands (compute-suggestions))
               ;; TODO What if there are no suggestions?
               ;; Ask the user to choose a command
               (choice (choose "Choose a command: "
                               (mapcar #'second commands)))
               (command-function (car (find choice commands
                                            :key #'second
                                            :test #'string=))))
          (if command-function
              (funcall command-function)
              (message "~s is not a valid choice" choice))))
      (message "Failed to parse...")))


#+nil
(quickfix :buffer-string "   " :point 3)
