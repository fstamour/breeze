(in-package #:common-lisp-user)

(uiop:define-package #:breeze.refactor
  (:documentation "Snippets and refactoring commands")
  (:use #:cl #:breeze.command)
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
  (:import-from
   #:breeze.reader
   #:parse-string
   ;; Accessors
   #:node-source
   #:node-start
   #:node-end
   #:node-content
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
   #:function-node-p
   ;; Utility to create more predicates
   #:define-node-form-predicates
   )
  (:export
   ;; Simple commands
   #:insert-loop-clause-for-on-list
   #:insert-loop-clause-for-in-list
   #:insert-loop-clause-for-hash
   #:insert-handler-bind-form
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
               "Name of the object (paramater name of the method): "))
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
    insert-lambda))


(defparameter *qf* nil
  "Data from the latest quickfix invocation.
For debugging purposes ONLY.")

(let ((inner-node (assoc :inner-node *qf*)))
  inner-node
  ;; (node-symbol= 'uiop:define-package inner-node)
  )

#+ (or)
(let* ((*standard-output* *debug-io*)
       (pos (1- (getf *qf* :point)))
       (nodes (getf *qf* :nodes))
       (path (find-path-to-node pos nodes))
       (outer-node (caar path))
       (parent-node (car (before-last path)))
       (inner-node (car (lastcar path))))
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

#+(or) (in-package-form-p
        (car (getf *qf* :nodes)))

(defun augment-context-by-parsing-the-buffer (context)
  (let* ((buffer-string (context-buffer-string context))
         ;; Emacs's point starts at 1
         (position (1- (context-point context)))
         ;; Parse the buffer
         (nodes (parse-string buffer-string))
         (path (find-path-to-node position nodes))
         ;; Find the top-level form "at point"
         (outer-node (caar path))
         ;; Find the innermost form "at point"
         (inner-node (car (lastcar path)))
         (inner-node-index (cdr (lastcar path)))
         ;; Find the innermost form's parent
         (parent-node (car (before-last path))))
    #. `(progn ,@(loop :for key in '(position nodes path outer-node
                                     inner-node inner-node-index parent-node)
                       :collect
                       `(context-set context ',key ,key)))))



(defun validate-nearest-in-package (nodes outer-node)
  (let* ((previous-in-package-form
           (find-nearest-sibling-in-package-form nodes outer-node)))
    (when previous-in-package-form
      (let* ((package-designator (in-package-node-package
                                  previous-in-package-form))
             (package (find-package package-designator)))
        (when (null package)
          package-designator)))))

;; TODO
(defun compute-suggestions ()
  "Compute the list of applicable commands given the current context."
  (augment-context-by-parsing-the-buffer context))

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

(define-command quickfix ()
  "Given the context, suggest some applicable commands."
  (augment-context-by-parsing-the-buffer (command-context*))
  (let+ctx (nodes
            position
            path
            outer-node
            inner-node
            inner-node-index
            parent-node
            ;; Check if the closes defpackage was evaluated once
            (invalid-in-package (validate-nearest-in-package nodes outer-node))
            ;; Accumulate a list of commands that make sense to run in
            ;; the current context
            (commands))
    (declare (ignorable inner-node inner-node-index parent-node))

    (labels ((append-commands (cmds)
               (setf commands (append cmds commands)))
             (push-command (fn)
               (push fn commands))
             (push-command* (&rest fns)
               (mapcar #'push-command fns)))
      (cond
        ;; When the previous in-package form desginate a package tha
        ;; cannot be found (e.g. the user forgot to define a package.
        (invalid-in-package
         (message "The nearest in-package form designates a package that doesn't exists: ~s"        invalid-in-package)
         (return))
        ((ends-with-subseq ".asd" buffer-file-name
                           :test #'string-equal)
         (push-command 'insert-asdf))
        ;; When the buffer is empty, or only contains comments and
        ;; whitespaces.
        ((nodes-emptyp nodes)
         (push-command* 'insert-defpackage))
        ((or
          ;; in-between forms
          (null outer-node)
          ;; just at the start or end of a form
          (= position (node-start outer-node))
          (= position (node-end outer-node))
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
        ;; TODO Use breeze.cl:higher-order-function-p
        ;; TODO Must extract the symbol from the parent-node first
        ;; Higher-order functions
        ((and (mapcar-form-p inner-node)
              ;; TODO Find the position inside the form
              )
         (push-command 'insert-lambda))
        (t
         (append-commands
          *commands-applicable-inside-another-form-or-at-toplevel*))))

    ;; Deduplicate commands
    (setf commands (remove-duplicates commands))

    ;; Augment the commands with their descriptions.
    (setf commands (mapcar #'command-description commands))

    ;; Save some information for debugging
    (setf *qf* `((:context . ,(command-context*))
                 (:commands . ,commands)))

    ;; Ask the user to choose a command

    (let* ((choice (choose "Choose a command: "
                           (mapcar #'second commands)))
           (command-function (car (find choice commands
                                        :key #'second
                                        :test #'string=))))
      (funcall command-function))))


#+nil
(quickfix :buffer-string "   " :point 3)
