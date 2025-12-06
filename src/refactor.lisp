;; a.k.a. `code assists`, `code actions``

(in-package #:common-lisp-user)

(uiop:define-package #:breeze.refactor
    (:documentation "Snippets and refactoring commands")
  (:use #:cl #:breeze.command #:breeze.analysis)
  (:import-from #:alexandria
                #:ends-with-subseq
                #:if-let
                #:when-let
                #:when-let*
                #:symbolicate
                #:lastcar)
  (:import-from #:breeze.string
                #:trim-whitespace
                #:ensure-circumfix
                #:ensure-circumfixes)
  (:import-from #:breeze.utils
                #:before-last)
  (:import-from #:breeze.indirection
                #:indirect)
  (:import-from #:breeze.command-utils
                #:pulse-node
                #:current-node)
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
   #:insert-asdf
   #:insert-defclass
   #:insert-class-slot
   #:insert-defgeneric
   #:insert-defmethod
   #:insert-print-unreadable-object-boilerplate
   #:insert-make-load-form-boilerplate
   #:insert-initialize-instance~method
   #:insert-lambda
   #:insert-decoded-time-multiple-value-bind
   #:insert-make-array
   #:insert-fancy-emacs-propline
   #:insert-fancy-sbcl-shebang
   ;; Other commands
   #:declaim-inline
   #:quickinsert
   ;; TODO perhaps "quickfix" should go in "lint.lisp"
   #:quickfix))

(in-package #:breeze.refactor)

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
  (declare (context :top-level)
           #| TODO if current token is top-level and is "defun", just choose this command directly |#)
  (insert-defun-shaped "defun"))

(define-command insert-setf-defun ()
  "Insert a setf function form e.g. ~(defun (setf ...) ...)~"
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

;; TODO insert-let (need to loop probably)

(define-command insert-asdf ()
  "Insert an asdf system definition form."
  (declare (context :top-level
                    :extension ".asd"))
  (let ((system-name (read-string "Name of the system: "
                                  #| TODO infer-system-name |#))
        (author (read-string "Author: "
                             #| TODO infer-autor-name |#
                             breeze.config:*default-author*))
        ;; TODO Add default license from config
        (licence (read-string "Licence name: "
                              #| TODO infer-licence |#)))
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
              ";; :serial t"
              ":components"
              "  (#+(or) (:file \"todo\"))"
              ";; in order to test this system, load the test system"
              ,(format nil ":in-order-to ((test-op (load-op ~a/test)))"
                       system-name)
              ";; this tells asdf what to execute to run the tests"
              ":perform"
              "(test-op (o c)"
              "         (uiop:symbol-call"
              ,(format nil "          '~a.test 'run-tests))"
                       system-name)
              ")"))
    ;; TODO maybe insert test-system
    ))



;; TODO How could I reuse an hypothetical "add-slot" command?
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
  ;; TODO there's no "cl:defclass "context" just yet, so I've put
  ;; :expression for now.
  ;; (declare (context cl:defclass :slot-specifier))
  (declare (context :expression))
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
        ;; TODO leave empty to NOT insert :slot-names
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

(define-command insert-initialize-instance~method ()
  "Insert a ~(defmethod initialize-instance ...)~ form."
  (declare (context :top-level))
  (read-string-then-insert
   "Name of the class: "
   "(defmethod initialize-instance ((~a ~:*~a) &rest initargs &key &allow-other-keys)~
   ~%  \"Initialize an instance of the class ~~~:*~a~~)\"~
   ~%  (apply #'shared-initialize ~:*~a t initargs))"))

(define-command insert-lambda ()
  "Insert a lambda form."
  (declare (context :expression))
  (insert "(lambda ())"))

(define-command insert-decoded-time-multiple-value-bind ()
  "Insert a cl:multiple-value-bind form to bind the output of cl:get-decoded-time"
  (declare (context :expression))
  (insert "(multiple-value-bind (second minute hour date month year day daylight-p zone)
      (get-decoded-time))"))


;;; Edits

;; TODO convert between line comment and block-comment


;;; TODO move into (new file) +quicklisp


(define-command insert-make-array ()
  "Insert a make-array form."
  (declare (context :expression))
  (let* ((current-node (node-iterator (current-buffer)))
         (source (source current-node))
         (indentation (or
                       (position #\newline source
                                 :end (current-point)
                                 :from-end t)
                       0)))
    (declare (ignorable indentation))
    (insert "(make-array '(0)
              :element-type <>
              :adjustable t
              :fill-pointer t)")))

(define-command insert-fancy-emacs-propline ()
  "Insert a fancy emacs propline."
  ;; TODO infer package-name from file content, if it fails, use
  ;; "read-string-then-insert" instead (infer the default from the
  ;; file name, just like "insert defpackage").
  (insert
   ";;;; -*- Mode: LISP; Syntax: ANSI-Common-lisp; Base: 10; Package: ~a -*-"
   "TODO"))

(define-command insert-fancy-sbcl-shebang ()
  "Insert fancy sbcl shebang at the start of the buffer."
  (declare (context :beginning-of-buffer))
  (goto-char 0)
  (insert
   "#!/usr/bin/env sh
#| -*- mode: lisp; syntax: common-lisp; -*- |#
#|
exec rlwrap --no-warning sbcl --noinform --script $0 --debugger
|#
"))

;; DSL:
#++
((sym "make-array" "cl")
 '(0)
 :element-type <>
 :adjustable t
 :fill-pointer t)


#|

This is a proof-of-concept on how to take account of the indentation
when inserting something.

|#
#++
(let* ((indentation 2)
       (input "(make-array '(0)
       :element-type <>
:adjustable t
              :fill-pointer t)")
       (stream *standard-output*)
       (parser-state (parse input))
       (indentation
         (+ indentation
            (let (($node (make-node-iterator parser-state)))
              (go-down $node)
              (next $node)
              (breeze.pattern::skip $node #'whitespace-or-comment-node-p)
              (start $node)))))
  (declare (ignorable indentation))
  (let (($node (make-node-iterator parser-state)))
    (format t "~&  ")
    (loop
      :until (donep $node)
      :for node := (value $node)
      :repeat 100
      :do
         ;; (format t "~&~s: " (node-type (value $node)))
         (cond
           ;; ((quotep ))
           ;; do nothing for nodes with children
           ((children node))
           ((and (whitespace-node-p node)
                 (position #\Newline
                           input
                           :start (start node)
                           :end (end node)))
            (let ((position (position #\Newline
                                      input
                                      :start (start node)
                                      :end (end node))))
              (unless (zerop position)
                (write-string input stream
                              :start (start node)
                              :end position))
              (write-char #\Newline stream)
              ;; Assuming there's only one newline
              (loop :repeat indentation
                    :do (write-char #\Space stream))))
           (t
            (format t (node-string $node))))
         ;; (next $node)
         (breeze.iterator::next-preorder*
          $node
          ;; This hook is called after `go-down' is called on $node.
          (lambda ()
            ;; write [parent-start to current start]
            (write-string
             input ;; could be (source $node)
             stream
             :start (start (parent-node $node))
             :end (start $node))
            ;; (write-char #\()
            )
          ;; This hook is called after `pop-subtree' is called. The
          ;; hook is called only once, even if `pop-subtree' was
          ;; called many times
          (lambda ()
            ;; node-end to current start
            (write-string
             input ;; could be (source $node)
             stream
             :start (end node)
             :end (unless (donep $node) (start $node))))))))


;;;

;; TODO "Move the current form into the nearest parent \"let\" form."
;; TODO "Add or update defpackage to \"import-from\" the symbol at point."


;;;

;; TODO it shouldn't be too hard to reuse this code to insert other
;; kind of `declaim's.
;;
;; TODO this only works when the point is at the start of the defun :/
;; otherwise, it inserts the declaim in weid places
(define-command declaim-inline ()
  "Declaim inline the current top-level function."
  (let* (($current-node (current-node :pulsep t))
         ;; TODO get the "top-level" node, not the root
         ($root (top-level-node-iterator $current-node)))
    (with-match ($root (defun ?name))
      (cond
        (?name
         (pulse-node ?name)
         (insert-at
          (start $root)
          "(declaim (inline ~a))~%" (node-string ?name)))
        (t
         (pulse-node $root)
         (message "Unable to find the current top-level function's name."))))))


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
               (every #'whitespace-or-comment-node-p (root-subtree node-iterator)))
     ;; TODO Add a configuration to decide whether to shortcircuit or
     ;; not. Because suggesting to insert a "defpackage" form when in
     ;; an empty file is pretty much just my personal preference.
     #++
     (shortcircuit 'breeze.package-commands:insert-defpackage)
     'breeze.package-commands:insert-defpackage)))

(defun suggest-system-definition ()
  "When in an .asd file"
  (when (ends-with-subseq ".asd" (current-buffer-name)
                          :test #'string-equal)
    'insert-asdf))

(defun suggest-lambda ()
  "When inside a higher-order function, like mapcar."
  nil
  #++ ;; TODO
  (when-let* (($node (current-node-iterator))
              ($parent (parent $node))
              ($car (when (parens-node-p parent)
                     (go-down (copy-iterator $parent))))
              (name (when (token-node-p $car)
                      (name $car))))
    ;; TODO Use breeze.cl:higher-order-function-p
    ;; TODO check the position of $node in its parent (skip whitespace and comments)
    ;; Higher-order functions
    (when (and $node
               (parens-node-p parent)
               ;; (child-of-mapcar-node-p node-iterator)
               )
      (shortcircuit 'insert-lambda))))

(defun suggest-loop-clauses ()
  "When inside a loop form."
  (let* ((buffer (current-buffer))
         (node-iterator (node-iterator buffer)))
    (when (breeze.analysis::loop-form-p (parent node-iterator))
      (shortcircuit (commands-applicable-in-a-loop-form)))))

(defun suggest-package-definition-clauses ()
  "When inside a package definition form."
  nil #++  ;; TODO
  (let+ctx (inner-node)
    (when (and inner-node
               (or (defpackage-form-p inner-node)
                   (uiop/package--define-package-form-p inner-node)))
      (shortcircuit 'breeze.package-commands:insert-local-nicknames))))

(defun suggest-other ()
  "Otherwise"
  (let* ((current-node-iterator (current-node-iterator))
         (current-node (value current-node-iterator))
         ;; TODO we actually want to know if we're in a "top-level"
         ;; form, not "at the root", because we want the commands that
         ;; are applicable at top-level to also be applicable in forms
         ;; that preserves top-levelness like `progn'.
         (root-node (root-node (current-node-iterator)))
         (rootp (eq current-node root-node)))
    (if rootp
        (commands-applicable-at-toplevel)
        (commands-applicable-in-expression-context))))


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



#++
;; TODO temporarily disabled due to a re-organization
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

#++
(defun maybe-create-system ()
  (let ((buffer (current-buffer)))
    (cwd)))

(defun maybe-apply-fixes ()
  "Fix the first \"applicable\" linter issue. If there is any
automatically fixable linter issue in the current top-level form, it
will fix the issue and then stop the current command."
  ;; TODO cache the linter issues (in the *workspace*'s buffer)
  ;;
  ;; TODO It could be nice to let the user know that "there are issues
  ;; in the current form", but that it "doesn't know how to fix
  ;; them". Because otherwise, the "quickfix" command will show a
  ;; message saying "there's nothing to fix", but the user will see
  ;; the blue/yellow/red squigly in their editors and wonder why they
  ;; didn't get fixed by quickfix. Currently the command "fix buffer"
  ;; only returns `simple-node-conditions' that have a `replacement`.
  (when-let* ((fixes (breeze.lint:fix-buffer (current-buffer)))
              (current-top-level-node (root-node (current-node-iterator)))
              (applicable-fixes
               (remove-if-not
                (lambda (fix)
                  ;; keep only fixes that are part of the current top-level form
                  (eq current-top-level-node
                      (root-node (breeze.lint:target-node fix))))
                fixes)))
    ;; Apply the fix
    (let* ((fix (first applicable-fixes))
           (node (value (breeze.lint:target-node fix)))
           (replacement (breeze.lint:replacement fix)))
      (replace-region (start node) (end node)
                      (or replacement ""))
      (return-from-command))))

;; TODO FIXME TODO FIXME This doesn't actually filter the commands that are not inserts...
;;
;; TODO I might want to use the name "blueprints" instead of snippets
;; because I want these to be usable for _updating_ code, not just
;; creating new code
(define-command quickinsert ()
  "Given the context, suggest applicable snippets."
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
        (message "~s is not a valid choice" choice))))

(define-command quickfix ()
  "Given the context, suggest some applicable commands."
  ;; TODO this is definitely not the right place...
  #++ (maybe-ask-to-load-system)
  ;; TODO Is it the right place? maybe do that in the header-line??
  #++ (check-in-package)
  (let ((root (root-node (current-node-iterator))))
    (pulse (start root) (end root))
    (maybe-apply-fixes))
  (message "Nothing to fix in the current top-level form."))

#|

TODO there's some different kind of "quickfixes":

- quickfixes are "obviously" the right thing to do, you don't have to ask
- code actions are contextual commands
- contextual inserts (add a binding to a let)
- contextual changes (move an expression into a let, change let to let*)
  - contextual "inverts" (e.g. ~x~ → ~(not x)~)
  - or "alternatives" (e.g. ~'(a b c)~ → ~#(a b c)~)
- non-contextual snippets
- contextual delete (remove a binding in a let)
- other: if point is on a (trace ...), suggest to untrace something,
  to comment or remove the form, to replace trace by untrace, or add
  the equivalent untrace next to it.
- contextual move up/down, out/into (e.g. slots in defclass)

|#


#+nil
(quickfix :buffer-string "   " :point 3)
