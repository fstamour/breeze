(cl:in-package #:cl-user)

(in-package #:breeze.refactor)

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
  (insert-saving-excursion ")")
  ;; TODO check that NAME is a valid symbol
  (read-string-then-insert
   "Name: " "~a "
   (lambda (name)
     (ensure-circumfix circumfix name)))
  ;; TODO check that it's a valid form
  ;;
  ;; TODO it's valid to define a variable without an initial value,
  ;; in that case, the docstring must be set using a "(setf
  ;; (documentation ..."
  (read-string-then-insert
   "Initial value: " "~a~%")
  (read-string-then-insert
   "Documentation string " "~a" #'normalize-docstring))

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

(defun insert-defun-shaped (form-name
                            &key
                              (name-callback #'identity)
                              (arguments-callback #'identity)
                              body)
  "Start a command to insert a form that has the same shape as a
defun."
  (insert "(~a " form-name)
  (insert-saving-excursion ")")
  (read-string-then-insert "Name: " "~a " name-callback)
  (read-string-then-insert
   ;; Who needs to loop...?
   "Enter the arguments: " "~a~%  "
   (alexandria:compose #'normalize-lambda-list arguments-callback))
  (read-string-then-insert
   "Documentation string: "
   "  ~a~%"
   #'normalize-docstring)
  (when body
    ;; TODO maybe a plain "insert" function would be useful
    (insert "~a" body)))

(define-command insert-defun ()
  "Insert a defun form."
  (declare (context :top-level)
           #| TODO if current token at top-level and is "defun", just choose this command directly |#)
  (insert-defun-shaped "defun"))

(define-command insert-setf-defun ()
  "Insert a setf function form e.g. ~(defun (setf ...) ...)~"
  (declare (context :top-level))
  (insert-defun-shaped
   "defun"
   :name-callback
   (lambda (name)
     (ensure-circumfixes '("(" "setf" " ") name ")"))
   :arguments-callback
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

;; TODO rename to "insert-asdf-defsystem"
;; TODO ask whether to insert `:pathname`, `:serial`, `:perform test-op`
;;
(define-command insert-asdf ()
  "Insert an asdf system definition form."
  (declare (context :top-level
                    :extension ".asd"))
  (let ((system-name (read-string "Name of the system: "
                                  #| TODO infer-system-name |#))
        (author (read-string "Author: "
                             #| TODO infer-autor-name |#
                             :initial-input breeze.config:*default-author*
                             :history 'author))
        ;; TODO Add default license from config
        (licence (read-string "Licence name: "
                              #| TODO infer-licence |#
                              :history 'licence)))
    (insert "(asdf:defsystem #:~a~%~{  ~a~%~}"
            system-name
            `(;; TODO use normalize-docstring
              ":description \"\""
              ":version \"0.0.1\""
              ,(format nil ":author \"~a\"" author)
              ,(format nil ":licence \"~a\"" licence)
              ":depends-on ()"
              ;; TODO do insert this if there is a directory "src"
              ;; TODO ask the user if they want to create one
              ";; :pathname \"src\""
              ";; :serial t"
              ":components"
              ;; TODO suggest to add files (e.g. look in the current directory)
              ;; - list the file in the root of the project
              ;;   - infer where is the root of the project if there
              ;;     is no VCS markers nor asdf files
              ;;   - is there a readme
              ;;   - if the parent directory is a well-known directory
              ;;     (like quicklisp's local-projects directory or the
              ;;     user's home directory), then the directory might
              ;;     be the root
              ;;   - (harder?) if a _sibling_ directory is
              ;;     "definitely" the root of a project, then the
              ;;     parent directory is _not_ a root, but the current
              ;;     directory might be one
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


(define-command insert-class-slot ()
  "Insert a defclass slot form."
  ;; TODO there's no "cl:defclass "context" just yet, so I've put
  ;; :expression for now.
  ;; (declare (context cl:defclass :slot-specifier))
  (declare (context :expression))
  (let ((name (read-string "Name of the slot: "
                           :history 'slot)))
    (when (and name (plusp (length name)))
      ;; TODO insert "~% " if necessary (need to make sure the buffer
      ;; is in sync!)
      (insert
       "~%(~a~
   ~%    :initform nil~
   ~%    :initarg :~@*~a~
   ~%    :accessor ~@*~a~
   ~%    :documentation \"\")"
       name))))

(define-command insert-defclass ()
  "Insert a defclass form."
  (declare (context :top-level))
  (insert "(defclass")
  (insert-saving-excursion ")")
  (read-string-then-insert
   "Name of the class: "
   " ~a ()~%  (")
  (insert-saving-excursion ")")
  (loop :while (insert-class-slot :recursive-p t))
  ;; go forward one char, to get out of the list of slots
  (let ((point (current-point)))
    (goto-char (1+ point)))
  (insert-saving-excursion "\")")
  (insert "~%  (:documentation \""))

(define-command insert-defgeneric ()
  "Insert a defgeneric form."
  (declare (context :top-level))
  (let ((name (read-string "Name of the generic function: ")))
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
  (ensure-space)
  (insert "(lambda (")
  (insert-saving-excursion "))"))

(define-command insert-decoded-time-multiple-value-bind ()
  "Insert a cl:multiple-value-bind form to bind the output of cl:get-decoded-time"
  (declare (context :expression))
  (insert "(multiple-value-bind (second minute hour date month year day daylight-p zone)
      (get-decoded-time))"))

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

;; TODO insert-rowsell-shebang
;; TODO insert-fancy-ecl-shebang
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
