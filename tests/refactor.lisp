
(cl:in-package #:common-lisp-user)

(defpackage #:breeze.test.refactor
  (:use :cl #:breeze.refactor)
  ;; Importing non-exported symbols of the "package under test"
  (:import-from #:breeze.refactor
                #:augment-context-by-parsing-the-buffer)
  (:import-from #:breeze.command
                #:cancel-command
                #:continue-command
                #:context-plist-to-hash-table)
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
                #:is
                #:true
                #:false))

(in-package #:breeze.test.refactor)

(defparameter *directory* "./")

#++
(augment-context-by-parsing-the-buffer
 (context-plist-to-hash-table
  `(:buffer-string ,(string #\Newline)
    :position 1)))


;;; Test refactoring commands

(define-test "All commands must be exported"
  (let ((commands (remove-if #'breeze.xref:externalp (breeze.refactor::all-commands))))
    (false commands "The following commands are not exported:~%~{  - ~S~%~}" commands)))

(define-test "All commands must be tested"
  (let ((commands
          (set-difference
           (breeze.refactor::all-commands)
           (remove-if-not #'symbolp
                          (mapcar #'parachute:name (parachute:package-tests *package*)))
           :key #'symbol-name
           :test #'string=)))
    (false commands
           "The following commands don't have a corresponding test:~%~{  - ~S~%~}"
           commands)))

#++
(define-command insert-refactor-test ()
  "Insert a breeze:define-command form."
  (let ((name (read-string "Name of the command (symbol): ")))
    (insert
     (remove-indentation
      "(define-test ~(~a~)
         (is equal
             ;; TODO expected
             (drive-command #'~(~a~)
                            :inputs '()
                            :context '())))")
     name name)))

(defun drive-command (fn &key context inputs)
  "Execute a command FN, with the context CONTEXT and send it
INPUTS. Returns the execution trace as a pair of input/request.

N.B. \"Requests\" are what the command returns. \"inputs\" are answers to those requests"
  (let ((breeze.command::*current-command*))
    ;; Make sure it's ok to run a new command.
    (cancel-command "pre drive-command cleanup")
    (unwind-protect
         (loop
           ;; The first input is always nil
           :for input :in (cons nil inputs)
           ;; We start by calling the function (the command), this will
           ;; start a new thread that will either return "done" and
           ;; exit, or return another request and wait for more inputs.
           :for request = (apply fn context)
           ;; We call continue-command to send the input
             :then (if input
                       (funcall #'continue-command input)
                       (continue-command))
           ;; We collect the pair of input/request. This is practically
           ;; an execution trace, and we're going to assert things on
           ;; those traces.
           :collect (list input request)
           ;; Detect when the command is done.
           :while (and request
                       (not (string= "done" (car request)))))
      ;; Make sure we clean up correctly if there was an error.
      (cancel-command "post drive-command cleanup"))))

#++
(define-test insert-breeze-define-command
  (is equal
      (drive-command #'insert-handler-bind-form)))



(define-test insert-handler-bind-form
  (is equal
      '((nil
         ("insert" "(handler-bind
  ((error #'(lambda (condition)
    (describe condition *debug-io*))))
  (frobnicate))")))
      (drive-command #'insert-handler-bind-form)))

;; This reproduce a bug in chanl....
#++
(time
 (let ((old-zombie-threads (breeze.listener::find-threads-by-name :string-or-predicate "breeze command handler")))
   (loop :repeat 1000 :do (drive-command #'insert-handler-bind-form))
   (set-difference (breeze.listener::find-threads-by-name :string-or-predicate "breeze command handler") old-zombie-threads)
   ))

;; (untrace)

#++
(loop (drive-command #'insert-handler-bind-form))

#++
(progn
  (cancel-command)
  (prog1 (insert-handler-bind-form)
    (cancel-command)))


(define-test insert-handler-case-form
  (is equal
      '((nil
         ("insert" "(handler-case
  (frobnicate)
  (error (condition)
    (describe condition *debug-io*)))")))
      (drive-command #'insert-handler-case-form)))

(define-test loop-clause-for-on-list
  (is equal
      '((nil ("insert" " :for "))
        (nil ("read-string" "Enter the variable name for the iterator: " nil))
        ("el" ("insert" "el :on "))
        ("list" ("read-string" "Enter the the list to iterate on: " nil)))
      (drive-command #'insert-loop-clause-for-on-list
                     :inputs '(nil "el" "list"))))



;;; Testing quickfix... This is nice, but quickfix regroups pretty
;;; much everything, so it might be better to test smaller parts
;;; individually.

(defun test-quickfix (buffer-name pre post)
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
                   :inputs (list ""))))

(defun expect-suggestions (&rest expected-suggested-commands)
  `((nil ("choose" "Choose a command: " ,(mapcar (alexandria:compose #'second
                                                                     #'command-description)
                                                 expected-suggested-commands)))
    ("" ("message" "\"\" is not a valid choice"))))


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
