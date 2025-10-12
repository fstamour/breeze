(defpackage #:breeze.workbench
  (:documentation "Don't load this, it's for interactive development of breeze.")
  (:use #:cl #:breeze.thread))

(in-package #:breeze.workbench)

;; List all threads
(find-threads nil nil)

;; List all threads except the current one
(find-threads nil)

;; Find all of slime's "worker" thread
(find-threads-by-name "worker")

(bt:interrupt-thread
 (first (find-threads-by-name "worker"))
 #'break)

;; Find all of slime's "worker" thread, including this one
(find-threads-by-name "worker" :exclude-self-p nil)

;; Kill all Slime's worker threads
(kill-worker-threads)

;; Find the number of currently running breeze commands
(length (find-threads-by-name "breeze command handler"))

;; Kill all currently running breeze-commands
(kill-threads-by-name "breeze command handler")



(setf (breeze.logging:log-level) :debug)
(setf (breeze.logging:log-level) :info)



(setf *break-on-signals* 'error)
(setf *break-on-signals* nil)


(in-package #:breeze.command)

;;; Tracing important functions in the "command" package.

(trace
 find-actor
 ;; start-command ; don't: too verbose
 call-with-cancel-command-on-error
 cancel-command
 continue-command
 donep)


(trace
 %recv
 %send

 insert
 read-string
 read-string-then-insert
 choose)

(trace
 chanl:send
 chanl:recv)

(untrace)

;;; Manually testing "actors"

(defparameter *id*
  (start-command 'breeze.test.command::test-insert nil '("Mark")))

(find-actor *id* :errorp t)

(recv-from (find-actor *id* :errorp t))

(donep
 (find-actor *actor-id-counter* :errorp t))

(recv-from
 (find-actor *actor-id-counter* :errorp t))

(outgoing-messages-p
 (find-actor *actor-id-counter* :errorp t))

(clear-actors)

;;; Inspecting a command's actor after the command is done

;; Find the latest command
(apply #'max (alexandria:hash-table-keys *actors*))

;; Find the ids of the commands that ran for/from a specific file
(loop
  :for actor-id :being :the :hash-key :of *actors* :using (hash-value actor)
  :for filename = (current-buffer-filename (context actor))
  :when (alexandria:ends-with-subseq "breeze/src/+parachute.lisp" filename)
    :collect actor-id)

(defparameter *a* (gethash 9 *actors*))
(context *a*)


;;; Prototyping the request thingy...

(request 'x)
;; => nil

(handler-bind
    ((request #'(lambda (request)
                  ;; (format t "~%request: ~s" request)
                  (if (eq (what request) 'x)
                      (answer 42)
                      (signal request)))))
  (mapcar (lambda (what)
            (multiple-value-list (request what)))
          '(x y)))
;; => ((42 T) (NIL))

(with-answers
    ())



;; refactor.lisp

(in-package #:breeze.refactor)

(list-all-commands)

(trace
 shortcircuit
 compute-suggestions
 suggest-defpackage
 suggest-system-definition
 suggest-lambda
 suggest-loop-clauses
 suggest-defpackage-clauses
 suggest-other)


;;; reader.lisp

(in-package #:breeze.reader)

(sb-profile:profile
 parse
 read-all-forms
 eclector.parse:read-from-string
 make-instance
 eclector.parse-result:make-expression-result
 eclector.parse-result:read-preserving-whitespace
 raw)

;; #++ (sb-profile:report)


;;; iterator.lisp

(in-package #:breeze.iterator)

(trace
 make-tree-iterator
 collect
 current-depth-done-p
 donep
 value
 next)


;;; parser.lisp

(in-package #:breeze.parser)

(trace
 read-char*
 read-while)

(trace
 read-whitespaces
 read-block-comment
 read-line-comment
 read-sharp-dispatching-reader-macro
 read-punctuation
 ;; read-quoted-string
 read-string
 read-token
 read-parens
 read-extraneous-closing-parens
 read-any
 parse)

(trace children
       :wherein breeze.test.parser::goto-position/all)

(trace value next donep
       :wherein breeze.test.parser::goto-position/all)

(trace goto-position
       :wherein breeze.test.parser::goto-position/all)

(trace breeze.test.parser::goto-position/all)

(untrace)


(sb-profile:profile
 goto-position
 children
 make-recursive-iterator
 breeze.iterator:collect
 breeze.iterator::current-depth-done-p
 donep
 value
 next)

(sb-profile:report)
(sb-profile:reset)
(sb-profile:unprofile)



(in-package #:breeze.pattern)

(trace merge-bindings)

;; (trace match) ;; don't! it's called too many times by flymake

(untrace)

(in-package #:breeze.test.pattern)

(test-match '(:zero-or-more a b) #(a b a b))

(trace compile-pattern)

(trace match :methods t)

(trace :wherein test-match-ref
       ;; match
       merge-bindings)

(trace
 :wherein test-match
 :methods t
 match)

(trace
 :wherein test-match*
 :methods t
 match)

(trace
 :wherein test-match*
 ;; :methods t
 match)

(in-package #:breeze.test.analysis)

(trace in-package-node-p
       :wherein test-in-package-node-p)

(trace :wherein test-lint
       in-package-node-p)

(trace match
       :wherein test-in-package-node-p)

(trace
 :wherein test-match-parse
 match
 breeze.analysis::match-symbol-to-token
 breeze.analysis::node-string-equal)

(trace
 :wherein breeze.analysis::match-symbol-to-token
 breeze.analysis::node-string-equal)

(trace match-symbol-to-token match
       breeze.pattern::match-symbol
       breeze.pattern::match-qualification
       breeze.pattern::match-package
       breeze.pattern::same-package-p
       breeze.pattern::same-symbol-p)

(trace :wherein test-either
       match-symbol-to-token
       match
       breeze.analysis::node-string-equal)

(untrace)


(trace lint)



(in-package #:breeze.listener)

(trace suggest-symbol
       suggest-package
       suggest-class)



(progn
  ;; List the slot of a condition
  (sb-kernel::condition-assigned-slots *condition*)

  ;; Get the first element of a condition's format arguments
  (car
   (slot-value *condition*
               'sb-kernel::format-arguments)))



(defparameter *condition* *last-condition*
  "Just a quick way to save the last-condition.")



#+ (or)
(type-of *condition*)
;; => SB-PCL::MISSING-SLOT



(prin t)
(commmon-lisp:print :oups)
(cl:prin :oups)
(call-with-correction-suggestion (lambda () (eval '(prin))))
(make-instance 'typos)




#|

TODO Would be nice to have a "Shadow-import all" restart.

|#


