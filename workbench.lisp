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

;; Find all of slime's "worker" thread, including this one
(find-threads-by-name "worker" :exclude-self-p nil)

;; Kill all Slime's worker threads
(kill-worker-threads)

;; Find the number of currently running breeze commands
(length (find-threads-by-name "breeze command handler"))

;; Kill all currently running breeze-commands
(kill-threads-by-name "breeze command handler")


(defvar *default-trace-report-default* sb-debug:*trace-report-default*)

;; tracing is very very useful for debugging, but the default way sbcl
;; often prints "way too much" stuff
(defun trace-report (depth function event stack-frame values)
  ;; (pprint-logical-block stream values :prefix ... :suffix ...)
  (let ((*print-pretty* nil)
        (stream *standard-output*))
    (terpri stream)
    (pprint-logical-block (stream values
                                  :per-line-prefix (format nil "~v@{~A~:*~}" depth "  |"))
      ;; (loop :repeat depth :do (format stream "  |"))
      (pprint-indent :current depth stream)
      (case event
        (:enter
         (format stream "~3d (~a ~{~a~^ ~})" depth function values))
        (:exit
         (format stream "~3d => ~{~a~^, ~}" depth values))
        (t
         (format stream "~3d ~s (~a ~{~a~^ ~})" depth event function values))))))

(setf sb-debug:*trace-report-default* 'trace-report)



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
  :for filename = (context-buffer-file-name (context actor))
  :when (alexandria:ends-with-subseq "breeze/kite/kite.lisp" filename)
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

(let* ((context (cdr (assoc :context *qf*)))
       (inner-node (gethash 'BREEZE.COMMAND::INNER-NODE context))
       (nodes (gethash 'BREEZE.COMMAND::NODEs context)))
  ;; context
  ;; (mapcar-form-p inner-node)
  (values nodes
          (nodes-emptyp nodes))
  ;; (node-symbol= 'uiop:define-package inner-node)
  )

(let* ((*standard-output* *debug-io*)
       (nodes)
       (path)
       (outer-node)
       (parent-node)
       (inner-node))
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

(cdr (assoc :context *qf*))


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

;; #+ (or) (sb-profile:report)



;;; lossless-reader.lisp

(in-package #:breeze.lossless-reader)

(trace
 read-string*
 read-char*
 read-while)

(trace
 read-whitespaces
 read-block-comment
 read-line-comment
 read-sharpsign-dispatching-reader-macro
 read-punctuation
 ;; read-quoted-string
 read-string
 read-token
 read-parens
 read-extraneous-closing-parens
 read-any
 parse)

(untrace)




(in-package #:breeze.pattern)

(trace iterator-next
       iterator-maybe-push
       iterator-maybe-pop)

(trace merge-bindings)

(trace match)

(untrace)


(in-package #:breeze.test.pattern)

(test-match '(:zero-or-more a b) '(a b a b))

(trace compile-pattern)

(trace match :methods t)

(trace :wherein test-match-ref
       ;; match
       merge-bindings)


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
 breeze.analysis::match-node)

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


