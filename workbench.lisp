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



;; This is the default
(log:config :info)

(log:config :debug)
(log:config '(breeze command) :debug)
(log:config '(breeze command) :error)

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
