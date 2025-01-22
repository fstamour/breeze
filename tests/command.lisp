(in-package #:common-lisp-user)

(uiop:define-package #:breeze.test.command
    (:documentation "Tests for breeze.command.")
  (:use :cl #:breeze.command)
  (:import-from #:alexandria
                #:assoc-value)
  ;; symbols not exported
  (:import-from #:breeze.command
                #:id
                #:find-actor
                #:context-plist-to-hash-table
                #:donep
                #:command-handler
                #:thread
                #:channel-in
                #:channel-out
                #:context
                #:context*
                #:*command*)
  (:import-from #:parachute
                #:define-test
                #:define-test+run
                #:is
                #:true
                #:false)
  (:export #:drive-command))

(in-package #:breeze.test.command)

(define-test donep
  (true (donep nil)))

(define-test context-plist-to-hash-table
  (let ((plist (alexandria:hash-table-plist
                (context-plist-to-hash-table '(buffer-string "asdf" ok 42)))))
    (is equal "asdf" (getf plist 'buffer-string)
        "This context should contain the key 'buffer-string with the value \"asdf\".")
    (false (getf plist :buffer-string)
           "Contexts should not contain the key :buffer-string.")
    (is = 42 (getf plist 'ok)
        "This context should contain the key 'ok with the value 42.")))

;; (trace donep)

(define-test command-handler-initialization
  (let ((handler (make-instance 'command-handler)))
    (false (donep handler) "A fresh command-handler should not be considered done.")
    (false (thread handler) "A fresh command-handler should not have a thread yet.")
    (true (channel-in handler) "A fresh command-handler should have an inbound channel")
    (true (channel-out handler) "A fresh command-handler should have an outbound channel")
    (false (context handler) "A fresh command-handler should not have a context yet.")))



(defun drive-command (fn &key
                           context
                           inputs
                           extra-args
                           ask-for-missing-input-p)
  "Execute a command FN, with the context CONTEXT and send it
INPUTS. Returns the execution trace as a pair of input/request.

N.B. \"Requests\" are what the command returns. \"inputs\" are answers to those requests"
  (let* ((id (start-command fn context extra-args))
         (command (find-actor id :errorp t)))
    (is = id (id command)
        "Find-actor should find an actor with the right id.")
    (unwind-protect
         (loop
           ;; The first input is always nil
           :with input = nil
           ;; We call continue-command to send the input
           :for request = (if input
                              (continue-command id input)
                              (continue-command id))
           ;; We collect the pair of input/request. This is practically
           ;; an execution trace, and we're going to assert things on
           ;; those traces.
           :collect (list input request)
           :do (unless request
                 (error "Commands should not return nil... ideally"))
               ;; Detect when the command is done.

           :until (string= "done" (car request))
           ;; Otherwise, check if the request looks ok
           :do (let ((request-type (alexandria:make-keyword (string-upcase (car request)))))
                 (ecase request-type
                   ;; TODO Add the other types of requests
                   ;; Check if we're missing any input
                   ((:choose :read-string)
                    (if (first inputs)
                        (setf input (pop inputs))
                        (cond
                          (ask-for-missing-input-p
                           (breeze.command::send-out *command* request)
                           (setf input (breeze.command::recv1)))
                          (t
                           (error "Missing input for request ~S ~s" request input)))))
                   (:insert)
                   (:message)
                   (:backward-char))
                 (unless (member request-type '(:choose :read-string))
                   (setf input nil))))
      ;; This is flaky, the other thread might or might not be done already
      #++
      (unless (donep command)
        (error "Command not done.")))))





(define-test cancel-command
  ;; TODO Run commands, but cancel them at _any_ time
  )



(define-command test-insert (name)
  "This is a dummy command to test the \"insert\" request."
  (insert "hi ~a" name))

(define-test insert
  ;; Repeat the test to shake off race conditions
  (loop :for i :below 10 :do
    (is equal
        '((nil ("insert" "hi Mark"))
          (nil ("done")))
        (drive-command 'test-insert :extra-args '("Mark")))))



(define-command test-read-string ()
  "This is a dummy command to test the \"read-string\" request."
  (let ((name (read-string "what's your name? ")))
    (insert "hi ~a" name)))

(define-test read-string
  (loop :for i :below 10 :do
    (is equal
        '((nil ("read-string" "what's your name? " nil))
          ("Mark" ("insert" "hi Mark"))
          (nil ("done")))
        (drive-command 'test-read-string :inputs '("Mark")))))

;; TODO
(define-test choose)

;; TODO
(define-test insert-at)

;; TODO
(define-test read-string-then-insert)

;; TODO
(define-test replace-region)

;; TODO
(define-test backward-char)

;; TODO
(define-test message)

(define-test buffer-string
  (is string=
      "asdf"
      (buffer-string
       (alexandria:plist-hash-table
        '(buffer-string "asdf")))))

;; TODO
(define-test buffer-name)

;; TODO
(define-test buffer-file-name)

;; TODO
(define-test point)

;; TODO
(define-test point-min)

;; TODO
(define-test point-max)

;; TODO Test node-iterator
#++
(node-iterator
 (context-plist-to-hash-table
  `(:buffer-string ,(string #\Newline)
    :position 1)))
