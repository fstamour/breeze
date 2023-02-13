(in-package #:common-lisp-user)

(defpackage #:breeze.test.command
  (:documentation "Tests for breeze.command.")
  (:use :cl #:breeze.command)
  (:import-from #:alexandria
                #:assoc-value)
  (:import-from #:breeze.command
                ;; symbols not exported
                #:context-plist-to-hash-table
                #:make-command-thread
                #:donep
                #:command-handler
                #:command-thread
                #:command-channel-in
                #:command-channel-out
                #:command-context
                #:command-context*
                #:current-command*
                #:*current-command*)
  (:import-from #:parachute
                #:define-test
                #:is
                #:true
                #:false)
  (:export #:context-plist-to-hash-table))

(in-package #:breeze.test.command)

(define-test context-plist-to-hash-table
  (let ((plist (alexandria:hash-table-plist
                (context-plist-to-hash-table '(buffer-string "asdf" ok 42)))))
    (is equal "asdf" (getf plist 'buffer-string))
    (false (getf plist :buffer-string))
    (is = 42 (getf plist 'ok))))

(define-test command-handler-initialization
  (let ((handler (make-instance 'command-handler)))
    (false (command-thread handler))
    (true (command-channel-in handler))
    (true (command-channel-out handler))
    (false (command-context handler))))


(define-test cancel-command
  ;; Cancel command should never fail
  (loop :repeat 3 :do (false (and (cancel-command) nil))))



(define-command test-insert (name)
  "This is a dummy command to test the \"insert\" request."
  (insert "hi ~a" name))

(define-test insert
  ;; Repeat the test to shake off race conditions
  (loop :for i :below 10 :do
    (cancel-command)
    (false (current-command*)
           "~d There should be no running commands." i)
    (is equal '("insert" "hi Mark") (test-insert :name "Mark")
        "~d The test-insert command should request to insert that string." i)
    ;; At this point the command's thread might or might not be done.
    (is equal '("done") (continue-command)
        "~d The command should be done after continuing." i)
    (false (current-command*)
           "~d There should be no running commands anymore." i)
    (true (donep (current-command*))
          "~d The command should still be done." i)))



(define-command test-read-string ()
  "This is a dummy command to test the \"read-string\" request."
  (let ((name (read-string "what's your name? ")))
    (insert "hi ~a" name)))

(define-test read-string
  (loop :for i :below 10 :do
    (cancel-command)
    (false (current-command*)
           "~d There should be no running commands." i)
    (is equal '("read-string" "what's your name? " nil)
        (test-read-string)
        "~d The test-read-string command should request to prompt the user for his name." i)
    (false (donep (current-command*))
           "~d The command should not be done yet." i)
    (is equal '("insert" "hi Mark") (continue-command "Mark")
        "~d The test-insert command should request to insert that string." i)
    (is equal '("done") (continue-command)
        "~d The command should be done after continuing." i)
    (false (current-command*)
           "~d There should be no running commands anymore." i)
    (true (donep (current-command*)))))

(define-test choose)
(define-test insert-at)

(define-test read-string-then-insert)
(define-test replace-region)
(define-test backward-char)
(define-test message)

(define-test context-buffer-string
  (is string=
      "asdf"
      (context-buffer-string
       (alexandria:plist-hash-table
        '(buffer-string "asdf")))))

(define-test context-buffer-name)
(define-test context-buffer-file-name)
(define-test context-point)
(define-test context-point-min)
(define-test context-point-max)
