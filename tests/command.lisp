(in-package #:common-lisp-user)

(uiop:define-package #:breeze.test.command
    (:documentation "Tests for breeze.command.")
  (:use :cl #:breeze.command)
  (:import-from #:alexandria
                #:assoc-value)
  ;; importing non-exported symbols not exported
  (:import-from #:breeze.command
                #:id
                #:find-actor
                #:donep
                #:command-handler
                #:thread
                #:channel-in
                #:channel-out
                #:context
                #:context*
                #:*command*
                #:command-name-for-editor
                #:command-lambda-list-for-editor)
  (:import-from #:parachute
                #:define-test
                #:define-test+run
                #:is
                #:true
                #:false)
  (:export #:drive-command)
  (:export #:fake-command-handler
           #:mock-send-out
           #:mock-recv-into
           #:with-fake-command-handler))

(in-package #:breeze.test.command)

(define-test donep
  (true (donep nil)))

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

;; TODO
(define-test buffer-name)

;; TODO
(define-test buffer-filename)

;; TODO
(define-test point)

;; TODO
(define-test point-min)

;; TODO
(define-test point-max)



(defclass fake-command-handler ()
  ((context
    :initarg :context
    :initform (make-hash-table)
    :accessor context
    :documentation "The context of the command.")
   (mocks
    :initform nil
    :accessor mocks)))

(defmethod send-into ((command fake-command-handler) value)
  (break "Not implemented: send-into: ~s" value))

(defmethod recv-from ((command fake-command-handler))
  (break "Not implemented: recv-from"))

(defmethod recv-into ((command fake-command-handler))
  (let ((mock (pop (mocks command))))
    (if mock
        (if (eq (car mock) 'mock-recv-into)
            (funcall (cdr mock))
            (error "Exepected a call to `recv-into' got ~s" (car mock)))
        (error "Unexpected recv-into value."))))

(defmethod send-out ((command fake-command-handler) value)
  (let ((mock (pop (mocks command))))
    (if mock
        (if (eq (car mock) 'mock-send-out)
            (funcall (cdr mock) value)
            (error "Exepected a call to `send-out' got ~s" (car mock)))
        (error "Unexpected send-out value: ~s" value))))

(defmacro mock-recv-into (() &body body)
  `(lambda ()
     ,@body))

(defmacro mock-send-out ((var) &body body)
  (check-type var symbol)
  `(lambda (,var)
     ,@body
     nil))

(defmacro with-fake-command-handler (mocks
                                     &body body)
  `(let ((*command* (make-instance 'fake-command-handler)))
     ,@(loop :for mock :in (reverse mocks)
             :collect `(push (cons ',(car mock) ,mock)
                             (mocks *command*)))
     ,@body
     ;; TODO check that there are no mocks left
     ;; (fasle (mocks *command*) "...")
     ))

(define-test+run insert
  (with-fake-command-handler
      ((mock-send-out (value)
         (is equalp '("insert" "A, B, C") value)))
    (insert "~{~a~^, ~}" '(a b c))))

(define-test+run read-string
  (with-fake-command-handler
      ((mock-send-out (value)
         (is equalp '("read-string" "> " "initial value") value))
       (mock-recv-into ()
         '("user input")))
    (is equalp "user input"
        (read-string "> " "initial value"))))

(define-test+run read-string-then-insert
  (with-fake-command-handler
      ((mock-send-out (value)
         (is equalp '("read-string" "> " nil) value))
       (mock-recv-into ()
         '("user input"))
       (mock-send-out (value)
         (is equalp '("insert" "~> \"user input\" <~") value)))
    (read-string-then-insert "> " "~~> ~s <~~")))

(define-test+run choose
  (with-fake-command-handler
      ((mock-send-out (value)
         (is equalp '("choose" "choose one: " (a b c)) value))
       (mock-recv-into () '("choice")))
    (is equalp "choice"
        (choose "choose one: " '(a b c)))))

(define-test+run insert-at
  (with-fake-command-handler
      ((mock-send-out (value)
         (is equalp '("insert-at" 32 "asdf") value)))

    (insert-at 32 "~a" '|asdf|)))

(define-test+run insert-at-saving-excursion
  (with-fake-command-handler
      ((mock-send-out (value)
         (is equalp '("insert-at-saving-excursion" 32 "asdf") value)))
    (insert-at-saving-excursion 32 "~a" '|asdf|)))

(define-test+run replace-region
  (with-fake-command-handler
      ((mock-send-out (value)
         (is equalp '("replace" 0 100 "---") value)))
    (replace-region 0 100 "---")))

(define-test+run message
  (with-fake-command-handler
      ((mock-send-out (value)
         (is equalp '("message" "hello WORMS") value)))
    (message "hello ~a" :worms)))

(define-test+run find-file
  (with-fake-command-handler
      ((mock-send-out (value)
         (is equalp '("find-file" "hello.lisp" ()) value)))
    (find-file "hello.lisp"))
  (with-fake-command-handler
      ((mock-send-out (value)
         (is equalp '("find-file" "hello.lisp" t) value)))
    (find-file "hello.lisp" t)))

(define-test+run return-value-from-command
  (with-fake-command-handler
      ((mock-send-out (value)
         (is equalp '("return" 42) value)))
    (return-value-from-command 42)))

(define-test+run goto-char
  (with-fake-command-handler
      ((mock-send-out (value)
         (is equalp '("goto-char" 89) value)))
    (goto-char 89)))

(define-test+run pulse
  (with-fake-command-handler
      ((mock-send-out (value)
         (is equalp '("pulse" 0 100) value)))
    (pulse 0 100)))



(define-test+run command-name-for-editor
  (is string= "breeze-x" (command-name-for-editor 'x))
  (is string= "breeze-find-file" (command-name-for-editor 'find-file))
  (is string= "breeze-dwim" (command-name-for-editor 'breeze-dwim))
  (is string= "breeze-dwim" (command-name-for-editor '|breeze-dwim|))
  (is string= "breeze-dwim" (command-name-for-editor '|breeze-DWIM|)))

(defun same-symbol-name-p (expected got)
  (every #'string= expected got))

(define-test+run same-symbol-name-p
  (true (same-symbol-name-p '(#:x) '(x)))
  (true (same-symbol-name-p '(#:x) '(:x)))
  (false (same-symbol-name-p '(#:x) '(:|x|))))

(define-test+run command-lambda-list-for-editor
  (is same-symbol-name-p ()
      (command-lambda-list-for-editor '()))
  (is same-symbol-name-p '(#:x)
      (command-lambda-list-for-editor '(x)))
  (is same-symbol-name-p '(#:x #:y)
      (command-lambda-list-for-editor '(x y)))
  (is same-symbol-name-p '(#:x #:&option #:y)
      (command-lambda-list-for-editor '(x &option y)))
  (is same-symbol-name-p '(#:x #:y)
      (command-lambda-list-for-editor '(x &optional y)))
  (is same-symbol-name-p '(#:x #:y)
      (command-lambda-list-for-editor '(x &optional (y 0))))
  (is same-symbol-name-p '(#:x #:y)
      (command-lambda-list-for-editor '(x &key (y "default" yp)))))
