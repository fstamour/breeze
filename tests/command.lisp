(in-package #:common-lisp-user)

(defpackage #:breeze.test.command
  (:documentation "Tests for breeze.command.")
  (:use :cl #:breeze.command)
  (:import-from #:alexandria
                #:assoc-value)
  (:import-from #:breeze.command
                ;; symbols not exported
                #:make-tasklet
                #:command-handler
                #:command-context*
                #:command-context
                #:command-tasklet
                #:*current-command*)
  (:import-from #:parachute
                #:define-test
                #:is
                #:true
                #:false))

(in-package #:breeze.test.command)

#+ (or)
(progn
  (make-instance
   'command-handler
   :tasklet (make-tasklet ()
              (format t "~&hi"))
   :context nil)

  (chanl:task-thread
   (command-tasklet *current-command*))

  )

#+ (or)
(define-test command-first-callback-is-run-on-start
  (let ((*current-command*)
        (x 0))
    (start-command
     nil
     #'(lambda ()
         (incf x)
         (is *current-command*)
         (is (eq 'hash-table (type-of (command-context*))))))
    (is (= 1 x))))

(define-test context-buffer-string
  (is string=
      "asdf"
      (context-buffer-string
       (alexandria:plist-hash-table
        '(:buffer-string "asdf")))))

(define-test context-buffer-name)
(define-test context-buffer-file-name)
(define-test context-point)
(define-test context-point-min)
(define-test context-point-max)


(define-test choose)
(define-test read-string)
(define-test insert-at)
(define-test insert)
(define-test read-string-then-insert)
(define-test replace-region)
(define-test backward-char)
