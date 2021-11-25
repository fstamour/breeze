(in-package #:common-lisp-user)

(defpackage #:breeze.command.test
  (:documentation "Tests for breeze.command.")
  (:use :cl)
  (:import-from #:breeze.test
		#:deftest
		#:is)
  (:import-from #:alexandria
		#:assoc-value)
  (:import-from #:breeze.command
		#:start-command
		#:call-next-callback
		;; symbols not exported
		#:command-context
		#:command-callback
		#:*current-command*))

(in-package #:breeze.command.test)

(deftest command-nil
  (let ((*current-command*))
    (start-command nil nil)
    (is *current-command*)
    (is (null (command-context *current-command*)))
    (is (null (command-callback *current-command*)))))

(deftest command-first-callback-is-run-on-start
  (let ((*current-command*)
	(x 0))
    (start-command nil (lambda ()
			 (incf x)))
    (is *current-command*)
    (is (null (command-context *current-command*)))
    (is (null (command-callback *current-command*)))
    (is (= 1 x))))

(deftest command-context-is-set
  (let ((*current-command*))
    (start-command '((a . 42)) nil)
    (is *current-command*)
    (is (command-context *current-command*))
    (is (= 42 (assoc-value (command-context *current-command*) 'a)))
    (is (null (command-callback *current-command*)))))

(deftest command-multiple-callbacks
  (let* ((*current-command*)
	 (result (start-command nil
				(lambda ()
				  (values 1
					  (lambda (x)
					    (+ x 2)))))))
    (is (= 1 result))
    (is (command-callback *current-command*))
    (setf result (call-next-callback 3))
    (is (= 5 result))))

(deftest command-multiple-callbacks-with-conditions
  (flet ((command ()
	   (values 1
		   (lambda (x)
		     (if (evenp x)
			 (+ x 2)
			 (+ x 3))))))
    ;; First invocation (with even argument)
    (let* ((*current-command*)
	   (result (start-command nil #'command)))
      (is (= 1 result))
      (is (command-callback *current-command*))
      (setf result (call-next-callback 2))
      (is (= 4 result)))
    ;; Second invocation (with odd argument)
    (let* ((*current-command*)
	   (result (start-command nil #'command)))
      (is (= 1 result))
      (is (command-callback *current-command*))
      (setf result (call-next-callback 3))
      (is (= 6 result)))))


#|
TODO add tests for
#:command-context*
#:context-buffer-string
#:context-buffer-name
#:context-buffer-file-name
#:context-point
#:context-point-min
#:context-point-max

#:choose
#:read-string
#:insert-at
#:insert
#:read-string-then-insert
#:replace-region
#:backward-char
|#
