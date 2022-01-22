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
		#:continue-command
		#:cancel-command
		#:define-command
		;; symbols not exported
		#:make-tasklet
		#:command-handler
		#:command-context*
		#:command-context
		#:command-tasklet
		#:*current-command*))

(in-package #:breeze.command.test)

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

(deftest command-first-callback-is-run-on-start
  (let ((*current-command*)
	(x 0))
    (start-command
     nil
     #'(lambda ()
	 (incf x)
	 (is *current-command*)
	 (is (eq 'hash-table (type-of (command-context*))))))
    (is (= 1 x))))

(deftest context-buffer-string)
(deftest context-buffer-name)
(deftest context-buffer-file-name)
(deftest context-point)
(deftest context-point-min)
(deftest context-point-max)


(deftest choose)
(deftest read-string)
(deftest insert-at)
(deftest insert)
(deftest read-string-then-insert)
(deftest replace-region)
(deftest backward-char)
