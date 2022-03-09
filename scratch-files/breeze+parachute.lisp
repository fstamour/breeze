
(in-package #:common-lisp-user)

(defpackage #:breeze+parachute
  (:use :cl #:alexandria)
  (:import-from #:breeze.user
                #:current-packages)
  (:export
   #:run-all-tests))

(in-package #:breeze+parachute)

(defun run-all-tests ()
  (parachute:test (current-packages)))

;; (run-all-tests)

;; TODO less reporting, only report errors?
(push (cons "run parachute tests"
            #'(lambda (string)
                (declare (ignore string))
                (run-all-tests)))
      breeze.listener::*interactive-eval-hooks*)

;; Something to infer which package uses parachute
#+nil
(let ((packages (make-hash-table)))
  (loop :for symbol :being :the
          :symbol :of (find-package 'binstring.test)
        :do (setf (gethash (symbol-package symbol) packages) t))
  (hash-table-keys packages))
