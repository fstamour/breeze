
(defpackage #:breeze.dogfood
  (:documentation "Breeze commands and utilities to help with breeze's development.")
  (:use #:cl
        #:breeze.string
        #:breeze.command
        #:breeze.parser
        #:breeze.pattern
        #:breeze.workspace)
  (:import-from #:alexandria
                 #:when-let)
  (:export
   #:breeze-relative-pathname
   #:find-breeze-packages
   #:generate-breeze-reference
   #:insert-command-test))

(in-package #:breeze.dogfood)

(defun breeze-relative-pathname (pathname)
  "Returns a pathname relative to breeze's location."
  (if (uiop:relative-pathname-p pathname)
      (asdf:system-relative-pathname :breeze pathname)
      pathname))

(defvar *breeze-home*
  (breeze-relative-pathname ""))

(defun find-breeze-packages ()
  ;; TODO maybe filter out package "breeze.dogfood"
  (remove-if (lambda (package)
               (position #\. (package-name package) :start (length #1="breeze.")))
             (breeze.xref:find-packages-by-prefix #1#)))

#++
(when (breeze.syntax-tree::list-car-symbol= outer-node 'define-test+run)
      ;; TODO Maybe make a command "replace-form" or "replace-car"
      (breeze.command:replace-region
       (breeze.syntax-tree:node-start outer-node-car)
       (breeze.syntax-tree:node-end outer-node-car)
       "define-test")
      (breeze.command:return-from-command))

(defun generate-breeze-reference ()
  (breeze.documentation::generate-documentation
   ;; root
   (breeze-relative-pathname "docs/")
   ;; packages
   (find-breeze-packages)))



;;; Oh yiisss! In this "page", I create a command that can generate
;;; tests _interactively_ for a command.
;;;
;;; TODO (WIP) be able to update existing tests (currently, I
;;; re-generate them manually, which will get very annoying fast)

(defun insert-assert-request (expected i)
  "Helper function to split an assertion into multiple _if_ there's a
newline in the expected result."
  (if expected
      (if (position #\Newline expected)
          (progn
            (insert "~%        (is equal")
            (insert "~%            '(")
            (loop :for line :in (split-by-newline expected)
                  :for i :from 0
                  :unless (zerop i)
                    :do (insert "~%             ")
                  :do (insert "~s" line))
            (insert ")")
            (insert "~%            (split-by-newline (~:R request)))" i))
          (insert "~%        (is string= ~s (~:R request))" expected i))
      (insert "~%        (false (~:R request))" i)))

(defun test-at-point (buffer)
  (let (($root (root (node-iterator buffer))))
    (let-match
        (((:either
           (:symbol define-test parachute)
           (:symbol define-test+run parachute)
           (:symbol define-test+run-interactively parachute))
          ?name)
         $root)
      (when ?name
        (values $root (node-string ?name))))))

(defun command-name-p (string)
  (and (member
         string
         (mapcar #'symbol-name (list-all-commands))
         :test #'string-equal)
       t))

;; (command-name-p "insert-class-slot")

;; TODO (defun extract-trace (node-iterator))

;; TODO
#++
(defun update-command-test (node-itereator test-name)
  (message "Not implemented: updating the test ~s" test-name))

(defun generate-command-test (node-itereator)
  (let* ((name
           ;; Ask the user
           (choose "Name of the command (symbol): "
                   #| Oops: circular dependencies between the "tests" and "dogfood" systems |#
                   #++
                   (breeze.test.refactor::missing-tests)
                   (breeze.command:list-all-commands)))
         (symbol (etypecase name
                   (symbol name)
                   (string (read-from-string name))))
         #++ #| Oops: circular dependencies between the "tests" and "dogfood" systems |#
         (trace (breeze.test.refactor::drive-command symbol
                                                     :inputs '()
                                                     :context '()
                                                     :ask-for-missing-input-p t))
         (inputs (remove-if #'null (mapcar #'first trace)))
         (*print-case* :downcase))
    (insert "(define-test+run ~(~a~)" symbol)
    (insert "~%    (let* ((trace (drive-command #'~(~a~)" symbol)
    (insert "~%                                 :inputs '~s" inputs)
    (insert "~%                                 :context '())))")
    (insert "~%      (common-trace-asserts '~(~a~) trace ~d)"
            symbol (length trace))
    (loop
      :for (input request) :in (butlast trace)
      :for i :from 1
      :do
         (insert "~%      (destructuring-bind (input request) (~:R trace)" i)
         (insert "~%        (declare (ignorable input))")
         (if input
             (insert "(is string= ~s input)" input)
             (insert "(false input)")
             ;; (insert "(declare (ignore input))")
             )
         (loop
           :for part :in request
           :for j :from 1
           :do (insert-assert-request part j))
         (insert ")"))
    (insert "))")))

(define-command insert-command-test ()
  "Insert a missing test!"
  (multiple-value-bind (node-iterator test-name)
      (test-at-point (current-buffer))
    ;; (message "Current node: ~s" (when node-iterator (around (node-string node-iterator) 0 30)))
    ;; (message "Current test name: ~s" test-name)
    (when (and test-name
               (not (command-name-p test-name)))
      (message "Cancelling: the test's name doesn't correspond to any command's name.")
      (return-from-command))
    (if test-name
        (update-command-test node-iterator test-name)
        (generate-command-test node-iterator))))


;; This is emacs lisps to add a binding to the command "insert-test"
;; defined just above:
#+elisp
(progn
  (breeze-refresh-commands)
  (define-key breeze-minor-mode-map (kbd "C-,") #'breeze-insert-command-test))
