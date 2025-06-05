
(defpackage #:breeze.dogfood
  (:documentation "Breeze commands to help with breeze's development.")
  (:use #:cl
        #:breeze.string
        #:breeze.command
        #:breeze.lossless-reader
        #:breeze.pattern
        #:breeze.workspace)
  (:import-from #:alexandria
                 #:when-let)
  (:export
   #:insert-command-test))

(in-package #:breeze.dogfood)

#++
(when (breeze.syntax-tree::list-car-symbol= outer-node 'define-test+run)
      ;; TODO Maybe make a command "replace-form" or "replace-car"
      (breeze.command:replace-region
       (breeze.syntax-tree:node-start outer-node-car)
       (breeze.syntax-tree:node-end outer-node-car)
       "define-test")
      (breeze.command:return-from-command))



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


(trace :wherein insert-command-test
       breeze.analysis::match-symbol-to-token
       ;; match
       breeze.analysis::node-string-equal)

(untrace)

(defun test-at-point (buffer)
  (let ((root-node-iterator (root-node-iterator (node-iterator buffer))))
    (breeze.analysis::with-match
        (root-node-iterator
         ((:alternation
           parachute:define-test
           parachute:define-test+run
           parachute:define-test+run-interactively) ?name)
         (?name))
      ;; TODO would be nice if (match symbol node-iterator)
      ;; returned a node-iterator instead of a node
      (when-let ((?name (get-bindings '?name)))
        (values root-node-iterator
                (node-content (parse-state (current-buffer))
                              ?name))))))

(defun command-name-p (string)
  (and (member
         string
         (mapcar #'symbol-name (list-all-commands))
         :test #'string-equal)
       t))

;; (command-name-p "insert-class-slot")

(defun extract-trace (node-iterator))

(defun update-command-test (node-itereator test-name)
  (message "Not implemented: updating the test ~s" test-name))

(defun generate-command-test (node-itereator)
  (let* ((name
           ;; Ask the user
           (choose "Name of the command (symbol): "
                   (breeze.test.refactor::missing-tests)))
         (symbol (etypecase name
                   (symbol name)
                   (string (read-from-string name))))
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
         (insert "~%        ")
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

(defun map-top-level-forms
  ;; forms that "preserves" top-level-ness
  ;; prong
  ;; locally, macrolet, symbol-macrolet
  ;; eval-when
  ;;
  )

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
