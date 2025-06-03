
(defpackage #:breeze.dogfood
  (:documentation "Breeze commands to help with breeze's development.")
  (:use #:cl
        #:breeze.string
        #:breeze.command
        #:breeze.lossless-reader
        #:breeze.pattern
        #:breeze.workspace)
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

(compile-pattern '((:alternation
                    parachute:define-test
                    parachute:define-test+run) ?name))

(trace :wherein insert-command-test
       breeze.analysis::match-symbol-to-token
       ;; match
       breeze.analysis::node-string-equal)

(untrace)

(match
    (compile-pattern '(:alternation
                      parachute:define-test
                      parachute:define-test+run))
  'parachute:define-test)

(match
    (compile-pattern '(:alternation
                      parachute:define-test
                      parachute:define-test+run))
  'parachute:define-test+run)



(define-command insert-command-test ()
  "Insert a missing test!"
  (let* ((node-iterator (copy-iterator (current-buffer))))
    (let ((outer-node (root-node-iterator node-iterator)))
      (message "Current node: ~s"
               (around
                (node-string outer-node)
                0 30))
      (message "Current test name: ~s"
               (breeze.analysis::with-match
                   (outer-node
                    ((:alternation
                       parachute:define-test
                       parachute:define-test+run) ?name)
                     (?name))
                 ;; TODO would be nice if (match symbol node-iterator)
                 ;; returned a node-iterator instead of a node
                 (when ?name
                   (node-content (parse-state (current-buffer))
                                 ?name))))
      (return-from-command))
    (let* ((name
             (or
              ;; Get from the context, or...
              #++
              (when (breeze.syntax-tree::list-car-symbol= outer-node 'define-test)
                (breeze.syntax-tree:node-content
                 (second (breeze.syntax-tree:node-content outer-node))))
              ;; Ask the user
              (choose "Name of the command (symbol): "
                      (or (breeze.test.refactor::missing-tests)
                          (breeze.refactor::list-all-commands)))))
           (symbol (etypecase name
                     (symbol name)
                     (string (read-from-string name))))
           (trace (breeze.test.refactor::drive-command symbol
                                                       :inputs '()
                                                       :context '()
                                                       :ask-for-missing-input-p t))
           (inputs (remove-if #'null (mapcar #'first trace)))
           (*print-case* :downcase))
      ;; We got the name from the current top-level form, which means we
      ;; want to insert the snippet elsewhere.
      (when (symbolp name)
        ;; TODO a "go to position" command would be better lol
        (breeze.command:insert-at (end outer-node) "~%~%"))
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
      (insert "))"))))


;; This is emacs lisps to add a binding to the command "insert-test"
;; defined just above:
#+elisp
(progn
  (breeze-refresh-commands)
  (define-key breeze-minor-mode-map (kbd "C-,") #'breeze-insert-command-test))
