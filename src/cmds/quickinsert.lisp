(cl:in-package #:cl-user)

(in-package #:breeze.refactor)

(defun shortcircuit (x)
  (throw 'shortcircuit x))

;; TODO (idea) filter by ":category :template"

(defun commands-for-context (context-predicate)
  (remove-if-not context-predicate (list-all-commands)))

(defun suggest-package-definition ()
  "When the buffer is empty, or only contains comments and whitespaces."
  (let* ((buffer (current-buffer))
         (node-iterator (node-iterator buffer)))
    (when (and node-iterator
               (every #'whitespace-or-comment-node-p (root-subtree node-iterator)))
     ;; TODO Add a configuration to decide whether to shortcircuit or
     ;; not. Because suggesting to insert a "defpackage" form when in
     ;; an empty file is pretty much just my personal preference.
     #++
     (shortcircuit 'breeze.package-commands:insert-defpackage)
     ;; TODO also suggest to insert "in-package", or a propline
     'breeze.package-commands:insert-defpackage)))

(defun suggest-system-definition ()
  "When in an .asd file"
  (when (ends-with-subseq ".asd" (current-buffer-name)
                          :test #'string-equal)
    'insert-asdf))

(defun suggest-lambda ()
  "When inside a higher-order function, like mapcar."
  nil
  #++ ;; TODO
  (when-let* (($node (current-node-iterator))
              ($parent (parent $node))
              ($car (when (parens-node-p parent)
                     (go-down (copy-iterator $parent))))
              (name (when (token-node-p $car)
                      (name $car))))
    ;; TODO Use breeze.cl:higher-order-function-p
    ;; TODO check the position of $node in its parent (skip whitespace and comments)
    ;; Higher-order functions
    (when (and $node
               (parens-node-p parent)
               ;; (child-of-mapcar-node-p node-iterator)
               )
      (shortcircuit 'insert-lambda))))

(defun suggest-loop-clauses ()
  "When inside a loop form."
  (let* ((node-iterator (current-node-iterator)))
    (when (loop-form-p (or (parent node-iterator)
                           node-iterator))
      (shortcircuit (commands-for-context 'loop-context-p)))))

(defun suggest-package-definition-clauses ()
  "When inside a package definition form."
  nil #++  ;; TODO
  (let+ctx (inner-node)
    (when (and inner-node
               (or (defpackage-form-p inner-node)
                   (uiop/package--define-package-form-p inner-node)))
      (shortcircuit 'breeze.package-commands:insert-local-nicknames))))

(defun suggest-other ()
  "Otherwise"
  (commands-for-context
   (if (top-level-node-p (current-node-iterator))
       'top-level-context-p
       'expression-context-p)))

(defun compute-suggestions ()
  "Given the current commands' context, suggests an appropriate list of
commands that the user might want to run."
  (alexandria:ensure-list
   (catch 'shortcircuit
     (mapcar #'funcall
             '(suggest-package-definition
               suggest-system-definition
               suggest-lambda
               suggest-loop-clauses
               suggest-package-definition-clauses
               suggest-other)))))


(defun ensure-flat-list (x)
  "~(flatten (ensure-list))~"
  (alexandria:flatten (alexandria:ensure-list x)))

(defun sanitize-list-of-commands (commands &key
                                             filter
                                             (fallback 'list-all-commands))
  (let* (;; Some methods returns lists, some just a symbol.
         ;; We flatten that to just a list of symbols.
         (commands (ensure-flat-list commands))
         (commands (or commands
                       (and fallback
                            (funcall fallback))))
         (commands
           ;; Filter out commands that are not "inserts"
           (if filter
               (remove-if-not filter commands)
               commands))
         ;; Deduplicate commands
         (commands (remove-duplicates commands))
         ;; Augment the commands with their descriptions.
         (command-descriptions (mapcar #'command-description commands)))
    ;; Save some information for debugging
    (setf *qf* `((:context . ,(context*))
                 (:commands . ,command-descriptions)))
    ;; Returns the list of applicable commands
    command-descriptions))

(defun insert-command-p (command-symbol)
  (symbol-starts-with command-symbol '#:insert-))

;; TODO I might want to use the name "blueprints" instead of snippets
;; because I want these to be usable for _updating_ code, not just
;; creating new code
(define-command quickinsert ()
  "Given the context, suggest applicable snippets."
  (let* (;; Compute the applicable commands
         (commands (sanitize-list-of-commands
                    (compute-suggestions)
                    :filter 'insert-command-p))
         ;; TODO What if there are no suggestions?
         ;; Ask the user to choose a command
         (choice (choose "Choose a command: "
                         (mapcar #'second commands)))
         (command-function (car (find choice commands
                                      :key #'second
                                      :test #'string=))))
    (if command-function
        (funcall command-function)
        (message "~s is not a valid choice" choice))))
