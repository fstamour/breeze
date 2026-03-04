(cl:in-package #:cl-user)

(in-package #:breeze.refactor)

(defun shortcircuit (x)
  (throw 'shortcircuit x))

(defun commands-applicable-at-toplevel ()
  "Create a list of all the commands applicable in the TOP-LEVEL context."
  (remove-if-not (lambda (command)
                   (eq (get command 'context) :top-level))
                 ;; TODO filter by ":category :template"
                 (list-all-commands)))

(defun commands-applicable-in-a-loop-form ()
  "Create a list of all the commands applicable directly under a \"loop\" clause."
  (remove-if-not (lambda (command)
                   (eq (get command 'context) 'cl:loop))
                 (list-all-commands)))

;; That's some Java-level name
(defun commands-applicable-in-expression-context ()
  "Create a list of all the commands applicable where an expression would be expected.

TODO maybe find a better nomenclature?"
  (remove-if-not (lambda (command)
                   (eq (get command 'context) :expression))
                 (list-all-commands)))

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
  (let* ((buffer (current-buffer))
         (node-iterator (node-iterator buffer)))
    (when (breeze.analysis::loop-form-p (parent node-iterator))
      (shortcircuit (commands-applicable-in-a-loop-form)))))

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
  (let* ((current-node-iterator (current-node-iterator))
         (current-node (value current-node-iterator))
         ;; TODO we actually want to know if we're in a "top-level"
         ;; form, not "at the root", because we want the commands that
         ;; are applicable at top-level to also be applicable in forms
         ;; that preserves top-levelness like `progn'.
         (root-node (root-node (current-node-iterator)))
         (rootp (eq current-node root-node)))
    (if rootp
        (commands-applicable-at-toplevel)
        (commands-applicable-in-expression-context))))

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
  (alexandria:flatten
   (copy-seq
    (alexandria:ensure-list x))))

(defun sanitize-list-of-commands (commands)
  ;; Some methods returns lists, some just a symbol.
  ;; We flatten that to just a list of symbols.
  (setf commands (ensure-flat-list commands))
  ;; Fallback to suggesting _all_ commands.
  (unless commands
    ;; TODO This is wrong when called from "quickinsert"
    (setf commands (copy-seq (list-all-commands))))
  ;; Deduplicate commands
  (setf commands (remove-duplicates commands))
  ;; Augment the commands with their descriptions.
  (setf commands (mapcar #'command-description commands))
  ;; Save some information for debugging
  (setf *qf* `((:context . ,(context*))
               (:commands . ,commands)))
  ;; Returns the list of applicable commands
  commands)


;; TODO FIXME TODO FIXME This doesn't actually filter the commands that are not inserts...
;;
;; TODO I might want to use the name "blueprints" instead of snippets
;; because I want these to be usable for _updating_ code, not just
;; creating new code
(define-command quickinsert ()
  "Given the context, suggest applicable snippets."
  (let* (;; Compute the applicable commands
         (commands (sanitize-list-of-commands (compute-suggestions)))
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
