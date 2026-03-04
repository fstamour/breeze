(cl:in-package #:cl-user)

(in-package #:breeze.refactor)

(defparameter *qf* nil
  "Data from the latest quickfix invocation.
For debugging purposes ONLY.")

#++
;; TODO temporarily disabled due to a re-organization
(defun maybe-ask-to-load-system ()
  (when-let ((filename (current-buffer-filename)))
    (multiple-value-bind (status system)
        (breeze.asdf:loadedp filename)
      (when (eq :not-loaded status)
        (when (ask-y-or-n-p "The current file is part of the system \"~a\", but has not been loaded yet. Do you want to load it now? (y/n) "
                            (asdf:component-name system))
          (message "Loading system \"~a\"..." system)
          (asdf:load-system system)
          (message "System \"~a\" successfully loaded." system)
          (return-from-command))))))

#++
(defun maybe-create-system ()
  (let ((buffer (current-buffer)))
    (cwd)))

(defun maybe-apply-fixes ()
  "Fix the first \"applicable\" linter issue. If there is any
automatically fixable linter issue in the current top-level form, it
will fix the issue and then stop the current command."
  ;; TODO cache the linter issues (in the *workspace*'s buffer)
  ;;
  ;; TODO It could be nice to let the user know that "there are issues
  ;; in the current form", but that it "doesn't know how to fix
  ;; them". Because otherwise, the "quickfix" command will show a
  ;; message saying "there's nothing to fix", but the user will see
  ;; the blue/yellow/red squigly in their editors and wonder why they
  ;; didn't get fixed by quickfix. Currently the command "fix buffer"
  ;; only returns `simple-node-conditions' that have a `replacement`.
  (when-let* ((fixes (breeze.lint:fix-buffer (current-buffer)))
              (top-level-node (root-node (current-node-iterator)))
              (applicable-fixes
               (remove-if-not
                (lambda (fix)
                  ;; keep only fixes that are part of the current top-level form
                  (eq top-level-node
                      (root-node (breeze.lint:target-node fix))))
                fixes)))
    ;; Apply the fix
    (let* ((fix (first applicable-fixes))
           (node (value (breeze.lint:target-node fix)))
           (replacement (breeze.lint:replacement fix)))
      (replace-region (start node) (end node)
                      (or replacement ""))
      (return-from-command))))




(define-command quickfix ()
  "Given the context, suggest some applicable commands."
  ;; TODO this is definitely not the right place...
  #++ (maybe-ask-to-load-system)
  ;; TODO Is it the right place? maybe do that in the header-line??
  #++ (check-in-package)
  (let ((root (root-node (current-node-iterator))))
    (pulse (start root) (end root))
    (maybe-apply-fixes)
    (message "Nothing automatically fixable in the current top-level form ~s."
             (list (current-buffer-name)
                   (start root)
                   (end root)))))


#|

TODO there's some different kind of "quickfixes":

- quickfixes are "obviously" the right thing to do, you don't have to ask
- code actions are contextual commands
- contextual inserts (add a binding to a let)
- contextual changes (move an expression into a let, change let to let*)
  - contextual "inverts" (e.g. ~x~ → ~(not x)~)
  - or "alternatives" (e.g. ~'(a b c)~ → ~#(a b c)~)
- non-contextual snippets
- contextual delete (remove a binding in a let)
- other: if point is on a (trace ...), suggest to untrace something,
  to comment or remove the form, to replace trace by untrace, or add
  the equivalent untrace next to it.
- contextual move up/down, out/into (e.g. slots in defclass)

|#


#+nil
(quickfix :buffer-string "   " :point 3)
