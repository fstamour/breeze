(require 'ert)
(require 'ert-x)

(ert-deftest breeze-init-no-listener-loaded ()
  "Make sure that `breeze-init' has sane behaviour if it can't find sly or slime."
  ;; Temporarily unbind listener functions
  (cl-letf (((symbol-function 'slime) nil)
            ((symbol-function 'sly) nil)
            ((symbol-function 'inferior-lisp) nil))
    (should (equal
             '(error "Please load either slime or sly.")
             (should-error (breeze-init))))))
