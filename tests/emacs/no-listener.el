(require 'ert)
(require 'ert-x)

(ert-deftest breeze-init-no-listener-loaded ()
  "Make sure that `breeze-init' has sane behaviour if it can't find sly or slime."
  (should (equal
           '(error "Please start either slime or sly.")
           (should-error (breeze-init)))))
