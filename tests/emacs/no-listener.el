(require 'ert)
(require 'ert-x)

;; Remove any autoloaded listener functions (e.g. from guix site-level
;; packages) so we can test the "no listener" scenario.
(fmakunbound 'slime)
(fmakunbound 'sly)
(fmakunbound 'inferior-lisp)

(ert-deftest breeze-init-no-listener-loaded ()
  "Make sure that `breeze-init' has sane behaviour if it can't find sly or slime."
  (should (equal
           '(error "Please load either slime or sly.")
           (should-error (breeze-init)))))
