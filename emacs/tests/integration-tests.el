;; -*- lexical-binding: t -*-
;;
;; Integration tests that require a connected listener (slime or sly).
;; Set the environment variable BREEZE_LISTENER to "slime" or "sly".
;;

(require 'ert)


;;; Listener setup

(unless (breeze-listener-connected-p nil)
  (let ((listener-name (getenv "BREEZE_LISTENER")))
    (unless listener-name
      (error "BREEZE_LISTENER must be set to \"slime\" or \"sly\"."))
    (let ((listener (intern listener-name)))
      ;; load sly or slime
      (require listener)
      ;; TODO make the lisp implementation configurable
      (setq inferior-lisp-program "sbcl")
      ;; initialize sly or slime
      (funcall listener)
      (breeze-wait-for-listener)
      ;; load breeze
      (breeze-init))))

;;; Tests

;; TODO add tests directly on breeze-%eval

;; TODO add assertions about "inferior-lisp"
(ert-deftest test/breeze-connection ()
  ;; one and only one of these should be true at a time
  (should (breeze--xor
           (breeze-sly-connected-p)
           (breeze-slime-connected-p)))
  (should (breeze-listener-connected-p)))

(ert-deftest test/breeze-eval ()
  ;; Integers
  (should (= (breeze-eval "(+ 1 2)") 3))
  ;; Strings
  (should (string= (breeze-eval "\"hi\"") "hi"))
  ;; Symbols
  (should (eq (breeze-eval "cl:t") t))
  (should (eq (breeze-eval "cl:nil") nil))
  (should (eq t (breeze-eval "t")))
  (should (eq t (breeze-eval "t")))
  (should (eq nil (breeze-eval "nil")))
  (should (eq nil (breeze-eval "nil"))))

;; TODO Figure out how to evaluate something without triggering the debugger when an error occurs
;; (ert-deftest breeze-eval-empty-string ()
;;   :expected-result :failed
;;   (breeze-eval ""))

;; (let ((slime-event-hooks (list (lambda (event)
;;                                  (message "Event: %S" (list (car event)
;;                                                             (length (cdr event))))
;;                                  nil))))
;;   (breeze-eval "(error \"oups\")"))
;; (breeze-eval "(read)")

(ert-deftest test/breeze-init ()
  (should (eq t (breeze-validate-if-package-exists "CL")))
  (should (eq nil (breeze-validate-if-package-exists "this package probably doesn't exists"))))

;; TODO only after (breeze-ensure)
;; (should (eq t (breeze-validate-if-breeze-package-exists)))
;; (should (eq t (breeze-ensure)))


(ert-deftest test/breeze-erts ()
  (ert-test-erts-file
   (breeze-relative-path "emacs/tests/breeze.erts")))
