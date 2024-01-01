
(require 'ert)

(defun breeze--xor (a b)
  (or (and a (not b))
      (and (not a) b)))

(ert-deftest test/breeze--xor ()
  (should (equal '(nil t t nil)
                 (mapcar (lambda (args)
                           (apply 'breeze--xor args))
                         '((nil nil)
                           (nil t)
                           (t nil)
                           (t t))))))



;; TODO true only if connected!
(ert-deftest test/breeze-connection ()
  (should (breeze--xor
           (breeze-sly-connected-p)
           (breeze-slime-connected-p)))
  (should (eq t (breeze-check-if-connected-to-listener))))

(ert-deftest test/breeze-eval ()
  ;; Integers
  (should (= (breeze-eval "(+ 1 2)") 3))
  ;; Strings

  (should (string= (breeze-eval "\"hi\"") "hi"))
  ;; Symbols
  (should (eq (breeze-eval "cl:t") t))
  (should (eq (breeze-eval "cl:nil") nil))
  (should (eq t (breeze-eval-predicate "t")))
  (should (eq t (breeze-eval-predicate "T")))
  (should (eq nil (breeze-eval-predicate "nil")))
  (should (eq nil (breeze-eval-predicate "NIL"))))

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




(ert-deftest test/breeze-relative-path ()
  (should (file-exists-p (breeze-relative-path)))
  (should (file-exists-p (breeze-relative-path "src/")))
  (should (file-exists-p (breeze-relative-path "src/breeze.el")))
  (should (file-exists-p (breeze-relative-path "src/ensure-breeze.lisp"))))

(ert-deftest test/breeze-init ()
  (should (eq t (breeze-validate-if-package-exists "CL")))
  (should (eq nil (breeze-validate-if-package-exists "this package probably doesn't exists"))))
;; t


;; TODO only after (breeze-ensure)
;; (should (eq t (breeze-validate-if-breeze-package-exists)))


;; (should (eq t (breeze-ensure)))

;; (breeze-init)



(ert-deftest test/breeze-intergration ()
  (ert-test-erts-file "breeze.erts"))
