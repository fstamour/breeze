;; -*- lexical-binding: t -*-

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



(ert-deftest test/breeze-%symbolicate ()
  (should (eq 'sly (breeze-%symbolicate2 "sly")))
  (should (eq 'sly (breeze-%symbolicate2 'sly)))
  (should (eq 'slime (breeze-%symbolicate2 "slime")))
  (should (eq 'slime (breeze-%symbolicate2 'slime)))
  (should (eq 'sly-eval (breeze-%symbolicate2 'sly "eval")))
  (should (eq 'slime-eval (breeze-%symbolicate2 'slime "eval")))
  (should (eq 'slime-connected-hook
              (breeze-%symbolicate2 'slime "connected-hook"))))



;; TODO true only if connected!
(ert-deftest test/breeze-connection ()
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




(ert-deftest test/breeze-relative-path ()
  (should (file-exists-p (breeze-relative-path)))
  (should (file-exists-p (breeze-relative-path "src/")))
  (should (file-exists-p (breeze-relative-path "emacs/breeze.el")))
  (should (file-exists-p (breeze-relative-path "src/ensure-breeze.lisp"))))

(ert-deftest test/breeze-init ()
  (should (eq t (breeze-validate-if-package-exists "CL")))
  (should (eq nil (breeze-validate-if-package-exists "this package probably doesn't exists"))))
;; t


;; TODO only after (breeze-ensure)
;; (should (eq t (breeze-validate-if-breeze-package-exists)))

;; (should (eq t (breeze-ensure)))



(ert-deftest test/breeze-intergration ()
  (ert-test-erts-file "breeze.erts"))


;; TODO (ert-deftest test/generate-bindings-documentation )

;; TODO (ert-deftest test/update-list-of-stubs)


;; Testing the "after-change-function"

(ert-deftest test/breeze-after-change-functions ()
  (let ((breeze-changes))
    (with-temp-buffer
      (rename-buffer "test1")
      (add-hook 'after-change-functions 'breeze-after-change-function nil t)
      (insert "abcd")
      (delete-char -1)
      (goto-char (point-min))
      (search-forward "bc")
      (replace-match "BC")
      (should (equal breeze-changes
                     '((:buffer-name "test1" :buffer-file-name nil :replace-at 1 :end 3 :text "BC")
                       (:buffer-name "test1" :buffer-file-name nil :delete-at 3 :length 1)
                       (:buffer-name "test1" :buffer-file-name nil :insert-at 0 :text "abcd"))))
      (should (equal "aBC" (buffer-substring-no-properties (point-min) (point-max)))))))
