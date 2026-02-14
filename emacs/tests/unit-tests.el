;; -*- lexical-binding: t -*-

(require 'ert)

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



(ert-deftest test/breeze-relative-path ()
  (should (file-exists-p (breeze-relative-path)))
  (should (file-exists-p (breeze-relative-path "src/")))
  (should (file-exists-p (breeze-relative-path "emacs/breeze.el")))
  (should (file-exists-p (breeze-relative-path "src/ensure-breeze.lisp"))))


;; TODO (ert-deftest test/generate-bindings-documentation )

;; TODO (ert-deftest test/update-list-of-stubs)


;; Testing the "after-change-function"

(ert-deftest test/breeze-after-change-functions ()
  (let ((breeze-changes))
    (cl-letf (((symbol-function 'breeze-disabled-p) (lambda () nil)))
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
        (should (equal "aBC" (buffer-substring-no-properties (point-min) (point-max))))))))
