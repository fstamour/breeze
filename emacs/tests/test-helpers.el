;; -*- lexical-binding: t -*-
;;
;; Shared test helpers for breeze's emacs tests.
;;

;; Remove 'vc-refresh-state from find-file-hook because git is not
;; available in the container and will incorrectly make some tests
;; fail.
(remove-hook 'find-file-hook 'vc-refresh-state)

(defun breeze--xor (a b)
  (or (and a (not b))
      (and (not a) b)))
