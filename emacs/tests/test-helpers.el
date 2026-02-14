;; -*- lexical-binding: t -*-
;;
;; Shared test helpers for breeze's emacs tests.
;;

(defun breeze--xor (a b)
  (or (and a (not b))
      (and (not a) b)))
