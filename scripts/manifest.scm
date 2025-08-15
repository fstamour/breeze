;; this manifest is meant for running integration tests

;; TODO variants:
;; - per lisp implementation
;; - sly vs slime
;; - emacs, nvim, vscode
;; - x vs no-x

(specifications->manifest
 (list
  ;; "emacs-no-x"
  "emacs" "scrot"
  "coreutils"

  ;; "cl-slime"
  "emacs-slime"
  ;; "emacs-sly"
  "cl-bordeaux-threads"
  "cl-alexandria"

  "cl-all"

  "sbcl"
  "abcl"
  "ecl"
  "clisp"
  "clasp-cl"
  "gcl"
  "ccl"))
