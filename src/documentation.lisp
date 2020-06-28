(in-package #:common-lisp-user)

(defpackage #:breeze.documentation
  (:nicknames ":br.doc")
  (:use :cl #:alexandria)
  (:export
   #:find-undocumented-symbols))

(in-package #:breeze.documentation)

(defun find-undocumented-symbols (package)
  "Find symbols in a package that lacks documentation."
  (let ((result))
    (do-external-symbols (symbol package result)
      (when (and (fboundp symbol)
		 (not (documentation symbol 'function)))
	(push symbol result))
      ;; TODO Check for other type of documentation, it currently only
      ;; checks functions
      )))
