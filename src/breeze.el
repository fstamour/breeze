;;; package -- breeze integration with emacs

;;; Commentary:

;;; Code:

;;; Scratch section
;; (defun breeze-eval-defun )


;;; Requires
(require 'cl)
(require 'evil) ;; Actually optional

;;; Groups and customs
(defgroup breeze nil
  "breeze")

(defcustom breeze-default-author ""
  "The default author when generating asdf system."
  :type 'string
  :group 'breeze)

(defcustom breeze-default-licence "GNU GPLv3"
  "The default licence when generating asdf system."
  :type 'string
  :group 'breeze)


;;; Variables

(defvar breeze-mode-map (make-sparse-keymap))


;;; Utilities

(cl-defun breeze-choose-and-call-command (prompt commands &key allow-other-p)
  "Let the user choose a command to run from a list."
  (let* ((choice (completing-read prompt (mapcar 'car commands)))
	 (command (or (cadr (assoc choice commands))
		      (and allow-other-p choice))))
    (when command
      (call-interactively command))))

(defun breeze-contains-space-or-capital (str)
  "Return nil if STR does not contain space or capital letters.
Othewise, return the position of the character."
  (let ((case-fold-search nil)) ;; make it case-sensitive
    (string-match "[[:blank:][:upper:]]" str)))

(defun breeze-sanitize-symbol (name)
  "Wrap a symbol NAME with pipe \"|\" symbol, if needed."
  (if (breeze-contains-space-or-capital name)
      (concat "|" name "|")
    name))


;;; Insertion commands (mostly skeletons)

(define-skeleton breeze-insert-defpackage
  "Skeleton for a CL package"
  ""
  (let ((package-name (breeze-sanitize-symbol (skeleton-read "Package name: ")))
	(nicknames (loop for nickname = (skeleton-read "Package nickname: ")
			 while (not (zerop (length nickname)))
			 collect (format ":%s" nickname))))
    (concat
     "(in-package #:common-lisp-user)\n\n"
     "(defpackage #:" package-name "\n"
     (when nicknames
       (concat "  " (prin1-to-string `(:nicknames ,@nicknames)) "\n"))
     "  (:use :cl))\n\n"
     "(in-package #:" package-name ")\n\n")))

(define-skeleton breeze-insert-defun
  "Insert a defun form."
  "" ;; Empty prompt. Ignored/
  > "(defun " (skeleton-read "Function name: ") " (" ("Enter an argument name: " str " ") ")" \n
  > _ ")")

(define-skeleton breeze-insert-defmacro
  "Insert a defvar form."
  "" ;; Empty prompt. Ignored/
  > "(defmacro " (skeleton-read "Function name: ") " (" ("Enter an argument name: " str " ") ")" \n
  > _ ")")

(define-skeleton breeze-insert-defvar
  "Insert a defvar form."
  "" ;; Empty prompt. Ignored/
  > "(defvar *" (skeleton-read "Name: ") "* " (skeleton-read "Initial value: ") \n
  > "\"" (skeleton-read "Documentation string: ") "\")")

(define-skeleton breeze-insert-defparameter
  "Insert a defvar form.")
(define-skeleton breeze-insert-let
  "Insert a defvar form.")

(define-skeleton breeze-insert-asdf
  "Skeleton for an asdf file."
  "" ;; Empty prompt. Ignored.
  "(defpackage #:" (setq v1 (skeleton-read "System name: ")) ".asd" \n
  > "(:use :cl :asdf))" \n
  \n
  > "(in-package #:" v1 ".asd)" \n
  \n
  > "(asdf:defsystem \"" v1 "\"" \n
  > ":description \"\"" \n
  > ":version \"0.1.0\"" \n
  > ":author \"" (skeleton-read "Author name: " breeze-default-author) "\"" \n
  > ":licence \"" (skeleton-read "Licence name: " breeze-default-licence) "\"" \n
  > ":depends-on ()" \n
  > ":serial t" \n
  > ":components" \n
  > "((:module \"src\"" \n
  > ":components ())" \n
  > "(:module \"tests\"" \n
  > ":components ())))")

(defun breeze-indent-defun-at-point ()
  "Indent the whole form without moving."
  (interactive)
  (save-excursion
    (slime-beginning-of-defun)
    (indent-sexp)))

(defun breeze-get-symbol-package (symbol)
  "SYMBOL must be a string.  Return a list with the package name and the symbol name."
  (cl-destructuring-bind (output value)
      (slime-eval `(swank:eval-and-grab-output
		    ,(format (format "%s"
				     `(let ((symbol (quote %s)))
					(check-type symbol symbol)
					(format t "\"~(~a ~a~)\""
						(package-name (symbol-package symbol))
						(symbol-name symbol))))
			     symbol)))

    (split-string output)))

;; (breeze-get-symbol-package "cl:find")
;; => ("common-lisp" "find")

(defun breeeze-import-from ()
  (interactive)
  "Assuming there's a defpackage. And paredit-mode is enabled."
  (let* ((symbol (slime-symbol-at-point))
	 (case-fold-search t)) ;; Case insensitive search
    (destructuring-bind (package-name symbol-name)
	(breeze-get-symbol-package symbol)
      (message "%s:%s" package-name symbol-name)
      (save-excursion
	(goto-char (point-min))
	(if (re-search-forward (concat "import-from[[:space:]:#]+" package-name))
	    (progn
	      (insert (format "\n#:%s" symbol-name))
	      (slime-beginning-of-defun)
	      (indent-sexp)
	      (slime-eval-defun))
	  ;; TODO else, search for defpackage, add it at the end
	  )))))


;; (slime-rex (VAR ...) (SEXP &optional PACKAGE THREAD) CLAUSES ...)

;; (slime-interactive-eval "(breeze/el:)")

;; (global-set-key
;;  (kbd "<f5>")
;;  (lambda ()
;;    (interactive)
;;    (slime-interactive-eval
;;     (concat "(breeze/el::insert-let "
;; 	    (replace-match "\\\""  "FIXEDCASE" "LITERAL")
;; 	    (slime-defun-at-point)
;; 	    "4"
;; 	    ")"))))



(defun breeze-insert ()
  "Choose someting to insert."
  (interactive)
  ;; TODO Filter the choices based on the context
  (breeze-choose-and-call-command
   "What do you want to insert? "
   '(("asdf:defsystem" breeze-insert-asdf)
     ("defpackage" breeze-insert-defpackage)
     ("defun" breeze-insert-defun)
     ("defvar" breeze-insert-defvar)
     ("defparameter" breeze-insert-defparameter)
     ("let" breeze-insert-let)
     ("defmacro" breeze-insert-defmacro)
     ;; TODO
     ;; defclass
     ;; slots
     ;; defgeneric
     ;; defmethod
     )))


;;; Abbrev

(define-abbrev-table 'breeze-mode-abbrev-table
  '(
    ("defmacro" "" breeze-insert-defmacro)
    ("defpackage" "" breeze-insert-defpackage)
    ("defparam" "" breeze-insert-defparameter)
    ("defsystem" "" breeze-insert-asdf)
    ("defun" "" breeze-insert-defun)
    ("defvar" "" breeze-insert-defvar)
  ))

(defun breeze-new-buffer ()
  (interactive)
  (switch-to-buffer-other-window
   (breeze-make-temporary-buffer))
  (breeze-mode)
  ;; (breeze-choose-major-mode)
  )



(defun breeze-quicklisp-local-project-directories ()
  "Get the list of quicklisp's local project directories."
  (car
   (read-from-string
    (cl-destructuring-bind (output value)
	(slime-eval `(swank:eval-and-grab-output
		      ,(format "%s" `(breeze/el:get-ql-local-project-directories))))
      value))))

(defun breeze-choose-local-project-directories ()
  "Let the user choose the directory if there's more than one."
  (first (breeze-quicklisp-local-project-directories)))

;; (breeze-choose-local-project-directories) 

(defun breeze-quickproject (name)
  "Create a project using quickproject."
  ;; (interactive)
  (interactive "sName of the project: ")
  (let (
	;; TODO Let the user choose a directory outside of quicklisp's local
	;; project directories.  See (read-directory-name "Directory: ").
	(directory (breeze-choose-local-project-directories))
	;; TODO Let the user choose
	(author breeze-default-author)
	(licence breeze-default-licence)
	;; TODO depends-on
	;; TODO include-copyright
	;; TODO template-directory
	;; TODO template-parameters
	)
    (slime-interactive-eval
     (concat
      "(breeze/el:make-project \"" directory name "\""
      " :author \"" author "\""
      " :license \"" licence "\""
      ")"))))

;; e.g. (breeze-quickproject "foo")


;;
(define-derived-mode breeze-mode slime-mode
  "Breeze mode."
  :lighter " sctch"
  :keymap breeze-mode-map)

(global-set-key (kbd "C-c s") 'breeze-insert)
(define-key slime-mode-map (kbd "C-M-q") 'breeze-indent-defun-at-point)

(provide 'breeze)
;;; breeze.el ends here
