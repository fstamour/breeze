;;; package -- breeze integration with emacs

;;; Commentary:
;;
;; Features:
;; - snippets with abbrevs
;; - capture
;; - interactive make-project

;;; Code:


;;; Scratch section
;; (defun breeze-eval-defun )



;;; Requires
(require 'cl-lib)
(require 'slime)


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

(defcustom breeze-capture-folder "~/breeze-capture"
  "The folder where to save scratch files."
  :type 'directory
  :group 'breeze)

(defcustom breeze-capture-template
"

(ql:quickload '(alexandria))

;; Make it easier to debug
(declaim (optimize (speed 0) (safety 3) (debug 3)))

#|

Goal:

Motivation:

First lead:

|#

"
  "The fixed template to insert in a new scratch file."
  :type 'string
  :group 'breeze)



;;; Variables

(defvar breeze-mode-map
  (make-sparse-keymap)
  "Keymap for breeze-mode")

(defvar breeze/status
  "OK"
  "TBD Status of the current project.")


;;; Integration with lisp listener

(defun breeze-eval (string)
  (slime-eval `(swank:eval-and-grab-output ,string)))

(defun breeze-eval-value-only (string)
  (cl-destructuring-bind (output value)
      (breeze-eval string)
    value))

(defun breeze-eval-predicate (string)
  (cl-equalp "t" (breeze-eval-value-only string)))

;; (and
;;  (eq t (breeze-eval-predicate "t"))
;;  (eq t (breeze-eval-predicate "T"))
;;  (eq nil (breeze-eval-predicate "nil"))
;;  (eq nil (breeze-eval-predicate "NIL")))

(defun breeze-eval-list (string)
  (let ((list (car
	       (read-from-string
		(breeze-eval-value-only string)))))
    (when (listp list)
      list)))

(defun breeze-interactive-eval (string)
  (slime-eval `(swank:interactive-eval ,string)))


;;; Prerequisites checks

(cl-defun breeze/check-if-slime-is-connected (&optional verbosep)
  "Make sure that slime is connected, signals an error otherwise."
  (if (slime-current-connection)
      (when verbosep
	(message "Slime already started."))
    (progn
      (when verbosep
	(message "Starting slime..."))
      (slime))))

(defun breeze/validate-if-breeze-package-exists ()
  "Returns true if the package \"breeze\" exists in the inferior-lisp."
  (breeze-eval-predicate "(and (or (find-package \"BREEZE\") (find-package \"breeze\")) t)"))

;; (breeze/validate-if-breeze-package-exists)

(defvar breeze/breeze.el load-file-name
  "Path to \"breeze.el\".")

(defun breeze/system-definition ()
  "Get the path to \"breeze.asd\" based on the (load-time) location of \"breeze.el\"."
  (expand-file-name
   (concat
    (file-name-directory
     breeze/breeze.el)
    "../breeze.asd")))

(defun breeze/ensure-breeze (&optional verbosep)
  "Make sure that breeze is loaded in swank."
  (unless (breeze/validate-if-breeze-package-exists)
    (when verbosep
      (message "Loading breeze's system..."))
    (breeze-interactive-eval
     (format "(progn (load \"%s\") (require 'breeze))"
	     (breeze/system-definition))))
  (when verbosep
    (message "Breeze loaded in inferior-lisp.")))

;; (breeze/ensure-breeze)

;; See slime--setup-contribs, I named this breeze-init so it _could_ be added to slime-contrib,
;; I haven't tested it yet though.


(defmacro breeze/with-slime (&rest body)
  `(progn
     (breeze-init)
     ,@body))

;; (macroexpand-1 '(breeze/with-slime (print 42)))

;;; Utilities

(cl-defun breeze-command-description (command &optional shortp)
  "Take a symbol COMMAND and return a user-friendly description (to use with completions)."
  (let ((description (car
		      (cl-member
		       command
		       '((breeze-insert-asdf "asdf:defsystem" "insert %s")
			 (breeze-insert-defpackage "defpackage" "insert %s")
			 (breeze-insert-defun "defun" "insert %s")
			 (breeze-insert-defvar "defvar" "insert %s")
			 (breeze-insert-defparameter "defparameter" "insert %s")
			 (breeze-insert-let "let" "insert %s")
			 (breeze-insert-defmacro "defmacro" "insert %s")
			 (breeze-insert-loop-clause-for-hash "loop clause: hash-table iteration" "insert %s")
			 (breeze-insert-loop-clause-for-on-list "loop clause: iterate ON list" "insert %s")
			 (breeze-insert-loop-clause-for-in-list "loop clause: iterate IN list" "insert %s"))
		       :key #'car))))
    (when description
      (if shortp
	  (second description)
	(format (third description) (second description))))))

;; (breeze-command-description 'breeze-insert-asdf t)
;; => "asdf:defsystem"
;; (breeze-command-description 'breeze-insert-asdf)
;; => "insert asdf:defsystem"

(cl-defun breeze-command-ensure-alist (commands &optional shortp)
  "Transform a list of command symbols COMMANDS to an alist (symbol . description)"
  (mapcar #'(lambda (command)
	      (if (consp command)
		  command
		(cons (breeze-command-description command shortp) command)))
	  commands))
;; (breeze-command-ensure-alist '(breeze-insert-asdf breeze-insert-defpackage))
;; => (("insert asdf:defsystem" . breeze-insert-asdf) ("insert defpackage" . breeze-insert-defpackage))

(cl-defun breeze-choose-and-call-command (prompt commands &key allow-other-p)
  "Let the user choose a command to run from a list."
  (let* ((commands-alist (breeze-command-ensure-alist commands t))
	 (choice (completing-read prompt (mapcar #'first commands-alist)))
	 (command (cdr (assoc choice commands-alist))))
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

(defun breeze-infer-project-name ()
  "Try to find the project's name"
  ;; TODO look at current *package*
  ;; TODO look at git's remotes' url
  ;; TODO look at .asd file <===
  (when (vc-root-dir) ;; TODO this doesn't always work :(
    (file-name-nondirectory
     (directory-file-name
      (vc-root-dir)))))

;; (breeze-infer-project-name)

(defun breeze-is-in-test-directory ()
  "Try to figure out if the current file is in the test directory."
  (when buffer-file-name
    (member
     (file-name-nondirectory
      (directory-file-name
       (file-name-directory
	buffer-file-name)))
     '("test" "tests" "t"))))

(defun breeze-infer-package-name-from-file (&optional file-name)
  "Given a FILE-NAME or the buffer's file name, infer a proper package name."
  (let ((project (breeze-infer-project-name))
	(testp (breeze-is-in-test-directory))
	(result
	 (file-name-base (or file-name buffer-file-name))))
    (message "project \"%s\"" project)
    (when project
      (setf result (concat project "." result)))
    (when testp
      (setf result (concat result ".test")))
    result))

(define-skeleton breeze-insert-defpackage
  "Skeleton for a CL package"
  "" ;; Empty prompt. Ignored.
  (let ((package-name
	 (breeze-sanitize-symbol
	  (skeleton-read "Package name: "
			 (if buffer-file-name
			     (breeze-infer-package-name-from-file)
			   ""))))
	(nicknames
	 (cl-loop for nickname = (string-trim (skeleton-read "Package nickname: "))
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
  "" ;; Empty prompt. Ignored.
  > "(defun " (skeleton-read "Function name: ")
  " (" ("Enter an > argument name: "
	str " ")
  (fixup-whitespace) ")" \n _ ")")

(define-skeleton breeze-insert-defmacro
  "Insert a defmacro form."
  "" ;; Empty prompt. Ignored.
  > "(defmacro " (skeleton-read "Macro name: ")
  " (" ("Enter
  > an argument name: " str " ") (fixup-whitespace) ")" \n _ ")")

(define-skeleton breeze-insert-defvar
  "Insert a defvar form."
  "" ;; Empty prompt. Ignored.
  > "(defvar *"
  (skeleton-read "Name: ") "* "
  (skeleton-read "Initial value: ") \n
  > "\"" (skeleton-read "Documentation string: ") "\")")

(define-skeleton breeze-insert-defparameter
  "Insert a defparameter form."
  "" ;; Empty prompt. Ignored.
  > "(defparameter *" (skeleton-read "Name: ") "* "
  (skeleton-read "Initial value: ") \n
  > "\"" (skeleton-read "Documentation string: ") "\")")

(define-skeleton breeze-insert-let
  "Insert a let defvar form."
  "" ;; Empty prompt. Ignored.
  > "(let ((" @ ")))")

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
  ;; TODO Use emacs' built-in user-name?
  > ":author \"" (skeleton-read "Author name: " breeze-default-author) "\"" \n
  > ":licence \"" (skeleton-read "Licence name: " breeze-default-licence) "\"" \n
  > ":depends-on ()" \n
  > ":serial t" \n
  > ":components" \n
  > "((:module \"src\"" \n
  > ":components ())" \n
  > "(:module \"tests\"" \n
  > ":components ())))")

(define-skeleton breeze-insert-loop-clause-for-hash
  "Skeleton to insert a loop clause to iterate on a hash-table."
  "" ;; Empty prompt. Ignored.
  > " :for "
  (skeleton-read "Enter the variable name for the key: ")
  " :being :the :hash-key :of "
  (skeleton-read "Enter the name of the hash-table: ")
  " :using (hash-value "
  (skeleton-read "Enter the variable name for the value: ")
  ")")

(define-skeleton breeze-insert-loop-clause-for-on-list
  "Skeleton to insert a loop clause to iterate on a list."
  "" ;; Empty prompt. Ignored.
  > " :for "
  (skeleton-read "Enter the variable name for the iterator: ")
  " :on "
  (skeleton-read "Enter the name of the list: "))

(define-skeleton breeze-insert-loop-clause-for-in-list
  "Skeleton to insert a loop clause to iterate on a list."
  "" ;; Empty prompt. Ignored.
  > " :for "
  (skeleton-read "Enter the variable name for the iterator: ")
  " :in "
  (skeleton-read "Enter the name of the list: "))

(defun breeze-new-buffer-p ()
  "Check if the current buffer is \"new\"."
  (or
   ;; Check the size of the buffer
   (zerop (buffer-size))
   ;; Check if it's all whitespaces
   (zerop (length (string-trim (buffer-string))))
   ;; TODO Check if it's all comments
   ;; TODO See the function "bobp" (beginnig-of-buffer-p)
   ))

;; WIP
(defun breeze-buffer-has-defpackage ()
  "Check if a buffer already contains a defpackage form.")

;; WIP
(defun breeze-in-loop ()
  "Check if it's a valid place to add a loop clause.")

(defun breeze-insert ()
  "Choose someting to insert."
  (interactive)
  ;; TODO filter the choices based on the context
  (if (or
       (= 1 (point))
       (breeze-new-buffer-p))
      (call-interactively 'breeze-insert-defpackage)
    (breeze-choose-and-call-command
     "What do you want to insert? "
     '(breeze-insert-asdf
       breeze-insert-defpackage
       breeze-insert-defun
       breeze-insert-defvar
       breeze-insert-defparameter
       breeze-insert-let
       breeze-insert-defmacro
       breeze-insert-loop-clause-for-hash
       breeze-insert-loop-clause-for-on-list
       breeze-insert-loop-clause-for-in-list
       ;; TODO
       ;; defclass
       ;; slots
       ;; defgeneric
       ;; defmethod
       ))))


;;; abbrev

;; P.S. This doesn't work anymore because (I think) breeze-mode is a
;; minor mode.
(progn
  (when (boundp 'breeze-mode-abbrev-table)
    (clear-abbrev-table breeze-mode-abbrev-table))
  (define-abbrev-table 'breeze-mode-abbrev-table
    '(("defmacro1" "" breeze-insert-defmacro)
      ("defpackage1" "" breeze-insert-defpackage)
      ("defparam1" "" breeze-insert-defparameter)
      ("defsystem1" "" breeze-insert-asdf)
      ("defun1" "" breeze-insert-defun)
      ("defvar1" "" breeze-insert-defvar))))


;;; code modification

(defun breeze-indent-defun-at-point ()
  "Indent the whole form without moving the point."
  (interactive)
  (if (zerop (current-column))
      (indent-sexp)
    (save-excursion
      (slime-beginning-of-defun)
      (indent-sexp))))

(defun breeze-get-symbol-package (symbol)
  "SYMBOL must be a string.  Returns a list with the package name and the symbol name."
  (breeze/with-slime
   (cl-destructuring-bind (output value)
       (breeze-eval
	(format (format "%s"
			`(let ((symbol (quote %s)))
			   (check-type symbol symbol)
			   (format t "\"~(~a ~a~)\""
				   (package-name (symbol-package symbol))
				   (symbol-name symbol))))
		symbol))

     (split-string output))))

;; (breeze-get-symbol-package "cl:find")
;; => ("common-lisp" "find")

(defun breeze-add-import-from ()
  "Add or update defpackage to \"import-from\" the symbol at point.

   TODO it assumes there's a defpackage form in the current
   buffer, that it has an exisint \"import-from\" from and that
   paredit-mode is enabled."
  (interactive)
  (let ((symbol (slime-symbol-at-point))
	(case-fold-search t)) ;; case insensitive search
    (destructuring-bind (package-name symbol-name)
	(breeze-get-symbol-package symbol)
      ;; (message "%s:%s" package-name symbol-name)
      (save-excursion
	(goto-char (point-min)) ;; Go to the start of the buffer
	(if (re-search-forward (concat "import-from[[:space:]:#]+" package-name) nil t)
	    (progn
	      (insert (format "\n#:%s" symbol-name))
	      (slime-beginning-of-defun)
	      (indent-sexp)
	      (slime-eval-defun))
	  (progn
	    ;; Find the defpackage form
	    (re-search-forward "defpackage")
	    (slime-beginning-of-defun)
	    (forward-sexp)
	    (backward-char)
	    (insert (concat "\n(:import-from #:" package-name "\n#:" symbol-name ")"))
	    ))
	;; Evaluate the defpackage form
	(slime-eval-defun))
      ;; Replace package:symbol by symbol, in the current buffer
      (save-excursion
	(goto-char (point-min)) ;; Go to the start of the buffer
	(while (re-search-forward symbol nil t)
	  (replace-match symbol-name))))))


;; (slime-goto-package-source-definition "breeze")
;; (slime-goto-xref)

;; (slime-rex (var ...) (sexp &optional package thread) clauses ...)

;; (slime-interactive-eval "(breeze.swank:)")

;; (global-set-key
;;  (kbd "<f5>")
;;  (lambda ()
;;    (interactive)
;;    (slime-interactive-eval
;;     (concat "(breeze.swank::insert-let "
;; 	    (replace-match "\\\""  "fixedcase" "literal")
;; 	    (slime-defun-at-point)
;; 	    "4"
;; 	    ")"))))

;; Idea: (defun breeze-wrap-with-let-form ())

(defun breeze-move-form-into-let ()
  "Move the current form into the nearest parent \"let\" form."
  (interactive)
  (let* ((form (slime-sexp-at-point-or-error))
	 (new-variable)
	 ;; Find the position of the top-level form
	 (beginning-of-defun (if (zerop (current-column))
				 (point)
			       (save-excursion
				 (slime-beginning-of-defun)
				 (point))))
	 (case-fold-search t) ;; case insensitive search
	 ;; Find the position of the previous "let"
	 (let-point (save-excursion (re-search-backward "let"))))
    (save-excursion
      ;; Check if we found a parent let form
      (if (and let-point
	       (>= let-point beginning-of-defun))
	  (progn
	    (save-excursion
	      ;; Find the place to add the new binding
	      (re-search-backward "let")
	      (re-search-forward "(")
	      (save-excursion
		(insert (format "( %s)" form))
		;; Add a newline if necessary
		(if (eq (char-after) (string-to-char "("))
		    ;; also add a space to help indentation later
		    (insert "\n ")))
	      (forward-char)
	      ;; Ask the user to name the new-variable
	      (setq new-variable (read-string "Enter a new for the new-variable: "))
	      ;; Insert the name of the variable in the let form.
	      (insert new-variable))
	    ;; Replace the form at point by the new variable
	    (let ((start (point)))
	      (forward-sexp)
	      (delete-region start (point)))
	    (insert new-variable)
	    ;; reindent the whole top-level form
	    (slime-beginning-of-defun)
	    (indent-sexp))
	(message "Failed to find a parent let form.")))))


;;; Suggestions

(defun breeze-next ()
  "Ask breeze for suggestions on what to do next."
  (interactive)
  (cl-destructuring-bind (output value)
      (slime-eval `(swank:eval-and-grab-output
		    ;; TODO send more information to "next"
		    ;; What's the current buffer? does it visit a file?
		    ;; What's the current package? is it covered by breeze.user:*current-packages*
		    "(breeze.user:next)"))
    (message "%s" output)))

(defun breeze-current-paragraph ()
  (string-trim
   (save-mark-and-excursion
     (mark-paragraph)
     (buffer-substring (mark) (point)))))

(defun breeze-delete-paragraph ()
  (save-mark-and-excursion
    (mark-paragraph)
    (delete-region (point) (mark))))

(defun %breeze-expand-to-defuns (paragraph)
  (cl-loop for line in
	   (split-string paragraph "\n")
	   for parts = (split-string line)
	   collect
	   (format "(defun %s %s\n)\n" (car parts) (rest parts))))

(defun %breeze-expand-to-defuns ()
  "Takes a paragraph where each line describes a function, the
first word is the function's name and the rest are the
arguments. Use to quickly scaffold a bunch of functions."
  (interactive)
  (let ((paragraph (breeze-current-paragraph)))
    (breeze-delete-paragraph)
    (loop for form in (%breeze-expand-to-defuns paragraph)
	  do (insert form "\n"))))

(defun breeze-compute-buffer-args ()
  (apply 'format
	 ":buffer-name %s
          :buffer-file-name %s
          :buffer-string %s
          :point %s
          :point-min %s
          :point-max %s"
	 (mapcar #'prin1-to-string
		 (list
		  (buffer-name)
		  (buffer-file-name)
		  (buffer-substring-no-properties (point-min) (point-max))
		  (point)
		  (point-min)
		  (point-max)))))

(defun breeze-get-quickfix-suggestions ()
  (breeze-eval-list
   (format
    "(breeze.quickfix:quickfix %s)"
    (breeze-compute-buffer-args))))

(defun breeze-quickfix ()
  "Suggest valid actions to the user based on the point."
  (interactive)
  ;; TODO
  (let* ((suggestions (breeze-get-quickfix-suggestions))
	 (commands
	  (mapcar #'(lambda (command-name)
		      (let ((command (intern (downcase command-name))))
			(if (fboundp command)
			    command
			  (error "quickfix suggested an invalid command %s" command))))
		  suggestions)))
    (if suggestions
	(breeze-choose-and-call-command "Choose a quickfix to apply" commands)
      (message "No quickfixes found in this context."))))

(defun breeze-quickfix ()
  "SHADOWS the real breeze-quickfix, just for quick prototyping."
  (interactive)
  (cl-loop
   for i below 5
   for
   request = (breeze-eval-list
	      (format
	       "(breeze.quickfix::prototyping-stuff %s)"
	       (breeze-compute-buffer-args)))
   then (breeze-eval-list
	 (format
	  "(breeze.quickfix::prototyping-stuff2 %s)"
	  (prin1-to-string response)))
   while request
   for response = (pcase (car request)
		    ("choose"
		     (completing-read (second request)
				      (third request)))
		    ("read-string"
		     (read-string (second request)))
		    ("insert"
		     (goto-char (second request))
		     (apply #'insert (cddr request)))
		    ("insert-saving-excursion"
		     (save-excursion
		       (goto-char (second request))
		       (apply #'insert (cddr request))))
		    ("replace"		; TODO -saving-excursion variant
		     (cl-destructuring-bind
			 (point-from point-to replacement-stirng)
			 (cdr request)
		       ;; TODO
		       )))
   do (message "Resquest received: %s response to send %s"
	       (prin1-to-string request)
	       (prin1-to-string response))))


;;; code evaluation

(defun breeze-get-recently-evaluated-forms ()
  "Get recently evaluated forms from the server."
  (cl-destructuring-bind (output value)
      (slime-eval `(swank:eval-and-grab-output
		    "(breeze.swank:get-recent-interactively-evaluated-forms)"))
    (split-string output "\n")))

(defun breeze-reevaluate-form ()
  (interactive)
  (let ((form (completing-read  "Choose recently evaluated form: "
				(breeze-get-recently-evaluated-forms))))
    (when form
      (slime-interactive-eval form))))


;;; project scaffolding

(defun breeze-quicklisp-local-project-directories ()
  "Get the list of quicklisp's local project directories."
  (car
   (read-from-string
    (cl-destructuring-bind (output value)
	(slime-eval `(swank:eval-and-grab-output
		      ,(format "%s" `(breeze.swank:get-ql-local-project-directories))))
      value))))

(defun breeze-choose-local-project-directories ()
  "Let the user choose the directory if there's more than one."
  (let ((directories (breeze-quicklisp-local-project-directories)))
    (if (eq 1 (length directories))
	(first directories)
      (completing-read "Choose a local-project directory: " directories))))

;; (breeze-choose-local-project-directories)

(defun breeze-quickproject ()
  "Create a project named NAME using quickproject."
  (interactive)
  (let ((name (read-string "Name of the project: "))
	;; TODO let the user choose a directory outside of quicklisp's local
	;; project directories.  see (read-directory-name "directory: ").
	(directory (breeze-choose-local-project-directories))
	;; TODO let the user choose
	(author breeze-default-author)
	(licence breeze-default-licence)
	;; TODO depends-on
	;; TODO include-copyright
	;; TODO template-directory
	;; TODO template-parameters
	)
    (slime-interactive-eval
     (concat
      "(breeze.swank:make-project \"" directory name "\""
      " :author \"" author "\""
      " :license \"" licence "\""
      ")"))
    (message "\"%s\" created" (concat directory name "/"))
    (find-file (concat directory name "/README.md"))))

;; e.g. (breeze-quickproject "foo")


;;; capture

(defun breeze-list-lisp-files (directory)
  ;; just the name, no extension, no directory
  (loop for file in
	(directory-files directory)
	when (and (not (string-prefix-p "." file))
		  (string-suffix-p ".lisp" file))
	collect (file-name-sans-extension file)))


(define-skeleton breeze-insert-header-template
  "" ;; TODO docstring
  "" ;; empty prompt. ignored.
  \n
  "(ql:quickload '(alexandria))" \n
  \n
  ";; make it easier to debug" \n
  "(declaim (optimize (speed 0) (safety 3) (debug 3)))" \n
  \n
  "#|" \n
  \n
  "goal:" \n
  \n
  "motivation:" \n
  \n
  "first lead:" \n
  \n
  "|#")

(defun breeze-capture ()
  ;; TODO docstring
  (interactive)
  ;; TODO check if directory exists, creates it if not.
  ;;   (mkdir breeze-capture-folder)
  (let* ((name (completing-read
		"name of the file and package: "
		(breeze-list-lisp-files breeze-capture-folder)))
	 (file (concat breeze-capture-folder "/" name ".lisp"))
	 (file-exists (file-exists-p file)))
    (find-file file)
    (unless file-exists
      (breeze-insert-defpackage))))


;;; managing threads

;; TODO
;; breeze-kill-worker-thread


;;; Mode-line indicator

(defun breeze/set-status (new-status)
  "Update the status variable with NEW-STATUS and update the mode-line."
  (setf breeze/status new-status)
  (force-mode-line-update))

(defun breeze/configure-mode-line ()
  "Add breeze's status to the mode-line-format, if not already there."
  (interactive)
  (unless
      (cl-remove-if-not
       #'(lambda (el)
	   (and (listp el)
		(eq 'breeze/status (car el))))
       mode-line-format)
    (setf mode-line-format
	  (append mode-line-format
		  '((breeze/status  ("--> " breeze/status " <--")))))))


;;; mode

(define-minor-mode breeze-mode
  "Breeze mimor mode."
  :lighter " brz"
  :keymap breeze-mode-map)

;; Analoguous to org-insert-structure-template
(define-key breeze-mode-map (kbd "C-c C-,") 'breeze-insert)

;; Analoguous to org-goto
(define-key breeze-mode-map (kbd "C-c C-j") #'imenu)

;; Analoguous to Visual Studio Code's "quickfix"
(define-key breeze-mode-map (kbd "C-.") #'breeze-quickfix)

;; I think the reason I needed that was because of a conflict of keybindings,
;; paredit's M-q seems to do the job.
;; (define-key breeze-mode-map (kbd "C-M-q") 'breeze-indent-defun-at-point)

;; eval keymap - because we might want to keep an history
(defvar breeze/eval-map (make-sparse-keymap))
;; eval last expression
(define-key breeze-mode-map (kbd "C-c e") breeze/eval-map)
;; choose an expression from history to evaluate
(define-key breeze/eval-map (kbd "e") 'breeze-reevaluate-form)

(defun enable-breeze-mode ()
  "Enable breeze-mode."
  (interactive)
  (breeze-mode 1))

(defun disable-breeze-mode ()
  "Disable breeze-mode."
  (interactive)
  (breeze-mode -1))

(add-hook 'breeze-mode-hook 'breeze/configure-mode-line)
(add-hook 'breeze-mode-hook 'breeze-init)

(defun breeze ()
  "Start slime and breeze"
  (interactive)
  (breeze-init t))

(provide 'breeze)
;;; breeze.el ends here
