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

;; (setf debug-on-error t)
;; (setf debug-on-error nil)


;;; Requires
(require 'cl-lib)
(require 'slime)


;;; Groups and customs
(defgroup breeze nil
  "breeze")


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
  ;; TODO check breeze-cl-to-el-list
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
      (progn
	(when verbosep
	  (message "Slime already started."))
	t)
    (progn
      (when verbosep
	(message "Starting slime..."))
      (slime))))

(defun breeze/validate-if-package-exists (package)
  "Returns true if the package PACKAGE exists in the inferior-lisp."
  (breeze-eval-predicate
   (format "(and (or (find-package \"%s\") (find-package \"%s\")) t)"
	   (downcase package) (upcase package))))
;; (breeze/validate-if-package-exists "cl")

(defun breeze/validate-if-breeze-package-exists ()
  "Returns true if the package \"breeze\" exists in the inferior-lisp."
  (breeze/validate-if-package-exists "breeze"))

;; (breeze/validate-if-breeze-package-exists)

(defvar breeze/breeze.el load-file-name
  "Path to \"breeze.el\".")

(defun breeze/system-definition ()
  "Get the path to \"breeze.asd\" based on the (load-time) location
of \"breeze.el\"."
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
     (format "(progn (load \"%s\") (require 'breeze)
        (format t \"~&Breeze loaded!~%%\"))"
	     (breeze/system-definition))))
  (when verbosep
    (message "Breeze loaded in inferior-lisp.")))

;; (breeze/ensure-breeze)

;; See slime--setup-contribs, I named this breeze-init so it _could_
;; be added to slime-contrib,
;; I haven't tested it yet though.
(defun breeze-init (&optional verbosep)
  "Ensure that breeze is initialized correctly on swank's side."
  (interactive)
  (breeze/check-if-slime-is-connected)
  (breeze/ensure-breeze verbosep)
  (breeze-interactive-eval "(breeze.user::initialize)")
  (when verbosep
    (message "Breeze initialized.")))


(defmacro breeze/with-slime (&rest body)
  `(progn
     (breeze-init)
     ,@body))

;; (macroexpand-1 '(breeze/with-slime (print 42)))

;;; Utilities

(cl-defun breeze-command-description (command)
  "Take a symbol COMMAND and return a user-friendly description (to use with completions)."
  (let ((description (car
		      (cl-member
		       command
		       '(
			 ;; No more commands to describe!!!  They were
			 ;; all migrated to common-lisp
			 )
		       :key #'car))))
    (when description
      (format (second description)))))

(cl-defun breeze-command-ensure-alist (commands)
  "Transform a list of command symbols COMMANDS to an alist (symbol . description)"
  (mapcar #'(lambda (command)
	      (if (consp command)
		  command
		(cons (breeze-command-description command) command)))
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

;; TODO Move to common lisp
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

;; TODO Move to common lisp
(defun breeze-is-in-test-directory ()
  "Try to figure out if the current file is in the test directory."
  (when buffer-file-name
    (member
     (file-name-nondirectory
      (directory-file-name
       (file-name-directory
	buffer-file-name)))
     '("test" "tests" "t"))))

;; TODO Move to common lisp
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


;;; code modification

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




(defun breeze-cl-to-el-list (list)
  "Convert NIL to nil and T to t.
Common lisp often returns the symbols capitalized, but emacs
lisp's reader doesn't convert them."
  (if (eq list 'NIL)
      nil
    (mapcar #'(lambda (el)
		(case el
		  (NIL nil)
		  (T t)
		  (t el)))
	    list)))

(defun breeze-command-start (name)
  (breeze-cl-to-el-list
   (breeze-eval-list
    (format "(%s %s)" name (breeze-compute-buffer-args)))))

(defun breeze-command-continue (response send-response-p)
  (let ((request
	  (breeze-cl-to-el-list
	   (breeze-eval-list
	    (if send-response-p
		(format
		 "(breeze.command:call-next-callback %s)"
		 (prin1-to-string response))
	      "(breeze.command:call-next-callback)")))))
    (message "Breeze: request received: %s"
	     (prin1-to-string request))
    request))

(defun breeze-command-process-request (request)
  (pcase (car request)
    ("choose"
     (completing-read (second request)
		      (third request)))
    ("read-string"
     (read-string (second request)))
    ("insert-at"
     (cl-destructuring-bind (_ position string)
	 request
       (when (numberp position) (goto-char position))
       (insert string)))
    ("insert-at-saving-excursion"
     (cl-destructuring-bind (_ position string)
	 request
       (save-excursion
	 (when (numberp position) (goto-char position))
	 (insert string))))
    ("insert"
     (cl-destructuring-bind (_ string) request (insert string)))
    ("replace"	      ; TODO -saving-excursion variant
     (cl-destructuring-bind
	 (point-from point-to replacement-stirng)
	 (cdr request)
       ;; TODO
       ))
    ("backward-char"
     (backward-char (second request))
     ;; Had to do this hack so the cursor is positioned
     ;; correctly... probably because of aggressive-indent
     (funcall indent-line-function))))


(defun breeze-run-command (name)
  "Runs a \"breeze command\". TODO Improve this docstring."
  (interactive)
  (cl-loop
   ;; guards against infinite loop
   for i below 1000
   for
   ;; Start the command
   request = (breeze-command-start name)
   ;; Continue the command
   then (breeze-command-continue response send-response-p)
   ;; "Request" might be nil, if it is, we're done
   while request
   ;; Wheter or not we need to send arguments to the next callback.
   for send-response-p = (member (car request)
				 '("choose" "read-string"))
   ;; Process the command's request
   for response = (breeze-command-process-request request)
   ;; Log request and response (for debugging)
   do (message "Breeze: request received: %s response to send %s"
	       (prin1-to-string request)
	       (prin1-to-string response))))

(defun breeze-quickfix ()
  "SHADOWS the real breeze-quickfix, just for quick prototyping."
  (interactive)
  (case 1
    (1
     (breeze-run-command
      "breeze.refactor:quickfix"))
    (2
     (breeze-run-command
      "breeze.command::read-string-then-insert-test"))
    (3
     (breeze-run-command
      "breeze.refactor::insert-asdf"))
    (4
     (breeze-run-command
      "breeze.refactor::insert-defpackage"))))


;;; code evaluation

(defun breeze-get-recently-evaluated-forms ()
  "Get recently evaluated forms from the server."
  (cl-destructuring-bind (output value)
      (slime-eval `(swank:eval-and-grab-output
		    "(breeze.listener:get-recent-interactively-evaluated-forms)"))
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
		      ,(format "%s" `(breeze.listener:get-ql-local-project-directories))))
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
	(author user-full-name)
	(licence "Public domain")
	;; TODO depends-on
	;; TODO include-copyright
	;; TODO template-directory
	;; TODO template-parameters
	)
    (slime-interactive-eval
     (concat
      "(breeze.listener:make-project \"" directory name "\""
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

;; (breeze/configure-mode-line)


;;; mode

(define-minor-mode breeze-mode
  "Breeze mimor mode."
  :lighter " brz"
  :keymap breeze-mode-map)

;; Analoguous to org-insert-structure-template
;; (define-key breeze-mode-map (kbd "C-c C-,") 'breeze-insert)

;; Analoguous to org-goto
(define-key breeze-mode-map (kbd "C-c C-j") #'imenu)

;; Analoguous to Visual Studio Code's "quickfix"
(define-key breeze-mode-map (kbd "C-.") #'breeze-quickfix)

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
;; (add-hook 'breeze-mode-hook 'breeze-init)
;; TODO This should be in the users' config
;; (add-hook 'slime-lisp-mode-hook 'breeze-init)
(add-hook 'slime-connected-hook 'breeze-init)

(defun breeze ()
  "Start slime and breeze"
  (interactive)
  (breeze-init t))

(provide 'breeze)
;;; breeze.el ends here
