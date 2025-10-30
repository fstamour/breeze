(in-package #:common-lisp-user)

(defpackage #:breeze.test.documentation
  (:use :cl #:breeze.documentation)
  (:import-from #:breeze.dogfood
                #:breeze-relative-pathname
                #:find-breeze-packages)
  (:import-from #:parachute
                #:define-test
                #:define-test+run
                #:is
                #:true
                #:false
                #:finish))

(in-package #:breeze.test.documentation)

(defun find-undocumented-symbols-in-dummy-package ()
  ""
  (labels ((format-symbol (symbol)
             (format nil "~a:~a"
                     (package-name (symbol-package symbol))
                     (symbol-name symbol)))
           (sort-undocumented (undocumented-list)
             (sort undocumented-list
                   #'(lambda (a b)
                       (destructuring-bind (a-what a-symbol) a
                         (destructuring-bind (b-what b-symbol) b
                           (if (eq a-what b-what)
                               (string< (format-symbol a-symbol)
                                        (format-symbol b-symbol))
                               (string< (symbol-name a-what)
                                        (symbol-name b-what)))))))))
    ;; Sort
    (sort-undocumented
     ;; Keep only the first 2 element of each result
     (mapcar #'(lambda (undocumented)
                 (destructuring-bind (what symbol &rest _)
                     undocumented
                   (declare (ignore _))
                   (list what symbol)))
             ;; Find in Dummy package
             (find-undocumented-symbols 'breeze.dummy.test)))))

(define-test find-undocumented-symbols
  (let ((undocumented-symbols (find-undocumented-symbols-in-dummy-package)))
    (is
     equal undocumented-symbols
     '((:function breeze.dummy.test:function-undocumented)
       (:generic-method breeze.dummy.test:generic-function-undocumented)
       (:method breeze.dummy.test:another-generic-function)
       (:method breeze.dummy.test:generic-function-undocumented)
       (:package "BREEZE.DUMMY.TEST")
       (:special-variable breeze.dummy.test:*bound-variable-undocumented*)
       (:special-variable breeze.dummy.test:*unbound-variable-undocumented*)))))


;;;; Updating docs/emacs_integration.org automatically

(defun parse-org-mode-table (table)
  ;; split by lines
  ;; split by |
  ;; remove empty parts (this leaves only the cells)
  ;; trim the cells
  (let* ((lines (breeze.string:split-by-newline table)))
    (mapcar (lambda (line)
              (mapcar (lambda (cell)
                        (string-trim '(#\Space) cell))
                      (remove-if (lambda (part)
                                   (zerop (length part)))
                                 (uiop:split-string line :separator '(#\|)))))
            lines)))


(define-test+run parse-org-mode-table
  (is equalp '(("Command" "Description" "Default Key")
               ("-------------------------+-----------------------------------------------------------------------------------------------+-------------")
               ("breeze-quickfix"
                "Choose from a list of commands applicable to the current context."
                "`C-.`")
               ("breeze-capture"
                "Quickly create a lisp file in a pre-determined directory."
                "")
               ("breeze-scaffold-project"
                "Interactively create a project in `quicklisp`'s `local-projects` folder using `quickproject`."
                ""))
      (parse-org-mode-table
       "| Command                 | Description                                                                                   | Default Key |
|-------------------------+-----------------------------------------------------------------------------------------------+-------------|
| breeze-quickfix         | Choose from a list of commands applicable to the current context.                             | `C-.`       |
| breeze-capture          | Quickly create a lisp file in a pre-determined directory.                                     |             |
| breeze-scaffold-project | Interactively create a project in `quicklisp`'s `local-projects` folder using `quickproject`. |             ")))

(defun find-and-read-emacs-integration-docs ()
  "Find and read the \"emacs integration\" org-mode file under \"docs/\"."
  (let ((org-file (breeze-relative-pathname "docs/emacs_integration.org")))
    (true org-file "Should be able to compute emacs_integration.org's path.")
    (true (probe-file org-file) "The file ~s should exists." org-file)
    (let ((content (alexandria:read-file-into-string org-file)))
      (true content "The content of ~s should not be null." org-file)
      (values org-file content))))

(defun find-and-parse-commands-table (org-file content)
  "Find and parse the org-mode table that lists all the commands."
  (let* ((header-needle "* Commands and default keymap")
         (header (search header-needle content))
         (table-start-needle "| Command")
         (table-start (and header (search table-start-needle content :start2 header)))
         (table-end-needle (format nil "|~%~%"))
         (table-end (and table-start (search table-end-needle content :start2 table-start)))
         (table (and table-start table-end
                     (subseq content table-start table-end))))
    (true header "Should be able to find the header ~s in the file ~s"
          header-needle org-file)
    (true table-start "Should be able to find the table that starts with ~s after the header ~s in the file ~s"
          table-start-needle header-needle org-file)
    (true table-end "Should be able to find the end of the table (~s)."
          table-end-needle)
    (true table
          "Should be able to extract the table from the file ~s" org-file)
    (values table-start table-end table)))

(defun destructure-command-table (table)
  "Validate that the commands table has the expected shape."
  (let* ((expected-header '("Command" "Description" "Default Key"))
         (number-of-columns (length expected-header))
         (cells (parse-org-mode-table table))
         (table-header (first cells))
         (table-header-separator (second cells))
         (table-content (cddr cells)))
    (is equal expected-header
        table-header
        "Unexpected table header")
    (is = 1 (length table-header-separator)
        "There can be only one table header separator.")
    (true (every (lambda (c)
                   (member c '(#\+ #\-) :test #'char=))
                 (first table-header-separator))
          "The table header separator should be made up only of the + and - characters.")
    (loop
      :for i from 2
      :for row :in table-content
      :do (is = number-of-columns (length row)
              "The row number ~d has ~d columns, but it's supposed to have ~d,~%The row:~%~s"
              i (length row) number-of-columns row))
    table-content))

(defun render-command-table-row (row column-widths stream &key
                                                            (sep #\|)
                                                            (space #\Space))
  (write-char #\| stream)
  (loop
    :for cell :in row
    :for cell-left :on row
    :for width :in column-widths
    :do
       (write-char space stream)
       (when cell
         (write-string cell stream))
       (dotimes (i (- width (length cell)))
         (write-char space stream))
       (when (cdr cell-left)
         (write-char sep stream)))
  (write-char #\| stream)
  (fresh-line stream))

(define-test+run render-command-table-row
  (is string=
      "| a | b    | c           |
"
      (with-output-to-string (s)
        (render-command-table-row '("a" "b" "c") '(2 5 12) s)))
  (is string=
      "|~a~&~b~~~~&~c~~~~~~~~~~~|
"
      (with-output-to-string (s)
        (render-command-table-row '("a" "b" "c") '(2 5 12) s :sep #\& :space #\~)))
  (is string=
      "|  a |      | -  |
"
      (with-output-to-string (s)
        (render-command-table-row '(" a " nil "-") '(3 5 3) s))))

(defun render-commands-table (table-content stream)
  (let* ((header '("Command" "Description" "Default Key"))
         (number-of-columns (length header))
         (colum-widths
           (loop :for i :below number-of-columns
                 :collect (reduce #'max (append (list header) table-content)
                                  :key (lambda (row)
                                         ;; +2 for the padding
                                         (+ 2 (length (nth i row))))))))
    (render-command-table-row header colum-widths stream)
    (render-command-table-row (mapcar #'null header)
                              colum-widths stream
                              :sep #\+ :space #\-)
    (dolist (row table-content)
      (render-command-table-row row
                                colum-widths
                                stream))))

(define-test+run render-commands-table
  (is string= "| Command  | Description  | Default Key  |
|----------+--------------+--------------|
| a        | ccc          | bb           |
| ab       | acacacafc    | baba         |
"
      (with-output-to-string (s)
        (render-commands-table '(("a" "ccc" "bb")
                                 ("ab" "acacacafc" "baba"))
                               s))))

;;; Putting it all together:
(define-test+run update-emacs-integration-docs
  (multiple-value-bind (org-file content)
      (finish (find-and-read-emacs-integration-docs))
    (multiple-value-bind (table-start table-end table)
        (finish (find-and-parse-commands-table org-file content))
      (declare (ignorable table-start table-end))
      (let ((table-content (finish (destructure-command-table table))))
        (breeze.logging:log-debug "~&~s" table-content)
        ;; Make sure that all command names in the table has a corresponding command symbol in the current image.
        (let* ((all-commands
                 ;; TODO this is a hack, we should filter out "non interactive" commands
                 (remove 'breeze.lint:lint
                         (breeze.command:list-all-commands)) #| this is a list of symbols |#)
               (commands-in-the-table (mapcar #'car table-content))
               (command->key-binding)
               (command-name->symbol
                 (loop
                   ;; see `expected-header'
                   :for (command-string description key-binding) :in table-content
                   :for command-name := (string-upcase (breeze.string:without-prefix "breeze-" command-string))
                   :for command-symbol := (car (or
                                                (member command-name all-commands :test #'string=)
                                                (member (string-upcase command-string) all-commands :test #'string=)))
                   :do
                      (true command-symbol
                            "Couldn't find a symbol corresponding to the command name ~s (~s)"
                            command-string command-name)
                      (when command-symbol
                        (let ((generated-name (breeze.command::command-name-for-editor command-symbol)))
                          (is string= command-string generated-name
                              "The name of the command ~s in the org-mode table doesn't match the generated name~%got:~%  ~s~%expected:~%  ~s"
                              command-symbol
                              command-string
                              generated-name)))
                      (unless (zerop (length key-binding))
                        (push (cons command-symbol key-binding) command->key-binding))
                   :collect (cons command-string command-symbol))))
          ;; keep that alist in the same order than "table-content",
          ;; it's easier to debug that way
          (setf command->key-binding (nreverse command->key-binding))
          ;; TODO could be nice to validate (or even update!) the key
          ;; bindings in breeze.el
          (breeze.logging:log-debug "~&All the commands currently in the org-mode table:~&  ~s" commands-in-the-table)
          (breeze.logging:log-debug "~&The mapping from command name to symbol:~&  ~s" command-name->symbol)
          (breeze.logging:log-debug "~&The key bindings in the org-mode table:~&  ~s" command->key-binding)
          ;; TODO filter out unexported command, or test commands
          (let* ((sorted-commands
                   (sort all-commands
                         (lambda (a b)
                           ;; if command was already present, in the table keep it first
                           (let ((a-present-p (member a command-name->symbol
                                                      :key #'cdr))
                                 (b-present-p (member b command-name->symbol
                                                      :key #'cdr)))
                             (cond
                               ((and a-present-p b-present-p)
                                ;; keep the existing order
                                (< (length b-present-p) (length a-present-p)))
                               (a-present-p t)
                               (b-present-p nil)
                               ;; otherwise, alphabetical
                               (t
                                (string< a b)))))))
                 (new-table-content
                   (loop
                     :for command :in sorted-commands
                     :for name := (breeze.command::command-name-for-editor command)
                     :for description := (breeze.string:summarize (breeze.command:command-docstring command))
                     :for key-binding := (or (alexandria:assoc-value command->key-binding command) "")
                     :collect (list name description key-binding))))
            (breeze.logging:log-debug "~&~s" new-table-content)
            (let* ((new-table
                     (with-output-to-string (s)
                       (render-commands-table new-table-content s)))
                   (prefix (subseq content 0 table-start))
                   (suffix (subseq content (+ 2 table-end)))
                   (new-content (list prefix new-table suffix)))
              (alexandria:with-output-to-file (out org-file
                                                   :if-does-not-exist :error
                                                   :if-exists :supersede)
                (dolist (part new-content)
                  (write-string part out))))))))))

;;;; TODO try to extract breeze.el's docstring, to include into the
;;;; documentation.
#+=((defun read-breeze.el ()
      (let ((eof (gensym)))
        (alexandria:with-input-from-file
            (input
             (merge-pathnames "src/breeze.el"
                              (breeze.asdf:system-directory 'breeze)))
          (loop for form = (read input nil eof)
                until (eq form eof)
                collect form))))

    (let ((forms ))
      (loop for form in (read-breeze.el)
            when (and (listp form)
                      (eq 'defun (car form)))
              collect (second form))))


(define-test+run "update or create per-command documentation files"
  (loop :for command :in (breeze.command:list-all-commands)))


(define-test generate-documentation
  (let ((root (breeze-relative-pathname "docs/")))
    (with-output-to-string (*trace-output*)
      (breeze.documentation::generate-documentation
       root
       (find-breeze-packages))
      (breeze.report::render
       (make-instance
        'breeze.report:report
        :systems `((breeze
                    ;; Include scratch files
                    :extra-files ,(directory
                                   (make-pathname
                                    :directory
                                    `(,@(pathname-directory
                                         (breeze-relative-pathname
                                          "scratch-files/"))
                                        :wild-inferiors)
                                    :name :wild
                                    :type "lisp"))))
        :output-dir root)))))
