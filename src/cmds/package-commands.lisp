(defpackage #:breeze.package-commands
  (:documentation "Package-related commands")
  (:use #:cl
        #:breeze.analysis
        #:breeze.command
        #:breeze.package
        #:breeze.workspace)
  (:import-from #:breeze.buffer
                #:current-package)
  (:export #:insert-defpackage
           #:insert-local-nicknames
           #:insert-in-package-cl-user
           #:previous-in-package
           #:goto-package-definition
           #:export-symbol-at-point
           #:import-symbols-for-top-level-form))

(in-package #:breeze.package-commands)


;;; Commands for inserting and modifying package-related forms

(define-command insert-defpackage ()
  "Insert a defpackage form."
  (declare (context :top-level))
  (let ((package-name
          (read-string
           "Name of the package: "
           :initial-input
           (infer-package-name-from-file (current-buffer-filename)))))
    (when (in-package-cl-user-p)
      (insert
       "(cl:in-package #:cl-user)~%~%"))
    (progn
      ;; TODO if nil (insert "(uiop:define-package ")
      (insert "(defpackage "))
    ;; TODO don't insert the (in-package ...) if it already exists
    ;; TODO add documentation "Tests for the package X"
    (insert
     "#:~a~
    ~%  (:documentation \"\")~
    ~%  (:use #:cl))~
    ~%~
    ~%(in-package #:~a)"
     package-name package-name)))

(define-command insert-local-nicknames ()
  "Insert local nicknames."
  (declare (context (:child-of :package-definition))) ; TODO
  (insert
   "(:local-nicknames ~{~a~^~%~})"
   (loop :for name = (read-string "Name of the package to alias: ")
         :while (plusp (length name))
         :for alias = (read-string "Alias of the package: ")
         :collect (format nil "(#:~a #:~a)" alias name))))

(define-command insert-in-package-cl-user ()
  "Insert ~(cl:in-package #:cl-user)~."
  (declare (context :top-level))
  (insert "(cl:in-package #:cl-user)"))

(define-command previous-in-package ()
  "Go to the nearest previous ~cl:in-package~ form."
  ;; TODO current-package-node
  (let (($in-package-node (current-package (current-buffer))))
    (cond
      ($in-package-node
       (goto-char (start $in-package-node)))
      (t
       ;; TODO wrap around? (that doesn't seem useful)
       (message "No in-package form found before point.")))))

;; TODO export
(define-command goto-package-definition ()
  "Goto package definition (of the current package)."
  'TODO
  )

(define-command export-symbol-at-point ()
  "Export symbol at point."
  'TODO
  )

;; TODO map tokens, find all symbols in source package, filter based
;; on target package, generate #:import-from forms
(define-command import-symbols-for-top-level-form ()
  "Import symbols for top level form."
  ;; TODO use current-top-level (which is TODO)
  (let (($top (top-level-node-iterator (node-iterator (current-buffer))))
        (source-package (current-package (current-buffer)))
        symbols)
    ;; map over all tokens
    (map-subtree-preorder
     $top
     (lambda ($n &aux (node (value $n)))
       (when (and (token-node-p node)
                  (not (package-marker node)))
         ;; find the corresponding symbol in the current package
         (let ((symbol (find-symbol (name node) source-package)))
           (if symbol
               (let ((home (symbol-package symbol)))
                 (when (and (not (eq home source-package))
                            ;; TODO not in a :used package

                            )
                   (push symbol symbols)))
               (message "Symbol ~s not found in source-package ~s"
                        (name node)
                        source-package))))))
    (message "Current package: ~s~%~{ - ~a~^~%~}"
             source-package
             (mapcar #'breeze.string:symbol-package-qualified-name symbols))))

;; TODO linter rules for defpackage (eg use #:)
;; TODO quickinsert for import-from/export
