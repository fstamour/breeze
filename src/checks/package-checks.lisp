(defpackage #:breeze.package-checks
  (:documentation "Package-related checks.")
  (:use #:cl #:breeze.generics)
  (:import-from #:alexandria
                #:when-let
                #:when-let*)
  (:import-from #:breeze.checks
                #:defcheck)
  (:import-from #:breeze.package
                #:in-package-node-p)
  (:import-from #:breeze.buffer
                #:current-package)
  (:import-from #:breeze.parser
                #:node-string-designator))

(in-package #:breeze.package-checks)

;; TODO detect #:package:symbol => package:symbol
;; TODO warn about defpackage without a (:use)  (it's not portable)
;; TODO detect when a qualified symbol could use a local-nickname instead of a longer package-prefix

#++
(defun check-in-package ()
  "Make sure the previous in-package form desginates a package that can
be found. If it's not the case (e.g. because the user forgot to define
a package and/or evaluate the form that defines the package) then show
a message and stop the current command."
  (let (($package (current-package-node (current-buffer))))
    (if $package
        (let ((package-name (node-string-designator $package)))
          (unless (find-package package-name)
            (message "The nearest in-package form designates a package that doesn't exists: ~s"
                     package-name)
            (return-from-command)))
        (;; TODO message no (in-package ...) found...
         (return-from-command)))))

(defcheck warn-undefined-in-package (node-iterator)
  :top-level-p t
  :livep t
  (when-let* ((package-designator-node (in-package-node-p node-iterator))
              (package-name (node-string-designator package-designator-node)))
    (unless (find-package package-name)
      (breeze.diagnostics:node-style-warning
       node-iterator
       (format nil "Package ~s is not currently defined." package-name)))))

;; TODO tests!
;; TODO make it work out-of-image
(defun token-available-in-package-p (token package)
  (when package
    (when-let* ((package-prefix (breeze.parser:package-prefix token))
                (token-package (find-package package-prefix)))
      (or (eq package token-package)
          ;; lookup the symbol in both packages and check if it's the same symbol.
          (let* ((name (name token))
                 (symbol-in-package (find-symbol name package))
                 (symbol-in-token-package (find-symbol name token-package)))
            (and symbol-in-package
                 symbol-in-token-package
                 (eq symbol-in-package symbol-in-token-package)))))))

;; TODO tests!
;; TODO make it work out-of-image
(defun token-external-p (token)
  (when-let* ((symbol-name (name token))
              (package-name (breeze.parser:package-prefix token))
              (package (find-package package-name)))
    (multiple-value-bind (symbol status)
        (find-symbol (name token) package)
      (declare (ignore symbol))
      (eq :external status))))

;; TODO tests!!!
(defcheck warn-unnecessary-or-internal-package-prefix
    (node-iterator &key buffer)
  :node-type breeze.parser:token
  "Warn about symbols that have a package prefix but that is redundant in the current package, allow to automatically remove the unnecessary package prefix.

For example:

(in-package #:foo)
(foo:bar)
 ^^^^
Unnecessary package prefix.

(foo::baz)
 ^^^^^
Unnecessary package prefix.

Also warns about using \"possibly internal\" symbols (symbols with 2
package markers like ~foo::baz~).

(in-package #:a)

(b::public)
 ^^^
Unnecessary double package marker: the symbol ~s is external.

(b::private)
 ^^^

Avoid using internal symbols."
  (let ((token (breeze.parser:value node-iterator)))
    (cond
      ((token-available-in-package-p
        token
        (current-package buffer (start node-iterator)))
       (breeze.diagnostics:node-style-warning
        node-iterator
        (format nil "Unnecessary package prefix")
        :replacement (breeze.parse-tree:token-raw-name node-iterator)))
      ((breeze.parser:token-possibly-internal-p token)
       ;; TODO check if the symbol is external
       (if (token-external-p token)
           (breeze.diagnostics:node-style-warning
            node-iterator
            (format nil "Unnecessary double package marker: the symbol ~s is external."
                    (name token))
            :replacement (format nil "~a:~a"
                                 (breeze.parse-tree:token-raw-package-prefix node-iterator)
                                 (breeze.parse-tree:token-raw-name node-iterator)))
           (breeze.diagnostics:node-style-warning
            node-iterator
            (format nil "Avoid using internal symbols.")))))))
