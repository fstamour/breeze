(cl:in-package #:common-lisp-user)

(uiop:define-package #:breeze.refactor
    (:documentation "Refactoring commands")
  (:use #:cl
        #:breeze.command
        #:breeze.analysis
        #:breeze.command-utils)
  (:import-from #:alexandria
                #:ends-with-subseq
                #:if-let
                #:when-let
                #:when-let*
                #:symbolicate
                #:lastcar)
  (:import-from #:breeze.utils
                #:before-last)
  (:import-from #:breeze.string
                #:trim-whitespace
                #:ensure-circumfix
                #:ensure-circumfixes)
  (:import-from #:breeze.indirection
                #:indirect)
  (:import-from #:breeze.command-utils
                #:pulse-node
                #:current-node)
  (:export
   ;; Simple transformation commands
   #:insert-breeze-define-command
   #:insert-parachute-define-test
   #:insert-loop-clause-for-on-list
   #:insert-loop-clause-for-in-list
   #:insert-loop-clause-for-hash
   #:insert-handler-bind-form
   #:insert-handler-case-form
   #:insert-defvar
   #:insert-defparameter
   #:insert-defconstant
   #:insert-define-constant
   #:insert-defun-shaped
   #:insert-defun
   #:insert-setf-defun
   #:insert-defmacro
   #:insert-asdf
   #:insert-defclass
   #:insert-class-slot
   #:insert-defgeneric
   #:insert-defmethod
   #:insert-print-unreadable-object-boilerplate
   #:insert-make-load-form-boilerplate
   #:insert-initialize-instance~method
   #:insert-lambda
   #:insert-decoded-time-multiple-value-bind
   #:insert-make-array
   #:insert-fancy-emacs-propline
   #:insert-fancy-sbcl-shebang
   ;; Other commands
   #:declaim-inline
   #:extract-as-defun
   #:quickinsert
   ;; TODO perhaps "quickfix" should go in "lint.lisp"
   #:quickfix))

(in-package #:breeze.refactor)

;; TODO "Move the current form into the nearest parent \"let\" form."
;; TODO "Add or update defpackage to \"import-from\" the symbol at point."

;; TODO it shouldn't be too hard to reuse this code to insert other
;; kind of `declaim's.
;;
;; TODO this only works when the point is at the start of the defun :/
;; otherwise, it inserts the declaim in weid places
(define-command declaim-inline ()
  "Declaim inline the current top-level function."
  (let* (($current-node (current-node :pulsep t))
         ($root (top-level-node-iterator $current-node)))
    (with-match ($root (defun ?name))
      (cond
        (?name
         (pulse-node ?name)
         (insert-at
          (start $root)
          "(declaim (inline ~a))~%" (node-string ?name)))
        (t
         (pulse-node $root)
         (message "Unable to find the current top-level function's name."))))))

(define-command extract-as-defun ()
  "Extract as defun."
  ;; TODO if current node is a lamda, replace "lambda" by defun,
  ;; choose a name, don't ask about the lamda-list (unless it's a
  ;; closure!), and replace the expression by '<the-name> (or #'...)
  ;;
  ;; TODO if current node is whitespace-node-p, extract the "next"
  ;; (non-comment or whitespace) node
  (let* (($node (current-node-iterator))
         ($top (top-level-node-iterator $node)))
    ;; TODO replace $node by (<name> ...<args>)
    ;; TODO (harder) detect "free" variables and use them to populate
    ;; the lambda-list
    (goto-char (start $top))
    (insert-saving-excursion "~%~%")
    (insert-defun-shaped
     "defun"
     :body (node-string $node))))
