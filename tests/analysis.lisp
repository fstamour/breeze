(defpackage #:breeze.test.analysis
  (:documentation "Tests for the package breeze.analysis")
  (:use #:cl #:breeze.analysis)
  (:import-from #:parachute
                #:define-test
                #:define-test+run
                #:is
                #:true
                #:false
                #:of-type))

(in-package #:breeze.test.analysis)

;;; Fixing formatting issues...

(defun parens-has-leading-whitespaces-p (node)
  (and (parens-node-p node)
       (whitespace-node-p (first (node-children node)))))

(defun parens-has-trailing-whitespaces-p (node)
  (and (parens-node-p node)
       (whitespace-node-p (alexandria:lastcar (node-children node)))))

(defun cdr-if (condition list)
  (if condition (cdr list) list))

(defun butlast-if (condition list)
  (if condition (butlast list) list))

(defun fix-trailing-whitespaces-inside-parens (node)
  (let ((first-child (parens-has-leading-whitespaces-p node))
        (last-child (parens-has-trailing-whitespaces-p node)))
    (if (or first-child last-child)
        (copy-parens
         node
         :children (butlast-if
                    last-child
                    (cdr-if first-child (node-children node))))
        node)))


(defun test-remove-whitespaces (input output)
  (let* ((input (format nil input))
         (output (format nil output))
         (state (parse input)))
    (breeze.kite:is
     :comparator 'string=
     :form `(unparse ,state nil 'fix-trailing-whitespaces-inside-parens)
     :got (unparse state nil 'fix-trailing-whitespaces-inside-parens)
     :expected output)))

(define-test+run remove-whitespaces
    (test-remove-whitespaces "( )" "()")
  (test-remove-whitespaces "(~%~%~%)" "()")
  (test-remove-whitespaces "(   ) " "() ")
  (test-remove-whitespaces " ( ) " " () ")
  ;; TODO handle indentation levels!
  ;; (test-remove-whitespaces "(;;~%  )" "(;;~% )")
  (test-remove-whitespaces "( x)" "(x)")
  (test-remove-whitespaces "( x )" "(x)"))


