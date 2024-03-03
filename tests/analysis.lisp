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


;;; Integrating pattern.lisp and lossless-parser.lisp

#++
(match `#(in-package ,(term :?package))
  '(in-package #:cl-user))

#++
(let* ((string "(in-package #:cl-user)")
       (state (parse string))
       (node (first (tree state)))
       (package-term (term :?package))
       (bindings (match `#(in-package ,package-term)
                   ;; '(in-package #:cl-user)
                   node)))
  bindings
  node
  ;; (getf bindings package-term)
  )



;;; Basic tree inspection

#++ ;; Sanity-check
(mapcar #'read-from-string
        '("in-package"
          "common-lisp:in-package"
          "cl:in-package"
          "cl-user::in-package"
          "common-lisp-user::in-package"))

(defun parse-then-in-package-node-p (string)
  (let* ((state (parse string))
         (node (first (tree state))))
    (in-package-node-p state node)))

(define-test+run in-package-node-p
  (true (every #'parse-then-in-package-node-p
               '("in-package"
                 "common-lisp:in-package"
                 "cl:in-package"
                 "cl-user::in-package"
                 "common-lisp-user::in-package")))
  (true (every #'(lambda (s)
                   (parse-then-in-package-node-p (format nil "(~a)" s)))
               '("in-package"
                 "common-lisp:in-package"
                 "cl:in-package"
                 "cl-user::in-package"
                 "common-lisp-user::in-package"))))

(define-test find-node
  (is equal
      '((whitespace . 0) (parens . 1) (parens . 1) (parens . 1) (parens . 1)
        (parens . 1) (parens . 1) (parens . 1) (parens . 1) (whitespace . 2))
      (loop :with input = " ( loop ) "
            :with state = (parse input)
            :for i :from 0 :below (length input)
            :for path = (find-node i (tree state))
            :collect (cons (node-type (car path)) (cdr path)))))

(define-test find-path-to-position
  (is equalp
      '((whitespace)
        (parens whitespace)
        (parens whitespace)
        (parens token)
        (parens token)
        (parens token)
        (parens token)
        (parens whitespace)
        (parens)
        (whitespace))
      (loop :with input = " ( loop ) "
            :with state = (parse input)
            :for i :from 0 :below (length input)
            :for path = (find-path-to-position state i)
            :collect
            (mapcar (lambda (path)
                      (node-type (car path)))
                    path)
            #++(list i (length path)))))


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
