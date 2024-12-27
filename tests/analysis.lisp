(defpackage #:breeze.test.analysis
  (:documentation "Tests for the package breeze.analysis")
  (:use #:cl #:breeze.analysis)
  (:import-from #:parachute
                #:define-test
                #:define-test+run
                #:is
                #:true
                #:false
                #:of-type)
  ;; importing unexported symbols
  (:import-from #:breeze.pattern
                #:termp
                #:term-name)
  ;; importing unexported symbols
  (:import-from #:breeze.analysis
                #:malformed-if-node-p))

(in-package #:breeze.test.analysis)


;;; Integrating pattern.lisp and lossless-parser.lisp

(defun normalize-bindings (bindings)
  "This is only to make it easier to compare the bindings in the tests."
  (or (eq t bindings)
      (alexandria:alist-plist
       (sort (loop :for (key . value) :in bindings
                   :collect (cons (if (termp key)
                                      (term-name key)
                                      key)
                                  value))
             #'string<
             :key #'car))))

(defun make-binding (term input)
  (normalize-bindings
   (breeze.pattern::make-binding term input)))

(defun test-match-parse (pattern string &optional skip-whitespaces-and-comments)
  (let* ((state (parse string))
         (*match-skip* (when skip-whitespaces-and-comments
                         #'whitespace-or-comment-node-p))
         (bindings (match (compile-pattern pattern) state))
         (bindings (normalize-bindings bindings)))
    (values bindings state)))



(define-test+run "match pattern nil and (nil) against parse trees"
  ;; pattern nil
  (loop
    :for skip-p :in '(nil t)
    :do (progn
          ;; TODO I'm not sure what should be the right things
          ;;  - on one hand the parse tree _is_ nil
          ;;  - on the other hand, (read "") would error
          ;; (false (test-match-parse nil ""))
          (false (test-match-parse nil "  " skip-p))
          (false (test-match-parse nil "; hi" skip-p))
          (false (test-match-parse nil "#| hi |#" skip-p))
          (false (test-match-parse nil "nil" skip-p))
          (false (test-match-parse nil "NIL" skip-p))
          (false (test-match-parse nil "nIl" skip-p))
          (false (test-match-parse nil "cl:nil" skip-p))
          (false (test-match-parse nil "cl::nil" skip-p))
          (false (test-match-parse nil "common-lisp:nil" skip-p))
          (false (test-match-parse nil "common-lisp::nil" skip-p))
          (false (test-match-parse nil "common-lisp-user::nil" skip-p))
          (false (test-match-parse nil "common-lisp-user:nil" skip-p))))
  (progn
    (false (test-match-parse '(nil) ""))
    (false (test-match-parse '(nil) "  "))
    (false (test-match-parse '(nil) "; hi"))
    (false (test-match-parse '(nil) "#| hi |#"))
    (true (test-match-parse '(nil) "nil"))
    (true (test-match-parse '(nil) "NIL"))
    (true (test-match-parse '(nil) "nIl"))
    (true (test-match-parse '(nil) "cl:nil"))
    (true (test-match-parse '(nil) "cl::nil"))
    (true (test-match-parse '(nil) "common-lisp:nil"))
    (true (test-match-parse '(nil) "common-lisp::nil"))
    ;; TODO For now we don't check _all_ the package a symbol might be
    ;; part of
    (false (test-match-parse '(nil) "common-lisp-user::nil"))
    (false (test-match-parse '(nil) "common-lisp-user:nil")))
  (progn
    (false (test-match-parse '(nil) "" t))
    (false (test-match-parse '(nil) "  " t))
    (false (test-match-parse '(nil) "; hi" t))
    (false (test-match-parse '(nil) "#| hi |#" t))

    (true (test-match-parse '(nil) " nil " t))

    (true (test-match-parse '(nil) " #| t |# NIL" t))
    (true (test-match-parse '(nil) "    nIl" t))
    (true (test-match-parse '(nil) " ;; look ma!
                                    cl:nil" t))
    (true (test-match-parse '(nil) " #||# cl::nil" t))
    (true (test-match-parse '(nil) " #|;;|# common-lisp:nil " t))
    (true (test-match-parse '(nil) " common-lisp::nil " t))
    ;; TODO For now we don't check _all_ the package a symbol might be
    ;; part of
    (false (test-match-parse '(nil) "common-lisp-user::nil" t))
    (false (test-match-parse '(nil) "common-lisp-user:nil" t))))

(define-test+run "match the patterns t and (t) against parse trees"
  ;; These should return nil because we're trying to match 1 symbol
  ;; against a list of nodes (even if that list is empty).
  (loop
    :for skip-p :in '(nil t)
    :do (progn
          (false (test-match-parse t "" skip-p))
          (false (test-match-parse t "  " skip-p))
          (false (test-match-parse t "; hi" skip-p))
          (false (test-match-parse t "#| hi |#" skip-p))
          (false (test-match-parse t "t" skip-p))
          (false (test-match-parse t "T" skip-p))
          (false (test-match-parse t "t" skip-p))
          (false (test-match-parse t "cl:t" skip-p))
          (false (test-match-parse t "cl::t" skip-p))
          (false (test-match-parse t "common-lisp:t" skip-p))
          (false (test-match-parse t "common-lisp::t" skip-p))
          ;; TODO For now we don't check _all_ the package a symbol might be
          ;; part of
          (false (test-match-parse t "common-lisp-user::t" skip-p))
          (false (test-match-parse t "common-lisp-user:t" skip-p))
          (progn
            (false (test-match-parse t "  t" skip-p))
            (false (test-match-parse t "t  " skip-p))
            (false (test-match-parse t "t ; hi" skip-p))
            (false (test-match-parse t "; t " skip-p))
            (false (test-match-parse t "t #| hi |#" skip-p))
            (false (test-match-parse t "#| t |#" skip-p))
            (false (test-match-parse t "#| hi |# t" skip-p)))))
  (progn
    ;; These should return the same thing whether the match is
    ;; skipping comments and whitespaces or not.
    (loop
      :for skip-p :in '(nil t)
      :do (progn
            (false (test-match-parse '(t) "" skip-p))
            (false (test-match-parse '(t) "  " skip-p))
            (false (test-match-parse '(t) "; hi" skip-p))
            (false (test-match-parse '(t) "#| hi |#" skip-p))
            (true (test-match-parse '(t) "t" skip-p))
            (true (test-match-parse '(t) "T" skip-p))
            (true (test-match-parse '(t) "t" skip-p))
            (true (test-match-parse '(t) "cl:t" skip-p))
            (true (test-match-parse '(t) "cl::t" skip-p))
            (true (test-match-parse '(t) "common-lisp:t" skip-p))
            (true (test-match-parse '(t) "common-lisp::t" skip-p))
            ;; TODO For now we don't check _all_ the package a symbol might be
            ;; part of
            (false (test-match-parse '(t) "common-lisp-user::t" skip-p))
            (false (test-match-parse '(t) "common-lisp-user:t" skip-p))))
    (progn
      (false (test-match-parse '(t) "t ; hi"))
      (false (test-match-parse '(t) "t  "))
      (false (test-match-parse '(t) "t #| hi |#"))
      (true (test-match-parse '(t) "t ; hi" t))
      (true (test-match-parse '(t) "t  " t))
      (true (test-match-parse '(t) "t #| hi |#" t))
      (true (test-match-parse '(t) "  t" t))
      (false (test-match-parse '(t) "; '(t) " t))
      (false (test-match-parse '(t) "#| t |#" t))
      (true (test-match-parse '(t) "#| hi |# t" t)))
    (true (test-match-parse '((t)) " (t) " t))))

;; TODO test pattern 1
;; TODO test pattern 'x
;; TODO test pattern :x
;; TODO test pattern "x"
;; TODO test pattern "some-node" (I'll have to think about the syntax)

(define-test+run "match terms against parse trees"
  (progn
    (is equalp
        (make-binding :?x #())
        (test-match-parse :?x ""))
    (is equalp
        (make-binding :?x #())
        (test-match-parse :?x "" t))
    (is equalp
        (make-binding :?x (nodes (token 0 1)))
        (test-match-parse :?x "x"))
    (is equalp
        (make-binding :?x (nodes (whitespace 0 1) (token 1 2)))
        (test-match-parse :?x " x"))
    (is equalp
        (make-binding :?x (nodes (whitespace 0 1) (token 1 2)))
        (test-match-parse :?x " x" t)))
  (progn
    (false (test-match-parse '(:?x) ""))
    (false (test-match-parse '(:?x) "" t))
    (is equalp
        (make-binding :?x (token 0 1))
        (test-match-parse '(:?x) "x"))
    (false (test-match-parse '(:?x) " x"))
    (is equalp
        (make-binding :?x (token 1 2))
        (test-match-parse '(:?x) " x" t))
    (is equalp
        (make-binding :?x (parens 0 4 (nodes (token 1 3))))
        (test-match-parse '(:?x) "(42)"))
    (is equalp
        (make-binding :?x (token 1 3))
        (test-match-parse '((:?x)) "(42)"))))

(define-test+run "match vector against parse trees"
  (false (test-match-parse 'x "x"))
  (true (test-match-parse #(x) "x"))
  (true (test-match-parse '((x)) "(x)")))


;;; Basic tree inspection

#++ ;; Sanity-check
(mapcar #'read-from-string
        '("in-package"
          "common-lisp:in-package"
          "cl:in-package"
          "cl-user::in-package"
          "common-lisp-user::in-package"))

(defun test-in-package-node-p (string)
  (let* ((state (parse string))
         (node (first-node (tree state))))
    ;; The funky reader macro and quasiquote is to fuck with slime and
    ;; sly's regex-based search for "(in-package". Without this the
    ;; rest of the file is evaluated in cl-user by slime and sly.
    (let ((package-designator-node
            #.`(,'in-package-node-p state node)))
      (when package-designator-node
        (node-content state package-designator-node)))))

(define-test+run in-package-node-p
  (is equal "x" (test-in-package-node-p "(in-package x)"))
  (is equal "#)" (test-in-package-node-p "(in-package #)"))
  (is equal ":x" (test-in-package-node-p "(in-package :x)"))
  (is equal "#:x" (test-in-package-node-p "(in-package #:x)"))
  (is equal "\"x\"" (test-in-package-node-p "(in-package \"x\")"))
  (is equal "x" (test-in-package-node-p "( in-package x )"))
  (is equal "x" (test-in-package-node-p "( in-package #| ∿ |# x )"))
  (is equal "x" (test-in-package-node-p "(cl:in-package x)"))
  (is equal "x" (test-in-package-node-p "(cl::in-package x)"))
  (is equal "42" (test-in-package-node-p "(cl::in-package 42)"))
  ;; TODO ? Not sure it's worth it lol...
  ;; (is equal "x" (test-in-package-node-p "('|CL|::|IN-PACKAGE| x)"))
  (null (test-in-package-node-p "(cl:)")))

(defun test-malformed-if-node-p (string)
  (let* ((state (parse string))
         (node (first (tree state))))
    (malformed-if-node-p state node)))

#++ ;; WIP
(define-test+run malformed-if-node-p
  (false (test-malformed-if-node-p "(if a b c)"))
  (true (test-malformed-if-node-p "(if a b c d)")))



(define-test find-node
  (is equal
      '((whitespace . 0) (parens . 1) (parens . 1) (parens . 1) (parens . 1)
        (parens . 1) (parens . 1) (parens . 1) (parens . 1) (whitespace . 2))
      (loop :with input = " ( loop ) "
            :with state = (parse input)
            :for i :from 0 :below (length input)
            :for path = (find-node i (tree state))
            :collect (cons (node-type (car path)) (cdr path)))))

(define-test+run find-path-to-position
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


;;; Testing the linter

(defun test-lint (buffer-string)
  (lint :buffer-string buffer-string))


(define-test+run lint
  (false (test-lint ""))
  (false (test-lint ";; "))
  (false (test-lint "asdf       ; qwer"))
  (false (test-lint "
(asdf
   xzcv    ; qwer
)"))
  (is equal '((0 2 :error "Syntax error")) (test-lint "#+"))
  (false (test-lint "(in-package :cl-user)"))
  (false (test-lint "(in-package 42)"))
  ;; TODO it's quoted, don't check the package-designator
  ;; (false (test-lint "'(in-package 42)"))
  (is equal (test-lint "(in-package #)")
      '((0 14 :error "Syntax error")))
  (is equal (test-lint "(in-package # )")
      '((0 15 :error "Syntax error")))
  (is equal '((0 56 :warning
               "Package PLEASE-DONT-DEFINE-A-PACKAGE-WITH-THIS-NAME is not currently defined."))
      (test-lint "(in-package please-dont-define-a-package-with-this-name)"))
  #++ ;; TODO check if "in-package" is NOT quoted
  (progn
    (false (test-lint "'(in-package :PLEASE-DONT-DEFINE-A-PACKAGE-WITH-THIS-NAME)"))
    (false (test-lint "`(in-package :PLEASE-DONT-DEFINE-A-PACKAGE-WITH-THIS-NAME)")))
  (is equalp
      '((1 3 :warning "Extraneous whitespaces."))
      (test-lint "(  )"))
  (is equalp
      '((2 4 :warning "Extraneous internal whitespaces."))
      (test-lint "(x  y)"))
  (is equalp
      '((3 4 :warning "Extraneous trailing whitespaces.")
        (1 2 :warning "Extraneous leading whitespaces."))
      (test-lint "( x )")))

#++ ;; TODO other cases of extraneous whitespaces:
"
   ;; asdf
#|
  |# <- here at the start of the line
"

#++ ;; Syntax errors
(progn
  (test-lint "(")
  (test-lint "')")
  (test-lint "'1")
  (test-lint "..")
  (test-lint "( . )")
  (test-lint "( a . )")
  (test-lint "( a . b . c )")
  (test-lint "( a . b c )")
  (test-lint "#1=")
  (test-lint "#1=#1#")
  (test-lint "(;;)")
  (test-lint "::")
  (test-lint "x::")
  (test-lint "::x")
  (test-lint "a:b:c")
  (test-lint "a:::b")
  (test-lint "b:")
  (test-lint "b::")
  (test-lint "\\")
  (test-lint "\\\\") ;; Should be OK
  (test-lint "|")
  (test-lint "'")
  (test-lint "(#++;;)")
  (test-lint "(#+;;)")
  (test-lint "(#)")
  (test-lint ",")
  (test-lint ",@")
  (test-lint "`,@x")
  (test-lint "`(a b . ,@x)") ; "has undefined consequences"
  ;; TODO "unknown character name"
  (test-lint "1/0")
  ;; TODO check for invalid radix
  (test-lint "#|")
  (test-lint "#c(a b c d)"))

;; Formatting Style
#++
(progn
  (test-lint "#+ ()")
  (test-lint "     ; this is ok")
  (test-lint ";I don't like this")
  (test-lint ";    not that"))

#++ ;; Style warnings
(progn
  (test-lint "like::%really"))


(define-test+run lint
  #++ ;; TODO the "fix" is to eval the defpackage, usually
  (is equal '((0 56 :warning
               "Package PLEASE-DONT-DEFINE-A-PACKAGE-WITH-THIS-NAME is not currently defined."))
      (test-lint "(in-package please-dont-define-a-package-with-this-name)"))
  (is equalp
      '((1 3 :warning "Extraneous whitespaces."))
      (test-lint "(  )"))
  (is equalp
      '((2 4 :warning "Extraneous internal whitespaces."))
      (test-lint "(x  y)"))
  (is equalp
      '((3 4 :warning "Extraneous trailing whitespaces.")
        (1 2 :warning "Extraneous leading whitespaces."))
      (test-lint "( x )")))

#++
(let ((diags))
  (values (with-output-to-string (*standard-output*)
            (setf diags (lint :buffer-string "( a b ( )  ) " :fixp t)))
          diags))

(defun test-fix (input)
  (multiple-value-list (fix :buffer-string (format nil input))))

(define-test+run test-fix
  (is equal '("()" nil) (test-fix "()"))
  ;; TODO these don't work anymore since I modified ERROR-INVALID-NODE
  ;; to signal an error. They were working by accident anyway...
  ;;
  ;; (is equal '(")" nil) (test-fix ")")) ; TODO if reasonable
  ;; (is equal '("()" t) (test-fix "("))
  ;; (is equal '("((()))" t) (test-fix "((("))
  (is equal '("()" t) (test-fix "( )"))
  (is equal '("()" t) (test-fix "(~%)"))
  (is equal '("() " t) (test-fix "(   ) "))
  (is equal '("() " t) (test-fix "( ) "))
  (is equal '(" ()" t) (test-fix " ( )"))
  (is equal '(" () " t) (test-fix " ( ) "))
  (is equal '("(a)" t) (test-fix "( a)"))
  (is equal '("(a)" t) (test-fix "(a )"))
  (is equal '("(a)" t) (test-fix "(  a  )"))
  (is equal '("(a b)" t) (test-fix "(a   b)"))
  (is equal '("((a))" t) (test-fix "(~%  (~%    a~%  )~%)"))
  (is equal '("((a))" t) (test-fix "((~%~%    a~%~%  ))"))
  ;; TODO handle indentation levels!
  #++
  (progn
    (is equal '("(;;~% )" t) (test-fix "(;;~%    )"))
    (is equal '("(;;~% )" t) (test-fix "(;;~% ~%)"))
    ;; TODO This should be detected as "extraneous internal newlines"...
    (is equal '("(;;~% )" t) (test-fix "(;;~% ~%)")))
  #++ ;; TODO more whitespace fixes
  (progn
    (is equal '("#+(or)" t) (test-fix "#+ (or)"))
    (is equal '("(+ (- 1 2) 3)" t) (test-fix "(+(- 1 2)3)")))
  #++ ;; TODO
  (progn
    ;; TODO (defpackage -> replace symbols by uninterned symbols
    (is equal '("(in-package \"x\")" t) (test-fix "(in-package \"x\")"))
    (is equal '("(in-package #:x)" t) (test-fix "(in-package :x)"))
    (is equal '("(in-package #:x)" t) (test-fix "(in-package 'x)"))
    (is equal '("(trace x)" t) (test-fix "(trace 'x)"))
    (is equal '("(block x)" t) (test-fix "(block 'x)"))
    (is equal '("(return-from x)" t) (test-fix "(return-from 'x)"))))


#++ ;; TODO this crashes because it tries to call (read "#)") inside
    ;; breeze.analysis::warn-undefined-in-package
(breeze.analysis::analyse :buffer-string "(in-package #)")
