(defpackage #:breeze.test.analysis
  (:documentation "Tests for the package breeze.analysis")
  (:use #:cl #:breeze.analysis #:breeze.workspace)
  (:import-from #:parachute
                #:define-test
                #:define-test+run
                #:is
                #:true
                #:false
                #:of-type
                #:finish)
  ;; importing unexported symbols
  (:import-from #:breeze.pattern
                #:termp
                #:term-name)
  ;; importing unexported symbols
  (:import-from #:breeze.analysis
                #:malformed-if-node-p
                #:match-symbol-to-token
                #:target-node
                #:replacement)
  ;; importing unexported symbols
  (:import-from #:breeze.test.pattern
                #:bindings-alist))

(in-package #:breeze.test.analysis)


;;; Integrating pattern.lisp and lossless-parser.lisp

(define-test+run match-symbol-to-token
  (true (match-symbol-to-token t (make-node-iterator "t")))
  (true (match-symbol-to-token nil (make-node-iterator "nil")))
  (true (match-symbol-to-token nil (make-node-iterator "common-lisp:nil")))
  #++ ;; TODO not implemented yet
  (true (match-symbol-to-token nil (make-node-iterator "common-lisp-user:nil"))))

(defun test-match-parse (pattern string &optional skip-whitespaces-and-comments)
  (finish
   (let* ((state (parse string))
          (bindings (match (compile-pattern pattern) state
                      :skipp (when skip-whitespaces-and-comments
                               #'whitespace-or-comment-node-p))))
     (values bindings state))))

(define-test+run "match pattern nil and (nil) against parse trees"
  ;; pattern nil
  (progn
    ;; TODO I'm not sure what should be the right things
    ;;  - on one hand the parse tree _is_ nil
    ;;  - on the other hand, (read "") would error
    ;; (false (test-match-parse nil ""))
    (false (test-match-parse nil "  " nil))
    (false (test-match-parse nil "; hi" nil))
    (false (test-match-parse nil "#| hi |#" nil))
    (false (test-match-parse nil "nil" nil))
    (false (test-match-parse nil "NIL" nil))
    (false (test-match-parse nil "nIl" nil))
    (false (test-match-parse nil "cl:nil" nil))
    (false (test-match-parse nil "cl::nil" nil))
    (false (test-match-parse nil "common-lisp:nil" nil))
    (false (test-match-parse nil "common-lisp::nil" nil))
    #++ ;; TODO not implemented yet
    (false (test-match-parse nil "common-lisp-user::nil" nil))
    #++ ;; TODO not implemented yet
    (false (test-match-parse nil "common-lisp-user:nil" nil)))
  (progn
    ;; TODO I'm not sure what should be the right things
    ;;  - on one hand the parse tree _is_ nil
    ;;  - on the other hand, (read "") would error
    ;; (false (test-match-parse nil ""))
    (false (test-match-parse nil "  " t))
    (false (test-match-parse nil "; hi" t))
    (false (test-match-parse nil "#| hi |#" t))
    (false (test-match-parse nil "nil" t))
    (false (test-match-parse nil "NIL" t))
    (false (test-match-parse nil "nIl" t))
    (false (test-match-parse nil "cl:nil" t))
    (false (test-match-parse nil "cl::nil" t))
    (false (test-match-parse nil "common-lisp:nil" t))
    (false (test-match-parse nil "common-lisp::nil" t))
    #++ ;; TODO not implemented yet
    (false (test-match-parse nil "common-lisp-user::nil" t))
    #++ ;; TODO not implemented yet
    (false (test-match-parse nil "common-lisp-user:nil" t)))
  (progn
    (false (test-match-parse '(nil) ""))
    (false (test-match-parse '(nil) "  "))
    (false (test-match-parse '(nil) "; hi"))
    (false (test-match-parse '(nil) "#| hi |#"))
    (true (test-match-parse '(nil) "nil")) ;;;;;;;;;;;;;;;;;;;;;;;;;;
    (true (test-match-parse '(nil) "NIL"))
    (true (test-match-parse '(nil) "nIl"))
    (true (test-match-parse '(nil) "cl:nil"))
    (true (test-match-parse '(nil) "cl::nil"))
    (true (test-match-parse '(nil) "common-lisp:nil"))
    (true (test-match-parse '(nil) "common-lisp::nil"))
    #++ ;; TODO not implemented yet
    (false (test-match-parse '(nil) "common-lisp-user::nil"))
    #++ ;; TODO not implemented yet
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
    #++ ;; TODO not implemented yet
    (false (test-match-parse '(nil) "common-lisp-user::nil" t))
    #++ ;; TODO not implemented yet
    (false (test-match-parse '(nil) "common-lisp-user:nil" t))))

(define-test+run "match the patterns t and (t) against parse trees"
  ;; These should return nil because we're trying to match 1 symbol
  ;; against a list of nodes (even if that list is empty).
  (progn
    (progn
      (false (test-match-parse t "" nil))
      (false (test-match-parse t "  " nil))
      (false (test-match-parse t "; hi" nil))
      (false (test-match-parse t "#| hi |#" nil))
      (false (test-match-parse t "t" nil))
      (false (test-match-parse t "T" nil))
      (false (test-match-parse t "t" nil))
      (false (test-match-parse t "cl:t" nil))
      (false (test-match-parse t "cl::t" nil))
      (false (test-match-parse t "common-lisp:t" nil))
      (false (test-match-parse t "common-lisp::t" nil))
      #++ ;; TODO not implemented yet
      (false (test-match-parse t "common-lisp-user::t" nil))
      #++ ;; TODO not implemented yet
      ;; t (and nil) is not exported from common-lisp-user
      (false (test-match-parse t "common-lisp-user:t" nil))
      (progn
        (false (test-match-parse t "  t" nil))
        (false (test-match-parse t "t  " nil))
        (false (test-match-parse t "t ; hi" nil))
        (false (test-match-parse t "; t " nil))
        (false (test-match-parse t "t #| hi |#" nil))
        (false (test-match-parse t "#| t |#" nil))
        (false (test-match-parse t "#| hi |# t" nil))))
    (progn
      (false (test-match-parse t "" t))
      (false (test-match-parse t "  " t))
      (false (test-match-parse t "; hi" t))
      (false (test-match-parse t "#| hi |#" t))
      (false (test-match-parse t "t" t))
      (false (test-match-parse t "T" t))
      (false (test-match-parse t "t" t))
      (false (test-match-parse t "cl:t" t))
      (false (test-match-parse t "cl::t" t))
      (false (test-match-parse t "common-lisp:t" t))
      (false (test-match-parse t "common-lisp::t" t))
      #++ ;; TODO not implemented yet
      (false (test-match-parse t "common-lisp-user::t" t))
      #++ ;; TODO not implemented yet
      (false (test-match-parse t "common-lisp-user:t" t))
      (progn
        (false (test-match-parse t "  t" t))
        (false (test-match-parse t "t  " t))
        (false (test-match-parse t "t ; hi" t))
        (false (test-match-parse t "; t " t))
        (false (test-match-parse t "t #| hi |#" t))
        (false (test-match-parse t "#| t |#" t))
        (false (test-match-parse t "#| hi |# t" t)))))
  (progn
    (progn
      (progn
        (false (test-match-parse '(t) "" nil))
        (false (test-match-parse '(t) "  " nil))
        (false (test-match-parse '(t) "; hi" nil))
        (false (test-match-parse '(t) "#| hi |#" nil))
        (true (test-match-parse '(t) "t" nil))
        (true (test-match-parse '(t) "T" nil))
        (true (test-match-parse '(t) "t" nil))
        (true (test-match-parse '(t) "cl:t" nil))
        (true (test-match-parse '(t) "cl::t" nil))
        (true (test-match-parse '(t) "common-lisp:t" nil))
        (true (test-match-parse '(t) "common-lisp::t" nil))

        #++ ;; TODO not implemented yet
        (false (test-match-parse '(t) "common-lisp-user::t" nil))
        (false (test-match-parse '(t) "common-lisp-user:t" nil)))
      (progn
        (false (test-match-parse '(t) "" t))
        (false (test-match-parse '(t) "  " t))
        (false (test-match-parse '(t) "; hi" t))
        (false (test-match-parse '(t) "#| hi |#" t))
        (true (test-match-parse '(t) "t" t))
        (true (test-match-parse '(t) "T" t))
        (true (test-match-parse '(t) "t" t))
        (true (test-match-parse '(t) "cl:t" t))
        (true (test-match-parse '(t) "cl::t" t))
        (true (test-match-parse '(t) "common-lisp:t" t))
        (true (test-match-parse '(t) "common-lisp::t" t))
        #++ ;; TODO not implemented yet
        (false (test-match-parse '(t) "common-lisp-user::t" t))
        #++ ;; TODO not implemented yet
        (false (test-match-parse '(t) "common-lisp-user:t" t))))
    (progn
      (true (test-match-parse '(t) "t ; hi"))
      (true (test-match-parse '(t) "t  "))
      (true (test-match-parse '(t) "t #| hi |#"))
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

(defun test-match-terms-against-parse-tree (pattern string
                                            skip-whitespaces-and-comments
                                            expected-binding)
  (let ((binding (test-match-parse pattern string skip-whitespaces-and-comments)))
    (cond
      (expected-binding
       (true binding)
       (parachute:of-type '(or binding binding-set) binding)
       (when (binding-set-p binding)
         (let ((hash-table (breeze.pattern::bindings binding)))
           (is = 1 (hash-table-count hash-table))
           (maphash (lambda (term binding*)
                      (declare (ignore term))
                      (setf binding binding*))
                    hash-table)))
       (is breeze.pattern::term= (term :?x) (from binding))
       (is equalp expected-binding (to binding)
           "matching ~s against ~s (~s) should have bound :?x to ~s"
           pattern string (parse string) expected-binding))
      (t
       (false binding)))))

(define-test+run "match terms against parse trees"
  (progn
    (test-match-terms-against-parse-tree
     :?x "" nil
     nil)
    (test-match-terms-against-parse-tree
     :?x "" t
     nil)
    (test-match-terms-against-parse-tree
     :?x "x" nil
     (token 0 1))
    (test-match-terms-against-parse-tree
     :?x "x" t
     (token 0 1))
    (test-match-terms-against-parse-tree
     :?x " x" t
     (whitespace 0 1)))
  (progn
    (test-match-terms-against-parse-tree
     '(:?x) "" nil
     nil)
    (test-match-terms-against-parse-tree
     '(:?x) "" t
     nil)
    (test-match-terms-against-parse-tree
     '(:?x) "x" nil
     (token 0 1))
    (test-match-terms-against-parse-tree
     '(:?x) "x" t
     (token 0 1))
    (test-match-terms-against-parse-tree
     '(:?x) " x" nil
     (whitespace 0 1))
    (test-match-terms-against-parse-tree
     '(:?x) " x" t
     (token 1 2))
    ;; TODO I'm not sure I like the behaviour of these last 2:
    ;; A. (match '(:?x) (parens 0 4 #((token 1 3))))
    ;; B. (match '((:?x)) (parens 0 4 #((token 1 3))))
    ;; Both bind :?x to (token 1 3), but it would make (more?) sense
    ;; if A bound :?x to (parens ...)
    (test-match-terms-against-parse-tree
     '(:?x) "(42)" nil
     (token 1 3))
    (test-match-terms-against-parse-tree
     '((:?x)) "(42)" nil
     (token 1 3))))

(define-test+run "match vector against parse trees"
  (false (test-match-parse 'x "x"))
  (true (test-match-parse #(x) "x"))
  (true (test-match-parse '((x)) "(x)")))

(defun test-alternation (pattern string expected-binding)
  (finish
   (let ((binding (match (compile-pattern pattern) (make-node-iterator string))))
     (cond
       (expected-binding (is eq t binding))
       (t (false binding))))))

(define-test+run "match alternation against parse trees"
  (test-alternation '(:alternation a b) "a" t)
  (test-alternation '(:alternation a b) "b" t)
  (test-alternation '(:alternation a b) "c" nil)
  (test-alternation '(:alternation a b) "breeze.test.analysis::a" t)
  (test-alternation '(:alternation a b) "breeze.analysis::a" nil))

#++
(trace :wherein test-alternation
       match-symbol-to-token
       match
       breeze.analysis::node-string-equal)


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
         (node (make-node-iterator state)))
    ;; The funky reader macro and quasiquote is to fuck with slime and
    ;; sly's regex-based search for "(in-package". Without this the
    ;; rest of the file is evaluated in cl-user by slime and sly.
    (let ((package-designator-node
            #.`(,'in-package-node-p node)))
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


;;; child-of-mapcar-node-p

#++ ;; TODO WIP
(let* ((input "(mapcar )")
       (node-iterator (make-node-iterator input)))
  (loop :for i :below (length input)
        :do (goto-position node-iterator i)
        :collect (child-of-mapcar-node-p node-iterator)))


;;; malformed-if-node-p

(defun test-malformed-if-node-p (string)
  (malformed-if-node-p (make-node-iterator string)))

(define-test+run malformed-if-node-p
  ;; TODO add tests with comments, e.g. (if #|...|# ...)
  (false (test-malformed-if-node-p "(if a b c)"))
  (parachute:skip
      "not implemented yet"
      (true (test-malformed-if-node-p "(if a)")))
  (true (test-malformed-if-node-p "(if a b c d)"))
  ;; TODO this works by shear luck: it successfully match up to "d"
  ;; and considers that a successful match, but it didn't match
  ;; against the whole form.
  (true (test-malformed-if-node-p "(if a b c d e)")))


;;; Testing the linter

(defun test-lint (buffer-string)
  (lint-buffer (make-buffer :string buffer-string)))

(define-test+run lint
  (false (test-lint ""))
  (false (test-lint ";; "))
  (false (test-lint "asdf       ; qwer"))
  (false (test-lint "
(asdf
   xzcv    ; qwer
)"))
  (is equal '((0 nil :error "Syntax error")) (test-lint "#+"))
  (false (test-lint "(in-package :cl-user)"))
  (false (test-lint "(in-package 42)"))
  ;; TODO it's quoted, don't check the package-designator
  ;; (false (test-lint "'(in-package 42)"))
  (is equal (test-lint "(in-package #)")
      '((0 nil :error "Syntax error")))
  (is equal (test-lint "(in-package # )")
      '((0 nil :error "Syntax error")))
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
      (test-lint "( x )"))
  (is equalp
      '((3 4 :warning "Missing internal whitespace(s)."))
      (test-lint "(\"a\"x)")))

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

(defun test-fix (buffer-string &aux (buffer-string (format nil buffer-string)))
  (let* ((buffer (make-buffer :string buffer-string))
         (fixes (fix-buffer buffer)))
    (loop :for fix :in fixes
          :for node = (value (target-node fix))
          :for replacement = (replacement fix)
          :collect (list node replacement))))

(define-test+run test-fix
  (is equalp nil (test-fix "()"))
  ;; TODO these don't work anymore since I modified ERROR-INVALID-NODE
  ;; to signal an error. They were working by accident anyway...
  ;;
  ;; (is equalp '(")" nil) (test-fix ")")) ; TODO if reasonable
  ;; (is equalp '("()" t) (test-fix "("))
  ;; (is equalp '("((()))" t) (test-fix "((("))
  (is equalp `((,(whitespace 1 2) nil)) (test-fix "( )"))
  (is equalp `((,(whitespace 1 2) nil)) (test-fix "(~%)"))
  (is equalp `((,(whitespace 1 4) nil)) (test-fix "(   ) "))
  (is equalp `((,(whitespace 1 2) nil)) (test-fix "( ) "))
  (is equalp `((,(whitespace 2 3) nil)) (test-fix " ( )"))
  (is equalp `((,(whitespace 2 3) nil)) (test-fix " ( ) "))
  (is equalp `((,(whitespace 1 2) nil)) (test-fix "( a)"))
  (is equalp `((,(whitespace 2 3) nil)) (test-fix "(a )"))
  (is equalp `((,(whitespace 1 3) nil)
               (,(whitespace 4 6) nil))
      (test-fix "(  a  )"))
  (is equalp `((,(whitespace 2 5) " ")) (test-fix "(a   b)"))
  (is equalp `((,(whitespace 1 4) nil)
               (,(whitespace 5 10) nil)
               (,(whitespace 11 14) nil)
               (,(whitespace 15 16) nil))
      (test-fix "(~%  (~%    a~%  )~%)"))
  (is equalp `((,(whitespace 2 8) nil)
               (,(whitespace 9 13) nil))
      (test-fix "((~%~%    a~%~%  ))"))
  ;; TODO handle indentation levels!
  #++
  (progn
    (is equalp '("(;;~% )" t) (test-fix "(;;~%    )"))
    (is equalp '("(;;~% )" t) (test-fix "(;;~% ~%)"))
    ;; TODO This should be detected as "extraneous internal newlines"...
    (is equalp '("(;;~% )" t) (test-fix "(;;~% ~%)")))
  #++ ;; TODO more whitespace fixes
  (progn
    (is equalp '("#+(or)" t) (test-fix "#+ (or)"))
    (is equalp '("(+ (- 1 2) 3)" t) (test-fix "(+(- 1 2)3)")))
  #++ ;; TODO
  (progn
    ;; TODO (defpackage -> replace symbols by uninterned symbols
    (is equalp '("(in-package \"x\")" t) (test-fix "(in-package \"x\")"))
    (is equalp '("(in-package #:x)" t) (test-fix "(in-package :x)"))
    (is equalp '("(in-package #:x)" t) (test-fix "(in-package 'x)"))
    (is equalp '("(trace x)" t) (test-fix "(trace 'x)"))
    (is equalp '("(block x)" t) (test-fix "(block 'x)"))
    (is equalp '("(return-from x)" t) (test-fix "(return-from 'x)")))
  ;; "\"a\"'(\"b\"c)" => "\"a\" '(\"b\" c)"
  )


#++ ;; TODO this used to crash because it would try to call (read "#)") inside
    ;; breeze.analysis::warn-undefined-in-package, add regression test
(breeze.analysis::analyse :buffer-string "(in-package #)")
