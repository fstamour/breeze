(defpackage #:breeze.test.lint
  (:documentation "Tests for the package breeze.lint")
  (:use #:cl #:breeze.lint #:breeze.buffer #:breeze.analysis)
  (:import-from #:breeze.generics
                #:eqv)
  (:import-from #:parachute
                #:define-test
                #:define-test+run
                #:is
                #:true
                #:false
                #:of-type
                #:finish))

(in-package #:breeze.test.lint)


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


;; TODO add more "test-lint" tests with syntax errors

#++
(test-lint
 "(LET* ((#:BINDINGS1694
        (MATCH #(#(#<TERM :?_ {1022EB0623}> #<TERM ?NAME {1022EB0633}>))
               (COPY-ITERATOR ROOT-NODE-ITERATOR) :SKIPP
               #'WHITESPACE-OR-COMMENT-NODE-P)))
  (FLET ((BREEZE.ANALYSIS::GET-BINDINGS (BREEZE.ANALYSIS::TERM-NAME)
           (WHEN #:BINDINGS1694
             (ALEXANDRIA:WHEN-LET* ((TERM
                                     (GETHASH BREEZE.ANALYSIS::TERM-NAME
                                              #<HASH-TABLE :TEST EQL :COUNT 2 {1022EB0543}>))
                                    (BINDING
                                     (FIND-BINDING #:BINDINGS1694 TERM)))
               (TO BINDING)))))
    (DECLARE (IGNORABLE (FUNCTION BREEZE.ANALYSIS::GET-BINDINGS)))
    (LET ((?NAME (GET-BINDINGS '?NAME)))
      (IF ?NAME
          (LET ((NAME (NODE-CONTENT (PARSE-STATE (CURRENT-BUFFER)) ?NAME)))
            (MESSAGE \"Name: ~s\" NAME))
          (MESSAGE \"Not in a top-level form.\")))))")


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
  (is eqv `((,(whitespace 1 2) nil)) (test-fix "( )"))
  (is eqv `((,(whitespace 1 2) nil)) (test-fix "(~%)"))
  (is eqv `((,(whitespace 1 4) nil)) (test-fix "(   ) "))
  (is eqv `((,(whitespace 1 2) nil)) (test-fix "( ) "))
  (is eqv `((,(whitespace 2 3) nil)) (test-fix " ( )"))
  (is eqv `((,(whitespace 2 3) nil)) (test-fix " ( ) "))
  (is eqv `((,(whitespace 1 2) nil)) (test-fix "( a)"))
  (is eqv `((,(whitespace 2 3) nil)) (test-fix "(a )"))
  (is eqv `((,(whitespace 1 3) nil)
               (,(whitespace 4 6) nil))
      (test-fix "(  a  )"))
  (is eqv `((,(whitespace 2 5) " ")) (test-fix "(a   b)"))
  (is eqv `((,(whitespace 1 4) nil)
               (,(whitespace 5 10) nil)
               (,(whitespace 11 14) nil)
               (,(whitespace 15 16) nil))
      (test-fix "(~%  (~%    a~%  )~%)"))
  (is eqv `((,(whitespace 2 8) nil)
               (,(whitespace 9 13) nil))
      (test-fix "((~%~%    a~%~%  ))"))
  ;; TODO handle indentation levels!
  #++
  (progn
    (is eqv '("(;;~% )" t) (test-fix "(;;~%    )"))
    (is eqv '("(;;~% )" t) (test-fix "(;;~% ~%)"))
    ;; TODO This should be detected as "extraneous internal newlines"...
    (is eqv '("(;;~% )" t) (test-fix "(;;~% ~%)")))
  #++ ;; TODO more whitespace fixes
  (progn
    (is eqv '("#+(or)" t) (test-fix "#+ (or)"))
    (is eqv '("(+ (- 1 2) 3)" t) (test-fix "(+(- 1 2)3)")))
  #++ ;; TODO
  (progn
    ;; TODO (defpackage -> replace symbols by uninterned symbols
    (is eqv '("(in-package \"x\")" t) (test-fix "(in-package \"x\")"))
    (is eqv '("(in-package #:x)" t) (test-fix "(in-package :x)"))
    (is eqv '("(in-package #:x)" t) (test-fix "(in-package 'x)"))
    (is eqv '("(trace x)" t) (test-fix "(trace 'x)"))
    (is eqv '("(block x)" t) (test-fix "(block 'x)"))
    (is eqv '("(return-from x)" t) (test-fix "(return-from 'x)")))
  ;; "\"a\"'(\"b\"c)" => "\"a\" '(\"b\" c)"
  )

#++ ;; TODO this used to crash because it would try to call (read "#)") inside
    ;; breeze.analysis::warn-undefined-in-package, add regression test
(breeze.analysis::analyse :buffer-string "(in-package #)")
