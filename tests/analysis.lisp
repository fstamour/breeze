(defpackage #:breeze.test.analysis
  (:documentation "Tests for the package breeze.analysis")
  (:use #:cl #:breeze.analysis)
  (:import-from #:breeze.generics
                #:eqv)
  (:import-from #:parachute
                #:define-test
                #:define-test+run
                #:is
                #:true
                #:false
                #:of-type
                #:finish)
  ;; importing unexported symbols
  ;; TODO these should be exported
  (:import-from #:breeze.pattern
                #:varp
                #:name)
  ;; importing unexported symbols
  (:import-from #:breeze.analysis
                #:malformed-if-node-p
                #:match-symbol-to-token)
  ;; importing unexported symbols
  (:import-from #:breeze.test.pattern
                #:bindings-alist))

(in-package #:breeze.test.analysis)


;;; Integrating pattern.lisp and parser.lisp

(defun test-parse-symbol-node (string)
  (parse-symbol-node (make-node-iterator string)))

(define-test+run parse-symbol-node
  (is eqv '(:current "X") (test-parse-symbol-node "x"))
  (is eqv '(:current "42") (test-parse-symbol-node "42"))
  (is eqv '(:keyword "X") (test-parse-symbol-node ":x"))
  (is eqv '(:uninterned "X") (test-parse-symbol-node "#:x"))
  (is eqv '(:qualified "X" "P") (test-parse-symbol-node "p:x"))
  (is eqv '(:possibly-internal "X" "P") (test-parse-symbol-node "p::x"))
  (is eqv '(:possibly-internal "X" "KEYWORD") (test-parse-symbol-node "::x"))
  ;; TODO this test fails:
  (is eqv '(:qualified "X" "KEYWORD") (test-parse-symbol-node "keyword:x"))
  (is eqv '(:possibly-internal "X" "KEYWORD") (test-parse-symbol-node "keyword::x"))
  (is eqv '(:qualified "NIL" "COMMON-LISP") (test-parse-symbol-node "common-lisp:nil"))
  (is eqv '(:nil "()" (symbol-name :cl)) (test-parse-symbol-node "()"))
  (is eqv '(:keyword "asdf:fe") (test-parse-symbol-node ":|asdf:fe|"))
  (false (test-parse-symbol-node ""))
  (is eqv '(:uninterned "") (test-parse-symbol-node "#:"))
  (false (test-parse-symbol-node "::"))
  (false (test-parse-symbol-node "p:::x"))
  (false (test-parse-symbol-node "p::"))
  (false (test-parse-symbol-node "a:a:x")))

(define-test+run match-symbol-to-token
  (true (match-symbol-to-token t (make-node-iterator "t")))
  (true (match-symbol-to-token nil (make-node-iterator "nil")))
  (true (match-symbol-to-token nil (make-node-iterator "common-lisp:nil")))
  (true (match-symbol-to-token nil (make-node-iterator "common-lisp-user:nil")))
  (true (match-symbol-to-token 'defun (make-node-iterator "defun")))
  (true (match-symbol-to-token 'uiop:define-package
                               (make-node-iterator "uiop:define-package"))))

(defun test-match-parse (pattern string &optional skip-whitespaces-and-comments)
  (finish
   (let* ((state (parse string))
          (bindings (match (compile-pattern pattern) state
                      :skipp (when skip-whitespaces-and-comments
                               #'whitespace-or-comment-node-p))))
     (values bindings state))))

(define-test+run "match sym - regression tests"
  (is eq t (test-match-parse (sym "UIOP" "DEFINE-PACKAGE") "uiop:define-package")))

(define-test+run "match sym - :wild symbol-name and package-name"
  (is eq t (test-match-parse (sym :wild :wild :wild) "x"))
;;; :current
  (progn
    (is eq t (test-match-parse (sym :wild :wild :current) "x"))
    (is eq nil (test-match-parse (sym :wild :wild :current) ":x"))
    (is eq nil (test-match-parse (sym :wild :wild :current) "#:x"))
    (is eq nil (test-match-parse (sym :wild :wild :current) "p:x"))
    (is eq nil (test-match-parse (sym :wild :wild :current) "p::x")))
;;; :keyword
  (progn
    (is eq nil (test-match-parse (sym :wild :wild :keyword) "x"))
    (is eq t (test-match-parse (sym :wild :wild :keyword) ":x"))
    (is eq nil (test-match-parse (sym :wild :wild :keyword) "#:x"))
    (is eq nil (test-match-parse (sym :wild :wild :keyword) "p:x"))
    (is eq nil (test-match-parse (sym :wild :wild :keyword) "p::x")))
;;; :uninterned-symbol
  (progn
    (is eq nil (test-match-parse (sym :wild :wild :uninterned) "x"))
    (is eq nil (test-match-parse (sym :wild :wild :uninterned) ":x"))
    (is eq t (test-match-parse (sym :wild :wild :uninterned) "#:x"))
    (is eq nil (test-match-parse (sym :wild :wild :uninterned) "p:x"))
    (is eq nil (test-match-parse (sym :wild :wild :uninterned) "p::x")))
;;; :qualified-symbol
  (progn
    (is eq nil (test-match-parse (sym :wild :wild :qualified) "x"))
    (is eq nil (test-match-parse (sym :wild :wild :qualified) ":x"))
    (is eq nil (test-match-parse (sym :wild :wild :qualified) "#:x"))
    (is eq t (test-match-parse (sym :wild :wild :qualified) "p:x"))
    (is eq nil (test-match-parse (sym :wild :wild :qualified) "p::x")))
;;; :possibly-internal-symbol
  (progn
    (is eq nil (test-match-parse (sym :wild :wild :possibly-internal) "x"))
    (is eq nil (test-match-parse (sym :wild :wild :possibly-internal) ":x"))
    (is eq nil (test-match-parse (sym :wild :wild :possibly-internal) "#:x"))
    (is eq nil (test-match-parse (sym :wild :wild :possibly-internal) "p:x"))
    (is eq t (test-match-parse (sym :wild :wild :possibly-internal) "p::x"))))

(define-test+run "match sym - :wild symbol-name and qualification"
;;; :current
  (progn
    (is eq nil (test-match-parse (sym nil :wild :wild) "x"))
    (is eq nil (test-match-parse (sym :keyword :wild :wild) "x"))
    #++ ;; TODO need some kind of *current-package* here, because it
        ;; cannot be reliably inferred from the string alone.
    (progn
      (is eq nil (test-match-parse (sym :cl :wild :wild) "x"))
      (is eq nil (test-match-parse (sym #.*package* :wild :wild) "x"))
      (is eq nil (test-match-parse (sym "CL" :wild :wild) "x"))))
;;; :keyword
  (progn
    (is eq t (test-match-parse (sym :keyword :wild :wild) ":x"))
    (is eq t (test-match-parse (sym :keyword :wild :wild) "::x"))
    (is eq nil (test-match-parse (sym :cl :wild :wild) ":x"))
    (is eq nil (test-match-parse (sym #.*package* :wild :wild) ":x"))
    (is eq nil (test-match-parse (sym nil :wild :wild) ":x")))
;;; :uninterned-symbol
  (progn
    (is eq t (test-match-parse (sym nil :wild :wild) "#:x"))
    (is eq nil (test-match-parse (sym :keyword :wild :wild) "#:x"))
    (is eq nil (test-match-parse (sym :cl :wild :wild) "#:x"))
    (is eq nil (test-match-parse (sym #.*package* :wild :wild) "#:x"))
    (is eq nil (test-match-parse (sym "nil" :wild :wild) "#::x")))
;;; :qualified-symbol
  (progn
    (is eq nil (test-match-parse (sym nil :wild :wild) "p:x"))
    (is eq nil (test-match-parse (sym :keyword :wild :wild) "p:x"))
    (is eq nil (test-match-parse (sym :cl :wild :wild) "p:x"))
    (is eq nil (test-match-parse (sym #.*package* :wild :wild) "p:x"))
    (is eq nil (test-match-parse (sym "p:" :wild :wild) "p:x"))
    (is eq t (test-match-parse (sym "P" :wild :wild) "p:x"))
    (is eq nil (test-match-parse (sym "p" :wild :wild) "p:x"))
    (is eq t (test-match-parse (sym :P :wild :wild) "p:x"))
    (is eq t (test-match-parse (sym :P :wild :wild) "P:x")))
;;; :possibly-internal-symbol
  (progn
    (is eq nil (test-match-parse (sym nil :wild :wild) "p::x"))
    (is eq nil (test-match-parse (sym :keyword :wild :wild) "p::x"))
    (is eq nil (test-match-parse (sym :cl :wild :wild) "p::x"))
    (is eq nil (test-match-parse (sym #.*package* :wild :wild) "p::x"))
    (is eq nil (test-match-parse (sym "p:" :wild :wild) "p::x"))
    (is eq t (test-match-parse (sym "P" :wild :wild) "p::x"))
    (is eq nil (test-match-parse (sym "p" :wild :wild) "p::x"))
    (is eq t (test-match-parse (sym :P :wild :wild) "p::x"))
    (is eq t (test-match-parse (sym :P :wild :wild) "P::x"))))

(define-test+run "match sym - :wild package-name and qualification"
  ;; N.B. the third :wild is optional
;;; :current
  (progn
    (is eq t (test-match-parse (sym :wild "X" :wild) "X"))
    (is eq t (test-match-parse (sym :wild 'x :wild) "X"))
    (is eq t (test-match-parse (sym :wild 'x :wild) "x"))
    (is eq t (test-match-parse (sym :wild :x :wild) "x"))
    (is eq t (test-match-parse (sym :wild :x :wild) "X"))
    (is eq t (test-match-parse (sym :wild "X" :wild) "x"))
    (is eq nil (test-match-parse (sym :wild "x" :wild) "X")))
;;; :keyword
  (progn
    (is eq t (test-match-parse (sym :wild "X" :wild) ":X"))
    (is eq t (test-match-parse (sym :wild 'x :wild) ":X"))
    (is eq t (test-match-parse (sym :wild 'x :wild) ":x"))
    (is eq t (test-match-parse (sym :wild :x :wild) ":x"))
    (is eq t (test-match-parse (sym :wild :x :wild) ":X"))
    (is eq t (test-match-parse (sym :wild "X" :wild) ":x"))
    (is eq nil (test-match-parse (sym :wild "x" :wild) ":X")))
;;; :uninterned-symbol
  (progn
    (is eq t (test-match-parse (sym :wild "X" :wild) "#:X"))
    (is eq t (test-match-parse (sym :wild 'x :wild) "#:X"))
    (is eq t (test-match-parse (sym :wild 'x :wild) "#:x"))
    (is eq t (test-match-parse (sym :wild :x :wild) "#:x"))
    (is eq t (test-match-parse (sym :wild :x :wild) "#:X"))
    (is eq t (test-match-parse (sym :wild "X" :wild) "#:x"))
    (is eq nil (test-match-parse (sym :wild "x" :wild) "#:X")))
;;; :qualified-symbol
  (progn
    (is eq t (test-match-parse (sym :wild "X" :wild) "p:X"))
    (is eq t (test-match-parse (sym :wild 'x :wild) "p:X"))
    (is eq t (test-match-parse (sym :wild 'x :wild) "p:x"))
    (is eq t (test-match-parse (sym :wild :x :wild) "p:x"))
    (is eq t (test-match-parse (sym :wild :x :wild) "p:X"))
    (is eq t (test-match-parse (sym :wild "X" :wild) "p:x"))
    (is eq nil (test-match-parse (sym :wild "x" :wild) "p:X")))
;;; :possibly-internal-symbol
  (progn
    (is eq t (test-match-parse (sym :wild "X" :wild) "p::X"))
    (is eq t (test-match-parse (sym :wild 'x :wild) "p::X"))
    (is eq t (test-match-parse (sym :wild 'x :wild) "p::x"))
    (is eq t (test-match-parse (sym :wild :x :wild) "p::x"))
    (is eq t (test-match-parse (sym :wild :x :wild) "p::X"))
    (is eq t (test-match-parse (sym :wild "X" :wild) "p::x"))
    (is eq nil (test-match-parse (sym :wild "x" :wild) "p::X"))))

(define-test+run "match sym - :wild package-name"
;;; :current
  (progn
    (progn
      (is eq t (test-match-parse (sym :wild :x :current) "x"))
      (is eq nil (test-match-parse (sym :wild "x" :current) "x"))
      (is eq t (test-match-parse (sym :wild "X" :current) "x")))
    (progn
      (is eq nil (test-match-parse (sym :wild :x :keyword) "x"))
      (is eq nil (test-match-parse (sym :wild "x" :keyword) "x"))
      (is eq nil (test-match-parse (sym :wild "X" :keyword) "x")))
    (progn
      (is eq nil (test-match-parse (sym :wild :x :uninterned) "x"))
      (is eq nil (test-match-parse (sym :wild "x" :uninterned) "x"))
      (is eq nil (test-match-parse (sym :wild "X" :uninterned) "x")))
    (progn
      (is eq nil (test-match-parse (sym :wild :x :qualified) "x"))
      (is eq nil (test-match-parse (sym :wild "x" :qualified) "x"))
      (is eq nil (test-match-parse (sym :wild "X" :qualified) "x")))
    (progn
      (is eq nil (test-match-parse (sym :wild :x :possibly-internal) "x"))
      (is eq nil (test-match-parse (sym :wild "x" :possibly-internal) "x"))
      (is eq nil (test-match-parse (sym :wild "X" :possibly-internal) "x"))))
;;; :keyword
  (progn
    (progn
      (is eq nil (test-match-parse (sym :wild :x :current) ":x"))
      (is eq nil (test-match-parse (sym :wild "x" :current) ":x"))
      (is eq nil (test-match-parse (sym :wild "X" :current) ":x")))
    (progn
      (is eq t (test-match-parse (sym :wild :x :keyword) ":x"))
      (is eq nil (test-match-parse (sym :wild "x" :keyword) ":x"))
      (is eq t (test-match-parse (sym :wild "X" :keyword) ":x")))
    (progn
      (is eq nil (test-match-parse (sym :wild :x :uninterned) ":x"))
      (is eq nil (test-match-parse (sym :wild "x" :uninterned) ":x"))
      (is eq nil (test-match-parse (sym :wild "X" :uninterned) ":x")))
    (progn
      (is eq nil (test-match-parse (sym :wild :x :qualified) ":x"))
      (is eq nil (test-match-parse (sym :wild "x" :qualified) ":x"))
      (is eq nil (test-match-parse (sym :wild "X" :qualified) ":x")))
    (progn
      (is eq nil (test-match-parse (sym :wild :x :possibly-internal) ":x"))
      (is eq nil (test-match-parse (sym :wild "x" :possibly-internal) ":x"))
      (is eq nil (test-match-parse (sym :wild "X" :possibly-internal) ":x"))))
;;; :uninterned-symbol
  (progn
    (progn
      (is eq nil (test-match-parse (sym :wild :x :current) "#:x"))
      (is eq nil (test-match-parse (sym :wild "x" :current) "#:x"))
      (is eq nil (test-match-parse (sym :wild "X" :current) "#:x")))
    (progn
      (is eq nil (test-match-parse (sym :wild :x :keyword) "#:x"))
      (is eq nil (test-match-parse (sym :wild "x" :keyword) "#:x"))
      (is eq nil (test-match-parse (sym :wild "X" :keyword) "#:x")))
    (progn
      (is eq t (test-match-parse (sym :wild :x :uninterned) "#:x"))
      (is eq nil (test-match-parse (sym :wild "x" :uninterned) "#:x"))
      (is eq t (test-match-parse (sym :wild "X" :uninterned) "#:x")))
    (progn
      (is eq nil (test-match-parse (sym :wild :x :qualified) "#:x"))
      (is eq nil (test-match-parse (sym :wild "x" :qualified) "#:x"))
      (is eq nil (test-match-parse (sym :wild "X" :qualified) "#:x")))
    (progn
      (is eq nil (test-match-parse (sym :wild :x :possibly-internal) "#:x"))
      (is eq nil (test-match-parse (sym :wild "x" :possibly-internal) "#:x"))
      (is eq nil (test-match-parse (sym :wild "X" :possibly-internal) "#:x"))))
;;; :qualified-symbol
  (progn
    (progn
      (is eq nil (test-match-parse (sym :wild :x :current) "p:x"))
      (is eq nil (test-match-parse (sym :wild "x" :current) "p:x"))
      (is eq nil (test-match-parse (sym :wild "X" :current) "p:x")))
    (progn
      (is eq nil (test-match-parse (sym :wild :x :keyword) "p:x"))
      (is eq nil (test-match-parse (sym :wild "x" :keyword) "p:x"))
      (is eq nil (test-match-parse (sym :wild "X" :keyword) "p:x")))
    (progn
      (is eq nil (test-match-parse (sym :wild :x :uninterned) "p:x"))
      (is eq nil (test-match-parse (sym :wild "x" :uninterned) "p:x"))
      (is eq nil (test-match-parse (sym :wild "X" :uninterned) "p:x")))
    (progn
      (is eq t (test-match-parse (sym :wild :x :qualified) "p:x"))
      (is eq nil (test-match-parse (sym :wild "x" :qualified) "p:x"))
      (is eq t (test-match-parse (sym :wild "X" :qualified) "p:x")))
    (progn
      (is eq nil (test-match-parse (sym :wild :x :possibly-internal) "p:x"))
      (is eq nil (test-match-parse (sym :wild "x" :possibly-internal) "p:x"))
      (is eq nil (test-match-parse (sym :wild "X" :possibly-internal) "p:x"))))
;;; :possibly-internal-symbol
  (progn
    (progn
      (is eq nil (test-match-parse (sym :wild :x :current) "p::x"))
      (is eq nil (test-match-parse (sym :wild "x" :current) "p::x"))
      (is eq nil (test-match-parse (sym :wild "X" :current) "p::x")))
    (progn
      (is eq nil (test-match-parse (sym :wild :x :keyword) "p::x"))
      (is eq nil (test-match-parse (sym :wild "x" :keyword) "p::x"))
      (is eq nil (test-match-parse (sym :wild "X" :keyword) "p::x")))
    (progn
      (is eq nil (test-match-parse (sym :wild :x :uninterned) "p::x"))
      (is eq nil (test-match-parse (sym :wild "x" :uninterned) "p::x"))
      (is eq nil (test-match-parse (sym :wild "X" :uninterned) "p::x")))
    (progn
      (is eq nil (test-match-parse (sym :wild :x :qualified) "p::x"))
      (is eq nil (test-match-parse (sym :wild "x" :qualified) "p::x"))
      (is eq nil (test-match-parse (sym :wild "X" :qualified) "p::x")))
    (progn
      (is eq t (test-match-parse (sym :wild :x :possibly-internal) "p::x"))
      (is eq nil (test-match-parse (sym :wild "x" :possibly-internal) "p::x"))
      (is eq t (test-match-parse (sym :wild "X" :possibly-internal) "p::x")))))

(define-test+run "match sym - :wild symbol-name"
  #++
  (;; Notes:
   ;; These combinations don't make sense:
   (progn
     (sym nil :wild :current)
     (sym nil :wild :keyword)
     (sym nil :wild :qualified)
     (sym nil :wild :possibly-internal)
     (sym :apackage :wild :uninterned)
     (sym :apackage :wild :keyword))
   ;; These combinations are redundant:
   (progn
     (sym nil :wild :uninterned)
     (sym :keyword :wild :keyword))
   ;; These combinations are actually useful:
   (progn
     (sym :apackage :wild :current)
     (sym :apackage :wild :qualified)
     (sym :apackage :wild :possibly-internal)))
;;; :current
  (progn
    (is eq nil (test-match-parse (sym nil :wild :uninterned) "x"))
    (is eq nil (test-match-parse (sym :keyword :wild :keyword) "x"))
    ;; not sure: this assumes that the current package is not the
    ;; correct one, but it could be
    (is eq nil (test-match-parse (sym :keyword :wild :current) "x"))
    (is eq nil (test-match-parse (sym :keyword :wild :qualified) "x"))
    (is eq nil (test-match-parse (sym :keyword :wild :possibly-internal) "x"))
    ;; not sure: this assumes that the current package is the correct
    ;; one, but it might not be
    (is eq t (test-match-parse (sym :cl :wild :current) "x"))
    (is eq nil (test-match-parse (sym :cl :wild :qualified) "x"))
    (is eq nil (test-match-parse (sym :cl :wild :possibly-internal) "x")))
;;; :keyword
  (progn
    (is eq nil (test-match-parse (sym nil :wild :uninterned) ":x"))
    (is eq t (test-match-parse (sym :keyword :wild :keyword) ":x"))
    (is eq nil (test-match-parse (sym :keyword :wild :current) ":x"))
    (is eq nil (test-match-parse (sym :keyword :wild :qualified) ":x"))
    (is eq nil (test-match-parse (sym :keyword :wild :possibly-internal) ":x"))
    (is eq nil (test-match-parse (sym :cl :wild :current) ":x"))
    (is eq nil (test-match-parse (sym :cl :wild :qualified) ":x"))
    (is eq nil (test-match-parse (sym :cl :wild :possibly-internal) ":x")))
;;; :uninterned-symbol
  (progn
    (is eq t (test-match-parse (sym nil :wild :uninterned) "#:x"))
    (is eq nil (test-match-parse (sym :keyword :wild :keyword) "#:x"))
    (is eq nil (test-match-parse (sym :keyword :wild :current) "#:x"))
    (is eq nil (test-match-parse (sym :keyword :wild :qualified) "#:x"))
    (is eq nil (test-match-parse (sym :keyword :wild :possibly-internal) "#:x"))
    (is eq nil (test-match-parse (sym :cl :wild :current) "#:x"))
    (is eq nil (test-match-parse (sym :cl :wild :qualified) "#:x"))
    (is eq nil (test-match-parse (sym :cl :wild :possibly-internal) "#:x")))
;;; :qualified-symbol
  (progn
    (is eq nil (test-match-parse (sym nil :wild :uninterned) "p:x"))
    (is eq nil (test-match-parse (sym :keyword :wild :keyword) "p:x"))
    (is eq nil (test-match-parse (sym :keyword :wild :current) "p:x"))
    (is eq nil (test-match-parse (sym :keyword :wild :qualified) "p:x"))
    (is eq nil (test-match-parse (sym :keyword :wild :possibly-internal) "p:x"))
    (is eq nil (test-match-parse (sym :cl :wild :current) "cl:x"))
    (is eq nil (test-match-parse (sym "cl" :wild :qualified) "cl:x"))
    (is eq t (test-match-parse (sym "CL" :wild :qualified) "cl:x"))
    (is eq t (test-match-parse (sym :cl :wild :qualified) "cl:x"))
    (is eq nil (test-match-parse (sym :cl :wild :possibly-internal) "cl:x")))
;;; :possibly-internal-symbol
  (progn
    (is eq nil (test-match-parse (sym nil :wild :uninterned) "p::x"))
    (is eq nil (test-match-parse (sym :keyword :wild :keyword) "p::x"))
    (is eq nil (test-match-parse (sym :keyword :wild :current) "p::x"))
    (is eq nil (test-match-parse (sym :keyword :wild :qualified) "p::x"))
    (is eq nil (test-match-parse (sym :keyword :wild :possibly-internal) "p::x"))
    (is eq nil (test-match-parse (sym :cl :wild :current) "cl::x"))
    (is eq nil (test-match-parse (sym "cl" :wild :qualified) "cl::x"))
    (is eq nil (test-match-parse (sym "CL" :wild :qualified) "cl::x"))
    (is eq t (test-match-parse (sym :cl :wild :possibly-internal) "cl::x"))
    (is eq nil (test-match-parse (sym "cl" :wild :possibly-internal) "cl::x"))
    (is eq t (test-match-parse (sym "CL" :wild :possibly-internal) "cl::x"))))

(define-test+run "match sym - :wild qualification"
;;; :current
  (progn
    (is eq nil (test-match-parse (sym nil "x" :wild) "x"))
    (is eq nil (test-match-parse (sym nil "y" :wild) "x"))
    (is eq nil (test-match-parse (sym :keyword "x" :wild) "x"))
    (is eq nil (test-match-parse (sym :keyword "y" :wild) "x"))
    (is eq t (test-match-parse (sym #.*package* 'x :wild) "x"))
    (is eq nil (test-match-parse (sym #.*package* 'y :wild) "x"))
    (is eq t (test-match-parse (sym :cl 'defun :wild) "defun"))
    (is eq nil (test-match-parse (sym :cl 'defmacro :wild) "defun")))
;;; :keyword
  (progn
    (is eq nil (test-match-parse (sym nil "x" :wild) ":x"))
    (is eq nil (test-match-parse (sym nil "y" :wild) ":x"))
    (is eq nil (test-match-parse (sym :keyword "x" :wild) ":x"))
    (is eq nil (test-match-parse (sym :keyword "y" :wild) ":x"))
    (is eq nil (test-match-parse (sym #.*package* 'x :wild) ":x"))
    (is eq nil (test-match-parse (sym #.*package* 'y :wild) ":x"))
    (is eq nil (test-match-parse (sym :cl 'defun :wild) ":defun"))
    (is eq nil (test-match-parse (sym :cl 'defmacro :wild) ":defun")))
;;; :uninterned-symbol
  (progn
    (is eq nil (test-match-parse (sym nil "x" :wild) "#:x"))
    (is eq nil (test-match-parse (sym nil "y" :wild) "#:x"))
    (is eq nil (test-match-parse (sym :keyword "x" :wild) "#:x"))
    (is eq nil (test-match-parse (sym :keyword "y" :wild) "#:x"))
    (is eq nil (test-match-parse (sym #.*package* 'x :wild) "#:x"))
    (is eq nil (test-match-parse (sym #.*package* 'y :wild) "#:x"))
    (is eq nil (test-match-parse (sym :cl 'defun :wild) "#:defun"))
    (is eq nil (test-match-parse (sym :cl 'defmacro :wild) "#:defun")))
;;; :qualified-symbol
  (progn
    (is eq nil (test-match-parse (sym nil "x" :wild) "p:x"))
    (is eq nil (test-match-parse (sym nil "y" :wild) "p:x"))
    (is eq nil (test-match-parse (sym :p "x" :wild) "p:x"))
    (is eq nil (test-match-parse (sym :p "y" :wild) "p:x"))
    (is eq nil (test-match-parse (sym #.*package* 'x :wild) "p:x"))
    (is eq nil (test-match-parse (sym #.*package* 'y :wild) "p:x"))
    (is eq t (test-match-parse (sym :cl 'defun :wild) "cl:defun"))
    (is eq nil (test-match-parse (sym :cl 'defmacro :wild) "cl:defun")))
;;; :possibly-internal-symbol
  (progn
    (is eq nil (test-match-parse (sym nil "x" :wild) "p::x"))
    (is eq nil (test-match-parse (sym nil "y" :wild) "p::x"))
    (is eq nil (test-match-parse (sym :p "x" :wild) "p::x"))
    (is eq nil (test-match-parse (sym :p "y" :wild) "p::x"))
    (is eq nil (test-match-parse (sym #.*package* 'x :wild) "p::x"))
    (is eq nil (test-match-parse (sym #.*package* 'y :wild) "p::x"))
    (is eq t (test-match-parse (sym :cl 'defun :wild) "cl::defun"))
    (is eq nil (test-match-parse (sym :cl 'defmacro :wild) "cl::defun"))))

(define-test+run "match pattern nil and (nil) against parse trees"
  ;; pattern nil
  (progn
    (false (test-match-parse nil "")
           "Nil should not match a parse tree (to match an empty parse tree, use an empty vector)")
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
    (false (test-match-parse nil "common-lisp-user::nil" nil))
    (false (test-match-parse nil "common-lisp-user:nil" nil)))
  (progn
    (false (test-match-parse nil "")
           "Nil should not match a parse tree (to match an empty parse tree, use an empty vector)")
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
    (false (test-match-parse nil "common-lisp-user::nil" t))
    (false (test-match-parse nil "common-lisp-user:nil" t)))
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
    (true (test-match-parse '(nil) "common-lisp-user::nil"))
    (true (test-match-parse '(nil) "common-lisp-user:nil")))
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
    (true (test-match-parse '(nil) "common-lisp-user::nil" t))
    (true (test-match-parse '(nil) "common-lisp-user:nil" t))))

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
      (false (test-match-parse t "common-lisp-user::t" nil))
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
      (false (test-match-parse t "common-lisp-user::t" t))
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
        (true (test-match-parse '(t) "common-lisp-user::t" nil))
        (true (test-match-parse '(t) "common-lisp-user:t" nil)))
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
        (true (test-match-parse '(t) "common-lisp-user::t" t))
        (true (test-match-parse '(t) "common-lisp-user:t" t))))
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

(defun test-match-vars-against-parse-tree (pattern string
                                            skip-whitespaces-and-comments
                                            expected-binding)
  (let ((binding (test-match-parse pattern string skip-whitespaces-and-comments))
        (state (parse string)))
    (cond
      ((eq t expected-binding)
       (is eq t binding
           "matching ~s against ~s (~s) should have returned T, got ~s."
           pattern string state binding))
      (expected-binding
       (true binding
             "matching ~s against ~s (~s) should have bound something"
             pattern string state)
       (parachute:of-type '(or binding substitutions (eql t)) binding)
       (when (substitutions-p binding)
         (let ((hash-table (breeze.pattern::bindings binding)))
           (is = 1 (hash-table-count hash-table)
               "Expected only 1 binding.")
           (maphash (lambda (var binding*)
                      (declare (ignore var))
                      (setf binding binding*))
                    hash-table)))
       (is eq :?x (from binding)
           "matching ~s against ~s (~s) should have bound the var :?x"
           pattern string state)
       (true (to binding)
             "matching ~s against ~s (~s) should have bound :?x to something"
             pattern string state)
       (is eqv expected-binding (value (to binding))
           "matching ~s against ~s (~s) should have bound :?x to ~s"
           pattern string state expected-binding))
      (t
       (false binding)))))

(define-test+run "match vars against parse trees"
  (progn
    (test-match-vars-against-parse-tree
     :?x "" nil
     (whitespace 0 0))
    (test-match-vars-against-parse-tree
     :?x "" t
     nil)
    (test-match-vars-against-parse-tree
     :?x "x" nil
     (token 0 1 :name "X"))
    (test-match-vars-against-parse-tree
     :?x "x" t
     (token 0 1 :name "X"))
    (test-match-vars-against-parse-tree
     :?x " x" nil
     (whitespace 0 1)))
  (progn
    (test-match-vars-against-parse-tree
     '(:?x) "" nil
     (whitespace 0 0))
    (test-match-vars-against-parse-tree
     '(:?x) "" t
     nil)
    (test-match-vars-against-parse-tree
     '(:?x) "x" nil
     (token 0 1 :name "X"))
    (test-match-vars-against-parse-tree
     '(:?x) "x" t
     (token 0 1 :name "X"))
    (test-match-vars-against-parse-tree
     '(:?x) " x" nil
     (whitespace 0 1))
    (test-match-vars-against-parse-tree
     '(:?x) " x" t
     (token 1 2 :name "X"))
    ;; TODO I'm not sure I like the behaviour of these last 2:
    ;; A. (match '(:?x) (parens 0 4 #((token 1 3))))
    ;; B. (match '((:?x)) (parens 0 4 #((token 1 3))))
    ;; Both bind :?x to (token 1 3), but it would make (more?) sense
    ;; if A bound :?x to (parens ...)
    (test-match-vars-against-parse-tree
     '(:?x) "(42)" nil
     (token 1 3 :name "42"))
    (test-match-vars-against-parse-tree
     '((:?x)) "(42)" nil
     (token 1 3 :name "42"))
    (test-match-vars-against-parse-tree
     '(defun :?x) "(defun f)" t
     (token 7 8 :name "F"))
    (test-match-vars-against-parse-tree
     '(defun :?x) "(defun fgh (x) (* x x x))" t
     (token 7 10 :name "FGH"))
    (test-match-vars-against-parse-tree
     '((:either defun defmethod) :?x) "(defun fgh (x) (* x x x))" t
     (token 7 10 :name "FGH"))
    (test-match-vars-against-parse-tree
     '((:either defun defmethod) :?x) "(defmethod g (x) (1+ x))" t
     (token 11 12 :name "G"))
    (test-match-vars-against-parse-tree
     '(defun (:either :?x (setf :?x))) "(defun fgh (x) (* x x x))" t
     (token 7 10 :name "FGH"))
    (test-match-vars-against-parse-tree
     '(defun (:either :?x (setf :?x))) "(defun (setf x) (* x x x))" t
     (parens 7 15 (vector (token 8 12 :name "SETF") (whitespace 12 13) (token 13 14 :name "X"))))
    (test-match-vars-against-parse-tree
     '((:symbol :define-package :uiop) :?x) "(uiop:define-package foo)" t
     (token 21 24 :name "FOO"))))

(define-test+run "match vector against parse trees"
  (false (test-match-parse 'x "x"))
  (true (test-match-parse #(x) "x"))
  (true (test-match-parse '((x)) "(x)")))

(defun test-either (pattern string expected-binding
                    &optional skip-whitespaces-and-comments)
  (finish
   (let* (($node (make-node-iterator string))
          (pattern (compile-pattern pattern))
          (binding (match pattern $node
                     :skipp (when skip-whitespaces-and-comments
                              #'whitespace-or-comment-node-p))))
     (cond
       ((eq expected-binding t)
        (is eq t binding
            "matching the pattern ~s against the parse tree of ~s should return T, got ~s (of type ~s) instead."
            pattern string binding (type-of binding)))
       (expected-binding
        (and
         (true binding
               "the pattern ~s should have matched the parse tree of ~s"
               pattern string)
         (of-type 'binding binding
                  "matching the pattern ~s against the parse tree of ~s should return an object of type \"binding\", got ~s (of type ~s) instead."
                  pattern string binding (type-of binding))
         (is eq pattern (from binding)
             "the binding from matching the pattern ~s against the parse tree of ~s should bind ~s, but got ~s instead"
             pattern string pattern (from binding))
         (of-type node-iterator (to binding)
                  "the binding from matching the pattern ~s against the parse tree of ~s should bind to an object of type node-iterator, but got ~s (type ~s) instead"
                  pattern string (to binding) (type-of (to binding)))
         (is equalp expected-binding (node-string (to binding))
             "the binding from matching the pattern ~s against the parse tree of ~s should bind _to_ ~s, but got ~s instead"
             pattern string expected-binding (to binding))))
       (t (false binding
                 "the pattern ~s should not have matched the parse tree of ~s"
                 pattern string)))
     binding)))

(define-test+run "match either against parse trees"
  (test-either '(:either a b) "a" t)
  (test-either '(:either a b) "  a " nil)
  (test-either '(:either a b) "  a " t :skipp)
  (test-either '(:either a b) "b" t)
  (test-either '(:either a b) "c" nil)
  (test-either '(:either a b) "breeze.test.analysis::a" t)
  (test-either '(:either a b) "breeze.analysis::a" nil))

(defun test-repetition (pattern string pos-start pos-end
                        &optional skip-whitespaces-and-comments)
  (let* (($node (make-node-iterator string))
         (pattern (compile-pattern pattern))
         (binding (match pattern $node
                    :skipp (when skip-whitespaces-and-comments
                             #'whitespace-or-comment-node-p))))
    (is = pos-end (pos $node)
        "matching the pattern ~s against the parse tree of ~s was expected to advance the iterator to ~s"
        pattern string pos-end)
    (true binding
          "the pattern ~s was expected to match against the parse tree of ~s."
          pattern string)
    (and (plusp pos-end)
         (of-type 'binding binding
                  "matching the pattern ~s against the parse tree of ~s should return an object of type \"binding\", got ~s (of type ~s) instead."
                  pattern string binding (type-of binding))
         (when binding
           (let ((from (from binding))
                 (to (to binding)))
             (is eq pattern from
                 "the binding from matching the pattern ~s against the parse tree of ~s should bind ~s, but got ~s instead"
                 pattern string pattern from)
             (let (($start (getf to :$start))
                   ($end (getf to :$end))
                   (bindings (getf to :bindings))
                   (times (getf to :times)))
               (declare (ignorable bindings times)) ;; TODO
               (is = pos-start (pos $start)
                   "the bindings from matching the pattern ~s against the parse tree of ~s was expected to start at position ~s, but got ~s instead"
                   pattern string pos-start (pos $start))
               (is = pos-end (pos $end)
                   "the bindings from matching the pattern ~s against the parse tree of ~s
was expected to end at position ~s (exclusive), but got ~s instead"
                   pattern string pos-end (pos $end))
               ;; TODO check bindings
               ;; TODO check times
               ))))
    ;; TODO maybe check the depth and/or the whole "positions" slot
    ;; of $node
    binding))

(define-test+run "match maybe against parse trees"
  ;; TODO these don't check the resulting bindings
  (test-repetition '(:maybe a) "a" 0 1)
  (test-repetition '(:maybe a) " a" 1 2 :skipp)
  (test-repetition '(:maybe a) "b" 0 0)
  (test-repetition '(:maybe a) "c" 0 0)
  (test-repetition '(:maybe a) "breeze.test.analysis::a" 0 1)
  (test-repetition '(:maybe a) "breeze.analysis::a" 0 0))

#++
(trace :wherein test-repetition
       match-symbol-to-token
       match
       breeze.analysis::node-string-equal)

(define-test+run "match zero-or-more against parse trees"
  (progn
    (test-repetition '(:zero-or-more a) "a" 0 1)
    (test-repetition '(:zero-or-more a) " a" 1 2 :skipp)
    (test-repetition '(:zero-or-more a) "b" 0 0)
    (test-repetition '(:zero-or-more a) "c" 0 0)
    (test-repetition '(:zero-or-more a) "breeze.test.analysis::a" 0 1)
    (test-repetition '(:zero-or-more a) "breeze.analysis::a" 0 0))
  (progn
    (test-repetition '(:zero-or-more a) "a a a" 0 1)
    (test-repetition '(:zero-or-more a) " a a a " 1 6 :skipp)
    (test-repetition '(:zero-or-more a) "a b" 0 1)
    (test-repetition '(:zero-or-more a) "a b" 0 1 :skipp)
    (test-repetition '(:zero-or-more a) "c" 0 0)
    (test-repetition '(:zero-or-more a) "breeze.test.analysis::a" 0 1)
    (test-repetition '(:zero-or-more a) "breeze.analysis::a" 0 0)))

(define-test+run "match zero-or-more + either against parse trees"
  (progn
    (test-repetition '(:zero-or-more (:either a b))
                     "a" 0 1)
    (test-repetition '(:zero-or-more (:either a b))
                     " a" 1 2 :skipp)
    (test-repetition '(:zero-or-more (:either a b))
                     "b" 0 1)
    (test-repetition '(:zero-or-more (:either a b))
                     "c" 0 0)
    (test-repetition '(:zero-or-more (:either a b))
                     "breeze.test.analysis::a" 0 1)
    (test-repetition '(:zero-or-more (:either a b))
                     "breeze.analysis::a" 0 0))
  (progn
    (test-repetition '(:zero-or-more (:either a b))
                     "a a a" 0 1)
    (test-repetition '(:zero-or-more (:either a b))
                     " a a a " 1 6 :skipp)
    (test-repetition '(:zero-or-more (:either a b))
                     "a b" 0 1)
    (test-repetition '(:zero-or-more (:either a b))
                     "a b" 0 3 :skipp)
    (test-repetition '(:zero-or-more (:either a b))
                     "a b a b b a a c" 0 13 :skipp)
    (test-repetition '(:zero-or-more (:either a b))
                     "c" 0 0)
    (test-repetition '(:zero-or-more (:either a b))
                     "breeze.test.analysis::b" 0 1)
    (test-repetition '(:zero-or-more (:either a b))
                     "breeze.analysis::a" 0 0)))


;;; Basic tree inspection


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


;;; TODO Quotepd

#++
(defun test-quotedp (input)
  (let* ((state (parse input))
         (it (make-node-iterator state)))
    (loop :for i :below (length input)
          :do (goto-position it i)
          :collect (list i (breeze.analysis::quotedp it)))))

#++
(define-test+run quotedp
  (test-quotedp "'a")
  (test-quotedp "`a")
  (test-quotedp ",a")
  (test-quotedp ",@a")
  (test-quotedp ",.a"))
