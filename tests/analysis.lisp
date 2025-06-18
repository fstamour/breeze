(defpackage #:breeze.test.analysis
  (:documentation "Tests for the package breeze.analysis")
  (:use #:cl #:breeze.analysis)
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
                #:termp
                #:term-name)
  ;; importing unexported symbols
  (:import-from #:breeze.analysis
                #:malformed-if-node-p
                #:match-symbol-to-token)
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
  (let ((binding (test-match-parse pattern string skip-whitespaces-and-comments))
        (state (parse string)))
    (cond
      (expected-binding
       (true binding
             "matching ~s against ~s (~s) should have bound something"
             pattern string state)
       (parachute:of-type '(or binding binding-set) binding)
       (when (binding-set-p binding)
         (let ((hash-table (breeze.pattern::bindings binding)))
           (is = 1 (hash-table-count hash-table)
               "Expected only 1 binding.")
           (maphash (lambda (term binding*)
                      (declare (ignore term))
                      (setf binding binding*))
                    hash-table)))
       (is breeze.pattern::term= (term :?x) (from binding)
           "matching ~s against ~s (~s) should have bound the term :?x"
           pattern string state)
       (true (to binding)
             "matching ~s against ~s (~s) should have bound :?x to something"
             pattern string state)
       (is equalp expected-binding (value (to binding))
           "matching ~s against ~s (~s) should have bound :?x to ~s"
           pattern string state expected-binding))
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
