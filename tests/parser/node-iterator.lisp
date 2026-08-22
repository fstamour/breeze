(defpackage #:breeze.test.node-iterator
  (:documentation "Tests for the package breeze.analysis")
  (:use #:cl #:breeze.parse-tree)
  (:import-from #:breeze.iterator
                #:iterator-value)
  (:import-from #:breeze.generics
                #:eqv)
  (:import-from #:breeze.pattern
                #:make-binding)
  (:import-from #:breeze.test-helpers
                #:iterator-at)
  (:import-from #:parachute
                #:define-test
                #:define-test+run
                #:is
                #:is-values
                #:true
                #:false
                #:of-type
                #:finish))

(in-package #:breeze.test.node-iterator)


;;; Enclosing form navigation

(defun test-npp (description &key matcher string at expect)
  "Test normalize-pattern-predicate."
  (is eqv expect (funcall (normalize-pattern-predicate matcher)
                          (iterator-at string :at at))
      "the matcher ~s ~a"
      matcher
      description))

(define-test+run normalize-pattern-predicate
  (is eq #'identity (normalize-pattern-predicate #'identity)
      "normalize-pattern-predicate should return functions as-is, no wrapping")
  (parachute:group (normalize-symbol)
    (test-npp "should not match a symbol"
              :string "a"
              :matcher 'a
              :expect nil)
    (test-npp "should not match an empty list"
              :string "()"
              :matcher 'a
              :expect nil)
    (test-npp "should not match the symbol nil"
              :string "nil"
              :matcher 'a
              :expect nil)
    (test-npp "should match a list that starts with the symbol"
              :string "(a)"
              :matcher 'a
              :expect t)
    (test-npp "should not match a list that starts with a different symbol"
              :string "(b c)"
              :matcher 'c
              :expect nil)
    (test-npp "should match a list that starts with the symbol, even if not at the root"
              :string "(b (c))" :at "(c)"
              :matcher 'c
              :expect t))
  (parachute:group (normalize-list)
    (test-npp "should match the symbol 'a"
              :string "a"
              :matcher '(a)
              :expect t)
    (test-npp "should not match the symbol 'a1"
              :string "a1"
              :matcher '(a1)
              :expect t)
    (test-npp "should not match the symbol 'b"
              :string "b"
              :matcher '(c)
              :expect nil)
    (test-npp "should not match the symbol nil"
              :string "nil"
              :matcher '(d)
              :expect nil)
    (test-npp "should not match the empty list"
              :string "()"
              :matcher '(e)
              :expect nil)
    (test-npp "(named either) should match the symbol 'a"
              :string "a"
              :matcher '(:named ?x (:either a b))
              :expect (make-binding '?x (iterator-value (token 0 1 :name "A"))))
    (parachute:group (vars-wrapped-in-either)
      (test-npp "should match the symbol 'a"
                :string "a"
                :matcher '(:either (:var ?x a) (:var ?x b))
                :expect (make-binding '?x (iterator-value (token 0 1 :name "A"))))
      (test-npp "should match the symbol 'A"
                :string "A"
                :matcher '(:either (:var ?x a) (:var ?x b))
                :expect (make-binding '?x (iterator-value (token 0 1 :name "A"))))
      (test-npp "should match the symbol 'b"
                :string "b"
                :matcher '(:either (:var ?x a) (:var ?x b))
                :expect (make-binding '?x (iterator-value (token 0 1 :name "B")))))
    (parachute:group (either-wrapped-in-var)
      (test-npp "should match the symbol 'a"
                :string "a"
                :matcher '(:either (:var ?x a) (:var ?x b))
                :expect (make-binding '?x (iterator-value (token 0 1 :name "A"))))
      (test-npp "should match the symbol 'A"
                :string "A"
                :matcher '(:either (:var ?x a) (:var ?x b))
                :expect (make-binding '?x (iterator-value (token 0 1 :name "A"))))
      (test-npp "should match the symbol 'b"
                :string "b"
                :matcher '(:either (:var ?x a) (:var ?x b))
                :expect (make-binding '?x (iterator-value (token 0 1 :name "B"))))))
  ;; TODO test passing already-compiled patterns
  #++
  (test-npp)
  ())


#++
(unwind-protect
     (progn
       (trace match
              compile-pattern
              breeze.pattern::%compile-pattern

              find-nearest-enclosing-node
              find-enclosing-node)
       (find-nearest-enclosing-node (iterator-at "(a b)") 'a))
  (untrace))

(defun test-find-nearest (description &key string at matcher expect)
  (is eqv expect (find-nearest-enclosing-node
                  (iterator-at string :at at)
                  matcher)
      "~d~%  :string ~s~%  at: ~s~%  matcher: ~s"
      description string at matcher))

(define-test+run find-nearest-enclosing-node
  (test-find-nearest "no parent node"
                     :matcher 'a
                     :string "a"
                     :expect nil)
  (test-find-nearest "parent node starts with the right symbol"
                     :matcher 'b
                     :string "(b c)" :at "c"
                     :expect (iterator-value (parens 0 5 (nodes (token 1 2 :name "B")
                                                                (whitespace 2 3)
                                                                (token 3 4 :name "C")))))
  (test-find-nearest "parent node start with the wrong symbol"
                     :matcher 'd
                     :string "(e f g)" :at "f"
                     :expect nil)
  (test-find-nearest "nested forms: returns the nearest (innermost) match"
                     :matcher 'h
                     :string "(h (h i))" :at "i"
                     :expect (iterator-value
                              (parens 3 8
                                      ;; notice: it returned the second 'h, not the first
                                      (nodes (token 4 5 :name "H")
                                             (whitespace 5 6)
                                             (token 6 7 :name "I"))))))

;; with-trace (match eqv)

(defun test-find-furthest (description &key string at matcher expect)
  (is eqv expect (find-furthest-enclosing-node
                  (iterator-at string :at at)
                  matcher)
      "~d~%  :string ~s~%  at: ~s~%  matcher: ~s"
      description string at matcher))

(define-test+run find-furthest-enclosing-node
  (test-find-furthest "no parent node"
                      :matcher 'a
                      :string "a"
                      :expect nil)
  (test-find-furthest "parent node starts with the right symbol"
                      :matcher 'b
                      :string "(b c)" :at "c"
                      :expect (iterator-value (parens 0 5 (nodes (token 1 2 :name "B")
                                                                 (whitespace 2 3)
                                                                 (token 3 4 :name "C")))))
  (test-find-furthest "parent node starts with the wrong symbol"
                      :matcher 'd
                      :string "(e f g)" :at "f"
                      :expect nil)
  (test-find-furthest "nested forms: returns the furthest (outermost) match"
                      :matcher 'h
                      :string "(h (h i))" :at "i"
                      :expect (iterator-value
                               (parens 0 9
                                       ;; notice: it returned the first 'h, not the second
                                       (nodes (token 1 2 :name "H")
                                              (whitespace 2 3)
                                              (parens 3 8
                                                      (nodes (token 4 5 :name "H")
                                                             (whitespace 5 6)
                                                             (token 6 7 :name "I"))))))))

(define-test+run find-all-enclosing-nodes
  (false (find-all-enclosing-nodes (iterator-at "a") 'a)
         "no parent node")
  (is eqv (list (iterator-value (parens 0 5 (nodes (token 1 2 :name "B")
                                                   (whitespace 2 3)
                                                   (token 3 4 :name "C")))))
      (find-all-enclosing-nodes (iterator-at "(b c)" :at "c") 'b)
      "one matching parent")
  (is eqv nil
      (find-all-enclosing-nodes (iterator-at "(e f g)" :at "f") 'd)
      "no match")
  (is eqv (list (iterator-value (parens 6 11 :_))
                (iterator-value (parens 0 13 :_)))
      (find-all-enclosing-nodes (iterator-at "(h (g (h i)))" :at "i") 'h)
      "nested forms: returns all matches, nearest-first"))
