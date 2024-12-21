
(cl:in-package #:cl-user)

(defpackage #:breeze.test.incremental-reader
  (:documentation "Test package for #:breeze.incremental-reader")
  (:use #:cl #:breeze.lossless-reader #:breeze.incremental-reader)
  ;; importing non-exported symbols
  (:import-from #:breeze.incremental-reader
                #:edit-and-parse
                #:check-edit
                #:apply-edit-to-source)
  (:import-from #:parachute
                #:define-test
                #:define-test+run
                #:is
                #:true
                #:false
                #:of-type
                #:fail)
  (:import-from #:breeze.kite
                #:is-equalp*
                #:is-equalp))

(in-package #:breeze.test.incremental-reader)

(define-test+run check-edit
  (progn
    (fail (check-edit "" '(:insert-at 10 " ")) 'error
          "Should detect invalid insertion at position greater than current state's source.")
    (fail (check-edit "" '(:insert-at -1 " ")) 'error
          "Should detect invalid insertion at negative position")
    (fail (check-edit "" '(:insert-at 0 "")) 'error
          "Should detect invalid insertion - empty string"))
  (progn
    (fail (check-edit "" '(:delete-at 10 1)) 'error
          "Should detect invalid deletion at position greater than current state's source.")
    (fail (check-edit "" '(:delete-at -1 1)) 'error
          "Should detect invalid deletion at negative position")
    (fail (check-edit "" '(:delete-at 0 0)) 'error
          "Should detect invalid deletion: deleting 0 characters")
    (fail (check-edit "" '(:delete-at 0 0)) 'error
          "Should detect invalid deletion: deleting < 0 characters")))



(defun test-apply-edit (input edit)
  (let ((state (parse input)))
    (apply-edit-to-source state edit)
    (source state)))

(define-test+run apply-edit
  (is equal "" (test-apply-edit "" '(:insert-at 0 "")))
  (is equal "a" (test-apply-edit "a" '(:insert-at 0 "")))
  (is equal "ba" (test-apply-edit "a" '(:insert-at 0 "b")))
  (is equal "ab" (test-apply-edit "a" '(:insert-at 1 "b")))
  (is equal "a b c" (test-apply-edit "a c" '(:insert-at 2 "b ")))
  (is equal "a b c" (test-apply-edit "a c" '(:insert-at 1 " b")))
  (is equal "b" (test-apply-edit "ab" '(:delete-at 0 1)))
  (is equal "a" (test-apply-edit "ab" '(:delete-at 1 1)))
  (is equal "" (test-apply-edit "ab" '(:delete-at 0 2))))



(defun test-edit-and-parse (input edit
                            &optional expected-output expected-tree)
  (let ((state (parse input)))
    (edit-and-parse state edit)
    (let ((output (unparse state nil))
          (tree (tree state)))
      (if expected-output
          (is-equalp* input output expected-output)
          (is-equalp* input output))
      (if expected-tree
          (is-equalp* input tree expected-tree)
          (is-equalp* input tree)))))

(define-test+run edit-and-parse
  (test-edit-and-parse "" '(:insert-at 0 " ")
                       " " (list (whitespace 0 1)))
  (test-edit-and-parse "" '(:insert-at 0 "a")
                       "a" (list (token 0 1)))
  (test-edit-and-parse "b" '(:insert-at 0 "a ")
                       "a b" (list (token 0 1) (whitespace 1 2) (token 2 3)))
  #++ ;; TODO see "Inserts at the beginning" and why it is broken
  (test-edit-and-parse "b" '(:insert-at 0 "a")
                       "ab" (list (token 0 2)))
  (test-edit-and-parse "a" '(:insert-at 1 " b")
                       "a b" (list (token 0 1) (whitespace 1 2) (token 2 3)))
  (test-edit-and-parse "a c" '(:insert-at 1 " b")
                       "a b c" (list (token 0 1) (whitespace 1 2)
                                     (token 2 3) (whitespace 3 4)
                                     (token 4 5)))
  (test-edit-and-parse "a c" '(:insert-at 2 "b ")
                       "a b c" (list (token 0 1) (whitespace 1 2)
                                     (token 2 3) (whitespace 3 4)
                                     (token 4 5))))

;; TODO make a test to make sure the nodes are re-used here
;; (is equalp "a b c" (test-edit-and-parse "a c" '(:insert-at 1 " b")))

#| TODO randomized test:

1. take a string, parse it

2. create an empty state, add one character at a time;
- incrementally parsing the resulting string
- until the original string as been "reconstructed"

3. compare the 2 resulting parse trees.

|#
