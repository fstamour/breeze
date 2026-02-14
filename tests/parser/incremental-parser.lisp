(cl:in-package #:cl-user)

;; TODO ensure breeze-minor-mode is enabled (it's not when I open a lisp file _before_ there is any connection...)

(defpackage #:breeze.test.incremental-parser
  (:documentation "Test package for #:breeze.incremental-parser")
  (:use #:cl #:breeze.parser #:breeze.incremental-parser)
  ;; importing non-exported symbols
  (:import-from #:breeze.incremental-parser
                ;; #:edit-and-parse
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
  (:import-from #:breeze+parachute
                #:is-equalp*
                #:is-equalp))

(in-package #:breeze.test.incremental-parser)

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


;; TODO WIP compact edits

(defun compact-inserts (edits)
  (loop
    :with current-edit := (copy-list (first edits))
    :with current-pos := (+ (second current-edit)
                            (length (third current-edit)))
    :for (edit . rest) :on (cdr edits)
    :if (and (eq :insert-at (first current-edit))
             (eq :insert-at (first edit))
             (= current-pos (second edit)))
      :do (setf (third current-edit) (concatenate 'string (third current-edit)
                                                  (third edit)))
          (incf current-pos (length (third edit)))
    :else
      :do (return (list current-edit edit rest))
    :finally
       (return current-edit)))

#++
(compact-inserts '((:INSERT-AT 1675 "c")
                   (:INSERT-AT 1676 "o")
                   (:INSERT-AT 1677 "m")
                   (:INSERT-AT 1678 "p")
                   (:INSERT-AT 1679 "a")
                   (:INSERT-AT 1680 "c")
                   (:INSERT-AT 1681 "t")
                   (:INSERT-AT 1682 " ")
                   (:INSERT-AT 1683 "e")
                   (:INSERT-AT 1684 "d")
                   (:INSERT-AT 1685 "i")
                   (:INSERT-AT 1686 "t")
                   (:INSERT-AT 1687 "s")))



(defun compact-deletes (edits)
  (loop
    :with current-edit := (copy-list (first edits))
    :for edits-left :on (cdr edits)
    :for edit := (first edits-left)
    :if (and (eq :delete-at (first current-edit))
             (eq :delete-at (first edit))
             (= (second current-edit) (second edit)))
      :do (setf (third current-edit) (+ (third current-edit) (third edit)))
    :else
      :return (values current-edit edits-left)
    :finally
       (return current-edit)))

#++
(compact-deletes '((:DELETE-AT 1675 1)
                   (:DELETE-AT 1675 1)
                   (:DELETE-AT 1675 1)
                   (:DELETE-AT 1675 1)))
;; => (:DELETE-AT 1675 4)

#++
(compact-deletes '((:DELETE-AT 1675 1)
                   (:DELETE-AT 1675 1)
                   (:insert-AT 1675 "hola")
                   (:DELETE-AT 1675 1)))
#| =>
(:DELETE-AT 1675 2)
((:INSERT-AT 1675 "hola") (:DELETE-AT 1675 1))
|#

(compact-deletes '((:insert-at 0 "hi")))
;; => (:INSERT-AT 0 "hi")

(compact-deletes '((:delete-at 0 1)))
;; => (:DELETE-AT 0 1)



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
  (is equal "" (test-apply-edit "ab" '(:delete-at 0 2)))
  ;; TODO :replace-at
  (is equal "cd2" (test-apply-edit "" '(:replace-at (0 . 0) "cd2")))
  (is equal "cd1" (test-apply-edit "ab" '(:replace-at (0 . 2) "cd1"))))



#++
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

#++ ;; TODO fix edit-and-parse (it assumses the parse tree is a list)
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
