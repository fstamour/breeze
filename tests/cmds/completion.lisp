(defpackage #:breeze.test.completion
  (:documentation "Tests for the package breeze.completion.")
  (:use #:cl #:breeze.completion #:breeze.parser)
  (:import-from #:breeze.test-helpers
                #:iterator-at)
  (:import-from #:breeze.completion
                #:maybe-previous-node)
  (:import-from #:parachute
                #:define-test
                #:define-test+run
                #:is
                #:true
                #:false
                #:of-type
                #:finish))

(in-package #:breeze.test.completion)

(defun test-maybe-previous-node (string &key description expected at)
  (multiple-value-bind ($node position)
      (iterator-at string :at at)
    (let* (($original (copy-iterator $node))
           (point (if (numberp at) at position))
           ($result (maybe-previous-node $node point)))
      (is eqv (iterator-value expected) $result
          "~a:~%\"~a\"~%~a~%  (maybe-previous-node ~s ~s)~%returned an iterator on~%  ~s~%but~%  ~s~%was expected."
          description
          string
          (format nil " ~a^" (breeze.string:repeat-string (1- point) " "))
          (value $original) point
          (value $result)
          expected))))

(define-test+run maybe-previous-node
  (test-maybe-previous-node "abc" :at 0 :expected (token 0 3 :name "ABC"))
  (test-maybe-previous-node "def" :at 1 :expected (token 0 3 :name "DEF"))
  (test-maybe-previous-node "ghi" :at 2 :expected (token 0 3 :name "GHI"))
  (test-maybe-previous-node "a " :at 0 :expected (token 0 1 :name "A"))
  (test-maybe-previous-node "b " :at 1 :expected (token 0 1 :name "B"))
  (test-maybe-previous-node "c " :at 2 :expected (token 0 1 :name "C"))
  (test-maybe-previous-node "(a b c)" :at 0 :expected (parens :_ :_ :_))
  (test-maybe-previous-node "(a b c)" :at 1 :expected (token 1 2 :name "A"))
  (test-maybe-previous-node "(a b c)" :at 2 :expected (token 1 2 :name "A"))
  (test-maybe-previous-node "(a b c)" :at 3 :expected (token 3 4 :name "B"))
  (test-maybe-previous-node "(a b c)" :at 4 :expected (token 3 4 :name "B"))
  (test-maybe-previous-node "(a b c)" :at 5 :expected (token 5 6 :name "C"))
  (test-maybe-previous-node "(a b c)" :at 6 :expected (token 5 6 :name "C"))
  (test-maybe-previous-node "(a b c)" :at 7 :expected (token 5 6 :name "C"))
  (test-maybe-previous-node "(make-h-t" :at :end :expected (token 1 9 :name "MAKE-H-T"))
  (test-maybe-previous-node
   "(make-hash-table :t"
   :at :end
   :expected (token 17 19 :package-marker '(17) :name "T"))
  (test-maybe-previous-node
   "(make-hash-table :test "
   :at :end
   :expected (token 17 22 :package-marker '(17) :name "TEST"))
  (test-maybe-previous-node
   "#|  "
   :at :end
   :expected (block-comment 0 -1 :errors '((:_)))))

;; testing it manua
;; (make-ha )
