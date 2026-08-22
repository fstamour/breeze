(defpackage #:breeze.nlp
  (:documentation "")
  (:use #:cl)
  (:import-from #:breeze.pattern
                #:compile-pattern
                #:match))

(in-package #:breeze.nlp)

(match (compile-pattern `("create" "a" "function"))
  (coerce (uiop:split-string "create a function") 'vector))
;; => T
;; => #<BREEZE.ITERATOR:TREE-ITERATOR depth: 0 pos: #(3) {1216740723}>

(defun process-input (input)
  (map 'vector #'string-upcase
       (uiop:split-string input)))

(defun either (list)
  (breeze.pattern:either (map 'vector #'symbol-name list)))

;; (either '(a b))
;; => #<BREEZE.PATTERN:EITHER 2 {122E6D83C3}>


(let* ((verbs '(create add test))
       (object '(function package class))
       (pattern
         (compile-pattern
          `((:var ?verb ,(either verbs))
            "A"
            (:var ?object ,(either object))))))
  ;; (match (either verbs) "CREATE")
  ;; => T, "CREATE"
  ;; pattern
 (loop :for input in '("create a function"
                        "test a function")
       :for parts := (process-input input)
       :collect parts
       :collect (match pattern parts)))
;; =>
;; (#("CREATE" "A" "FUNCTION") #<breeze.pattern:substitutions (2 bindings)>
;;  #("TEST" "A" "FUNCTION") #<breeze.pattern:substitutions (2 bindings)>)
