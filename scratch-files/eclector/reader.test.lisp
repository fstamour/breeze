;;;; TODO Take a look at the relevant ANSI tests: https://gitlab.common-lisp.net/ansi-test/ansi-test/-/tree/master/reader
;;;; TODO Take a look at those reader tests: https://github.com/informatimago/lisp/blob/4bfb6893e7840b748648b749b22078f2facfee0a/common-lisp/lisp-reader/reader-test.lisp

#|
This is a valid syntax:
#p
#+smth "..."
#-smth2 "..."
|#



;;; Test syntax-tree's utilities

(define-test form-predicates
  (true (loop-form-p (first (forms (parse-string "(loop)"))))))


;; TODO recurse into non-terminal nodes
#++
(defun contiguousp (nodes input)
  (loop :for node :in nodes
        :for next-node :in (cdr nodes)
        :always
        (or (null next-node)
            (is =
                (1+ (node-end node))
                (node-start next-node)
                "The node ~a ends at ~a and the next-node ~a starts at ~a in the input ~s"
                node (node-end node)
                next-node (node-start next-node)
                input))))

;; TODO This test is not true anymore, but I'll need to port it to the
;;      other reader I made.
#++
(define-test "nodes are contiguous"
  (loop :for input :in '(""
                         "1"
                         " 1"
                         " 1 "
                         " a b c "
                         "\"hi\""
                         ";; hello"
                         " ;; hello"
                         "a ;; hello"
                         "1 #|-|# \"x\" "
                         "( a b c ) d")
        :for nodes = (read-all-forms input)
        :do (true (contiguousp nodes input)
                  "~s is not contiguous" input)))

;; DONE read-all-forms' output should be contiguous (the end of one
;; form should be = to the start of the next form)
;; TODO it should span the whole input.
;; TODO there should be no overlap
;; TODO There shouldn't be any gaps in the node-source
;;
;; ^^^ these are redundant but I want to be extra sure



;;; Test PARSE-STRING

;; TODO Turn this into tests
;; TODO There should be equivalent tests for read-all-forms
#+nil
(loop :for node :in (parse-string "1 #|-|# \"x\" ")
      :collect
      (type-of node)
      ;; (node-raw node)
      )
;; (NODE SKIPPED-NODE STRING-NODE SKIPPED-NODE)


;;; Drafting some generative tests


#|
Each of these can be inserted anywhere
x
1
#C(2 3)
"asdf"
#||#

#'f
'x
'()

This one can only be inserted at the end (of a line)
                                        ; comment

'()
#()

|#

;; TODO (apply #'concatenate 'string (list node)) should= input
