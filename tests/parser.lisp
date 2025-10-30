
(cl:in-package #:cl-user)

(in-package #:breeze.test.parser)

#|

;;;; TODO Take a look at the relevant ANSI tests: https://gitlab.common-lisp.net/ansi-test/ansi-test/-/tree/master/reader
;;;; TODO Take a look at those reader tests: https://github.com/informatimago/lisp/blob/4bfb6893e7840b748648b749b22078f2facfee0a/common-lisp/lisp-reader/reader-test.lisp

Testing strategies
- generate random strings
- ddmin to reduce
- enable infinite loop guards
- detect when guards are "triggered"
- compare with cl:read
- compare with eclector:read
- test each read-* functions individually
- none should signal errors
- make a generic function validate-node to assert that they make
sense (e.g. a line comment should start with a ; and end with a
newline or +end+)

|#


;;; Reader state


;;; testing helpers

(defmacro with-state ((string &optional more-labels) &body body)
  (alexandria:once-only (string)
    `(let ((state (make-state (register-test-string ,string))))
       ;; Wraps #'is-equal to use the state's source as input
       ;; reminder: the input is only used (unless (eqv got expected))
       (labels ((test* (got &optional expected)
                  (is-equalp
                   :comparator 'eqv
                   :input ,string
                   :got got
                   :expected expected
                   :description *state-control-string*
                   :format-args (state-context state)))
                ,@more-labels)
         (declare (ignorable (function test*)))
         ,@(loop :for (label . _) :in more-labels
                  :collect `(declare (ignorable (function ,label))))
         ,@body))))

(defmacro %with-state* ((string &optional more-labels) &body body)
  (alexandria:once-only (string)
    `(list
      ,@(loop :for form :in body
              :collect `(with-state (,string ,more-labels) ,form)))))

(defmacro with-state* ((&rest more-labels) &body body)
  `(append
    ,@(loop :for form :in body
            :collect `(%with-state*
                          (,(car form) ,more-labels)
                        ,@(rest form)))))

;; TODO better name
(defmacro with-state*+predicates ((&key test-form extra-args more-labels)
                                  &body body)
  `(with-state*
       ((yes (,@extra-args) (test* ,test-form t))
        (no (,@extra-args) (test* ,test-form nil))
        ,@more-labels)
     ,@body))

#++
(with-state ("asdf")
  (test* t t))



#++
(with-state ("asdf")
  (format nil "This is a bug: read-any returned an invalid node, but we're not done reading the file...~%~?"
          *state-control-string*
          (state-context state)))

#++
(with-state ("asdf")
  (state-context state))


;;; Reader position (in the source string)

(define-test+run valid-position-p
  (with-state*+predicates (:test-form (valid-position-p state pos)
                           :extra-args (pos))
    ("" (no -1) (no 0) (no 1))
    (" " (no -1) (yes 0) (no 1))))

(define-test+run donep
  :depends-on (valid-position-p)
  (with-state*+predicates (:test-form (progn (setf (current-position state) pos)
                                             (donep state))
                           :extra-args (pos))
    ("" (yes -1) (yes 0) (yes 1))
    (" " (yes -1) (no 0) (yes 1))
    ("  " (yes -1) (no 0) (no 1) (yes 2))))


;;; Getting and comparing characters

(define-test+run at
  :depends-on (valid-position-p)
  (with-state* ()
    (""
     (test* (at state -1) nil)
     (test* (at state 0) nil)
     (test* (at state 1) nil))
    ("c"
     (test* (at state -1) nil)
     (test* (at state 0) #\c)
     (test* (at state 1) nil))))

(define-test+run at=
  :depends-on (at)
  (with-state* ()
    (""
     (test* (at= state -1 #\a) nil)
     (test* (at= state 0 #\b) nil)
     (test* (at= state 1 #\c) nil))
    ("c"
     (test* (at= state -1 #\c) nil)
     (test* (at= state 0 #\c) #\c)
     (test* (at= state 0 #\a) nil)
     (test* (at= state 1 #\c) nil))))

;; TODO test "current-char"
(define-test+run current-char)

;; TODO test "current-char="
(define-test+run current-char=)

;; TODO test "next-char"
(define-test+run next-char)

;; TODO test "next-char="
(define-test+run next-char=)


;;; Node object

(defun test-node-print-object (node &optional expected)
  (is-equalp
   :input node
   :got (prin1-to-string node)
   :form `(prin1-to-string ,node)
   :expected expected))

;; TODO might need more tests (it used to have more, before I refactor the nodes to be classes.
(define-test+run node-print-object
  (test-node-print-object
   (parens 3 5 nil) "(parens 3 5 nil)")
  (test-node-print-object
   (parens 3 5 'x) "(parens 3 5 x)"))

(define-test+run ensure-nodes
  (false (ensure-nodes nil))
  (is eqv #(t) (ensure-nodes t))
  (is eqv #(t) (ensure-nodes (ensure-nodes t)))
  (is eqv #(a b c) (ensure-nodes '(a b c))))

(define-test+run nodes
  (false (%nodes nil nil))
  (is eqv #(t) (%nodes t nil))
  (is eqv #(t t) (%nodes t t))
  (is eqv #(t) (%nodes nil t))
  (false (nodes))
  (is eqv #(t) (nodes t))
  (is eqv #(a b) (nodes 'a 'b))
  (is eqv #(a b c) (nodes 'a 'b 'c)))


;;; Low-level parsing helpers

(define-test+run read-char*
  :depends-on (current-char)
  (with-state* ()
    (""
     (test* (list (read-char* state) (current-position state)) '(nil 0))
     (test* (list (read-char* state #\a) (current-position state)) '(nil 0)))
    ("c"
     (test* (list (read-char* state) (current-position state)) '(#\c 1))
     (test* (list (read-char* state #\d) (current-position state)) '(nil 0)))))

;; TODO test read-while
(define-test+run read-while)

(defun test-find-all (needle string expected)
  (register-test-string string)
  (register-test-string needle)
  (is-equalp*
   (list 'find-all needle string)
   (find-all needle string)
   expected))

(define-test+run find-all
  (test-find-all "" "" nil)
  (test-find-all "a" "" nil)
  (test-find-all "" "a" nil)
  (test-find-all "a" "aaa" '(0 1 2))
  (test-find-all "b" "aaa" nil))


;;; Actual reader

(defun test-read-whitespaces (input expected-end)
  (with-state (input)
    (test* (read-whitespaces state)
           (when expected-end
             (whitespace 0 expected-end)))))

(define-test+run read-whitespaces
  ;; :depends-on (whitespacep)
  (test-read-whitespaces "" nil)
  (test-read-whitespaces "a" nil)
  (test-read-whitespaces " " 1)
  (test-read-whitespaces "  " 2))

(defun test-read-block-comment (input expected-end)
  (with-state (input)
    (is-equalp
     :input input
     :got (read-block-comment state)
     :form `(read-block-comment ,state)
     :expected (when expected-end
                 (block-comment
                  0 expected-end
                  ;; ATM, that's the only error the "block" comment
                  ;; nodes can have.
                  :errors (when (eq +end+ expected-end)
                            '(("Unterminated block comment")))))
     :comparator 'eqv)))

(define-test+run read-block-comment
  (test-read-block-comment "" nil)
  (test-read-block-comment "#|" +end+)
  (test-read-block-comment "#| " +end+)
  (test-read-block-comment "#||#" 4)
  (test-read-block-comment "#|#" +end+)
  (test-read-block-comment "#|#|#" +end+)
  (test-read-block-comment "#|#||##" +end+)
  (test-read-block-comment "#|#|#|#" +end+)
  (test-read-block-comment "#|#|#||##" +end+)
  (test-read-block-comment "#|#||#|## "
                           ;; There's 9 characters, the last # is not
                           ;; part of any comments
                           8))

(defun test-read-line-comment (input expected-end)
  (with-state ((format nil input))
    (test* (read-line-comment state)
           (when expected-end
             (line-comment 0 expected-end)))))

(define-test read-line-comment
  (test-read-line-comment "" nil)
  (test-read-line-comment ";" 1)
  (test-read-line-comment "; asdf~%" 6))



(defparameter *sharp-reader-test-cases* (make-hash-table :test 'equal))

(defun test-read-sharp* (&key
                           sharpsing-reader-function
                           node-type
                           input
                           expected-end
                           expected-pos
                           expected-children
                           expected-errors
                           extra-initargs
                           given-numeric-argument)
  "Helps testing the read-sharp-* functions.

SHARPSING-READER-FUNCTION is the function under test.

NODE-TYPE is the expected type of the node returned by
SHARPSING-READER-FUNCTION.

INPUT can be a list or a string, this is used to determine where to
start reading. If it is a string it is assumed that the first
character is a #. If it is a list, its element will be concatenated
into one string and the reader will start at the end of the first
element.

EXPECTED-END is the expected node's end. E.g. (true (eql expected-end (node-end node)))

EXPECTED-POS is the expected position of the parser's state after the
SHARPSING-READER-FUNCTION's execution.

EXPECTED-CHILDREN is the expected list of node's children

GIVEN-NUMERIC-ARGUMENT is passed to SHARPSING-READER-FUNCTION as the
\"optional sequence of digits\" (in the hyperspec's words) read by
the function read-sharp-dispatching-reader-macro
"
  (let* ((starting-position (if (listp input) (length (first input)) 1))
         (input (if (listp input) (apply 'concatenate 'string input) input))
         (expected-end (or expected-end (length input)))
         (expected-pos (or expected-pos expected-end))
         (expected-children (if (listp expected-children)
                                ;; coerce to vector
                                (if (breeze.utils:length>1? expected-children)
                                    (ensure-nodes expected-children)
                                    (first expected-children))
                                expected-children)))
    (with-state (input)
      (setf (current-position state) starting-position)
      ;;  make sure expected-children is nil for node types that don't
      ;; support children.
      (if (member node-type '(sharp-reference))
          (when expected-children
            (error "Node type ~s doesn't have a \"children\" slot, but expected-children is not nil, got: ~s."
                   node-type expected-children))
          (setf extra-initargs
                `(:children ,expected-children ,@extra-initargs)))
      (let* ((expected (apply #'make-instance
                              node-type
                              :start 0
                              :end expected-end
                              :errors expected-errors
                              extra-initargs))
             (got
               (is-equalp
                :comparator 'eqv
                :input input
                :got (funcall sharpsing-reader-function
                              state
                              ;; Assumes we started reading the
                              ;; # as the first character.
                              0 ;; TODO shouldn't this be "starting-position"?
                              given-numeric-argument)
                :form (list sharpsing-reader-function
                            state 0 given-numeric-argument)
                :expected expected)))
        ;; Collecting samples for further tests
        (setf (gethash input *sharp-reader-test-cases*) expected)
        (when (and got (plusp expected-end))
          (is-equalp
           :input input
           :expected expected-pos
           :form `(pos ,state)
           :got (current-position state)
           :description " the state's position after reading is wrong:"))
        got))))


;;; #\

(defun test-read-sharp-backslash (input expected-end expected-token-name)
  (test-read-sharp*
   :sharpsing-reader-function 'read-sharp-backslash
   :node-type 'sharp-char
   :input input
   :expected-end expected-end
   :expected-children (unless (= +end+ expected-end)
                        (token 1 expected-end
                               :name expected-token-name))))


(define-test+run read-sharp-backslash
  (test-read-sharp-backslash "#\\a" 3 "a")
  (test-read-sharp-backslash "#\\A" 3 "A")
  (test-read-sharp-backslash "#\\b " 3 "b")
  (test-read-sharp-backslash "#\\B " 3 "B")
  (test-read-sharp-backslash "#\\c(" 3 "c")
  (test-read-sharp-backslash "#\\C(" 3 "C")
  (test-read-sharp-backslash "#\\de" 4 "dE")
  (test-read-sharp-backslash "#\\De" 4 "DE")
  (test-read-sharp-backslash "#\\" +end+ nil)
  (test-read-sharp-backslash "#\\ " 3 " ")
  (test-read-sharp-backslash "#\\  " 3 " ")
  (test-read-sharp-backslash "#\\Space" 7 "SPACE")
  (test-read-sharp-backslash "#\\Space  " 7 "SPACE")
  (test-read-sharp-backslash "#\\  Space" 3 " ")
  (test-read-sharp-backslash "#\\ Space" 8 " SPACE")
  (test-read-sharp-backslash "#\\bell" 6 "bELL")
  (test-read-sharp-backslash "#\\;" 3 ";"))


;;; #'

(defun test-read-sharp-quote (input child expected-end)
  (test-read-sharp*
   :sharpsing-reader-function #'read-sharp-quote
   :node-type 'sharp-function
   :input input
   :expected-end expected-end
   :expected-children child))

(define-test+run read-sharp-quote
  (test-read-sharp-quote "#'" nil +end+)
  (test-read-sharp-quote "#' " (nodes (whitespace 2 3)) +end+)
  (test-read-sharp-quote "#'a" (nodes (token 2 3 :name "A")) 3)
  (test-read-sharp-quote "#' a" (nodes (whitespace 2 3)
                                           (token 3 4 :name "A"))
                             4)
  (test-read-sharp-quote "#'(lambda...)" (nodes (parens 2 13
                                                        (nodes (token 3 12 :name "LAMBDA..."))))
                             13))


;;; #(

(defun test-read-sharp-left-parens (input child expected-end)
  (test-read-sharp*
   :sharpsing-reader-function #'read-sharp-left-parens
   :node-type 'sharp-vector
   :input input
   :expected-end expected-end
   :expected-children child))

(define-test+run read-sharp-left-parens
  (test-read-sharp-left-parens "#()" (parens 1 3 nil) 3)
  (test-read-sharp-left-parens "#( )" (parens 1 4 (nodes (whitespace 2 3))) 4)
  (test-read-sharp-left-parens '("#1" "()") (parens 2 4 nil) 4)
  (test-read-sharp-left-parens '("#2" "( )") (parens 2 5 (nodes (whitespace 3 4))) 5))


;;; #*

(defun test-read-sharp-asterisk (input &key child end n)
  (test-read-sharp*
   :sharpsing-reader-function 'read-sharp-asterisk
   :node-type 'sharp-bitvector
   :input input
   :expected-end end
   :expected-children child
   :given-numeric-argument n))

(define-test+run read-sharp-asterisk
  (test-read-sharp-asterisk '("#" "*"))
  (test-read-sharp-asterisk '("#" "* ") :end 2)
  (test-read-sharp-asterisk '("#" "*0") :child 0)
  (test-read-sharp-asterisk '("#0" "*") :n 0)
  (test-read-sharp-asterisk '("#2" "*0") :child 0)
  (test-read-sharp-asterisk '("#2" "*0") :n 2 :child 0)
  ;; TODO this is actually a syntax error, as "101" is longer than 2
  (test-read-sharp-asterisk '("#2" "*101") :child 5 :n 2))


;;; #:

(defun test-read-sharp-colon (input child expected-end
                              &optional expected-errors)
  (test-read-sharp*
   :sharpsing-reader-function 'read-sharp-colon
   :node-type 'sharp-uninterned
   :input input
   :expected-end expected-end
   :expected-children child
   :expected-errors expected-errors))


(define-test+run read-sharp-colon
  (test-read-sharp-colon "#:" (token 2 2 :name "") 2)
  (test-read-sharp-colon "#: " (token 2 2 :name "") 2)
  (test-read-sharp-colon "#:||" (token 2 4 :name "") 4)
  (test-read-sharp-colon "#:|| " (token 2 4 :name "") 4)
  (test-read-sharp-colon "#: a" (token 2 2 :name "") 2)
  (test-read-sharp-colon "#: a " (token 2 2 :name "") 2)
  (test-read-sharp-colon "#:asdf" (token 2 6 :name "ASDF") nil))

#++
(progn
  (read-from-string "#:")
  (read-from-string "#: ")
  (read-from-string "#: a")
  ;; they all return => #:||
  )


;;; #.

(defun test-read-sharp-dot (input child expected-end)
  (test-read-sharp*
   :sharpsing-reader-function 'read-sharp-dot
   :node-type 'sharp-eval
   :input input
   :expected-end expected-end
   :expected-children child))

(define-test+run read-sharp-dot
  (test-read-sharp-dot "#." nil +end+)
  (test-read-sharp-dot "#.a" (nodes (token 2 3 :name "A")) 3)
  (test-read-sharp-dot "#. a" (nodes (whitespace 2 3)
                                         (token 3 4 :name "A")) 4))

;;; #b sharp-binary

(defun test-read-sharp-b (input &key child end)
  (test-read-sharp*
   :sharpsing-reader-function 'read-sharp-b
   :node-type 'sharp-binary
   :input input
   :expected-end end
   :expected-children child))

(define-test+run read-sharp-b
  (test-read-sharp-b "#b" :end +end+)
  (test-read-sharp-b "#B" :end +end+)
  (test-read-sharp-b "#bx" :end +end+)
  (test-read-sharp-b "#Bx" :end +end+)
  (test-read-sharp-b "#b1")
  (test-read-sharp-b "#B1")
  (test-read-sharp-b "#b666" :end +end+)
  (test-read-sharp-b "#b9" :end +end+))

;; TODO this in invalid (float binary): (read-from-string "#b0.1")
;; TODO this in invalid (ratinal binary): (read-from-string "#b0/2")
;; TODO this is valid (rational binary): (read-from-string "#b0/1")


;;; #o sharp-octal

(defun test-read-sharp-o (input &key child end)
  (test-read-sharp*
   :sharpsing-reader-function 'read-sharp-o
   :node-type 'sharp-octal
   :input input
   :expected-end end
   :expected-children child))

(define-test+run read-sharp-o
  (test-read-sharp-o "#o" :end +end+)
  (test-read-sharp-o "#O" :end +end+)
  (test-read-sharp-o "#ox" :end +end+)
  (test-read-sharp-o "#Ox" :end +end+)
  (test-read-sharp-o "#o1")
  (test-read-sharp-o "#O1")
  (test-read-sharp-o "#o666")
  (test-read-sharp-o "#o9" :end +end+))

;; TODO this in invalid (float octal): (read-from-string "#o0.1")
;; TODO this is valid (rational octal): (read-from-string "#o0/3")


;;; #x sharp-hexa

(defun test-read-sharp-x (input &key child end)
  (test-read-sharp*
   :sharpsing-reader-function 'read-sharp-x
   :node-type 'sharp-hexa
   :input input
   :expected-end end
   :expected-children child))

(define-test+run read-sharp-x
  (test-read-sharp-x "#x" :end +end+)
  (test-read-sharp-x "#X" :end +end+)
  (test-read-sharp-x "#xx" :end +end+)
  (test-read-sharp-x "#Xx" :end +end+)
  (test-read-sharp-x "#x1")
  (test-read-sharp-x "#X1")
  (test-read-sharp-x "#x666")
  (test-read-sharp-x "#xF")
  (test-read-sharp-x "#xf")
  (test-read-sharp-x "#xz" :end +end+))

;; TODO this in invalid (float hex): (read-from-string "#x0.1")
;; TODO this is valid (rational hex): (read-from-string "#x0/3")


;;; #r sharp-radix

(defun split-at (string char)
  (let ((p (position-if (lambda (c)
                          (char-equal c char))
                        string)))
    (list (subseq string 0 p)
          (subseq string p))))

;; (split-at "#01r23" #\r)
;; => ("#01" "r23")

(defun test-read-sharp-r (input &key end errors)
  (check-type input string)
  (let* (;; assumes input is a string
         (input (split-at input #\r))
         ;; assumes input starts with #
         (numeric-argument (let ((prefix (first input)))
                             (when (< 1 (length prefix))
                               ;; :start 1 to skip the "#"
                               (parse-integer prefix :start 1))))
         (child (let ((suffix (second input)))
                  (when (< 1 (length suffix))
                    ;; :start 1 to skip the "r"
                    (ignore-errors
                     (parse-integer suffix :start 1
                                    :radix numeric-argument))))))
    (test-read-sharp*
     :sharpsing-reader-function 'read-sharp-r
     :node-type 'sharp-radix
     :input input
     :expected-end end
     :expected-children child
     :extra-initargs `(:radix ,numeric-argument)
     :expected-errors errors
     :given-numeric-argument numeric-argument)))

(define-test+run read-sharp-r
  ;; no radix
  (progn
    (test-read-sharp-r
     "#r" :end +end+
     :errors '(("Missing radix in #nR reader macro.")))
    (test-read-sharp-r
     "#R" :end +end+
     :errors '(("Missing radix in #nR reader macro.")))
    (test-read-sharp-r
     "#rr" :end +end+
     :errors '(("Missing radix in #nR reader macro.")))
    (test-read-sharp-r
     "#Rr" :end +end+
     :errors '(("Missing radix in #nR reader macro.")))
    (test-read-sharp-r
     "#r1" :end +end+
     :errors '(("Missing radix in #nR reader macro.")))
    (test-read-sharp-r
     "#R1" :end +end+
     :errors '(("Missing radix in #nR reader macro.")))
    (test-read-sharp-r
     "#r666" :end +end+
     :errors '(("Missing radix in #nR reader macro.")))
    (test-read-sharp-r
     "#rF" :end +end+
     :errors '(("Missing radix in #nR reader macro.")))
    (test-read-sharp-r
     "#rf" :end +end+
     :errors '(("Missing radix in #nR reader macro.")))
    (test-read-sharp-r
     "#rz" :end +end+
     :errors '(("Missing radix in #nR reader macro."))))
  ;; bad radix
  (progn
    (test-read-sharp-r
     "#1r0"
     :end +end+
     :errors '(("Illegal radix ~s, must be an integer between 2 and 36 (inclusively)." 1)))
    (test-read-sharp-r "#37R0" :end +end+
                               :errors '(("Illegal radix ~s, must be an integer between 2 and 36 (inclusively)." 37))))
  ;; good radix
  (progn
    (test-read-sharp-r "#2r" :end +end+)
    (test-read-sharp-r "#2R" :end +end+)
    (test-read-sharp-r "#2rr" :end +end+)
    (test-read-sharp-r "#2Rr" :end +end+)
    (test-read-sharp-r "#2r1")
    (test-read-sharp-r "#2R1")
    ;; TODO add an error for this: the radix is valid, but the digits
    ;; don't match
    (test-read-sharp-r "#2r666" :end +end+)
    (test-read-sharp-r "#16rF")
    (test-read-sharp-r "#16rf")
    (test-read-sharp-r "#36rz")))


;;; #c sharp-complex

(defun test-read-sharp-c (input &key child end)
  (test-read-sharp*
   :sharpsing-reader-function 'read-sharp-c
   :node-type 'sharp-complex
   :input input
   :expected-end end
   :expected-children child))

(define-test+run read-sharp-c
  (test-read-sharp-c "#c" :end +end+)
  (test-read-sharp-c "#C" :end +end+)
  (test-read-sharp-c "#cx" :end +end+)
  (test-read-sharp-c "#Cx" :end +end+)
  (test-read-sharp-c "#c1" :end +end+)
  (test-read-sharp-c "#C1" :end +end+)
  ;; N.B. #c(1) is actually invalid
  (test-read-sharp-c "#c(1)"
                         :child (parens 2 5 (nodes (token 3 4 :name "1"))))
  (test-read-sharp-c "#C(1)"
                         :child (parens 2 5 (nodes (token 3 4 :name "1"))))
  (test-read-sharp-c "#c(1 2) a"
                         :child (parens 2 7
                                        (nodes (token 3 4 :name "1")
                                               (whitespace 4 5)
                                               (token 5 6 :name "2")))
                         :end 7)
  (test-read-sharp-c "#C(1 2) a"
                         :child (parens 2 7
                                      (nodes (token 3 4 :name "1")
                                             (whitespace 4 5)
                                             (token 5 6 :name "2")))
                         :end 7))


;;; #a

(defun test-read-sharp-a (input &key child end n)
  (test-read-sharp*
   :sharpsing-reader-function 'read-sharp-a
   :node-type 'sharp-array
   :input input
   :expected-end end
   :expected-children child
   :given-numeric-argument n))

(define-test+run read-sharp-a
  (test-read-sharp-a '("#" "a") :end +end+)
  (test-read-sharp-a '("#" "a ") :end +end+)
  (test-read-sharp-a '("#" "a0") :end +end+)
  (test-read-sharp-a '("#0" "a") :end +end+)
  (test-read-sharp-a '("#2" "a0") :end +end+)
  (test-read-sharp-a '("#2" "a0") :end +end+)
  ;; TODO this is actually a syntax error, as "101" is longer than 2
  (test-read-sharp-a '("#2" "a()") :child (parens 3 5 nil))
  (test-read-sharp-a '("#2" "a(1 2)")
                         :child (parens 3 8
                                        (nodes (token 4 5 :name "1")
                                               (whitespace 5 6)
                                               (token 6 7 :name "2"))))
  (test-read-sharp-a '("#2" "A()") :child (parens 3 5 nil)))


;;; #s

(defun test-read-sharp-s (input &key child end)
  (test-read-sharp*
   :sharpsing-reader-function 'read-sharp-s
   :node-type 'sharp-structure
   :input input
   :expected-end end
   :expected-children child))

(define-test+run read-sharp-s
  (test-read-sharp-s "#s" :end +end+)
  (test-read-sharp-s "#S" :end +end+)
  (test-read-sharp-s "#S(node)"
                         :child (nodes (parens 2 8 (nodes (token 3 7 :name "NODE")))))
  (test-read-sharp-s "#S(node) foo"
                         :child (nodes (parens 2 8 (nodes (token 3 7 :name "NODE"))))
                         :end 8))


;;; #p

(defun test-read-sharp-p (input &key child end)
  (test-read-sharp*
   :sharpsing-reader-function 'read-sharp-p
   :node-type 'sharp-pathname
   :input input
   :expected-end end
   :expected-children child))

(define-test+run read-sharp-p
  (test-read-sharp-p "#p" :end +end+)
  (test-read-sharp-p "#P" :end +end+)
  (test-read-sharp-p "#p\"/root/\""
                         :child (nodes (string-node 2 10))
                         :end 10)
  (test-read-sharp-p "#p\"/root/\"  foo"
                         :child (nodes (string-node 2 10))
                         :end 10))

#|
TODO make sure we handle this

This is a valid syntax:
#p
#+smth "..."
#-smth2 "..."
|#


;;; #n=

(defun test-read-sharp-equal (input &key child end label)
  (test-read-sharp*
   :sharpsing-reader-function 'read-sharp-equal
   :node-type 'sharp-label
   :input input
   :expected-end end
   :expected-children child
   :given-numeric-argument label
   :extra-initargs `(:label ,label)))

(define-test+run read-sharp-equal
  (progn
    (test-read-sharp-equal
     "#="
     :end +end+
     :child nil)
    (test-read-sharp-equal
     '("#1" "=")
     :child nil
     :end +end+
     :label 1)
    (test-read-sharp-equal
     '("#2" "= ")
     :child (nodes (whitespace 3 4))
     :end +end+
     :label 2)
    (test-read-sharp-equal
     '("#3" "=(foo)")
     :child (nodes (parens 3 8 (nodes (token 4 7 :name "FOO"))))
     :label 3)
    (test-read-sharp-equal
     '("#4" "= (foo)")
     :child (nodes (whitespace 3 4) (parens 4 9 (nodes (token 5 8 :name "FOO"))))
     :label 4))
  (progn
    (test-read-sharp-equal
     '("#" "=")
     :child nil
     :end +end+)
    (test-read-sharp-equal
     '("#" "=(bar)")
     :child (nodes (parens 2 7 (nodes (token 3 6 :name "BAR")))))
    (test-read-sharp-equal
     '("#" "= (bar)")
     :child (nodes (whitespace 2 3) (parens 3 8 (nodes (token 4 7 :name "BAR")))))))



;;; #n#

(defun test-read-sharp-sharp (input &key label end errors)
  (test-read-sharp*
   :sharpsing-reader-function 'read-sharp-sharp
   :node-type 'sharp-reference
   :input input
   :expected-end end
   :expected-errors errors
   :extra-initargs `(:label ,label)
   :given-numeric-argument label))

(define-test+run read-sharp-sharp
  (test-read-sharp-sharp
   "##"
   :end +end+
   :errors '(("Invalid label ~s, it must be an unsigned decimal integer." nil)))
  (test-read-sharp-sharp '("#1" "#") :label 1)
  (test-read-sharp-sharp '("#2" "# ") :label 2 :end 3))


;;; #+

(defun test-read-sharp-plus (input &key child end)
  (test-read-sharp*
   :sharpsing-reader-function 'read-sharp-plus
   :node-type 'sharp-feature
   :input input
   :expected-end end
   :expected-children child))

(define-test+run read-sharp-plus
  (test-read-sharp-plus "#+" :end +end+)
  (test-read-sharp-plus "#++" :child (nodes (token 2 3 :name "+")))
  (test-read-sharp-plus
   "#+ #+ x"
   :child (nodes (whitespace 2 3)
                 (sharp-feature 3 7
                                (nodes (whitespace 5 6)
                                       (token 6 7 :name "X")))))
  ;; See: the node "X" is not a children
  (test-read-sharp-plus
   "#+f x"
   :end 3
   :child (nodes (token 2 3 :name "F"))))


;;; #-

(defun test-read-sharp-minus (input &key child end)
  (test-read-sharp*
   :sharpsing-reader-function 'read-sharp-minus
   :node-type 'sharp-feature-not
   :input input
   :expected-end end
   :expected-children child))

(define-test+run read-sharp-minus
  (test-read-sharp-minus "#-" :end +end+)
  (test-read-sharp-minus "#--" :child (nodes (token 2 3 :name "-")))
  (test-read-sharp-minus
   "#- #- x"
   :child (nodes (whitespace 2 3)
                 (sharp-feature-not 3 7
                                    (nodes (whitespace 5 6)
                                           (token 6 7 :name "X"))))))



(defun test-read-sharp (input expected-type expected-end
                            &optional (expected-pos expected-end))
  (with-state (input)
    (let ((got (is-equalp* input
                           (read-sharp-dispatching-reader-macro state)
                           (make-instance expected-type
                                          :start 0
                                          :end expected-end)
                           #'eqv)))
      (when got
        (is-equalp* input
                    expected-pos
                    (current-position state))))))

(define-test+run read-sharp-dispatching-reader-macro
  (loop :for input :being
          :the :hash-key :of *sharp-reader-test-cases*
            :using (hash-value expected)
        :do (with-state (input)
              (is-equalp
               :input input
               :got (read-sharp-dispatching-reader-macro state)
               :expected expected
               :form `(read-sharp-dispatching-reader-macro ,state)
               :comparator 'eqv
               ;; :description description
               ;; :format-args format-args
               ))))

;; (read-from-string "#")

(define-test+run sharp-unknown
  (is eqv (sharp-unknown
           0 +end+)
      (with-state ("#_")
        (read-sharp-dispatching-reader-macro state)))
  (is eqv (sharp-unknown
           0 +end+
           :errors '(("Unterminated dispatch character reader macro.")))
      (with-state ("#")
        (read-sharp-dispatching-reader-macro state))))


;; (read-from-string "#\\ ") == (read-from-string "#\\Space")
;; This is an error (there must be no space between "#s" and "("): (read-from-string "#s ()")




(defun test-read-quote (input expected-type &optional expected-end &rest children)
  (with-state (input)
    (test* (read-quote state)
           (when expected-type
             (make-instance
              expected-type
              :start 0
              :end expected-end
              :children (ensure-nodes children))))))

(define-test+run read-quote
  (test-read-quote "" nil)
  (test-read-quote " " nil)
  (test-read-quote "'" 'quote-node +end+)
  (test-read-quote "' " 'quote-node +end+ (whitespace 1 2))
  (test-read-quote "'a" 'quote-node 2 (token 1 2 :name "A"))
  (test-read-quote "' a" 'quote-node 3 (whitespace 1 2) (token 2 3 :name "A"))
  (test-read-quote "'(a" 'quote-node +end+
                   (parens 1 +end+ (nodes
                                    (token 2 3 :name "A"))
                           :errors '(("Missing closing parenthesis."))))
  (test-read-quote "'(a)" 'quote-node 4 (parens 1 4 (nodes (token 2 3 :name "A"))))
  (test-read-quote "' #||# (a)" 'quote-node 10
                   (whitespace 1 2)
                   (block-comment 2 6)
                   (whitespace 6 7)
                   (parens 7 10 (nodes (token 8 9 :name "A"))))
  (test-read-quote "` #||# (a)" 'quasiquote 10
                   (whitespace 1 2)
                   (block-comment 2 6)
                   (whitespace 6 7)
                   (parens 7 10 (nodes (token 8 9 :name "A"))))
  (test-read-quote "," 'comma +end+)
  (test-read-quote ",a" 'comma 2 (token 1 2 :name "A"))
  (test-read-quote ",@" 'comma-at +end+)
  (test-read-quote ",@a" 'comma-at 3 (token 2 3 :name "A"))
  (test-read-quote ",." 'comma-dot +end+)
  (test-read-quote ",.a" 'comma-dot 3 (token 2 3 :name "A")))



(defun test-read-dot (input expected-type)
  (with-state (input)
    (is-equalp* input
                (read-punctuation state)
                (when expected-type
                  (dot 0 1))
                'eqv)))

(define-test+run read-dot
  :depends-on (current-char)
  (test-read-dot "" nil)
  (test-read-dot " " nil)
  (test-read-dot "." t))


;; TODO Add tests with VALIDP
(define-test+run read-quoted-string
  :depends-on (at)
  (with-state ("")
    (test* (read-quoted-string state #\| #\/) nil))
  (with-state ("|")
    (test* (read-quoted-string state #\| #\/) (range 0 +end+)))
  (with-state ("||")
    (test* (read-quoted-string state #\| #\/) (range 0 2)))
  (with-state ("| |")
    (test* (read-quoted-string state #\| #\/) (range 0 3)))
  (with-state ("|/||")
    (test* (read-quoted-string state #\| #\/) (range 0 4)))
  (with-state ("|/|")
    (test* (read-quoted-string state #\| #\/) (range 0 +end+))))

(defun test-read-string (input expected-end)
  (with-state (input)
    (test* (read-string state)
           (when expected-end
             (string-node 0 expected-end)))))

(define-test+run read-string
  :depends-on (read-quoted-string)
  (test-read-string "" nil)
  (test-read-string "\"" +end+)
  (test-read-string "\"\"" 2)
  (test-read-string "\" \"" 3)
  (test-read-string "\"~s\"" 4))

(define-test+run not-terminatingp
  (mapcar #'(lambda (char)
              (false (not-terminatingp char)
                     "~c is supposed to be a terminating character." char))
          '(#\; #\" #\' #\( #\) #\, #\`)))

(defun test-read-token (input expected-end
                        &optional
                          expected-name
                          expected-package-prefix
                          expected-package-marker
                          expected-errors)
  (with-state (input)
    (test* (read-token state)
           (when expected-end
             (token 0 expected-end
                    :package-prefix expected-package-prefix
                    :package-marker expected-package-marker
                    :name expected-name
                    :errors expected-errors)))))

;; Just making sure that `eqv' seems implemented correctly for the
;; `token` nodes. It should be a good indicator that it is implemented
;; correctly for the other types of nodes, as they are all generated
;; by the same macro.
(define-test+run eqv-token-node
  (true (eqv (token 0 1) (token 0 1)))
  (true (eqv (token 0 1 :name "A") (token 0 1 :name "A")))
  (false (eqv (token 0 1 :name "a") (token 0 1 :name "A")))
  (true (eqv (token 0 1 :errors '(("qwer")))
             (token 0 1 :errors '(("qwer"))))))

(define-test+run read-token
  :depends-on (current-char
               not-terminatingp
               read-quoted-string
               read-while)
  (test-read-token "" nil)
  (test-read-token " " nil)
  (test-read-token "+-*/" 4 "+-*/")
  (test-read-token "123" 3 "123")
  (test-read-token "| asdf |" 8 " asdf ")
  (test-read-token "| a\\|sdf |" 10 " a|sdf ")
  (test-read-token "| asdf |qwer#" 13 " asdf QWER#")
  (test-read-token "arg| asdf | " 11 "ARG asdf ")
  (test-read-token "arg| asdf |more" 15 "ARG asdf MORE")
  (test-read-token "arg| asdf |more|" +end+ "ARG asdf MORE")
  (test-read-token "arg| asdf |more|mmoooore|done" 29
                   "ARG asdf MOREmmooooreDONE")
  (test-read-token "arg| asdf |no  |mmoooore|done" 13
                   "ARG asdf NO")
  (test-read-token "look|another\\| case\\|I didn't think of| " 39
                   "LOOKanother| case|I didn't think of")
  (test-read-token "this.is.normal..." 17 "THIS.IS.NORMAL...")
  (test-read-token "\\asdf" 5 "aSDF")
  (test-read-token "\\;" 2 ";")
  (test-read-token "a\\;" 3 "A;")
  (test-read-token "a\\:b" 4 "A:B")
  (test-read-token "a:b" 3 "B" "A" '(1))
  (test-read-token "a::b" 4 "B" "A" '(2 1))
  (test-read-token "a:::b" 5 "B" "A" '(3 2 1)
                   '(("Invalid package marker.")))
  ;; N.B. here, because of the weird package-markers, the "name" of
  ;; the symbol is "AB", even though it doesn't really make sense.
  (test-read-token "a:b:c" 5 "C" "AB" '(3 1)
                   '(("Invalid package marker.")))
  (test-read-token ":a" 2 "A" nil '(0))
  (test-read-token "p:" 2 nil "P" '(1)
                   '(("Missing name after package marker.")))
  (test-read-token "::" 2 nil nil '(1 0)
                   '(("Missing name after package marker."))))

;; TODO read-extraneous-closing-parens

(defun test-read-parens (input expected-end errors &rest children)
  (with-state (input)
    (test* (read-parens state)
           (when expected-end
             (parens 0 expected-end
                     (ensure-nodes children)
                     :errors errors)))))

(define-test+run read-parens
  :depends-on (read-char*)
  (test-read-parens ")" nil nil)
  (test-read-parens "(" +end+ '(("Missing closing parenthesis.")))
  (test-read-parens "()" 2 nil)
  (test-read-parens "(x)" 3 nil (token 1 2 :name "X"))
  (test-read-parens "(.)" 3 nil (dot 1 2))
  (test-read-parens "( () )" 6 nil
                    (whitespace 1 2)
                    (parens 2 4 nil)
                    (whitespace 4 5)))

;; TODO read-any
(define-test read-any)


;;; Putting it all together

(defun test-parse (input &rest expected)
  (register-test-string input)
  (let* ((state (parse input))
         (tree (tree state)))
    (if expected
        (is-equalp* input tree (ensure-nodes expected) 'eqv)
        (is-equalp* input tree 'eqv))))

(define-test+run "parse"
  :depends-on (read-parens)
  (eq (parse "") nil)
  (test-parse " (" (whitespace 0 1) (parens 1 +end+ nil
                                            :errors '(("Missing closing parenthesis."))))
  (test-parse "  " (whitespace 0 2))
  (test-parse "#|" (block-comment
                    0 +end+
                    :errors '(("Unterminated block comment"))))
  (test-parse " #| "
              (whitespace 0 1)
              (block-comment
               1 +end+
               :errors '(("Unterminated block comment")))
              #++
              (whitespace 3 4))
  (test-parse "#||#" (block-comment 0 4))
  (test-parse "#|#||#" (block-comment
                        0 +end+
                        :errors '(("Unterminated block comment"))))
  (test-parse "#| #||# |#" (block-comment 0 10))
  (test-parse "'" (quote-node 0 +end+ nil))
  (test-parse "`" (quasiquote 0 +end+ nil))
  (test-parse "," (comma 0 +end+ nil))
  (test-parse "+-*/" (token 0 4 :name "+-*/"))
  (test-parse "123" (token 0 3 :name "123"))
  (test-parse "asdf#" (token 0 5 :name "ASDF#"))
  (test-parse "| asdf |" (token 0 8 :name " asdf "))
  (test-parse "arg| asdf | "
              (token 0 11 :name "ARG asdf ")
              (whitespace 11 12))
  (test-parse "arg| asdf |more" (token 0 15 :name "ARG asdf MORE"))
  (test-parse "arg| asdf |more|" (token 0 +end+ :name "ARG asdf MORE"))
  (test-parse "arg| asdf " (token 0 +end+ :name "ARG asdf "))
  (test-parse ";" (line-comment 0 1))
  (test-parse "; " (line-comment 0 2))
  (test-parse (format nil ";~%") (line-comment 0 1) (whitespace 1 2))
  (test-parse (format nil ";~%;") (line-comment 0 1) (whitespace 1 2) (line-comment 2 3))
  (test-parse "(12" (parens 0 +end+ (nodes (token 1 3 :name "12"))
                            :errors '(("Missing closing parenthesis."))))
  (test-parse "\"" (string-node 0 +end+))
  (test-parse "\"\"" (string-node 0 2))
  (test-parse "#:asdf"
              (sharp-uninterned 0 6
                                (token 2 6 :name "ASDF")))
  (test-parse "#2()"
              (sharp-vector 0 4
                            (parens 2 4 nil)))
  (test-parse "#<>" (sharp-unknown 0 +end+))
  (test-parse "#+ x" (sharp-feature 0 4
                                    (nodes
                                     (whitespace 2 3)
                                     (token 3 4 :name "X"))))
  (test-parse "(char= #\\; c)"
              (parens 0 13
                      (nodes (token 1 6 :name "CHAR=")
                             (whitespace 6 7)
                             (sharp-char 7 10 (token 8 10 :name ";"))
                             (whitespace 10 11)
                             (token 11 12 :name "C"))))
  (test-parse "(#\\;)" (parens 0 5
                               (nodes (sharp-char 1 4 (token 2 4 :name ";")))))
  (test-parse "#\\; " (sharp-char 0 3 (token 1 3 :name ";")) (whitespace 3 4))
  (test-parse "`( asdf)"
              (quasiquote 0 8
                          (nodes (parens 1 8
                                         (nodes
                                          (whitespace 2 3)
                                          (token 3 7 :name "ASDF"))))))
  (test-parse "#\\Linefeed" (sharp-char 0 10 (token 1 10 :name "LINEFEED")))
  (test-parse "#\\: asd"
              (sharp-char 0 3 (token 1 3 :name ":"))
              (whitespace 3 4)
              (token 4 7 :name "ASD"))
  (test-parse "(((  )))" (parens 0 8 (nodes (parens 1 7 (nodes (parens 2 6 (nodes (whitespace 3 5))))))))
  (test-parse "(#" (parens
                    0 +end+ (nodes (sharp-unknown
                                    1 +end+
                                    :errors '(("Unterminated dispatch character reader macro."))))
              :errors '(("Missing closing parenthesis."))))
  (test-parse "(#)" (parens
                     0 +end+ (nodes (sharp-unknown 1 +end+))
                     :errors '(("Missing closing parenthesis."))))
  ;; This shows that parsing stops when an error is encountered (or at
  ;; least when a node has `no-end-p'
  (test-parse "(#) "
              (parens
               0 +end+ (nodes (sharp-unknown 1 +end+))
               :errors '(("Missing closing parenthesis.")))
              #++ (whitespace 3 4))
  ;; TODO it would be nice if the "#' node would have an error instead
  ;; of "parens" missing a closing parenthesis and getting an
  ;; "extraneous-closing-parens" node
  ;; TL;DR: this should be 1 error, not 2
  (test-parse "(#') "
              (parens
               0 +end+
               (nodes (sharp-function
                       1 +end+
                       (nodes (extraneous-closing-parens
                               3 +end+
                               :errors '(("Extraneous closing parenthesis."))))))
               :errors '(("Missing closing parenthesis."))))
  (test-parse "#1=#1#"
              (sharp-label 0 6
                           1 (nodes (sharp-reference 3 6 1))))
  (test-parse "(;)" (parens 0 -1 (nodes (line-comment 1 3))
                            :errors '(("Missing closing parenthesis."))))
  ;; TODO This is wrong
  (test-parse "#+;;" (sharp-feature 0 4 (nodes (line-comment 2 4))))
  ;; TODO Is that what I want?
  (test-parse "#++;;" (sharp-feature 0 3 (nodes (token 2 3 :name "+"))) (line-comment 3 5))
  ;; TODO This is wrong... but _OMG_
  (test-parse (format nil "cl-user::; wtf~%reaally?")
              (token 0 9
                     :package-prefix "CL-USER"
                     :package-marker '(8 7)
                     :errors '(("Missing name after package marker.")))
              (line-comment 9 14)
              (whitespace 14 15)
              (token 15 23 :name "REAALLY?"))
  (test-parse "(in-package #)" (parens 0 -1
                                       (nodes (token 1 11 :name "IN-PACKAGE")
                                              (whitespace 11 12)
                                              (sharp-unknown 12 -1))
                                       :errors '(("Missing closing parenthesis."))))
  (test-parse "#!" (shebang 0 2)))

;; (tree (parse ")(#| #r"))

;; TODO this should not result in an "unterminated parens"
;; (tree (parse "(a:)"))
;; #((parens 0 -1 #((token 1 3 :package-prefix "A" :package-marker (2) :errors (("Missing name after package marker."))))))

#++ ;; this is cursed
(read-from-string "cl-user::; wtf
reaally?")

#++ ;; this is an error
(read-from-string "cl-user:; wtf
:reaally?")

;; Slightly cursed syntax:
;; "#+#."
;; e.g. "#+ #.(cl:quote x) 2" == "#+ x 2"

#++
(read-from-string ":\|")

(defparameter *code-with-conflicts-diff3-conflictstyle*
  (concatenate 'string ";;;; Some code with git conflict markers (diff3 style)
(some
 a
" "<<<<<<< Updated upstream
 b
 d)
" "||||||| Stash base
 c)
" "=======
 c
 e)
" ">>>>>>> Stashed changes
"))

(defparameter *code-with-conflicts*
  (concatenate 'string ";;;; Some code with git conflict markers
(some
 a
" "<<<<<<< ours
 b
 d)
" "=======
 c
 e)
" ">>>>>>> theirs"))

(defun search-line-starting-with (prefix string)
  (let ((start (search (concatenate 'string #.(string #\Newline) prefix)
                       string)))
    (when start
      (let ((end (position #\Newline string :start (+ start (length prefix) 1))))
        (range start (or end (length string)))))))

(defun find-git-conflict-markers (string &key (start 0))
  (alexandria:when-let*
      ((ours-marker (search-line-starting-with "<<<<<<< " string))
       (separator-marker (search-line-starting-with "=======" string))
       (theirs-marker (search-line-starting-with ">>>>>>> " string)))
    (let ((base-marker (search-line-starting-with "||||||| " string)))
      ;; prefix: from start to ours' start
      ;; ours: from ours' end to (or base separator)'s start
      ;; base: when base-marker, from base's end to separator's start
      ;; theirs: from separator's end to their's start
      ;; suffix: from their's end to end
      (let ((prefix-range (range start (start ours-marker)))
            (ours-range (range (end ours-marker)
                               (start (or base-marker separator-marker))))
            (base-range (when base-marker
                          (range (end base-marker) (start separator-marker))))
            (theirs-range (range (end separator-marker)
                                 (start theirs-marker)))
            (suffix-range (range (end theirs-marker)
                                 (length string))))
        (list prefix-range
              ours-range
              base-range
              theirs-range
              suffix-range)))))

(defun test-find-git-conflict-markers (string)
  (mapcar (lambda (range)
            (when range
              (subseq string (start range) (end range))))
          (find-git-conflict-markers string)))

(define-test+run find-git-conflict-markers
  (is eqv '(";;;; Some code with git conflict markers
(some
 a"
            "
 b
 d)"
            NIL "
 c
 e)"
            "")
      (test-find-git-conflict-markers *code-with-conflicts*))
  (is eqv '(";;;; Some code with git conflict markers (diff3 style)
(some
 a"
            "
 b
 d)"
            "
 c)"
            "
 c
 e)"
            "
")
      (test-find-git-conflict-markers *code-with-conflicts-diff3-conflictstyle*)))


;;; Unparse

(defun test-round-trip (string &key context check-for-error)
  (register-test-string string)
  (let* ((state (parse string))
         (result (unparse state nil))
         (success (equalp string result)))
    (is-equalp* (or context string) result string)
    #++ ;; TODO this can be adapted to find where the strings differ
    (let ((mismatch (mismatch content result)))
      (false mismatch "Failed to round-trip the file ~s. The first mismatch is at position: ~d"
             file
             mismatch))
    #++ ;; TODO check that the last node's end corresponds to the string's length
    (is = (length content) (node-end last-node)
                  "Failed to parse correctly the file ~s. The last node is: ~s"
                  file
                  last-node)
    (when (and success check-for-error)
      ;; TODO Would be nice to (signal ...), not error, just signal,
      ;; when there's a parsing failure, because right now it's pretty
      ;; hard to pinpoint where something went wrong.
      ;;
      ;; TODO check (recursively) that all nodes are valid
      (let ((bad-node (find-if-not #'valid-node-p (tree state))))
        (setf success
              (true (null bad-node)
                    "Failed to parse correctly ~S~%~?"
                    context
                    *state-control-string*
                    (list
                     (state-context state))))))
    success))

(define-test+run unparse
  (test-round-trip "#' () () ()")
  (test-round-trip " (")
  (loop :for string :being :the :hash-key :of *test-strings*
        :do (test-round-trip string)))

;; TODO make it easier to pin-point errors here...
(define-test+run round-trip-breeze
  (loop :for file :in (breeze.asdf:find-all-related-files 'breeze)
        :for content = (alexandria:read-file-into-string file)
        :do (test-round-trip content)))



(define-test+run node-contains-position-p
  (false (node-contains-position-p (token 1 2) 0))
  (true (node-contains-position-p (token 1 2) 1))
  (false (node-contains-position-p (token 1 2) 2)))

(define-test+run node-range-contains-position-p
  (false (node-range-contains-position-p (token 1 2) (token 5 7) 0))
  (true (node-range-contains-position-p (token 1 2) (token 5 7) 1))
  (true (node-range-contains-position-p (token 1 2) (token 5 7) 2))
  (true (node-range-contains-position-p (token 1 2) (token 5 7) 3))
  (true (node-range-contains-position-p (token 1 2) (token 5 7) 4))
  (true (node-range-contains-position-p (token 1 2) (token 5 7) 5))
  (true (node-range-contains-position-p (token 1 2) (token 5 7) 6))
  (false (node-range-contains-position-p (token 1 2) (token 5 7) 7)))

(define-test+run node-children-contains-position-p
  (false (node-children-contains-position-p (token 1 2) 1))
  (true (node-children-contains-position-p (parens 1 4 (token 1 3)) 1)))

(defun goto-position/all (input)
  (let* ((state (parse input))
         (it (make-node-iterator state)))
    (loop :for i :below (length input)
          :do (goto-position it i)
          :collect (list i (unless (donep it) (node-content state (value it)))))))

(define-test+run goto-position
  (is equal '((0 "a")) (goto-position/all "a"))
  (is equal '((0 "a") (1 " ") (2 "b")) (goto-position/all "a b"))
  (is equal '((0 "(a)") (1 "a") (2 "(a)")) (goto-position/all "(a)"))
  (is equal '((0 "a")
              (1 " ")
              (2 "(b c (d))")
              (3 "b")
              (4 " ")
              (5 "c")
              (6 " ")
              (7 "(d)")
              (8 "d")
              (9 "(d)")
              (10 "(b c (d))")
              (11 "e"))
      (goto-position/all "a (b c (d))e"))
  (is equal '((0 "a")
              (1 " ")
              (2 "(b c (d))")
              (3 "b")
              (4 " ")
              (5 "c")
              (6 " ")
              (7 "(d)")
              (8 "d")
              (9 "(d)")
              (10 "(b c (d))")
              (11 "e"))
      (goto-position/all "a (b c (d))e"))
  (is equal
      '((0 "#:x") (1 "#:x") (2 "x"))
      (goto-position/all "#:x"))
  (is equal
      '((0 ";; #:x") (1 ";; #:x") (2 ";; #:x") (3 ";; #:x") (4 ";; #:x") (5 ";; #:x"))
      (goto-position/all ";; #:x"))
  (is equal
      '((0 "#=3") (1 "#=3") (2 "3"))
      (goto-position/all "#=3"))
  (is equal
      '((0 "#4=") (1 "#4=") (2 "#4="))
      (goto-position/all "#4="))
  (is equal
      '((0 "#\\c") (1 "\\c") (2 "\\c"))
      (goto-position/all "#\\c"))
  (is equal
      '((0 "#(a)") (1 "(a)") (2 "a") (3 "(a)"))
      (goto-position/all "#(a)")))

;; TODO run `goto-position/all' on all "test strings"
