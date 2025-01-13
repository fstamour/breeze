
(cl:in-package #:cl-user)

(in-package #:breeze.test.lossless-reader)

#|

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
       ;; remainder: the input is only used (unless (equalp got expected))
       ;; the input is used to give
       (labels ((test* (got &optional expected)
                  (is-equalp
                   :input ,string
                   :got got
                   :expected expected
                   :description *state-control-string*
                   :format-args (state-context state)))
                ,@more-labels)
         (declare (ignorable (function test*)))
         ,@ (loop :for (label . _) :in more-labels
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
    (""  (no -1) (no 0)  (no 1))
    (" " (no -1) (yes 0) (no 1))))

(define-test+run donep
  :depends-on (valid-position-p)
  (with-state*+predicates (:test-form (progn (setf (pos state) pos)
                                             (donep state))
                           :extra-args (pos))
    (""  (yes -1) (yes 0) (yes 1))
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


;;; Node structs

(define-test+run node-constructor
  (is equalp (node 'x 0 0) (node 'x 0 0))
  (is equalp (node 'x 0 2 (node 'y 1 2)) (node 'x 0 2 (node 'y 1 2)))
  (is equalp
      (node 'x 0 3 (node 'y 1 2) (node 'z 2 3))
      (node 'x 0 3 (nodes (node 'y 1 2) (node 'z 2 3))))
  (is equalp (node 'parens 1 2) (parens 1 2))
  (is equalp (parens 1 2 'x) (parens 1 2 'x))
  (is equalp
      (parens 1 2 (node 'x 3 4))
      (parens 1 2 (nodes (node 'x 3 4)))))

(defun test-node-print-object (node &optional expected)
  (is-equalp
   :input node
   :got (prin1-to-string node)
   :form `(prin1-to-string ,node)
   :expected expected))

(define-test+run node-print-object
  (test-node-print-object (node 'asdf 1 3) "(node 'asdf 1 3)")
  (test-node-print-object #s(node :type boo :start 1 :end 2) "(node 'boo 1 2)")
  (test-node-print-object
   (list #s(node :type boo :start 1 :end 2))
   "((node 'boo 1 2))")
  (test-node-print-object
   (node 'asdf 1 3 (node 'qwer 3 5))
   "(node 'asdf 1 3 (node 'qwer 3 5))")
  (test-node-print-object
   (node 'asdf 1 3 (list (node 'qwer 3 5) (node 'uiop 6 8)))
   "(node 'asdf 1 3 (list (node 'qwer 3 5) (node 'uiop 6 8)))")
  (test-node-print-object
   (node 'asdf 1 3 (nodes (node 'qwer 3 5) (node 'uiop 6 8)))
   "(node 'asdf 1 3 #((node 'qwer 3 5) (node 'uiop 6 8)))")
  (test-node-print-object
   (node 'asdf 1 3 #((node 'qwer 3 5) (node 'uiop 6 8)))
   "(node 'asdf 1 3 #((node 'qwer 3 5) (node 'uiop 6 8)))")
  (test-node-print-object
   (parens 3 5) "(parens 3 5)")
  (test-node-print-object
   (parens 3 5 'x) "(parens 3 5 x)"))

(define-test+run ensure-nodes
  (false (ensure-nodes nil))
  (is equalp #(t) (ensure-nodes t))
  (is equalp #(t) (ensure-nodes (ensure-nodes t)))
  (is equalp #(a b c) (ensure-nodes '(a b c))))

(define-test+run nodes
  (false (%nodes nil nil))
  (is equalp #(t) (%nodes t nil))
  (is equalp #(t t) (%nodes t t))
  (is equalp #(t) (%nodes nil t))
  (false (nodes))
  (is equalp #(t) (nodes t))
  (is equalp #(a b) (nodes 'a 'b))
  (is equalp #(a b c) (nodes 'a 'b 'c)))


;;; Low-level parsing helpers

(define-test+run read-char*
  :depends-on (current-char)
  (with-state* ()
    (""
     (test* (list (read-char* state) (pos state)) '(nil 0))
     (test* (list (read-char* state #\a) (pos state)) '(nil 0)))
    ("c"
     (test* (list (read-char* state) (pos state)) '(#\c 1))
     (test* (list (read-char* state #\d) (pos state)) '(nil 0)))))

(define-test+run read-string*
  :depends-on (valid-position-p)
  (with-state* ()
    (""
     (test*
      (list
       (read-string* state "")
       (pos state))
      '(nil 0))
     (test*
      (list
       (read-string* state "#")
       (pos state))
      '(nil 0)))
    (";"
     (test*
      (list
       (read-string* state ";;")
       (pos state))
      '(nil 0)))
    (";;"
     (test*
      (list
       (read-string* state ";;")
       (pos state))
      '((0 2) 2)))))

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
                 (block-comment 0 expected-end)))))

(define-test+run read-block-comment
  :depends-on (read-string*)
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



(defparameter *sharpsign-reader-test-cases* (make-hash-table :test 'equal))

(defun test-read-sharpsign* (&key
                               sharpsing-reader-function
                               node-type
                               input
                               expected-end
                               expected-pos
                               expected-children
                               given-numeric-argument)
  "Helps testing the read-sharpsign-* functions.

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

EXPECTED-CHILDREN is the exepected list of node's children

GIVEN-NUMERIC-ARGUMENT is passed to SHARPSING-READER-FUNCTION as the
\"optional sequence of digits\" (in the hyperspec's words) read by
the function read-sharpsign-dispatching-reader-macro
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
      (setf (pos state) starting-position)
    (let* ((expected (node node-type 0
                             expected-end
                             expected-children))
             (got
               (is-equalp
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
        (setf (gethash input *sharpsign-reader-test-cases*) expected)
        (when (and got (plusp expected-end))
          (is-equalp
           :input input
           :expected expected-pos
           :form `(pos ,state)
           :got (pos state)
           :description " the state's position after reading is wrong:"))
        got))))


;;; #\

(defun test-read-sharpsign-backslash (input expected-end)
  (test-read-sharpsign*
   :sharpsing-reader-function 'read-sharpsign-backslash
   :node-type 'sharp-char
   :input input
   :expected-end expected-end
   :expected-children (unless (= +end+ expected-end)
                        (token 1 expected-end))))

(define-test+run read-sharpsign-backslash
  (test-read-sharpsign-backslash "#\\" +end+)
  (test-read-sharpsign-backslash "#\\ " 3)
  (test-read-sharpsign-backslash "#\\  " 3)
  (test-read-sharpsign-backslash "#\\Space" 7)
  (test-read-sharpsign-backslash "#\\Space  " 7)
  (test-read-sharpsign-backslash "#\\ Space" 8)
  (test-read-sharpsign-backslash "#\\bell" 6)
  (test-read-sharpsign-backslash "#\\;" 3))



;;; #'

(defun test-read-sharpsign-quote (input child expected-end)
  (test-read-sharpsign*
   :sharpsing-reader-function #'read-sharpsign-quote
   :node-type 'sharp-function
   :input input
   :expected-end expected-end
   :expected-children child))

(define-test+run read-sharpsign-quote
  (test-read-sharpsign-quote "#'" nil +end+)
  (test-read-sharpsign-quote "#' " (nodes (whitespace 2 3)) +end+)
  (test-read-sharpsign-quote "#'a" (nodes (token 2 3)) 3)
  (test-read-sharpsign-quote "#' a" (nodes (whitespace 2 3)
                                           (token 3 4))
                             4)
  (test-read-sharpsign-quote "#'(lambda...)" (nodes (parens 2 13
                                                            (token 3 12)))
                             13))


;;; #(

(defun test-read-sharpsign-left-parens (input child expected-end)
  (test-read-sharpsign*
   :sharpsing-reader-function #'read-sharpsign-left-parens
   :node-type 'sharp-vector
   :input input
   :expected-end expected-end
   :expected-children child))

(define-test+run read-sharpsign-left-parens
  (test-read-sharpsign-left-parens "#()" (parens 1 3) 3)
  (test-read-sharpsign-left-parens "#( )" (parens 1 4 (whitespace 2 3)) 4)
  (test-read-sharpsign-left-parens '("#1" "()") (parens 2 4) 4)
  (test-read-sharpsign-left-parens '("#2" "( )") (parens 2 5 (whitespace 3 4)) 5))


;;; #*

(defun test-read-sharpsign-asterisk (input &key child end n)
  (test-read-sharpsign*
   :sharpsing-reader-function 'read-sharpsign-asterisk
   :node-type 'sharp-bitvector
   :input input
   :expected-end end
   :expected-children child
   :given-numeric-argument n))

(define-test+run read-sharpsign-asterisk
  (test-read-sharpsign-asterisk '("#" "*"))
  (test-read-sharpsign-asterisk '("#" "* ") :end 2)
  (test-read-sharpsign-asterisk '("#" "*0") :child 0)
  (test-read-sharpsign-asterisk '("#0" "*") :n 0)
  (test-read-sharpsign-asterisk '("#2" "*0") :child 0)
  (test-read-sharpsign-asterisk '("#2" "*0") :n 2 :child 0)
  ;; TODO this is actually a syntax error, as "101" is longer than 2
  (test-read-sharpsign-asterisk '("#2" "*101") :child 5 :n 2))


;;; #:

(defun test-read-sharpsign-colon (input child &optional expected-end)
  (test-read-sharpsign*
   :sharpsing-reader-function 'read-sharpsign-colon
   :node-type 'sharp-uninterned
   :input input
   :expected-end expected-end
   :expected-children child))


(define-test+run read-sharpsign-colon
  (test-read-sharpsign-colon "#:" (token 2 2) 2)
  (test-read-sharpsign-colon "#: " (token 2 2) 2)
  (test-read-sharpsign-colon "#:||" (token 2 4) 4)
  (test-read-sharpsign-colon "#:|| " (token 2 4) 4)
  (test-read-sharpsign-colon "#: a" (token 2 2) 2)
  (test-read-sharpsign-colon "#: a " (token 2 2) 2)
  (test-read-sharpsign-colon "#:asdf" (token 2 6)))

#++
(progn
  (read-from-string "#:")
  (read-from-string "#: ")
  (read-from-string "#: a")
  ;; they all return => #:||
  )


;;; #.

(defun test-read-sharpsign-dot (input child expected-end)
  (test-read-sharpsign*
   :sharpsing-reader-function 'read-sharpsign-dot
   :node-type 'sharp-eval
   :input input
   :expected-end expected-end
   :expected-children child))

(define-test+run read-sharpsign-dot
  (test-read-sharpsign-dot "#." nil +end+)
  (test-read-sharpsign-dot "#.a" (nodes (token 2 3)) 3)
  (test-read-sharpsign-dot "#. a" (nodes (whitespace 2 3)
                                         (token 3 4))
                           4))

;;; #b sharp-binary

(defun test-read-sharpsign-b (input &key child end)
  (test-read-sharpsign*
   :sharpsing-reader-function 'read-sharpsign-b
   :node-type 'sharp-binary
   :input input
   :expected-end end
   :expected-children child))

(define-test+run read-sharpsign-b
  (test-read-sharpsign-b "#b" :end +end+)
  (test-read-sharpsign-b "#B" :end +end+)
  (test-read-sharpsign-b "#bx" :end +end+)
  (test-read-sharpsign-b "#Bx" :end +end+)
  (test-read-sharpsign-b "#b1")
  (test-read-sharpsign-b "#B1")
  (test-read-sharpsign-b "#b666" :end +end+)
  (test-read-sharpsign-b "#b9" :end +end+))

;; TODO this in invalid (float binary): (read-from-string "#b0.1")
;; TODO this in invalid (ratinal binary): (read-from-string "#b0/2")
;; TODO this is valid (rational binary): (read-from-string "#b0/1")


;;; #o sharp-octal

(defun test-read-sharpsign-o (input &key child end)
  (test-read-sharpsign*
   :sharpsing-reader-function 'read-sharpsign-o
   :node-type 'sharp-octal
   :input input
   :expected-end end
   :expected-children child))

(define-test+run read-sharpsign-o
  (test-read-sharpsign-o "#o" :end +end+)
  (test-read-sharpsign-o "#O" :end +end+)
  (test-read-sharpsign-o "#ox" :end +end+)
  (test-read-sharpsign-o "#Ox" :end +end+)
  (test-read-sharpsign-o "#o1")
  (test-read-sharpsign-o "#O1")
  (test-read-sharpsign-o "#o666")
  (test-read-sharpsign-o "#o9" :end +end+))

;; TODO this in invalid (float octal): (read-from-string "#o0.1")
;; TODO this is valid (rational octal): (read-from-string "#o0/3")


;;; #x sharp-hexa

(defun test-read-sharpsign-x (input &key child end)
  (test-read-sharpsign*
   :sharpsing-reader-function 'read-sharpsign-x
   :node-type 'sharp-hexa
   :input input
   :expected-end end
   :expected-children child))

(define-test+run read-sharpsign-x
  (test-read-sharpsign-x "#x" :end +end+)
  (test-read-sharpsign-x "#X" :end +end+)
  (test-read-sharpsign-x "#xx" :end +end+)
  (test-read-sharpsign-x "#Xx" :end +end+)
  (test-read-sharpsign-x "#x1")
  (test-read-sharpsign-x "#X1")
  (test-read-sharpsign-x "#x666")
  (test-read-sharpsign-x "#xF")
  (test-read-sharpsign-x "#xf")
  (test-read-sharpsign-x "#xz" :end +end+))

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

(defun test-read-sharpsign-r (input &key end)
  (check-type input string)
  (let* (;; assumes input is a string
         (input (split-at input #\r))
         ;; assumes input starts with #
         (numeric-argument (let ((prefix (first input)))
                             (when (< 2 (length prefix))
                               (parse-integer prefix :start 1)))))
    (test-read-sharpsign*
     :sharpsing-reader-function 'read-sharpsign-r
     :node-type 'sharp-radix
     :input input
     :expected-end end
     ;; :expected-children child
     :given-numeric-argument numeric-argument)))

 (parse-integer
  "#" :start 1 :junk-allowed t)

;; (trace read-sharpsign-r)

(define-test+run read-sharpsign-r
  ;; no radix
  (progn
    (test-read-sharpsign-r "#r" :end +end+)
    (test-read-sharpsign-r "#R" :end +end+)
    (test-read-sharpsign-r "#rr" :end +end+)
    (test-read-sharpsign-r "#Rr" :end +end+)
    (test-read-sharpsign-r "#r1" :end +end+)
    (test-read-sharpsign-r "#R1" :end +end+)
    (test-read-sharpsign-r "#r666" :end +end+)
    (test-read-sharpsign-r "#rF" :end +end+)
    (test-read-sharpsign-r "#rf" :end +end+)
    (test-read-sharpsign-r "#rz" :end +end+))
  ;; bad radix
  (progn
    (test-read-sharpsign-r "#1r0" :end +end+)
    (test-read-sharpsign-r "#37R0" :end +end+))
  ;; good radix
  (progn
    (test-read-sharpsign-r "#2r" :end +end+)
    (test-read-sharpsign-r "#2R" :end +end+)
    (test-read-sharpsign-r "#2rr" :end +end+)
    (test-read-sharpsign-r "#2Rr" :end +end+)
    ;; TODO WIP those tests don't passes just yet
    #++
    (test-read-sharpsign-r "#2r1")
    #++
    (test-read-sharpsign-r "#2R1")
    #|
    (test-read-sharpsign-r "#2r666" :end +end+)
    (test-read-sharpsign-r "#rF")
    (test-read-sharpsign-r "#rf")
    (test-read-sharpsign-r "#rz" :end +end+)
    |#))


;;; #c sharp-complex

(defun test-read-sharpsign-c (input &key child end)
  (test-read-sharpsign*
   :sharpsing-reader-function 'read-sharpsign-c
   :node-type 'sharp-complex
   :input input
   :expected-end end
   :expected-children child))

(define-test+run read-sharpsign-c
  (test-read-sharpsign-c "#c" :end +end+)
  (test-read-sharpsign-c "#C" :end +end+)
  (test-read-sharpsign-c "#cx" :end +end+)
  (test-read-sharpsign-c "#Cx" :end +end+)
  (test-read-sharpsign-c "#c1" :end +end+)
  (test-read-sharpsign-c "#C1" :end +end+)
  ;; N.B. #c(1) is actually invalid
  (test-read-sharpsign-c "#c(1)"
                         :child (parens 2 5 (node 'token 3 4)))
  (test-read-sharpsign-c "#C(1)"
                         :child (parens 2 5 (node 'token 3 4)))
  (test-read-sharpsign-c "#c(1 2) a"
                         :child (parens 2 7
                                        (nodes (node 'token 3 4)
                                               (node 'whitespace 4 5)
                                               (node 'token 5 6)))
                         :end 7)
  (test-read-sharpsign-c "#C(1 2) a"
                         :child (node 'parens 2 7
                                      (nodes (node 'token 3 4)
                                             (node 'whitespace 4 5)
                                             (node 'token 5 6)))
                         :end 7))


;;; #a

(defun test-read-sharpsign-a (input &key child end n)
  (test-read-sharpsign*
   :sharpsing-reader-function 'read-sharpsign-a
   :node-type 'sharp-array
   :input input
   :expected-end end
   :expected-children child
   :given-numeric-argument n))

(define-test+run read-sharpsign-a
  (test-read-sharpsign-a '("#" "a") :end +end+)
  (test-read-sharpsign-a '("#" "a ") :end +end+)
  (test-read-sharpsign-a '("#" "a0") :end +end+)
  (test-read-sharpsign-a '("#0" "a") :end +end+)
  (test-read-sharpsign-a '("#2" "a0") :end +end+)
  (test-read-sharpsign-a '("#2" "a0") :end +end+)
  ;; TODO this is actually a syntax error, as "101" is longer than 2
  (test-read-sharpsign-a '("#2" "a()") :child (parens 3 5))
  (test-read-sharpsign-a '("#2" "a(1 2)")
                         :child (parens 3 8
                                        (nodes (token 4 5)
                                               (whitespace 5 6)
                                               (token 6 7))))
  (test-read-sharpsign-a '("#2" "A()") :child (parens 3 5)))


;;; #s

(defun test-read-sharpsign-s (input &key child end)
  (test-read-sharpsign*
   :sharpsing-reader-function 'read-sharpsign-s
   :node-type 'sharp-structure
   :input input
   :expected-end end
   :expected-children child))

(define-test+run read-sharpsign-s
  (test-read-sharpsign-s "#s" :end +end+)
  (test-read-sharpsign-s "#S" :end +end+)
  (test-read-sharpsign-s "#S(node)"
                         :child (nodes (parens 2 8 (token 3 7))))
  (test-read-sharpsign-s "#S(node) foo"
                         :child (nodes (parens 2 8 (token 3 7)))
                         :end 8))


;;; #p

(defun test-read-sharpsign-p (input &key child end)
  (test-read-sharpsign*
   :sharpsing-reader-function 'read-sharpsign-p
   :node-type 'sharp-pathname
   :input input
   :expected-end end
   :expected-children child))

(define-test+run read-sharpsign-p
  (test-read-sharpsign-p "#p" :end +end+)
  (test-read-sharpsign-p "#P" :end +end+)
  (test-read-sharpsign-p "#p\"/root/\""
                         :child (nodes (node 'string 2 10))
                         :end 10)
  (test-read-sharpsign-p "#p\"/root/\"  foo"
                         :child (nodes (node 'string 2 10))
                         :end 10))


;;; #=n

(defun test-read-sharpsign-equal (input &key child end)
  (test-read-sharpsign*
   :sharpsing-reader-function 'read-sharpsign-equal
   :node-type 'sharp-label
   :input input
   :expected-end end
   :expected-children child
   :given-numeric-argument (first-node child)))

(define-test+run read-sharpsign-equal
  (test-read-sharpsign-equal "#=" :end +end+)
  (test-read-sharpsign-equal
   '("#1" "=")
   :child (nodes 1)
   :end +end+)
  (test-read-sharpsign-equal
   '("#2" "= ")
   :child (nodes 2 (whitespace 3 4))
   :end +end+)
  (test-read-sharpsign-equal
   '("#3" "=(foo)")
   :child (nodes 3 (parens 3 8 (token 4 7)))))



;;; #n#

(defun test-read-sharpsign-sharpsign (input &key child end)
  (test-read-sharpsign*
   :sharpsing-reader-function 'read-sharpsign-sharpsign
   :node-type 'sharp-reference
   :input input
   :expected-end end
   :expected-children child
   :given-numeric-argument child))

(define-test+run read-sharpsign-sharpsign
  (test-read-sharpsign-sharpsign "##" :end +end+)
  (test-read-sharpsign-sharpsign '("#1" "#") :child 1)
  (test-read-sharpsign-sharpsign '("#2" "# ") :child 2 :end 3))


;;; #+

(defun test-read-sharpsign-plus (input &key child end)
  (test-read-sharpsign*
   :sharpsing-reader-function 'read-sharpsign-plus
   :node-type 'sharp-feature
   :input input
   :expected-end end
   :expected-children child))

(define-test+run read-sharpsign-plus
  (test-read-sharpsign-plus "#+" :end +end+)
  (test-read-sharpsign-plus "#++" :child (nodes (token 2 3)))
  (test-read-sharpsign-plus
   "#+ #+ x"
   :child (nodes (whitespace 2 3)
                 (sharp-feature 3 7
                                (nodes (whitespace 5 6)
                                       (token 6 7))))))


;;; #-

(defun test-read-sharpsign-minus (input &key child end)
  (test-read-sharpsign*
   :sharpsing-reader-function 'read-sharpsign-minus
   :node-type 'sharp-feature-not
   :input input
   :expected-end end
   :expected-children child))

(define-test+run read-sharpsign-minus
  (test-read-sharpsign-minus "#-" :end +end+)
  (test-read-sharpsign-minus "#--" :child (nodes (token 2 3)))
  (test-read-sharpsign-minus
   "#- #- x"
   :child (nodes (whitespace 2 3)
                 (sharp-feature-not 3 7
                                    (nodes (whitespace 5 6)
                                           (token 6 7))))))



(defun test-read-sharpsign (input expected-type expected-end
                            &optional (expected-pos expected-end))
  (with-state (input)
    (let ((got (is-equalp* input
                           (read-sharpsign-dispatching-reader-macro state)
                           (node expected-type 0 expected-end))))
      (when got
        (is-equalp* input
                    expected-pos
                    (pos state))))))

(define-test+run read-sharpsign-dispatching-reader-macro
  (loop :for input :being
          :the :hash-key :of *sharpsign-reader-test-cases*
            :using (hash-value expected)
        :do (with-state (input)
              (is-equalp
               :input input
               :got (read-sharpsign-dispatching-reader-macro state)
               :expected expected
               :form `(read-sharpsign-dispatching-reader-macro ,state)
               ;; :description description
               ;; :format-args format-args
               ))))


;; (read-from-string "#\\ ") == (read-from-string "#\\Space")
;; This is an error (there must be no space between "#s" and "("): (read-from-string "#s ()")




(defun test-read-punctuation (input expected-type)
  (with-state (input)
    (is-equalp* input
                (read-punctuation state)
                (when expected-type
                  (punctuation expected-type 0)))))

(define-test+run read-punctuation
  :depends-on (current-char)
  (test-read-punctuation "" nil)
  (test-read-punctuation " " nil)
  (test-read-punctuation "'" 'quote)
  (test-read-punctuation "`" 'quasiquote)
  (test-read-punctuation "." 'dot)
  (test-read-punctuation "@" 'at)
  (test-read-punctuation "," 'comma)
  (test-read-punctuation "#" 'sharp)
  ;; anything else should return nil
  )


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
             (node 'string 0 expected-end)))))

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


(defun tsn (string &optional (start 0) (end (length string)))
  (%token-symbol-node string start end))

(defun tsn-padded (string)
  (let* ((prefix ":  ")
         (suffix "   ")
         (l (length string))
         (p (length prefix)))
    (tsn (concatenate 'string prefix string suffix)
         p (+ p l))))

(define-test token-symbol-node
  (progn
    (is equalp (node 'current-package-symbol 0 1) (tsn "x"))
    (is equalp (node 'keyword 1 2) (tsn ":x"))
    (is equalp (node 'uninterned-symbol 2 3) (tsn "#:x"))
    (is equalp
        (node 'qualified-symbol 0 3
              (nodes (node 'package-name 0 1)
                     (node 'symbol-name 2 3)))
        (tsn "p:x"))
    (is equalp
        (node 'possibly-internal-symbol 0 4
              (nodes
               (node 'package-name 0 1)
               (node 'symbol-name 3 4)))
        (tsn "p::x"))
    (false (tsn ""))
    (false (tsn "#:"))
    (false (tsn "::"))
    (false (tsn "p:::x"))
    (false (tsn "p::"))
    (false (tsn "::x"))
    (false (tsn "a:a:x")))
  (progn
    (is equalp (node 'current-package-symbol 3 4) (tsn-padded "x"))
    (is equalp (node 'keyword 4 5) (tsn-padded ":x"))
    (is equalp (node 'uninterned-symbol 5 6) (tsn-padded "#:x"))
    (is equalp (node 'qualified-symbol 3 6
                     (nodes (node 'package-name 3 4)
                            (node 'symbol-name 5 6)))
        (tsn-padded "p:x"))
    (is equalp (node 'possibly-internal-symbol 3 7
                     (nodes (node 'package-name 3 4)
                            (node 'symbol-name 6 7)))
        (tsn-padded "p::x"))
    (false (tsn-padded ""))
    (false (tsn-padded "#:"))
    (false (tsn-padded "::"))
    (false (tsn-padded "p:::x"))
    (false (tsn-padded "p::"))
    (false (tsn-padded "::x"))
    (false (tsn-padded "a:a:x"))))


(defun test-read-token (input expected-end)
  (with-state (input)
    (test* (read-token state)
           (when expected-end
             (token 0 expected-end)))))

(define-test+run read-token
  :depends-on (current-char
               not-terminatingp
               read-quoted-string
               read-while)
  (test-read-token "" nil)
  (test-read-token " " nil)
  (test-read-token "+-*/" 4)
  (test-read-token "123" 3)
  (test-read-token "| asdf |" 8)
  (test-read-token "| a\\|sdf |" 10)
  (test-read-token "| asdf |qwer#" 13)
  (test-read-token "arg| asdf | " 11)
  (test-read-token "arg| asdf |more" 15)
  (test-read-token "arg| asdf |more|" +end+)
  (test-read-token "arg| asdf |more|mmoooore|done" 29)
  (test-read-token "arg| asdf |no  |mmoooore|done" 13)
  (test-read-token "look|another\\| case\\| didn't think of| " 38)
  (test-read-token "this.is.normal..." 17)
  (test-read-token "\\asdf" 5)
  (test-read-token "\\;" 2)
  (test-read-token "a\\;" 3))


;; TODO read-extraneous-closing-parens

(defun test-read-parens (input expected-end &rest children)
  (with-state (input)
    (test* (read-parens state)
           (when expected-end
             (parens 0 expected-end (ensure-nodes children))))))

(define-test+run read-parens
  :depends-on (read-char*)
  (test-read-parens ")" nil)
  (test-read-parens "(" +end+)
  (test-read-parens "()" 2)
  (test-read-parens "(x)" 3 (token 1 2))
  (test-read-parens "(.)" 3 (punctuation 'dot 1))
  (test-read-parens "( () )" 6
                    (whitespace 1 2)
                    (parens 2 4)
                    (whitespace 4 5)))

;; TODO read-any
(define-test read-any)


;;; Putting it all toghether

(defun test-parse (input &rest expected)
  (register-test-string input)
  (let* ((state (parse input))
         (tree (tree state)))
    (if expected
        (is-equalp* input tree (ensure-nodes expected))
        (is-equalp* input tree))))

(define-test+run "parse"
  :depends-on (read-parens)
  (eq (parse "") nil)
  (test-parse " (" (whitespace 0 1) (parens 1 +end+))
  (test-parse "  " (whitespace 0 2))
  (test-parse "#|" (block-comment 0 +end+))
  (test-parse " #| "
              (whitespace 0 1)
              (block-comment 1 +end+)
              #++
              (whitespace 3 4))
  (test-parse "#||#" (block-comment 0 4))
  (test-parse "#|#||#" (block-comment 0 +end+))
  (test-parse "#| #||# |#" (block-comment 0 10))
  (test-parse "'" (punctuation 'quote 0))
  (test-parse "`" (punctuation 'quasiquote 0))
  ;; (test-parse "#" (punctuation 'sharp 0))
  (test-parse "," (punctuation 'comma 0))
  (test-parse "+-*/" (token 0 4))
  (test-parse "123" (token 0 3))
  ;; (test-parse "asdf#" (token 0 5))
  (test-parse "| asdf |" (token 0 8))
  (test-parse "arg| asdf | " (token 0 11) (whitespace 11 12))
  (test-parse "arg| asdf |more" (token 0 15))
  (test-parse "arg| asdf |more|" (token 0 +end+))
  (test-parse "arg| asdf " (token 0 +end+))
  (test-parse ";" (line-comment 0 1))
  (test-parse "; " (line-comment 0 2))
  (test-parse (format nil ";~%") (line-comment 0 1) (whitespace 1 2))
  (test-parse (format nil ";~%;") (line-comment 0 1) (whitespace 1 2) (line-comment 2 3))
  (test-parse "(12" (parens 0 +end+ (token 1 3)))
  (test-parse "\"" (node 'string 0 +end+))
  (test-parse "\"\"" (node 'string 0 2))
  (test-parse "#:asdf"
              (node 'sharp-uninterned 0 6
                    (node 'token 2 6)))
  (test-parse "#2()"
              (node 'sharp-vector 0 4
                    (node 'parens 2 4)))
  (test-parse "#<>" (node 'sharp-unknown 0 +end+))
  (test-parse "#+ x" (node 'sharp-feature 0 4
                           (nodes
                            (whitespace 2 3)
                            (token 3 4))))
  (test-parse "(char= #\\; c)"
              (parens 0 13
                      (nodes (token 1 6)
                             (whitespace 6 7)
                             (sharp-char 7 10 (token 8 10))
                             (whitespace 10 11)
                             (token 11 12))))
  (test-parse "(#\\;)" (parens 0 5
                               (nodes (sharp-char 1 4 (token 2 4)))))
  (test-parse "#\\; " (sharp-char 0 3 (token 1 3)) (whitespace 3 4))
  (test-parse "`( asdf)" (node 'quasiquote 0 1)
              (parens 1 8
                      (nodes
                       (whitespace 2 3)
                       (token 3 7))))
  (test-parse "#\\Linefeed" (sharp-char 0 10 (token 1 10)))
  (test-parse "#\\: asd" (sharp-char 0 3 (token 1 3)) (whitespace 3 4) (token 4 7))
  (test-parse "(((  )))" (parens 0 8 (parens 1 7 (parens 2 6 (whitespace 3 5)))))
  (test-parse "(#" (parens 0 +end+ (sharp-unknown 1 +end+)))
  (test-parse "(#)" (parens 0 +end+ (sharp-unknown 1 +end+)))
  (test-parse "(#) "
              (parens 0 +end+ (sharp-unknown 1 +end+))
              #++ (whitespace 3 4))
  (test-parse "(#') "
              (parens
               0 +end+
               (sharp-function
                1 +end+
                (nodes (node ':extraneous-closing-parens 3 +end+)))))
  (test-parse "#1=#1#"
              (sharp-label 0 6
                           (nodes 1 (sharp-reference 3 6 1))))
  (test-parse "(;)" (parens 0 -1 (line-comment 1 3)))
  ;; TODO This is wrong
  (test-parse "#+;;" (sharp-feature 0 4 (nodes (line-comment 2 4))))
  ;; TODO Is that what I want?
  (test-parse "#++;;" (sharp-feature 0 3 (nodes (token 2 3))) (line-comment 3 5))
  ;; TODO This is wrong... but _OMG_
  (test-parse (format nil "cl-user::; wtf~%reaally?")
              (token 0 9) (line-comment 9 14) (whitespace 14 15) (token 15 23))
  ;; TODO This is silly
  (test-parse ",@" (node 'comma 0 1) (node 'at 1 2))
  ;; TODO This is silly
  (test-parse ",." (node 'comma 0 1) (node 'dot 1 2))
  (test-parse "(in-package #)" (parens 0 -1
                                       (nodes (token 1 11)
                                              (whitespace 11 12)
                                              (sharp-unknown 12 -1)))))

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


;;; Unparse

(defun test-round-trip (string &key context check-for-error)
  (register-test-string string)
  (let* ((state (parse string))
         (result (unparse state nil))
         (success (equalp string result)))
    (is-equalp* (or context string) result string)
    (when (and success check-for-error)
      ;; Would be nice to (signal ...), not error, just signal, when
      ;; there's a parsing failure, because right now it's pretty hard
      ;; to pinpoint where something went wrong.
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
  (loop :for file :in (breeze.asdf:system-files 'breeze)
        :for content = (alexandria:read-file-into-string file)
        :do (let* ((state (parse content))
                   (last-node (last-node (tree state)))
                   (result (unparse state nil)))
              (walk state (lambda (node &rest args
                                   &key depth
                                     aroundp beforep afterp
                                     firstp lastp nth
                                   &allow-other-keys)
                            (declare (ignorable
                                      args
                                      depth
                                      aroundp beforep afterp
                                      firstp lastp nth))
                            (unless (valid-node-p node)
                              ;; There's just too many nodes, this
                              ;; makes parachute completely choke if I
                              ;; don't filter the results...
                              (true (valid-node-p node)
                                    "file: ~s node: ~s" file node))
                            #++ (when (parens-node-p node) (char= #\())
                            node))
              (is = (length content) (node-end last-node)
                  "Failed to parse correctly the file ~s. The last node is: ~s"
                  file
                  last-node)
              #++
              (is = (length content) (length result)
                  "Round-tripping the file ~s didn't give the same length.")
              (let ((mismatch (mismatch content result)))
                (false mismatch "Failed to round-trip the file ~s. The first mismatch is at position: "
                       file
                       mismatch)))))
