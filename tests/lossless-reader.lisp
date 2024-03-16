
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

#++
(progn
  (node 'boo 1 2)
  #s(node :type boo :start 1 :end 2)
  (list #s(node :type boo :start 1 :end 2))
  (list '#s(node :type boo :start 1 :end 2)))


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
  "Helps testing the read-sharpsign-* functions."
  (let* ((starting-position (if (listp input) (length (first input)) 1))
         (input (if (listp input) (apply 'concatenate 'string input) input))
         (expected-end (or expected-end (length input)))
         (expected-pos (or expected-pos expected-end)))
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
                              0
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
  (test-read-sharpsign-quote "#' " (list (whitespace 2 3)) +end+)
  (test-read-sharpsign-quote "#'a" (list (token 2 3)) 3)
  (test-read-sharpsign-quote "#' a" (list (whitespace 2 3)
                                          (token 3 4))
                             4)
  (test-read-sharpsign-quote "#'(lambda...)" (list (parens 2 13
                                                           (list (token 3 12))))
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
  (test-read-sharpsign-dot "#.a" (list (token 2 3)) 3)
  (test-read-sharpsign-dot "#. a" (list (whitespace 2 3)
                                        (token 3 4))
                           4))


;;; #c

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
                         :child (node 'parens 2 5 (list (node 'token 3 4))))
  (test-read-sharpsign-c "#C(1)"
                         :child (node 'parens 2 5 (list (node 'token 3 4))))
  (test-read-sharpsign-c "#c(1 2) a"
                         :child (node 'parens 2 7
                                      (list (node 'token 3 4)
                                            (node 'whitespace 4 5)
                                            (node 'token 5 6)))
                         :end 7)
  (test-read-sharpsign-c "#C(1 2) a"
                         :child (node 'parens 2 7
                                      (list (node 'token 3 4)
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
                                        (list (token 4 5)
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
                         :child (list (parens 2 8 (list (token 3 7)))))
  (test-read-sharpsign-s "#S(node) foo"
                         :child (list (parens 2 8 (list (token 3 7))))
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
                         :child (list (node 'string 2 10))
                         :end 10)
  (test-read-sharpsign-p "#p\"/root/\"  foo"
                         :child (list (node 'string 2 10))
                         :end 10))


;;; #=n

(defun test-read-sharpsign-equal (input &key child end)
  (test-read-sharpsign*
   :sharpsing-reader-function 'read-sharpsign-equal
   :node-type 'sharp-label
   :input input
   :expected-end end
   :expected-children child
   :given-numeric-argument (getf child :label)))

(define-test+run read-sharpsign-equal
  (test-read-sharpsign-equal "#=" :end +end+)
  (test-read-sharpsign-equal
   '("#1" "=")
   :child (list :label 1)
   :end +end+)
  (test-read-sharpsign-equal
   '("#2" "= ")
   :child (list :label 2
                :form (list (whitespace 3 4)))
   :end +end+)
  (test-read-sharpsign-equal
   '("#3" "=(foo)")
   :child (list :label 3
                :form (list (parens 3 8 (token 4 7))))))



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
  (test-read-sharpsign-plus "#++" :child (list (token 2 3)))
  (test-read-sharpsign-plus
   "#+ #+ x"
   :child (list (whitespace 2 3)
                (sharp-feature 3 7
                               (list (whitespace 5 6)
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
  (test-read-sharpsign-minus "#--" :child (list (token 2 3)))
  (test-read-sharpsign-minus
   "#- #- x"
   :child (list (whitespace 2 3)
                (sharp-feature-not 3 7
                                   (list (whitespace 5 6)
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
    (test* (read-quoted-string state #\| #\/) (list 0  +end+)))
  (with-state ("||")
    (test* (read-quoted-string state #\| #\/) '(0 2)))
  (with-state ("| |")
    (test* (read-quoted-string state #\| #\/) '(0 3)))
  (with-state ("|/||")
    (test* (read-quoted-string state #\| #\/) '(0 4)))
  (with-state ("|/|")
    (test* (read-quoted-string state #\| #\/) (list 0 +end+))))

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
              (list (node 'package-name 0 1)
                    (node 'symbol-name 2 3)))
        (tsn "p:x"))
    (is equalp
        (node 'possibly-internal-symbol 0 4
              (list
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
                     (list (node 'package-name 3 4)
                           (node 'symbol-name 5 6)))
        (tsn-padded "p:x"))
    (is equalp (node 'possibly-internal-symbol 3 7
                     (list (node 'package-name 3 4)
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
             (parens 0 expected-end children)))))

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
        (is-equalp* input tree expected)
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
                           (list
                            (whitespace 2 3)
                            (token 3 4))))
  (test-parse "(char= #\\; c)"
              (parens 0 13
                      (list (token 1 6)
                            (whitespace 6 7)
                            (sharp-char 7 10 (token 8 10))
                            (whitespace 10 11)
                            (token 11 12))))
  (test-parse "(#\\;)" (parens 0 5
                               (list (sharp-char 1 4 (token 2 4)))))
  (test-parse "#\\; " (sharp-char 0 3 (token 1 3)) (whitespace 3 4))
  (test-parse "`( asdf)" (node 'quasiquote 0 1)
              (parens 1 8
                      (list
                       (whitespace 2 3)
                       (token 3 7))))
  (test-parse "#\\Linefeed" (sharp-char 0 10 (token 1 10)))
  (test-parse "#\\: asd" (sharp-char 0 3 (token 1 3)) (whitespace 3 4) (token 4 7))
  (test-parse "(((  )))" (parens 0 8 (list (parens 1 7 (list (parens 2 6 (list (whitespace 3 5))))))))
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
                (list (node ':extraneous-closing-parens 3 +end+))))))

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
                   (last-node (alexandria:lastcar (tree state)))
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
