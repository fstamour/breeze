
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

#++
(defpackage #:breeze.test.reader2
  (:documentation "")
  (:use #:cl #:breeze.reader2))
#++
(in-package #:breeze.test.reader2)

(in-package #:breeze.reader2)


;;; Reader state

#++
(progn
  (node 'boo 1 2)
  #s(node :type boo :start 1 :end 2)
  (list #s(node :type boo :start 1 :end 2))
  (list '#s(node :type boo :start 1 :end 2)))


;;; testing helpers

(defparameter *ctx* nil)

(defun test (input got &optional (expected nil expectedp))
  (flet ((fmt (&rest args)
           (let ((str (apply #'format nil args))
                 (*print-circle* t)
                 (*print-right-margin* nil))
             (format *debug-io* "~&~a" str)
             str)))
    (if expectedp
        (if (equalp got expected)
            t
            (fmt "~A «~a» got: ~s expected: ~s" *ctx* input got expected))
        (fmt "~A «~a» => ~s" *ctx* input got))))

#++
(let ((*ctx* "Testing the \"test\" function."))
  (list
   (test 'meta (with-output-to-string (*debug-io*)
                 (test 'test-input 42 t))
         "«TEST-INPUT» got: 42 expected: T")
   (test 'meta (with-output-to-string (*debug-io*)
                 (test 'test-input 42 42))
         "")
   (test 'meta (test 'test-input 42 42) t)))

(defmacro with-state ((string) &body body)
  `(let ((state (make-state ,string)))
     ;; Wrap #'test
     (labels ((test* (got &optional (expected nil expectedp) extra-context
                      &aux (input (if (and extra-context
                                           (not (eq extra-context state)))
                                      (list extra-context
                                            (format nil "«~a»" (source state)))
                                      (source state))))
                ;; (list input got expected)
                (if expectedp
                    (test input got expected)
                    (test input got))))
       (declare (ignorable (function test*)))
       ,@body)))

(defmacro %with-state* ((string) &body body)
  (alexandria:once-only (string)
    `(list
      ,@(loop :for form :in body
              :collect `(with-state (,string) ,form)))))

(defmacro with-state* (&body body)
  `(append
    ,@(loop :for form :in body
            :collect `(%with-state* (,(car form)) ,@(rest form))))  )


;;; Reader position (in the source string)

;; TODO valid-position-p
(let ((*ctx* 'valid-position-p))
  (with-state*
    (""
     (test* (valid-position-p state -1) nil)
     (test* (valid-position-p state 0) nil)
     (test* (valid-position-p state 1) nil))
    (" "
     (test* (valid-position-p state -1) nil)
     (test* (valid-position-p state 0) t)
     (test* (valid-position-p state 1) nil))))

;; TODO donep
(with-state*
  ("" (test* (donep state) t))
  (" " (test* (donep state) nil))
  ("  " (test* (donep state) nil)))


;;; Getting and comparing characters

;; TODO at
(with-state*
  (""
   (test* (at state -1) nil)
   (test* (at state 0) nil)
   (test* (at state 1) nil)
   (test* (at state -1 #\a) nil)
   (test* (at state 0 #\b) nil)
   (test* (at state 1 #\c) nil))
  ("c"
   (test* (at state -1) nil)
   (test* (at state 0) #\c)
   (test* (at state 1) nil)
   (test* (at state -1 #\c) nil)
   (test* (at state 0 #\c) #\c)
   (test* (at state 0 #\a) nil)
   (test* (at state 1 #\c) nil)))


;;; Low-level parsing helpers

;; TODO read-char*
(with-state*
  (""
   (test* (list (read-char* state) (pos state)) '(nil 0))
   (test* (list (read-char* state #\a) (pos state)) '(nil 0)))
  ("c"
   (test* (list (read-char* state) (pos state)) '(#\c 1))
   (test* (list (read-char* state #\d) (pos state)) '(nil 0))))

;; TODO read-string*
(with-state*
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
    '((0 2) 2))))

;; TODO test read-while


;; TODO find-all
(defun test-find-all (needle string expected)
  (test (list 'find-all needle string)
        (find-all needle string)
        expected))
(list
 (test-find-all "" "" nil)
 (test-find-all "a" "" nil)
 (test-find-all "" "a" nil)
 (test-find-all "a" "aaa" '(0 1 2))
 (test-find-all "b" "aaa" nil))

;; TODO search-for
(defun test-search-for (needles input expected)
  (with-state (input)
    (test input (search-or needles state) expected)))
(list
 (test-search-for '("a" "b") "" nil)
 (test-search-for '("a" "b") "c" nil)
 (test-search-for '("a" "b") "-ab" 1)
 (test-search-for '("a" "b") "--ba" 2))


;;; Actual reader

;; TODO read-whitespaces
(defun test-read-whitespaces (input expected-end)
  (let ((*ctx* 'read-whitespaces))
    (with-state (input)
      (test* (read-whitespaces state)
             (when expected-end
               (whitespace 0 expected-end))))))
(list
 (test-read-whitespaces "" nil)
 (test-read-whitespaces "a" nil)
 (test-read-whitespaces " " 1)
 (test-read-whitespaces "  " 2))

;; TODO read-block-comment
(defun test-read-block-comment (input expected-end &rest children)
  (let ((*ctx* 'read-block-comment))
    (with-state (input)
      (test input (read-block-comment state)
            (when expected-end
              (block-comment 0 expected-end children))))))

(list
 (test-read-block-comment "" nil)
 (test-read-block-comment "#|" +end+)
 (test-read-block-comment "#| " +end+)
 (test-read-block-comment "#||#" 4)
 (test-read-block-comment "#|#" +end+)
 (test-read-block-comment "#|#|#" +end+ (block-comment 2 +end+))
 (test-read-block-comment "#|#||##" +end+ (block-comment 2 6))
 (test-read-block-comment "#|#|#|#" +end+
                          (block-comment 2 +end+
                                         (block-comment 4 +end+)))
 (test-read-block-comment "#|#|#||##" +end+
                          (block-comment 2 +end+
                                         (block-comment 4 8)))
 (test-read-block-comment "#|#||#|##"
                          ;; There's 9 characters, the last # is not part of any comments
                          8 (block-comment 2 6)))

;; TODO read-line-comment
(defun test-read-line-comment (input expected-end)
  (let ((*ctx* 'read-line-comment))
    (with-state ((format nil input))
      (test* (read-line-comment state)
             (when expected-end
               (line-comment 0 expected-end))))))
(list
 (test-read-line-comment "" nil)
 (test-read-line-comment ";" +end+)
 (test-read-line-comment "; asdf~%" 7))

;; TODO read-punctuation
(defun test-read-punctuation (input expected-type)
  (let ((*ctx* 'read-punctuation))
    (with-state (input)
      (test input
            (read-punctuation state)
            (when expected-type
              (punctuation expected-type 0))))))
(list
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


;; TODO read-quoted-string
;; TODO Add tests with VALIDP
(let ((*ctx* 'read-quoted-string))
  (list
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
     (test* (read-quoted-string state #\| #\/) (list 0 +end+)))))

;; TODO read-string
(defun test-read-string (input expected-end)
  (let ((*ctx* 'read-string))
    (with-state (input)
      (test* (read-string state)
             (when expected-end
               (node 'string 0 expected-end))))))
(list
 (test-read-string "" nil)
 (test-read-string "\"" +end+)
 (test-read-string "\"\"" 2)
 (test-read-string "\" \"" 3))

;; TODO not-terminatingp

;; TODO read-token
(defun test-read-token (input expected-end)
  (let ((*ctx* 'read-token))
    (with-state (input)
      (test* (read-token state)
             (when expected-end
               (token 0 expected-end))))))
(list
 (test-read-token "" nil)
 (test-read-token " " nil)
 (test-read-token "+-*/" 4)
 (test-read-token "123" 3)
 (test-read-token "| asdf |" 8)
 (test-read-token "| asdf |qwer#" 13)
 (test-read-token "arg| asdf | " 11)
 (test-read-token "arg| asdf |more" 15)
 (test-read-token "arg| asdf |more|" +end+)
 (test-read-token "arg| asdf |more|mmoooore|done" 29)
 (test-read-token "arg| asdf |no  |mmoooore|done" 13))


;; TODO read-extraneous-closing-parens

;; TODO read-parens
(defun test-read-parens (input expected-end &rest children)
  (with-state (input)
    (test* (read-parens state)
           (when expected-end
             (parens 0 expected-end children)))))
(list
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


;;; Putting it all toghether

;; TODO parse

(defun test-parse (input &rest expected)
  (if expected
      (test input (parse* input) expected)
      (test input (parse* input))))

(let ((*ctx* 'test-parse))
  (list
   (eq (parse "") nil)
   (test-parse "  " (whitespace 0 2))
   (test-parse "#|" (block-comment 0 +end+))
   (test-parse " #| "
               (whitespace 0 1)
               (block-comment 1 +end+)
               (whitespace 3 4))
   (test-parse "#||#" (block-comment 0 4))
   (test-parse "#|#||#" (block-comment 0 +end+ (block-comment 2 6)))
   (test-parse "#| #||# |#" (block-comment 0 10 (block-comment 3 7)))
   (test-parse "'" (punctuation 'quote 0))
   (test-parse "`" (punctuation 'quasiquote 0))
   (test-parse "#" (punctuation 'sharp 0))
   (test-parse "," (punctuation 'comma 0))
   (test-parse "+-*/" (token 0 4))
   (test-parse "123" (token 0 3))
   (test-parse "asdf#" (token 0 5))
   (test-parse "| asdf |" (token 0 8))
   (test-parse "arg| asdf | " (token 0 11) (whitespace 11 12))
   (test-parse "arg| asdf |more" (token 0 15))
   (test-parse "arg| asdf |more|" (token 0 +end+))
   (test-parse ";" (line-comment 0 +end+))
   (test-parse "(12" (parens 0 +end+ (token 1 3)))
   (test-parse "\"" (node 'string 0 +end+))))




#++
(multiple-value-bind (tree state)
    (parse "(foo)")
  (format nil "(~a ~a)"
          "ignore-errors"
          (node-content state (car tree))))


;; Slightly cursed syntax:
;; "#+#."
