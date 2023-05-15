#|

Ok, I'm tired of eclector... It is slow, it's brittle, it is hard to
make it do what I want (i.e. everything I do with eclector feels like
a huge kludge).

I'm gonna try my hand at making a reader... Worst case scenario I end
up with more test case for my eclector-based parser.

http://www.lispworks.com/documentation/HyperSpec/Body/02_.htm

|#

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

#|

Error recovery

- basic: use "synchronization points", places where it looks like a
good place to restart parsing after an invalid parse

- it would be much easier to pin-point the source of the failure if we
start from a previous good state (incremental parsing)

- I noticed that for lisp, a lot of things would be "easy" to parse
backward, this would help tremendously pin-pointing where a "bad
parse" begins.

|#

#++
(ql:quickload '#:trivial-timeout)

(defpackage #:breeze.reader2
  (:documentation "")
  (:use #:cl)
  (:import-from #:breeze.utils
                #:subseq-displaced
                #:+whitespaces+
                #:whitespacep)
  #++
  (:import-from #:trivial-timeout
                #:with-timeout)
  (:import-from #:alexandria
                #:when-let
                #:when-let*))

(in-package #:breeze.reader2)




;;; Reader state

(defclass state ()
  ((source
    :initarg :source
    :type string
    :accessor source
    :documentation "The string being parsed.")
   (pos
    :initform 0
    :initarg :pos
    :accessor pos
    :documentation "The position withing the string."))
  ;; TODO More state:
  ;; - current package
  ;; - readtable case (is it case converting)
  ;; - current input base (base of numbers)
  ;; - current depth?
  ;; - is inside quasiquotation?
  (:documentation "The reader's state"))

(defun make-state (string)
  (make-instance 'state  :source string))

(alexandria:define-constant +end+ -1)

#++
(defun print-node (node stream)
  (format stream "#S(NODE :TYPE ~A :START ~D :END ~D"
          (node-type node)
          (node-start node)
          ;; TODO maybe print +end+ if (= end +end+)
          (node-end node))
  (when-let ((node-children (node-children node)))
    (write-string " :CHILDREN ")
    (princ node-children stream))
  (write-char #\) stream))

#++
(remove-method #'print-object
               (find-method #'print-object
                            nil
                            '(node t)))

(defstruct (node
            (:constructor node (type start end &optional children))
            :constructor
                                        ; (:print-object print-node)
            (:predicate nodep)
            )
  (type 'nil
   :type symbol
   :read-only t)
  (start 0
   :type (integer 0)
   :read-only t)
  (end +end+
   :type (integer -1)
   :read-only t)
  (children '()
   :read-only t))

#++
(progn
  (node 'boo 1 2)
  #S(NODE :TYPE BOO :START 1 :END 2)
  (list #S(NODE :TYPE BOO :START 1 :END 2))
  (list '#s(node :type boo :start 1 :end 2)))

(defun whitespace (start end)
  (node 'whitespace start end))

(defun block-comment (start end &optional children)
  (node 'block-comment start end
        (if (nodep children)
            (list children)
            children)))

(defun line-comment (start end)
  (node 'line-comment start end))

(defun punctuation (type position)
  (node type position (1+ position)))

(defun token (start end)
  (node 'token start end))

(defun parens (start end &optional children)
  (node 'parens start end
        (if (nodep children)
            (list children)
            children)))

(defun node-content (state node)
  (subseq-displaced (source state)
                    (node-start node)
                    (and
                     (plusp (node-end node))
                     (node-end node))))


;;; testing helpers

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
            (fmt "«~a» got: ~s expected: ~s" input got expected))
        (fmt "«~a» => ~s" input got))))

(list
 (test 'meta (with-output-to-string (*debug-io*)
               (test 'test-input 42 t))
       "«TEST-INPUT» got: 42 expected: T")
 (test 'meta (with-output-to-string (*debug-io*)
               (test 'test-input 42 42))
       "")
 (test 'meta (test 'test-input 42 42) t))

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


;;; Degugging

;; I looked at trivial-timeout's code, it's safe to put nil (or a
;; float) here.
#++
(defparameter *timeout-threshold* 1)


;;; Reader position (in the source string)

(progn
  (defun valid-position-p (state position)
    (< -1 position (length (source state))))
  (with-state*
    (""
     (test* (valid-position-p state -1) nil)
     (test* (valid-position-p state 0) nil)
     (test* (valid-position-p state 1) nil))
    (" "
     (test* (valid-position-p state -1) nil)
     (test* (valid-position-p state 0) t)
     (test* (valid-position-p state 1) nil))))

(progn
  (defun donep (state)
    (not (valid-position-p state (pos state))))
  (with-state*
    ("" (test* (donep state) t))
    (" " (test* (donep state) nil))
    ("  " (test* (donep state) nil))))


;;; Getting and comparing characters

;; Could be further generalized by adding `&key key test`, and/or
;; making variants `at-if`, `at-if-not`.
(progn
  (defun at (state position &optional char)
    "Get the character at POSITION in the STATE's source.
Returns nil if POSITION is invalid.  If the optional parameter CHAR is
not nil, further compare the char at POSITION with CHAR and return the
character if they're char=."
    (when (valid-position-p state position)
      (when-let ((c (char (source state) position)))
        (when (or (null char)
                  (char= c char))
          c))))
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
     (test* (at state 1 #\c) nil))))

(defun current-char (state &optional char)
  "Get the character at the current STATE's position, without changing
the position."
  (at state (pos state) char))

(defun next-char (state &optional char)
  "Peek at the next character to be read, without changing the
position."
  (at state (1+ (pos state)) char))


;;; Low-level parsing helpers

(progn
  (defun read-char* (state &optional char)
    (when-let ((c (current-char state)))
      (when (or (null char)
                (char= c char))
        (incf (pos state))
        c)))
  (with-state*
    (""
     (test* (list (read-char* state) (pos state)) '(nil 0))
     (test* (list (read-char* state #\a) (pos state)) '(nil 0)))
    ("c"
     (test* (list (read-char* state) (pos state)) '(#\c 1))
     (test* (list (read-char* state #\d) (pos state)) '(nil 0)))))

(progn
  ;; rename to read-literal ?? or something like that
  (defun read-string* (state string)
    "Test if STRING in the STATE's source, at the current STATE's
position. If found, advance the STATE's position to _after_ the
occurence of STRING."
    (check-type string string)
    (let ((start (pos state))
          (end (+ (pos state) (length string))))
      (when (valid-position-p state (1- end))
        (when-let ((foundp (search string (source state) :start2 start :end2 end)))
          (setf (pos state) end)
          (list start end)))))
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
      '((0 2) 2)))))

(defun read-while (state predicate &aux (start (pos state)))
  (loop
    ;; :for guard :upto 10
    :for pos :from start
    :for c = (at state pos)
    :do (when (or (null c) (not (funcall predicate c)))
          (when (/= start pos)
            (setf (pos state) pos)
            (return (list start pos)))
          (return nil))))
;; TODO test read-while

;; Will be useful for finding some synchronization points
(progn
  (defun find-all (needle string)
    (when (and (plusp (length needle))
               (plusp (length string)))
      (loop :for pos = (search needle string :start2 0)
              :then (search needle string :start2 (+ pos (length needle)))
            :while pos
            :collect pos)))
  (defun test-find-all (needle string expected)
    (test (list 'find-all needle string)
          (find-all needle string)
          expected))
  (list
   (test-find-all "" "" nil)
   (test-find-all "a" "" nil)
   (test-find-all "" "a" nil)
   (test-find-all "a" "aaa" '(0 1 2))
   (test-find-all "b" "aaa" nil)))

(progn
  ;; TODO Not the most efficient...
  ;; Also, so far I only need it to search for "#|" or "|#"
  (defun search-or (needles state)
    (loop
      :with foundp
      :for needle :in needles
      :for pos = (search needle (source state) :start2 (pos state))
      :when pos
        :do (setf foundp t)
      :when pos
        :minimize pos :into min-pos
      :finally (when foundp (return min-pos))))

  (defun test-search-for (needles input expected)
    (with-state (input)
      (test input (search-or needles state) expected)))
  (list
   (test-search-for '("a" "b") "" nil)
   (test-search-for '("a" "b") "c" nil)
   (test-search-for '("a" "b") "-ab" 1)
   (test-search-for '("a" "b") "--ba" 2)))


;;; Actual reader

(progn
  (defun read-whitespaces (state &aux (start (pos state)))
    (loop
      :for pos :from start
      :for c = (at state pos)
      :while (and c (whitespacep c))
      :finally (when (/= pos start)
                 (setf (pos state) pos)
                 (return (whitespace start pos)))))
  (defun test-read-whitespaces (input expected-end)
    (with-state (input)
      (test* (read-whitespaces state)
             (when expected-end
               (whitespace 0 expected-end)))))
  (list
   (test-read-whitespaces "" nil)
   (test-read-whitespaces "a" nil)
   (test-read-whitespaces " " 1)
   (test-read-whitespaces "  " 2)))

(progn
  ;; This is more useful to trace, and easier to debug (we can more
  ;; easily see the START in the debugger).
  (defun read-block-comment (state  &aux (start (pos state)))
    "Read #||#"
    (when (read-string* state "#|")
      (loop
        :with inner-comments
        ;; :for guard :upto 10 ; infinite loop guard
        :for previous-pos = nil :then pos
        :for pos = #++ (position #\# (source state) :start (pos state))
                       (search-or '("#|" "|#") state)
        :when (and previous-pos (eql pos previous-pos))
          :do (error "read-block-comment: failed to move forward, this is a bug!
Previous position: ~A
New position ~A"
                     previous-pos
                     pos)
        :do
           (when pos
             ;; Whatever happens, we move forward
             (setf (pos state) (+ pos 2))
             (if (char= #\# (at state pos))
                 ;; if #|
                 (push (read-block-comment state) inner-comments)
                 ;; else |#
                 (return (block-comment start (pos state)
                                        (nreverse inner-comments)))))
           (unless pos
             (return (block-comment start +end+
                                    (nreverse inner-comments)))))))

  (defun test-read-block-comment (input expected-end &rest children)
    (with-state (input)
      (test input (read-block-comment state)
            (when expected-end
              (block-comment 0 expected-end children)))))

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
                            8 (block-comment 2 6))))

(progn
  (defun read-line-comment (state)
    "Read ;"
    (let ((start (pos state)))
      (when (read-char* state #\;)
        (let ((newline (search #.(format nil "~%") (source state) :start2 (pos state))))
          (if newline
              (progn
                (setf (pos state) (1+ newline))
                (line-comment start (pos state)))
              ;; TODO (defun (setf donep) ...)
              (progn
                (setf (pos state) (length (source state)))
                (line-comment start +end+)))))))
  (defun test-read-line-comment (input expected-end)
    (with-state ((format nil input))
      (test* (read-line-comment state)
             (when expected-end
               (line-comment 0 expected-end)))))
  (list
   (test-read-line-comment "" nil)
   (test-read-line-comment ";" +end+)
   (test-read-line-comment "; asdf~%" 7)))

(progn
  (defun read-punctuation (state)
    "Read ' or `"
    (when-let* ((current-char (current-char state))
                (foundp (member current-char
                                '((#\' . quote)
                                  (#\` . quasiquote)
                                  (#\. . dot)
                                  (#\@ . at)
                                  (#\, . comma)
                                  (#\# . sharp))
                                :key #'car)))
      (prog1 (punctuation (cdar foundp) (pos state))
        (incf (pos state)))))
  (defun test-read-punctuation (input expected-type)
    (with-state (input)
      (test input
            (read-punctuation state)
            (when expected-type
              (punctuation expected-type 0)))))
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
   ))

(progn
  (defun read-quoted-string (state delimiter escape &optional validp)
    "Read strings delimited by DELIMITER where the single escape character
is ESCAPE. Optionally check if the characters is valid if VALIDP is
provided."
    (let ((start (pos state)))
      (when (at state (pos state) delimiter)
        (loop
          ;; :for guard :upto 10
          :for pos :from (1+ (pos state))
          :for c = (at state pos)
          :do
             ;; (format *debug-io* "~%~A ~A" pos c)
             (cond
               ((null c)
                (setf (pos state) pos)
                (return (list start +end+)))
               ((char= c delimiter)
                (setf (pos state) (1+ pos))
                (return (list start (1+ pos))))
               ((char= c escape) (incf pos))
               ((and validp
                     (not (funcall validp c)))
                (setf (pos state) pos)
                (return (list start 'invalid))))))))
  ;; TODO Add tests with VALIDP
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

(progn
  (defun read-string (state)
    "Read \"\""
    (when-let ((string (read-quoted-string state #\" #\\)))
      (apply #'node 'string string)))
  (defun test-read-string (input expected-end)
    (with-state (input)
      (test* (read-string state)
             (when expected-end
               (node 'string 0 expected-end)))))
  (list
   (test-read-string "" nil)
   (test-read-string "\"" +end+)
   (test-read-string "\"\"" 2)
   (test-read-string "\" \"" 3)))

(defun not-terminatingp (c)
  "Test whether a character is terminating. See
http://www.lispworks.com/documentation/HyperSpec/Body/02_ad.htm"
  (and c
       (not (member c '#. (append
                           (coerce ";\"'(),`" 'list)
                           +whitespaces+)
                    :test #'char=))))

(progn
  (defun read-token (state &aux (start (pos state)))
    "Read one token."
    (loop
      :for char = (current-char state)
      :while (not-terminatingp char)
      :for part = (if (char= char #\|)
                      (read-quoted-string state #\| #\\)
                      (read-while state #'not-terminatingp))
      :while (and part (not-terminatingp (current-char state)))
      :finally (return (unless (= start (pos state))
                         (token start (if part (second part) +end+))))))
  (defun test-read-token (input expected-end)
    (with-state (input)
      (test* (read-token state)
             (when expected-end
               (token 0 expected-end)))))
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
   (test-read-token "arg| asdf |no  |mmoooore|done" 13)))


;; TODO Do something with this, to help error recovery, or at least
;; tell the user something.
(defun read-extraneous-closing-parens (state)
  (read-char* state #\))
  ;; We return nil all the time because this isn't valid lisp.
  nil)

;; don't forget to handle dotted lists
(progn
  (defun read-parens (state &aux (start (pos state)))
    (when (read-char* state #\()
      ;; Read while read-any != nil && char != )
      (loop
        ;; :for guard :below 1000 ; infinite loop guard
        :while (not (read-char* state #\))) ; good ending
        :for el = (read-any state)          ; mutual recursion
        :if el
          :collect el :into content
        :else
          :do (return (if (donep state)
                          (parens start +end+ content)
                          nil))
        :finally (return (parens start (pos state) content)))))
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
                     (whitespace 4 5))))

(defun read-any (state)
  (some #'(lambda (fn)
            (funcall fn state))
        '(read-whitespaces
          read-block-comment
          read-punctuation
          read-string
          read-line-comment
          read-token
          read-parens ; recursion
          read-extraneous-closing-parens)))

#++
(progn
  (trace
   read-string*
   read-char*)

  (trace
   %read-whitespaces
   %read-block-comment
   %read-token
   read-parens
   read-extraneous-closing-parens)

  (untrace))




;;; Putting it all toghether

(defun parse (string &aux (state (make-state string)))
  "Parse a string, stop at the end, or when there's a parse error."
  (values (loop
            :for node-start = (pos state)
            :for node = (read-any state)
            :while (and node
                        (plusp (node-end node)))
            :collect node)
          state))

;; (parse "#2()")
;; (parse "(")

;; TODO This is a hot mess :P
(defun parse* (string)
  "Parse a string, tries to recover when something is not a valid parse."
  (loop
    :with state = (make-state string)
    :with unknown-start
    :for token-start = (pos state)
    ;; :for guard :upto 1000               ; infinite loop guard
    :for token = (read-any state)

    :when (and token (not unknown-start)) ; Happy path
      :collect token :into result

    :when (and token unknown-start)   ;  Back on the happy path
      :append (list
               (prog1
                   (list 'unknown unknown-start token-start)
                 (setf unknown-start nil))
               token)
        :into result


    :when (and (not token)            ; Out of the happy path
               (not (donep state)))
      ;; if there's no token (e.g. unable to parse), but (not (donep
      ;; state)), advance and try again
      :do
         (unless unknown-start
           (setf unknown-start (pos state)))
         ;; TODO For now we simply increment, but later we'll search
         ;; for synchornization points, hence the setf instead of an
         ;; incf
         (setf (pos state) (1+ (pos state)))

    :when (donep state)               ; Done
      :do (if unknown-start
              (return (append result `((unknown ,unknown-start ,(pos state)))))
              (return result))))

(defun test-parse (input &rest expected)
  (if expected
      (test input (parse* input) expected)
      (test input (parse* input))))

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
 (test-parse "\"" (node 'string 0 +end+)))



;;; Benchmarks

(defparameter *files*
  (loop
    :with hash = (make-hash-table :test #'equal)
    :for file :in (mapcan
                   #'breeze.asdf:system-files
                   '(breeze breeze/test))
    :for content = (alexandria:read-file-into-string file)
    :do (setf (gethash file hash) content)
    :finally (return hash)))

;; Benchmarking this parser
#++
(time
 (loop :for file :being :the :hash-key :of *files* :using (hash-value content)
       :for parse = (handler-case (parse content)
                      (error (condition) :error))
       :when (eq parse :error)
         :collect file))

;; First working version
#|
Evaluation took:
0.144 seconds of real time
0.146014 seconds of total run time (0.138010 user, 0.008004 system)
101.39% CPU
612,255,386 processor cycles
8,077,888 bytes consed

... That's a lot of memory for something that doesn't copy the strings -_-
|#

;; After adding the "node" structure (instead of using lists) and reworking some loops:
#|
Evaluation took:
0.068 seconds of real time
0.069146 seconds of total run time (0.068182 user, 0.000964 system)
101.47% CPU
291,229,507 processor cycles
1,796,976 bytes consed

About twice as fast as the first version.

Cons about four times less!
|#



#|

On another computer, after removing all "with-timout"

Evaluation took:
0.012 seconds of real time
0.012056 seconds of total run time (0.012054 user, 0.000002 system)
100.00% CPU
42,368,760 processor cycles
1,604,320 bytes consed

|#


;; Comparing to the eclector-based reader

#++
(time
 (loop :for file :being :the :hash-key :of *files* :using (hash-value content)
       :for parse = (handler-case (breeze.reader:parse-string content)
                      (error (condition) :error))
       :when (eq parse :error)
         :collect file))


#|

Evaluation took:
0.160 seconds of real time
0.160275 seconds of total run time (0.143281 user, 0.016994 system)
100.00% CPU
673,506,595 processor cycles
8,128,768 bytes consed

But it failed to parse 3 files
|#


#|

On another computer:

Evaluation took:
0.024 seconds of real time
0.026949 seconds of total run time (0.026949 user, 0.000000 system)
112.50% CPU
46 lambdas converted
93,880,815 processor cycles
8,687,808 bytes consed

|#


#++
(time
 (loop :for file :being :the :hash-key :of *files* :using (hash-value content)
       :for parse = (handler-case (parse* content)
                      (error (condition) :error))
       :when (eq parse :error)
         :collect file))

#|

On another computer:

Evaluation took:
0.012 seconds of real time
0.012408 seconds of total run time (0.012393 user, 0.000015 system)
100.00% CPU
43,082,200 processor cycles
1,534,960 bytes consed

|#


;;;

#++
(multiple-value-bind (tree state)
    (parse "(foo)")
  (format nil "(~a ~a)"
          "ignore-errors"
          (node-content state (car tree))))


;; Slightly cursed syntax:
;; "#+#."
