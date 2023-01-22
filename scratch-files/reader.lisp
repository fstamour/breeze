#|

Ok, I'm tired of eclector... It is slow, it's brittle, it is hard to
make it do what I want (i.e. everything I do with eclector feels like
a huge kludge).

I'm gonna try my hand at making a reader... Worst case scenario I end
up with more test case for my eclector-based parser.

http://www.lispworks.com/documentation/HyperSpec/Body/02_.htm

|#


(defpackage #:breeze.reader2
  (:documentation "")
  (:use #:cl)
  (:import-from #:breeze.utils
                #:subseq-displaced
                #:+whitespaces+
                #:whitespacep)
  (:import-from #:alexandria
                #:when-let
                #:when-let*))

(in-package #:breeze.reader2)


;;; Reader state

(defclass state ()
  ((source
    :initform nil
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
  ;; - readtable case
  ;; - current input base
  (:documentation "The reader's state"))

(defun make-state (string)
  (make-instance 'state  :source string))

;; For testing
(defmacro with-state ((string) &body body)
  `(let ((state (make-state ,string)))
     ,@body))



;;; Reader position (in the source string)

(progn
  (defun valid-position-p (state position)
    (< -1 position (length (source state))))
  (equal '(nil nil t t)
         (list
          (with-state ("")
            (valid-position-p state -1))
          (with-state ("")
            (valid-position-p state 0))
          (with-state (" ")
            (valid-position-p state 0))
          (with-state ("  ")
            (valid-position-p state 0)))))

(progn
  (defun donep (state)
    (not (valid-position-p state (pos state))))
  (equal '(t nil nil)
         (list
          (with-state ("")
            (donep state))
          (with-state (" ")
            (donep state))
          (with-state ("  ")
            (donep state)))))


;;; Getting and comparing characters

;; Could be further generalized by adding `&key key test`, and/or
;; making variants `at-if`, `at-if-not`.
(defun at (state position &optional char)
  "Get the character at POSITION in the STATE's source.
Returns nil if POSITION is invalid.  If the optional parameter CHAR is
not nil, further compare the char at POSITION with CHAR and return the
character if they're char=."
  (when (valid-position-p state position)
    (let ((c (char (source state) position)))
      (cond
        ((null c) nil)
        ((null char) c)
        ((char= c char) c)))))

(defun current-char (state &optional char)
  "Get the character at the current STATE's position, without changing
the position."
  (unless (donep state)
    (at state (pos state) char)))

(defun next-char (state &optional char)
  "Peek at the next character to be read, without changing the
position."
  (let ((next-pos (1+ (pos state))))
    (at state next-pos char)))


;;; Low-level parsing helpers

(progn
  (defun lookahead-for-string (state string)
    "Search for STRING in the STATE's source, from the current STATE's
position. If found, advance the STATE's position to _after_ the
occurence of STRING."
    (check-type string string)
    (let ((start (pos state))
          (end (+ (pos state) (length string))))
      (when (valid-position-p state (1- end))
        (when-let ((foundp (search string (source state) :start2 start :end2 end)))
          (setf (pos state) end)
          (list start end)))))
  (equal
   (list
    (with-state ("")
      (list
       (lookahead-for-string state "")
       (pos state)))
    (with-state ("")
      (list
       (lookahead-for-string state "#")
       (pos state)))
    (with-state (";")
      (list
       (lookahead-for-string state ";;")
       (pos state)))
    (with-state (";;")
      (list
       (lookahead-for-string state ";;")
       (pos state))))
   '((nil 0) (nil 0) (nil 0) ((0 2) 2))))

(progn
  (defun skip-whitespaces (state)
    (loop
      :with starting-position = (pos state)
      :for pos :from starting-position
      :for c = (at state pos)
      :while (and c (whitespacep c))
      :finally (when (/= pos starting-position)
                 (setf (pos state) pos))))
  (equal '(0 1 2)
         (list
          (with-state ("")
            (skip-whitespaces state)
            (pos state))
          (with-state (" ")
            (skip-whitespaces state)
            (pos state))
          (with-state ("  ")
            (skip-whitespaces state)
            (pos state)))))

(defun read-while (state predicate)
  (let ((start (pos state)))
    (loop
      ;; :for guard :upto 10
      :for pos :from (pos state)
      :for c = (at state pos)
      :do (when (or (null c) (not (funcall predicate c)))
            (when (/= start pos)
              (setf (pos state) pos)
              (return (list start pos)))
            (return nil)))))



(defun %read-whitespaces (state start)
  (skip-whitespaces state)
  (list 'whitespace start (pos state)))

(defun read-whitespaces (state)
  (let ((start (pos state)))
    (when (whitespacep (current-char state))
      (%read-whitespaces state start))))

;; This is more useful to trace
(defun %read-block-comment (state start)
  (loop
    :with inner-comments
    ;; :for guard :upto 10 ; infinite loop guard
    :for pos = (position #\# (source state) :start (pos state))
    :do
       (cond
         ((or (null pos)
              (donep state))
          ;; TODO Find something else to parse from here
          ;; (setf (pos state) (length (source state)))
          (return (append (list 'block-comment start 'end)
                          (nreverse inner-comments))))
         ;; Closing
         ((and
           (<= (pos state) (1- pos))
           (at state (1- pos) #\|))
          (setf (pos state) (1+ pos))
          (return (append (list 'block-comment start (pos state))
                          (nreverse inner-comments))))
         ;; Nested comment
         ((at state (1+ pos) #\|)
          (setf (pos state) pos)
          (push (read-block-comment state) inner-comments)))))

(defun read-block-comment (state)
  "Read #||#"
  (let ((start (pos state)))
    (when (lookahead-for-string state "#|")
      (%read-block-comment state start))))

(defun read-line-comment (state)
  "Read ;"
  (let ((start (pos state)))
    (when (lookahead-for-string state ";")
      (let ((newline (search #.(format nil "~%") (source state) :start2 (pos state))))
        (if newline
            (progn
              (setf (pos state) (1+ newline))
              (list 'line-comment start (pos state)))
            ;; TODO (defun (setf donep) ...)
            (progn
              (setf (pos state) (length (source state)))
              (list 'line-comment start 'end)))))))

(defun read-quote-like (state)
  "Read ' or `"
  (let* ((start (pos state))
         (char (current-char state))
         (names '((#\' . quote)
                  (#\` . quasiquote)))
         (foundp (member char names :key #'car)))
    (when foundp
      (incf (pos state))
      (list (cdar foundp) start (1+ start)))))

(progn
  (defun read-quoted-string (state delimiter escape &optional validp)
    "Read strings delimted by DELIMITER where the single escpace character
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
                (return (list start 'end)))
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
   (equal (with-state ("")
            (read-quoted-string state #\| #\/))
          NIL)
   (equal
    (with-state ("|")
      (read-quoted-string state #\| #\/))
    '(0 END))
   (equal
    (with-state ("||")
      (read-quoted-string state #\| #\/))
    '(0 2))
   (equal
    (with-state ("| |")
      (read-quoted-string state #\| #\/))
    '(0 3))
   (equal
    (with-state ("|/||")
      (read-quoted-string state #\| #\/))
    '(0 4))
   (equal
    (with-state ("|/|")
      (read-quoted-string state #\| #\/))
    '(0 END))))

(defun read-string (state)
  "Read \"\""
  (when-let ((string (read-quoted-string state #\" #\\)))
    `(string ,@string)))

(defun terminatingp (c)
  "Test whether a character is terminating. See
http://www.lispworks.com/documentation/HyperSpec/Body/02_ad.htm"
  (member c '#. (append
                 (coerce "|;\"'(),`" 'list)
                 +whitespaces+)))

(defun %read-token (state start)
  "Read one token."
  (if (current-char state #\|)
      (read-quoted-string state #\| #\\)
      (loop
        :for part1 = (read-while state (complement #'terminatingp))
        :while (and part1 (current-char state #\|))
        :for part2 = (read-quoted-string state #\| #\\)
        :finally (return (if part1
                             (list start (second part1))
                             (when part2
                               (list start (second part2))))))))

(defun read-token (state)
  (when-let* ((start (pos state))
              (token (%read-token state start)))
    `(token ,@token)))

(list
 (equal
  (with-state ("")
    (read-token state))
  NIL)
 (equal
  (with-state (" ")
    (read-token state))
  NIL)
 (equal
  (with-state ("+-*/")
    (read-token state))
  '(token 0 4))
 (equal
  (with-state ("123")
    (read-token state))
  '(token 0 3))
 (equal
  (with-state ("| asdf |")
    (read-token state))
  '(token 0 8))
 (equal
  (with-state ("arg| asdf | ")
    (read-token state))
  '(token 0 11))
 (equal
  (with-state ("arg| asdf |more")
    (read-token state))
  '(token 0 15))
 (equal
  (with-state ("arg| asdf |more|")
    (read-token state))
  '(token 0 END)))


;; These things are considered 1 token -_-
;; (read-from-string "asd| |")
;; (read-from-string "asd#")


(defun read-any (state)
  (some #'(lambda (fn)
            (funcall fn state))
        '(read-whitespaces
          read-block-comment
          read-quote-like
          read-string
          read-line-comment
          read-token)))

(trace
 %read-whitespaces
 %read-block-comment
 %read-token)

(untrace)

(defun parse-string (string)
  (format t "~&##########")
  (loop
    :with state = (make-state string)
    :with unknown-start
    :for token-start = (pos state)
    :for guard :upto 1000               ; infinite loop guard
    :for token = (progn
                   #++
                   (progn
                     (format t "~&=====")
                     #1=(format t "~&~
                pos: ~a~
              ~%~ttoken-start: ~a~
              ~%~tunknown-start: ~a"
                                (pos state)
                                token-start
                                unknown-start))
                   (read-any state))

    ;; :do #1#  #2=(format t "~%~ttoken: ~a" token)

    :when (and token (not unknown-start))
      :collect token :into result
    :when (and token unknown-start)
      :append (list
               (prog1
                   (list 'unknown unknown-start token-start)
                 (setf unknown-start nil))
               token)
        :into result
    :when (and (not token)
               (not (donep state)))
      ;; if there's no token (e.g. unable to parse), but (not (donep
      ;; state)), advance and try again
      :do
         (unless unknown-start
           (setf unknown-start (pos state)))
         ;; For now we simply increment, but later we'll search for
         ;; synchornization points, hence the setf instead of an incf
         (setf (pos state) (1+ (pos state)))
    :when (donep state)
      :do (if unknown-start
              (return (append result `((unknown ,unknown-start ,(pos state)))))
              (return result))
          ;; :do #1# #2#
    ))

(list
 (equal (parse-string "") nil)
 (equal (parse-string "  ") '((WHITESPACE 0 2)))
 (equal (parse-string "#|") '((BLOCK-COMMENT 0 END)))
 (equal (parse-string " #| ") '((WHITESPACE 0 1)
                                (BLOCK-COMMENT 1 END)
                                (WHITESPACE 3 4)))
 (equal (parse-string "#||#") '((BLOCK-COMMENT 0 4)))
 (equal (parse-string "#|#||#") '((BLOCK-COMMENT 0 END (BLOCK-COMMENT 2 6))))
 (equal (parse-string "#| #||# |#") '((BLOCK-COMMENT 0 10 (BLOCK-COMMENT 3 7))))
 (equal (parse-string "'") '((QUOTE 0 1)))
 (equal (parse-string "`") '((QUASIQUOTE 0 1)))
 (equal (parse-string "+-*/") '((token 0 4)))
 (equal (parse-string "123") '((token 0 3)))
 (equal (parse-string "asdf#") '((token 0 5)))
 (equal (parse-string "| asdf |") '((token 0 8)))
 (equal (parse-string "arg| asdf | ") '((TOKEN 0 11) (WHITESPACE 11 12)))
 (equal (parse-string "arg| asdf |more") '((token 0 15)))
 (equal (parse-string "arg| asdf |more|") '((token 0 END)))
 (equal (parse-string ";") '((LINE-COMMENT 0 END))))

(parse-string "(12")

(parse-string "#")
(parse-string ",")
(parse-string "\"")
