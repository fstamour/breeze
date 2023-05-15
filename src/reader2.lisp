#|

Ok, I'm tired of eclector... It is slow, it's brittle, it is hard to
make it do what I want (i.e. everything I do with eclector feels like
a huge kludge).

I'm gonna try my hand at making a reader... Worst case scenario I end
up with more test case for my eclector-based parser.

http://www.lispworks.com/documentation/HyperSpec/Body/02_.htm

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


(defstruct (node
            (:constructor node (type start end &optional children))
            :constructor
            (:predicate nodep))
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


;;; Reader position (in the source string)

(defun valid-position-p (state position)
  (< -1 position (length (source state))))

(defun donep (state)
  (not (valid-position-p state (pos state))))


;;; Getting and comparing characters

;; Could be further generalized by adding `&key key test`, and/or
;; making variants `at-if`, `at-if-not`.
(defun at (state position &optional char)
  "Get the character at POSITION in the STATE's source.
Returns nil if POSITION is invalid.  If the optional parameter CHAR is
not nil, further compare the char at POSITION with CHAR and return the
character if they're char=."
  (when (valid-position-p state position)
    (when-let ((c (char (source state) position)))
      (when (or (null char) (char= c char)) c))))

(defun current-char (state &optional char)
  "Get the character at the current STATE's position, without changing
the position."
  (at state (pos state) char))

(defun next-char (state &optional char)
  "Peek at the next character to be read, without changing the
position."
  (at state (1+ (pos state)) char))


;;; Low-level parsing helpers

(defun read-char* (state &optional char)
  (when-let ((c (current-char state)))
    (when (or (null char)
              (char= c char))
      (incf (pos state))
      c)))

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


;; Will be useful for finding some synchronization points
(defun find-all (needle string)
  (when (and (plusp (length needle))
             (plusp (length string)))
    (loop :for pos = (search needle string :start2 0)
            :then (search needle string :start2 (+ pos (length needle)))
          :while pos
          :collect pos)))

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


;;; Actual reader

(defun read-whitespaces (state &aux (start (pos state)))
  (loop
    :for pos :from start
    :for c = (at state pos)
    :while (and c (whitespacep c))
    :finally (when (/= pos start)
               (setf (pos state) pos)
               (return (whitespace start pos)))))


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

(defun read-string (state)
  "Read \"\""
  (when-let ((string (read-quoted-string state #\" #\\)))
    (apply #'node 'string string)))

(defun not-terminatingp (c)
  "Test whether a character is terminating. See
http://www.lispworks.com/documentation/HyperSpec/Body/02_ad.htm"
  (and c
       (not (member c '#. (append
                           (coerce ";\"'(),`" 'list)
                           +whitespaces+)
                    :test #'char=))))

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


;; TODO Do something with this, to help error recovery, or at least
;; tell the user something.
(defun read-extraneous-closing-parens (state)
  (read-char* state #\))
  ;; We return nil all the time because this isn't valid lisp.
  nil)

;; don't forget to handle dotted lists
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
