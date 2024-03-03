(cl:in-package #:cl-user)

(uiop:define-package #:breeze.lossless-reader
    (:documentation "A fast, lossless, robust and superficial reader for a superset of
common lisp.")
  (:use #:cl)
  (:import-from #:breeze.utils
                #:subseq-displaced
                #:+whitespaces+
                #:whitespacep)
  (:import-from #:alexandria
                #:when-let
                #:when-let*)
  ;; Parsing state
  (:export #:state
           #:source
           #:pos
           #:tree
           #:make-state
           #:source-substring)
  ;; Nodes
  (:export #:+end+
           #:node
           #:node-start
           #:node-end
           #:node-type
           #:node-children
           #:copy-node
           #:valid-node-p
           #:node-content)
  ;; Node constructors
  (:export #:block-comment
           #:parens
           #:punctuation
           #:token
           #:whitespace
           #:line-comment
           #:string
           #:quote
           #:quasiquote
           #:dot
           #:comma
           #:sharp
           #:sharp-char
           #:sharp-function
           #:sharp-vector
           #:sharp-bitvector
           #:sharp-uninterned
           #:sharp-eval
           #:sharp-binary
           #:sharp-octal
           #:sharp-hexa
           #:sharp-complex
           #:sharp-structure
           #:sharp-pathname
           #:sharp-feature
           #:sharp-feature-not
           #:sharp-radix
           #:sharp-array
           #:sharp-label
           #:sharp-reference
           #:sharp-unknown)
  ;; Node copiers
  (:export #:copy-block-comment
           #:copy-parens
           #:copy-sharpsign
           #:copy-punctuation
           #:copy-token
           #:copy-whitespace
           #:copy-line-comment
           #:copy-string
           #:copy-quote
           #:copy-quasiquote
           #:copy-dot
           #:copy-comma
           #:copy-sharp
           #:copy-sharp-char
           #:copy-sharp-function
           #:copy-sharp-vector
           #:copy-sharp-bitvector
           #:copy-sharp-uninterned
           #:copy-sharp-eval
           #:copy-sharp-binary
           #:copy-sharp-octal
           #:copy-sharp-hexa
           #:copy-sharp-complex
           #:copy-sharp-structure
           #:copy-sharp-pathname
           #:copy-sharp-feature
           #:copy-sharp-feature-not
           #:copy-sharp-radix
           #:copy-sharp-array
           #:copy-sharp-label
           #:copy-sharp-reference
           #:copy-sharp-unknown)
  ;; Node predicates
  (:export #:block-comment-node-p
           #:parens-node-p
           #:punctuation-node-p
           #:token-node-p
           #:whitespace-node-p
           #:line-comment-node-p
           #:string-node-p
           #:quote-node-p
           #:quasiquote-node-p
           #:dot-node-p
           #:comma-node-p
           #:sharp-node-p
           #:sharp-char-node-p
           #:sharp-function-node-p
           #:sharp-vector-node-p
           #:sharp-bitvector-node-p
           #:sharp-uninterned-node-p
           #:sharp-eval-node-p
           #:sharp-binary-node-p
           #:sharp-octal-node-p
           #:sharp-hexa-node-p
           #:sharp-complex-node-p
           #:sharp-structure-node-p
           #:sharp-pathname-node-p
           #:sharp-feature-node-p
           #:sharp-feature-not-node-p
           #:sharp-radix-node-p
           #:sharp-array-node-p
           #:sharp-label-node-p
           #:sharp-reference-node-p
           #:sharp-unknown-node-p)
  ;; State utilities
  (:export #:at
           #:at=
           #:current-char
           #:current-char=
           #:next-char
           #:next-char=
           #:donep
           #:valid-position-p
           #:*state-control-string*
           #:state-context
           ;; parsing utilities
           #:read-char*
           #:find-all
           #:not-terminatingp
           #:read-string*
           #:read-while
           ;; sub parser
           #:read-line-comment
           #:read-parens
           #:read-sharpsign-dispatching-reader-macro
           #:read-punctuation
           #:read-quoted-string
           #:read-string
           #:read-token
           #:read-whitespaces
           #:read-block-comment)
  (:export
   ;; top-level parsing/unparsing
   #:parse
   #:unparse ;; maybe deprecate?
   #:walk))

(in-package #:breeze.lossless-reader)


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
    :documentation "The position within the string.")
   (tree
    :initform 0
    :initarg :pos
    :accessor tree
    :documentation "The parsed nodes."))
  ;; TODO More state:
  ;; - current package
  ;; - readtable case (is it case converting)
  ;; - current input base (base of numbers)
  ;; - current depth?
  ;; - is inside quasiquotation?
  (:documentation "The reader's state"))

(defun make-state (string)
  (make-instance 'state :source string))

(defmethod print-object ((state state) stream)
  (print-unreadable-object
      (state stream :type t :identity nil)
    (let ((excerpt (breeze.utils:around (source state)
                                        (pos state))))
      (format stream "~s ~d/~d"
              excerpt
              (length excerpt)
              (length (source state))))))



(alexandria:define-constant +end+ -1)

(defstruct (range
            (:constructor range (start end))
            :constructor
            (:predicate rangep))
  (start 0
   :type (integer 0)
   :read-only t)
  (end +end+
   :type (integer -1)
   :read-only t))

(defstruct (node
            ;; (:constructor node (type start end &optional children))
            :constructor
            (:predicate nodep)
            (:include range))
  (type 'nil
   :type symbol
   :read-only t)
  (children '()
   :read-only t))

(defun node (type start end &optional children)
  #++ (when (= +end+ end)
        (break))
  (make-node
   :type type
   :start start
   :end end
   :children children))


;;; Constructors

(macrolet ((aux (type
                 &key
                 children
                 (name type)
                 no-constructor-p)
             `(progn
                ;; predicate
                (defun
                    ,(alexandria:symbolicate type '-node-p)
                    (node)
                  ,(format nil "Is this a node of type ~s" type)
                  (and (nodep node)
                       (eq (node-type node) ',type)
                       node))
                ;; constructor
                ,(unless no-constructor-p
                   `(defun ,name (start end
                                  ,@(case children
                                      (&optional (list '&optional 'children))
                                      ((t) (list 'children))))
                      (node ',type start end ,@(when children (list 'children)))))
                ;; copier
                (defun ,(alexandria:symbolicate 'copy- name)
                    (node &key (start nil startp)
                            (end nil endp)
                            ,@(when children
                                (list `(children nil childrenp))))
                  (node ',type
                        (if startp start (node-start node))
                        (if endp end (node-end node))
                        ,@(when children
                            `((if childrenp children (node-children node)))))))))
  ;; TODO more of then needs children...
  (aux whitespace)
  (aux block-comment)
  (aux line-comment)
  (aux token)
  (aux parens :children &optional :no-constructor-p t)
  (aux punctuation :no-constructor-p t)
  (aux string :name string-node)
  (aux quote :name quote-node)
  (aux quasiquote)
  (aux dot)
  (aux comma)
  (aux sharp)
  (aux sharp-char :children t)
  (aux sharp-function :children t)
  (aux sharp-vector)
  (aux sharp-bitvector)
  (aux sharp-uninterned)
  (aux sharp-eval)
  (aux sharp-binary)
  (aux sharp-octal)
  (aux sharp-hexa)
  (aux sharp-complex)
  (aux sharp-structure)
  (aux sharp-pathname :children t)
  (aux sharp-feature :children t)
  (aux sharp-feature-not :children t)
  (aux sharp-radix)
  (aux sharp-array)
  (aux sharp-label)
  (aux sharp-reference)
  (aux sharp-unknown))

(defun punctuation (type position)
  (node type position (1+ position)))

(defun parens (start end &optional children)
  (node 'parens start end
        (if (nodep children)
            (list children)
            children)))

(defmethod print-object ((node node) stream)
  (let ((*print-case* :downcase)
        (children (node-children node)))
    (format stream "(~:[node '~;~]~s ~d ~d~:[ ~s~;~@[ (list ~{~s~^ ~})~]~])"
            (member (node-type node)
                    '(parens
                      token
                      whitespace
                      block-comment
                      line-comment
                      sharp-char
                      sharp-function
                      sharp-vector
                      sharp-bitvector
                      sharp-uninterned
                      sharp-eval
                      sharp-binary
                      sharp-octal
                      sharp-hexa
                      sharp-complex
                      sharp-structure
                      sharp-pathname
                      sharp-feature
                      sharp-feature-not
                      sharp-radix
                      sharp-array
                      sharp-label
                      sharp-reference
                      sharp-unknown))
            (node-type node)
            (node-start node)
            (node-end node)
            (listp children)
            children)))

;; TODO make a test out of this
#++
(mapcar
 #'princ-to-string
 (list
  (node 'asdf 1 3)
  (node 'asdf 1 3 (node 'qwer 3 5))
  (node 'asdf 1 3 (list (node 'qwer 3 5)
                        (node 'uiop 6 8)))
  (parens 3 5)))


;;; Predicates

(defun comment-node-p (node)
  "Is this node a block or line comment?"
  (or (line-comment-node-p node)
      (block-comment-node-p node)))


;;; Content and range

(defun source-substring (state start end)
  "Get a (displaced) substring of the state's source string."
  (subseq-displaced (source state)
                    start
                    (and (plusp end) end)))

;; TODO rename to node-string
(defun node-content (state node)
  "Get a (displaced) string of the node's range."
  (source-substring state (node-start node) (node-end node)))

(defmethod start ((node node))
  "Get the start position of the node."
  (node-start node))

(defmethod end ((node node))
  "Get the end position of the node."
  (node-end node))

(defmethod start ((range range))
  "Get the start of the range."
  (range-start range))

(defmethod end ((range range))
  "Get the end of the range."
  (range-end range))

(defmethod no-end-p ((x integer))
  "Does this number represent the +infinity?"
  (= +end+ x))

(defmethod no-end-p ((node node))
  "Is this node's end position open-ended?"
  (no-end-p (node-end node)))

(defmethod no-end-p ((range range))
  "Is this range open-ended?"
  (no-end-p (range-end range)))


;;; Reader position (in the source string)

(defun valid-position-p (state position)
  (< -1 position (length (source state))))

(defun donep (state)
  (not (valid-position-p state (pos state))))


;;; Getting and comparing characters

;; Could be further generalized by adding `&key key test`, and/or
;; making variants `at-if`, `at-if-not`.
(defun at (state position)
  "Get the character at POSITION in the STATE's source.
Returns nil if POSITION is invalid."
  (when (valid-position-p state position)
    (char (source state) position)))

;; TODO add tests with case-sensitive-p = nil
;; TODO split into at= and at-equal
(defun at= (state position char &optional (case-insensitive-p t))
  "Compare the character at POSITION in the STATE's source with the parameter CHAR and returns the CHAR if they're char=.
Returns nil if POSITION is invalid."
  (when-let ((c (at state position))) (and
                                       (if case-insensitive-p
                                           (char= c char)
                                           (char-equal c char))
                                       c)))

(defun current-char (state)
  "Get the character at the current STATE's position, without changing
the position."
  (at state (pos state)))

;; TODO add tests with case-sensitive-p = nil
;; TODO split into current-char= and current-char-equal
(defun current-char= (state char &optional (case-sensitive-p t))
  "Get the character at the current STATE's position, without changing
the position."
  (at= state (pos state) char case-sensitive-p))

(defun next-char (state &optional (n 1))
  "Peek at the next character to be read, without changing the
position."
  (at state (+ n (pos state))))

;; TODO add tests with case-sensitive-p = nil
;; TODO split into next-char= and next-char-equal
(defun next-char= (state char &optional (n 1) (case-sensitive-p t))
  "Peek at the next character to be read, without changing the
position."
  (at= state (+ n (pos state)) char case-sensitive-p))


;;; Low-level parsing helpers

;; TODO add tests with case-sensitive-p = nil
;; TODO split into read-char and read-char=
;; TODO implement using current-char=
(defun read-char* (state &optional char (case-sensitive-p t))
  (when-let ((c (current-char state)))
    (when (or (null char)
              (if case-sensitive-p
                  (char= c char)
                  (char-equal c char)))
      (incf (pos state))
      c)))

(defun read-string* (state string &optional (advance-position-p t))
  "Search STRING in the STATE's source, at the current STATE's
position. If found, optinally advance the STATE's position to _after_
the occurence of STRING."
  (check-type string string)
  (let ((start (pos state))
        (end (+ (pos state) (length string))))
    (when (valid-position-p state (1- end))
      (when-let ((foundp (search string (source state) :start2 start :end2 end)))
        (when advance-position-p
          (setf (pos state) end))
        (list start end)))))

;; 2023-05-20 only used in read-token
;; 2024-01-03 and read- dispatch reader macro
;; TODO return a range instead of a list
(defun read-while (state predicate &key (advance-position-p t) (start (pos state)))
  "Returns nil or (list start end)"
  (loop
    ;; :for guard :from 0
    :for pos :from start
    :for c = (at state pos)
    ;; :when (< 9000 guard) :do (error "read-while might be looping indefinitely...")
    :do (when (or (null c) (not (funcall predicate c)))
          (when (/= start pos)
            (when advance-position-p (setf (pos state) pos))
            (return (list start pos)))
          (return nil))))

;; Will be useful for finding some synchronization points
;;
;; 2023-05-20 not used anymore, since I refactored read-block-comment
;;
;; TODO maybe add a callback instead of building up a list... (not
;; sure if it's worth it (performance-wise), it'll heavily depends on
;; how and _if_ I use this).
(defun find-all (needle string)
  (when (and (plusp (length needle))
             (plusp (length string)))
    (loop :for pos = (search needle string :start2 0)
            :then (search needle string :start2 (+ pos (length needle)))
          :while pos
          :collect pos)))


;;; Actual reader

(defmacro defreader (name lambda-list &body body)
  `(defun ,name (state ,@lambda-list &aux (start (pos state)))
     (declare (ignorable start))
     ,@body))

(defreader read-whitespaces ()
  (loop
    :for pos :from start
    :for c = (at state pos)
    :while (and c (whitespacep c) (not (char= #\page c)))
    :finally (when (/= pos start)
               (setf (pos state) pos)
               (return (whitespace start pos)))))

(defreader read-block-comment ()
  "Read #||#"
  (when (read-string* state "#|" nil)
    (loop
      :with stack
      :with situation = 'other
      ;; :for guard :upto 1000 ; infinite loop guard
      :for char = (read-char* state)
      :do #++
          (format *debug-io* "~&situation: ~(~a~) char: ~s"
                  situation
                  char)
          (unless char
            (return (block-comment start +end+)))
          (ecase situation
            (other (case char
                     (#\| (setf situation 'pipe))
                     (#\# (setf situation 'sharp))))
            (sharp (case char
                     (#\|
                      (setf situation 'other)
                      (push (- (pos state) 2) stack))
                     (t
                      (setf situation 'other))))
            (pipe (case char
                    (#\#
                     (setf situation 'other)
                     (pop stack)
                     (when (null stack)
                       (return (block-comment start (pos state)))))
                    (t
                     (setf situation 'other))))))))

(defreader read-line-comment ()
  "Read ;"
  (when (read-char* state #\;)
    (let ((newline (search #.(format nil "~%")
                           (source state)
                           :start2 (pos state))))
      (if newline
          (progn
            (setf (pos state) (1+ newline))
            (line-comment start (pos state)))
          ;; TODO (defun (setf donep) ...)
          (progn
            (setf (pos state) (length (source state)))
            (line-comment start +end+))))))

;; TODO rename read-integer
(defun read-number (state &optional (radix 10))
  (let ((range (read-while state #'(lambda (char) (digit-char-p char radix)))))
    (when range
      (let ((*read-base* radix))
        (values (read-from-string (apply #'source-substring state range)) range)))))

;;; TODO in the following read-sharpsign-* functions, number should be
;;; renamed "prefix"

(defun read-sharpsign-backslash (state start number)
  (declare (ignore number))
  ;; TODO (if number) => invalid syntax
  (when (read-char* state #\\)
    (decf (pos state))
    (let ((token (when (valid-position-p state (1+ (pos state)))
                   (read-token state))))
      (node 'sharp-char start (if token (pos state) +end+) token))))

(defun read-any* (state)
  (multiple-value-bind (whitespaces form)
      (read-any state t)
    (let ((children (remove-if #'null (list whitespaces form)))
          (end (if (and form
                        (valid-node-p form))
                   (pos state)
                   +end+)))
      (values end children))))

(defun %read-sharpsign-any (state start type)
  (multiple-value-bind (end children)
      (read-any* state)
    (node type start end children)))

(defun read-sharpsign-quote (state start number)
  (declare (ignore number))
  ;; TODO (if number) => invalid syntax
  (when (read-char* state #\')
    (%read-sharpsign-any state start 'sharp-function)))

(defun read-sharpsign-left-parens (state start number)
  (declare (ignore number))
  ;; N.B. we use current-char instead of read-char, because we don't
  ;; want to consume the left-parens right away.
  (when (current-char= state #\()
    (let ((form (read-parens state)))
      (node 'sharp-vector start (if form (pos state) +end+) form))))

(defun read-sharpsign-asterisk (state start length)
  (declare (ignore length))
  (when (read-char* state #\*)
    (multiple-value-bind (bits range)
        (read-number state 2)
      (declare (ignore range))
      ;; TODO check (- (cdr range) (car range)) <= length
      (node 'sharp-bitvector start (pos state) bits))))

(defun read-sharpsign-colon (state start number)
  (declare (ignore number))
  ;; TODO (if number) => invalid syntax
  (when (read-char* state #\:)
    (let* ((token-start (pos state))
           (token (read-token state))
           (end (pos state)))
      (node 'sharp-uninterned start end
            (or token
                (token token-start end))))))

(defun read-sharpsign-dot (state start number)
  (declare (ignore number))
  ;; TODO (if number) => invalid syntax
  (when (read-char* state #\.)
    (%read-sharpsign-any state start 'sharp-eval)))

(defun %read-sharpsign-number (state start type radix)
  (let ((n (read-number state radix)))
    (node type start (if n (pos state) +end+))))

(defun read-sharpsign-b (state start number)
  (declare (ignore number))
  ;; TODO (if number) => invalid syntax
  (when (read-char* state #\b nil)
    (%read-sharpsign-number state start 'sharp-binary 2)))

(defun read-sharpsign-o (state start number)
  (declare (ignore number))
  ;; TODO (if number) => invalid syntax
  (when (read-char* state #\b nil)
    (%read-sharpsign-number state start 'sharp-octal 8)))

(defun read-sharpsign-x (state start number)
  (declare (ignore number))
  ;; TODO (if number) => invalid syntax
  (when (read-char* state #\x nil)
    (%read-sharpsign-number state start 'sharp-hexa 16)))

(defun read-sharpsign-r (state start radix)
  (when (read-char* state #\r nil)
    (let ((n (read-number state radix)))
      (node 'sharp-radix start (if n (pos state) +end+)))))

(defun read-sharpsign-c (state start number)
  (declare (ignore number))
  ;; TODO (if number) => invalid syntax
  (when (read-char* state #\c nil)
    (let ((form (read-parens state)))
      (node 'sharp-complex start (if form (pos state) +end+) form))))

(defun read-sharpsign-a (state start length)
  (declare (ignore length))
  (when (read-char* state #\a nil)
    (let ((form (read-parens state)))
      (node 'sharp-array start (if form (pos state) +end+) form))))

(defun read-sharpsign-s (state start number)
  (declare (ignore number))
  ;; TODO (if number) => invalid syntax
  (when (read-char* state #\s nil)
    (%read-sharpsign-any state start 'sharp-structure)))

(defun read-sharpsign-p (state start number)
  (declare (ignore number))
  ;; TODO (if number) => invalid syntax
  (when (read-char* state #\p nil)
    (%read-sharpsign-any state start 'sharp-pathname)))

(defun read-sharpsign-equal (state start number)
  (when (read-char* state #\=)
    (multiple-value-bind (end children)
        (read-any* state)
      ;; TODO sharp-label would benefit from having it own data
      ;; structure, this is abusing the children
      (node 'sharp-label start end
            (append
             (when (and (integerp number)
                        (<= 0 number))
               (list :label number))
             (when children
               (list :form children)))))))

(defun read-sharpsign-sharpsign (state start number)
  (when (read-char* state #\#)
    (node 'sharp-reference start (if (and (integerp number)
                                          (<= 0 number))
                                     (pos state)
                                     +end+)
          number)))

(defun read-sharpsign-plus (state start number)
  (declare (ignore number))
  ;; TODO (if number) => invalid syntax
  (when (read-char* state #\+)
    (%read-sharpsign-any state start 'sharp-feature)))

(defun read-sharpsign-minus (state start number)
  (declare (ignore number))
  ;; TODO (if number) => invalid syntax
  (when (read-char* state #\-)
    (%read-sharpsign-any state start 'sharp-feature-not)))

;; TODO #) and #<any whitespace> are **invalid**
;; See https://www.lispworks.com/documentation/HyperSpec/Body/02_dh.htm
(defreader read-sharpsign-dispatching-reader-macro ()
  "Read reader macros #..."
  (when (read-char* state #\#)
    (let ((number (read-number state)))
      (or
       (some
        (lambda (fn)
          (funcall fn state start number))
        '(read-sharpsign-backslash
          read-sharpsign-quote
          read-sharpsign-left-parens
          read-sharpsign-asterisk
          read-sharpsign-colon
          read-sharpsign-dot
          read-sharpsign-b
          read-sharpsign-o
          read-sharpsign-x
          read-sharpsign-r
          read-sharpsign-c
          read-sharpsign-a
          read-sharpsign-s
          read-sharpsign-p
          read-sharpsign-equal
          read-sharpsign-sharpsign
          read-sharpsign-plus
          read-sharpsign-minus))
       ;; Invalid syntax OR custom reader macro
       (sharp-unknown start +end+)))))


(defreader read-punctuation ()
  "Read ' or `"
  (when-let* ((current-char (current-char state))
              (foundp (member current-char
                              '((#\' . quote)
                                (#\` . quasiquote)
                                (#\. . dot)
                                (#\@ . at)
                                (#\, . comma)
                                (#\# . sharp)
                                (#\page . page))
                              :key #'car)))
    (prog1 (punctuation (cdar foundp) start)
      (incf (pos state)))))

(defun read-quoted-string (state delimiter escape &optional validp)
  "Read strings delimited by DELIMITER where the single escape character
is ESCAPE. Optionally check if the characters is valid if VALIDP is
provided."
  (let ((start (pos state)))
    (when (at= state (pos state) delimiter)
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

(defreader read-string ()
  "Read \"\""
  (when-let ((string (read-quoted-string state #\" #\\)))
    (apply #'node 'string string)))

(defun not-terminatingp (c)
  "Test whether a character is terminating. See
http://www.lispworks.com/documentation/HyperSpec/Body/02_ad.htm"
  (and c
       (not (position c '#. (concatenate 'string
                                         ";\"'(),`"
                                         +whitespaces+)
                      :test #'char=))))

(defun not-terminatingp-nor-escape (c)
  "Test whether a character is terminating or #\| or #\\. See
http://www.lispworks.com/documentation/HyperSpec/Body/02_ad.htm"
  (and c
       (not (position c '#. (concatenate 'string
                                         ";\"'(),`"
                                         "|\\"
                                         +whitespaces+)
                      :test #'char=))))

(defreader read-backslash ()
  (when (read-char* state #\\)
    (when (read-char* state)
      (list start (pos state)))))

(defreader read-pipe ()
  (when (current-char= state #\|)
    (read-quoted-string state #\| #\\)))

#++
(progn
  (trace
   not-terminatingp
   not-terminatingp-nor-pipe
   read-token
   read-backslash
   read-pipe
   read-while
   donep)
  (untrace))

(defreader read-token ()
  "Read one token."
  (loop
    :with escape-once
    :for char = (current-char state)
    :while (not-terminatingp char)
    :for part = (or
                 (read-backslash state)
                 (read-pipe state)
                 (read-while state #'not-terminatingp-nor-escape))
    :while (and part
                (not-terminatingp (current-char state))
                (not (donep state)))
    :finally (return (unless (= start (pos state))
                       (let ((end (if part (second part) +end+)))
                         (token start end)
                         ;; for debugging
                         #++
                         (node 'token start end
                               ;; Only for debugging
                               (source-substring state start end)))))))



;; TODO Do something with this, to help error recovery, or at least
;; tell the user something.
(defreader read-extraneous-closing-parens ()
  (when (read-char* state #\))
    (make-node :type :extraneous-closing-parens
               :start start
               :end +end+)))

(defun valid-node-p (node)
  (and node
       (typep node 'node)
       (not (no-end-p node))))


(defparameter *state-control-string*
  "position: ~D char: ~s context: «~a»")

(defun state-context (state)
  (let* ((pos (pos state))
         (string (source state)))
    `(,pos
      ,(at state pos)
      ,(breeze.utils:around string pos))))


;; don't forget to handle dotted lists
(defreader read-parens ()
  (when (read-char* state #\()
    ;; Read while read-any != nil && char != )
    (loop
      ;; :for guard :below 1000 ; infinite loop guard
      :while (not (read-char* state #\))) ; good ending
      :for el = (read-any state)          ; mutual recursion
      :when el
        :collect el :into content
      :unless (valid-node-p el)
        :do (return (parens start +end+ content))
      :finally (return (parens start (pos state) content)))))

;; TODO add tests with skip-whitespaces-p set
(defun read-any (state &optional skip-whitespaces-p)
  (if skip-whitespaces-p
      (let ((whitespaces (read-whitespaces state))
            (form (read-any state nil)))
        (values whitespaces form))
      (or
       (some #'(lambda (fn)
                 (funcall fn state))
             '(read-whitespaces
               read-block-comment
               read-sharpsign-dispatching-reader-macro
               read-string
               read-line-comment
               read-punctuation
               read-token
               read-parens                ; recursion
               read-extraneous-closing-parens))
       (unless (donep state)
         (error "This is a bug: read-any read nothing and would return nil, but we're not done reading the file...~%~?"
                *state-control-string*
                (state-context state))))))


;;; Putting it all toghether

(defun parse (string &aux (state (make-state string)))
  "Parse a string, stop at the end, or when there's a parse error."
  (setf (tree state)
        (loop
          ;; :for i :from 0
          :for node-start = (pos state)
          :for node = (read-any state)
          ;; :when (< 9000 i) :do (error "Really? over 9000 top-level forms!? That must be a bug...")
          :when node
            :collect node
          :while (and (valid-node-p node)
                      (not (donep state)))))
  state)

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

    ;; Loop invariant
    :when (= (pos state)
             token-start)
      :do (error "This is a bug: failed to advance the position at ~D, character ~C, token: ~s"
                 token-start
                 (at state token-start)
                 token)

    :when (and (valid-node-p token) (not unknown-start)) ; Happy path
      :collect token :into result

    :when (and (valid-node-p token) unknown-start)   ;  Back on the happy path
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


;;; Unparse

;; Should I pass the depth here too?
(defun write-node (node state stream)
  (when node
    (write-string (source state) stream
                  :start (start node)
                  :end (if (no-end-p node)
                           (length (source state))
                           (end node)))))

(defun %unparse (tree state stream depth transform)
  (when tree
    (if (listp tree)
        (mapcar (lambda (node)
                  (%unparse (funcall transform node)
                            state stream (1+ depth)
                            transform))
                tree)
        (case (node-type tree)
          (parens
           (write-char #\( stream)
           (%unparse (node-children tree) state stream depth transform)
           (unless (no-end-p tree)
             (write-char #\) stream)))
          (t
           (write-node (funcall transform tree) state stream))))))

(defun unparse (state &optional (stream t) (transform #'identity))
  (if stream
      (%unparse (tree state) state
                (if (eq t stream) *standard-output* stream)
                0
                transform)
      ;; if stream is nil
      (with-output-to-string (out)
        (%unparse (tree state) state out 0 transform))))
