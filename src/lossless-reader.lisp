(cl:in-package #:cl-user)

(uiop:define-package #:breeze.lossless-reader
    (:documentation "A fast, lossless, robust and superficial reader for a superset of
common lisp.")
  (:use #:cl #:breeze.logging)
  (:use-reexport #:breeze.iterator)
  (:import-from #:breeze.generics
                #:eqv)
  (:import-from #:breeze.string
                #:subseq-displaced
                #:+whitespaces+
                #:whitespacep)
  (:import-from #:alexandria
                #:when-let
                #:when-let*
                #:if-let)
  ;; Parsing state
  (:export #:state
           #:source
           #:pos
           #:tree
           #:make-state
           #:source-substring
           #:top-level-in-package)
  ;; Nodes
  (:export #:+end+
           #:range
           #:node
           #:node-start
           #:node-end
           #:start
           #:end
           #:add-offset
           #:node-type
           #:node-children
           #:copy-node
           #:valid-node-p
           #:node-content)
  ;; Node sequences
  (:export #:ensure-nodes
           #:append-nodes
           #:nodes
           #:nodesp
           #:nth-node
           #:first-node
           #:second-node
           #:last-node)
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
           #:comma-at
           #:comma-dot
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
           #:sharp-unknown
           #:current-package-symbol
           #:keyword
           #:uninterned-symbol
           #:qualified-symbol
           #:possibly-internal-symbol)
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
           #:symbol-node-p
           #:whitespace-node-p
           #:line-comment-node-p
           #:whitespace-or-comment-node-p
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
  #++ ;; not exporting, those are somewhat internals...
  ;; TODO split this package
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
           #:read-quote
           #:read-punctuation
           #:read-quoted-string
           #:read-string
           #:read-token
           #:read-whitespaces
           #:read-block-comment)
  (:export #:node-contains-position-p
           #:node-range-contains-position-p
           #:node-children-contains-position-p)
  ;; Node iterator
  (:export #:make-node-iterator
           #:node-iterator
           #:node-string
           #:goto-position
           #:parent-node
           #:root-node
           #:root-node-iterator
           #:previous-sibling
           #:next-sibling
           #:map-top-level-forms)
  (:export
   ;; top-level parsing
   #:parse))

(in-package #:breeze.lossless-reader)


;;; Reader state

(defclass state ()
  ((source
    :initarg :source
    :type string
    :accessor source
    :documentation "The string being parsed.")
   (iterator
    :initarg :iterator
    :type vector-iterator
    :accessor iterator
    :documentation "The iterator on the source.")
   (tree
    :initform 0
    :initarg :pos
    :accessor tree
    :documentation "The parsed nodes.")
   ;; TODO current-package is not used just yet
   (current-package
    :initform nil
    :accessor current-package
    :documentation "Current package"))
  ;; TODO More state:
  ;; - readtable case (is it case converting)
  ;; - current input base (base of numbers)
  ;; - labels and references (#n= and #n#)
  (:documentation "The reader's state"))

(defun make-state (string)
  (make-instance 'state
                 :source string
                 :iterator (make-vector-iterator string)))

(defmethod print-object ((state state) stream)
  (print-unreadable-object
      (state stream :type t :identity nil)
    (let ((excerpt (breeze.string:around (source state)
                                         (current-position state))))
      (format stream "~s ~d/~d"
              excerpt
              (length excerpt)
              (length (source state))))))


;;; The +end+

(alexandria:define-constant +end+ -1)


;;; Range class

(defclass range ()
  ((start :type (integer 0)
          :initarg :start
          :initform 0
          :accessor start)
   (end :type (integer -1)
        :initarg :end
        :initform +end+
        :accessor end)))

(defun range (start end)
  (make-instance 'range :start start :end end))

(declaim (inline range=))
(defun range= (a b)
  (and (= (start a) (start b))
       (= (end a) (end b))))

(defmethod eqv ((a range) (b range))
  (range= a b))


;;; Node class

(defclass node (range)
  ((node-type
    :type symbol
    :initform (error "The slot :node-type must be specified.")
    :initarg :type
    :accessor node-type)
   (children
    :initform nil
    :initarg :children
    :accessor children)))

(declaim (inline node-children))
(defun node-children (node)
  (slot-value node 'children))

(defun nodep (x)
  (typep x 'node))

(defun node-start (node)
  (start node))

(defun node-end (node)
  (end node))

(declaim (inline node=))
(defun node= (a b)
  (and (eq (node-type a)
           (node-type b))
       (range= a b)
       (eqv (node-children a) (node-children b))))

(defmethod eqv ((a node) (b node))
  (node= a b))

(defun ensure-nodes (x)
  "Ensure that that X is a sequence of node."
  (typecase x
    (null nil)
    (vector x)
    (cons (coerce x 'vector))
    (t (vector x))))

(defun append-nodes (nodes1 nodes2)
  "Concatenate two sequences of nodes."
  (concatenate 'vector nodes1 nodes2))

;; (declaim (inline %nodes))
(defun %nodes (x y)
  "Create a sequence of nodes. But less user-friendly."
  (if x
      (if y
          (append-nodes (ensure-nodes x) (ensure-nodes y))
          (ensure-nodes x))
      (when y (ensure-nodes y))))

(defun nodes (&optional node &rest nodes)
  "Create a sequence of nodes."
  (%nodes node nodes))

(defun nodesp (x)
  (vectorp x))

(defun nth-node (nodes n)
  (etypecase nodes
    (null nil)
    (node (nth-node (node-children nodes) n))
    (vector (aref nodes (if (minusp n) (+ n (length nodes)) n)))))

(defun first-node (nodes)
  (nth-node nodes 0))

(defun second-node (nodes)
  (nth-node nodes 1))

(defun last-node (nodes)
  (etypecase nodes
    (null nil)
    (node nodes)
    (vector (nth-node nodes -1))))

(defun node (type start end &optional child &rest children)
  (make-instance 'node
   :type type
   :start start
   :end end
   :children (if children
                 (%nodes child children)
                 child)))

(defun make-node (&rest args &key type start end children)
  (declare (ignore type start end children))
  (apply #'make-instance 'node args))


;;; Constructors

(macrolet ((aux (type
                 &key
                   children
                   (name type)
                   no-constructor-p)
             `(progn
                ;; predicate
                (defun
                    ;; name of the predicate
                    ,(alexandria:symbolicate type '-node-p)
                    ;; lambda list
                    (node)
                  ;; docstring
                  ,(format nil "Is this a node of type ~s" type)
                  ;; predicate's implementation
                  (and (nodep node)
                       (eq (node-type node) ',type)
                       node))
                ;; constructor
                ,(unless no-constructor-p
                   `(defun
                        ;; name of the constructor function
                        ,name
                        ;; lambda list
                        (start end
                         ,@(when children
                             (case children
                               ;; :children &optional
                               (&optional (list '&optional 'children))
                               ;; :children t
                               ((t) (list 'children))
                               ;; :children some-name
                               (t (alexandria:ensure-list children)))))
                      ,(format nil "Make a node of type ~s" type)
                      ;; constructor's implementation
                      ,(cond
                         ((null children)
                          `(node ',type start end))
                         ((eq t children)
                          `(node ',type start end children))
                         (t
                          `(node ',type start end ,@(list children))))))
                ;; copier
                (defun
                    ;; name of the copier function
                    ,(alexandria:symbolicate 'copy- name)
                    ;; lambda list
                    (node &key
                            (start nil startp)
                            (end nil endp)
                            ,@(when children
                                (list `(children nil childrenp))))
                  ;; docstring
                  ,(format nil "Make a shallow copy of a node of type ~s, possibly replacing one of the attributes." type)
                  ;; copier's implementation
                  (node ',type
                        (if startp start (node-start node))
                        (if endp end (node-end node))
                        ;; N.B. This is just a shallow copy
                        ,@(when children
                            `((if childrenp children (node-children node)))))))))
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
  (aux sharp-uninterned :children token)
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
  (aux sharp-label :children t)
  (aux sharp-reference :children label)
  (aux sharp-unknown))

(defun punctuation (type position)
  (node type position (1+ position)))

(defun parens (start end &optional children)
  (node 'parens start end
        (if (nodep children)
            (nodes children)
            children)))

(defun print-nodes (stream nodes colonp atp)
  (declare (ignore colonp atp))
  (format stream "(list ~{~s~^ ~})" nodes))

(defmethod print-object ((node node) stream)
  (let ((*print-case* :downcase)
        (children (node-children node)))
    (format stream "(~:[node '~;~]~s ~d ~d~:[ ~s~;~@[ ~/breeze.lossless-reader::print-nodes/~]~])"
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


;;; Predicates

(defun comment-node-p (node)
  "Is this node a block or line comment?"
  (or (line-comment-node-p node)
      (block-comment-node-p node)))

(defun whitespace-or-comment-node-p (node)
  "Is this node a whitespace, a block comment or line comment?"
  (etypecase node
    (node-iterator (whitespace-or-comment-node-p
                    (value node)))
    (node
     (or (whitespace-node-p node)
         (comment-node-p node)
         (eq 'page (node-type node))))))

(defun symbol-node-p (node)
  (and
   (nodep node)
   (member (node-type node) '(current-package-symbol
                              keyword
                              uninterned-symbol
                              qualified-symbol
                              possibly-internal-symbol))))


;;; Content and range

(defun source-substring (state start end)
  "Get a (displaced) substring of the state's source string."
  (subseq-displaced (source state)
                    start
                    (and (plusp end) end)))

(defun node-content (state node)
  "Get a (displaced) string of the node's range."
  (source-substring state (node-start node) (node-end node)))

(defmethod no-end-p ((x integer))
  "Does this number represent the +infinity?"
  (= +end+ x))

(defmethod no-end-p ((range range))
  "Is this range open-ended?"
  (= +end+ (end range)))

;; TODO this could be "range" instead of "node"
(defmethod add-offset ((node node) offset)
  (incf (start node) offset)
  (unless (minusp (end node))
    (incf (end node) offset)))


;;; Reader position (in the source string)

(defmethod current-position ((state state))
  (current-position (iterator state)))

(defmethod (setf current-position) (new-pos (state state))
  (setf (current-position (iterator state)) new-pos))

(defmethod donep ((state state))
  (donep (iterator state)))

(defun valid-position-p (state position)
  (< -1 position (length (source state))))


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
  (at state (current-position state)))

;; TODO add tests with case-sensitive-p = nil
;; TODO split into current-char= and current-char-equal
(defun current-char= (state char &optional (case-sensitive-p t))
  "Get the character at the current STATE's position, without changing
the position."
  (at= state (current-position state) char case-sensitive-p))

(defun next-char (state &optional (n 1))
  "Peek at the next character to be read, without changing the
position."
  (at state (+ n (current-position state))))

;; TODO add tests with case-sensitive-p = nil
;; TODO split into next-char= and next-char-equal
(defun next-char= (state char &optional (n 1) (case-sensitive-p t))
  "Peek at the next character to be read, without changing the
position."
  (at= state (+ n (current-position state)) char case-sensitive-p))


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
      (incf (current-position state))
      c)))

(defun read-string* (state string &optional (advance-position-p t))
  "Search STRING in the STATE's source, at the current STATE's
position. If found, optionally advance the STATE's position to _after_
the occurrence of STRING."
  (check-type string string)
  (let ((start (current-position state))
        (end (+ (current-position state) (length string))))
    (when (valid-position-p state (1- end))
      (when-let ((foundp (search string (source state) :start2 start :end2 end)))
        (when advance-position-p
          (setf (current-position state) end))
        (list start end)))))

;; 2023-05-20 only used in read-token
;; 2024-01-03 and read- dispatch reader macro
(defun read-while (state predicate &key (advance-position-p t)
                                     (start (current-position state)))
  "Returns nil or (list start end)"
  (loop
    ;; :for guard :from 0
    :for pos :from start
    :for c = (at state pos)
    ;; :when (< 9000 guard) :do (error "read-while might be looping indefinitely...")
    :do (when (or (null c) (not (funcall predicate c)))
          (when (/= start pos)
            (when advance-position-p (setf (current-position state) pos))
            (return (range start pos)))
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
  `(defun ,name (state ,@lambda-list &aux (start (current-position state)))
     (declare (ignorable start))
     ,@body))

(defreader read-whitespaces ()
  (loop
    :for pos :from start
    :for c = (at state pos)
    :while (and c (whitespacep c) (not (char= #\page c)))
    :finally (when (/= pos start)
               (setf (current-position state) pos)
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
                      (push (- (current-position state) 2) stack))
                     (t
                      (setf situation 'other))))
            (pipe (case char
                    (#\#
                     (setf situation 'other)
                     (pop stack)
                     (when (null stack)
                       (return (block-comment start (current-position state)))))
                    (t
                     (setf situation 'other))))))))

(defreader read-line-comment ()
  "Read ;"
  (when (read-char* state #\;)
    (let ((newline (search #.(format nil "~%")
                           (source state)
                           :start2 (current-position state))))
      (setf (current-position state) (if newline
                            newline
                            (length (source state))))
      (line-comment start (current-position state)))))

;; TODO rename read-integer
(defun read-number (state &optional (radix 10))
  (when-let ((range (read-while state
                                (lambda (char)
                                  (digit-char-p char radix)))))
    (values (parse-integer (source state)
                           :start (start range)
                           :end (end range)
                           :radix radix)
            range)))

;;; TODO in the following read-sharpsign-* functions, number should be
;;; renamed "prefix"

(defun read-sharpsign-backslash (state start number)
  (declare (ignore number))
  ;; TODO (if number) => invalid syntax
  (when (read-char* state #\\)
    (decf (current-position state))
    (let ((token (when (valid-position-p state (1+ (current-position state)))
                   (read-token state))))
      (node 'sharp-char start (if token (current-position state) +end+) token))))

(defun read-any* (state)
  "Like READ-ANY, but return the end of the read and a sequence of nodes
as two values (Whereas READ-ANY returns two nodes (also as values), the
first node being whitespaces.)"
  (let ((children)
        (end +end+))
    (loop
      ;; :for i :below 100 :do (when (< 100 i) (error "read-any* might be looping infinitely,"))
      (multiple-value-bind (whitespaces form)
          (read-any state t)
        (when whitespaces
          (push whitespaces children))
        (when form
          (push form children)
          (when (valid-node-p form)
            (setf end (current-position state))))
        ;; read while comments
        ;; (break)
        (when (not (comment-node-p form))
          (return))))
    (values end (nodes (nreverse children)))))

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
      (node 'sharp-vector start (if form (current-position state) +end+) form))))

(defun read-sharpsign-asterisk (state start length)
  (declare (ignore length))
  (when (read-char* state #\*)
    (multiple-value-bind (bits range)
        (read-number state 2)
      (declare (ignore range))
      ;; TODO check (- (cdr range) (car range)) <= length
      (node 'sharp-bitvector start (current-position state) bits))))

(defun read-sharpsign-colon (state start number)
  (declare (ignore number))
  ;; TODO (if number) => invalid syntax
  (when (read-char* state #\:)
    (let* ((token-start (current-position state))
           (token (read-token state))
           (end (current-position state)))
      (node 'sharp-uninterned start end
            (or token
                (token token-start end))))))

(defun read-sharpsign-dot (state start number)
  (declare (ignore number))
  ;; TODO (if number) => invalid syntax
  (when (read-char* state #\.)
    (%read-sharpsign-any state start 'sharp-eval)))

(defun %read-sharpsign-number (state start type radix)
  "Read a number"
  (let ((n (read-number state radix)))
    (node type start (if n (current-position state) +end+))))

(defun read-sharpsign-b (state start number)
  (declare (ignore number))
  ;; TODO (if number) => invalid syntax
  (when (read-char* state #\b nil)
    (%read-sharpsign-number state start 'sharp-binary 2)))

(defun read-sharpsign-o (state start number)
  (when (read-char* state #\o nil)
    (if number
        ;; (if number) => invalid syntax
        (node 'sharp-octal start +end+)
        (%read-sharpsign-number state start 'sharp-octal 8))))

(defun read-sharpsign-x (state start number)
  (declare (ignore number))
  ;; TODO (if number) => invalid syntax
  (when (read-char* state #\x nil)
    (%read-sharpsign-number state start 'sharp-hexa 16)))

(defun read-sharpsign-r (state start radix)
  (when (read-char* state #\r nil)
    (cond
      ((null radix)
       ;; radix missing in #R

       ;; Some ideas to improve error detection, reporting and correction
       ;;
       ;; TODO here we could try to call "read-number" anyway
       ;;
       ;; TODO we could try with radix = 36 to be the "most
       ;; inclusive", if that fails we can tell the user "char <X> is
       ;; not a valid digit, even if the radix was set correctly"
       ;;
       ;; TODO we could try to infer which radix would make sense? but
       ;; if they're using "R" it's probably because it's a weird
       ;; radix... we could tell the user which minimal radix would
       ;; work.
       ;;
       ;; TODO each node could have a list of errors (diagnostics?)
       ;; attached, so we can have better feedback than just "syntax
       ;; error" (could we reuse the node-children to store the
       ;; diagnostics?)
       ;;
       ;; TODO if we fail to parse this, it would be nice to tell the
       ;; caller (read-sharpsign-dispatching-reader-macro -> read-any)
       ;; where it would make sense to restart reading. I think it's
       ;; something doable for terminals (e.g. not read-parens)
       ;;
       ;; TODO instead of one sentinel value (+end+), we could use
       ;; negative positions... we could encode the "where to restart
       ;; reading" with this.
       ;;   - valid-node-p could use =(not (minusp 0))= to check if
       ;;     =(node-end node)= is considered valid.
       ;;   - -1 could still be used as "no-end"
       ;;   - any POS below -1 would mean "I think we can can restart
       ;;     parsing at position (- (1+ POS))
       ;; TODO write those in docs/*.org, like a normal human being
       (node 'sharp-radix start +end+))
      ((not (<= 2 radix 36))
       ;; illegal radix for #R: <X>.
       (node 'sharp-radix start +end+))
      (t
       (let ((n (read-number state radix)))
         (node 'sharp-radix start (if n (current-position state) +end+)))))))

(defun read-sharpsign-c (state start number)
  (declare (ignore number))
  ;; TODO (if number) => invalid syntax
  (when (read-char* state #\c nil)
    (let ((form (read-parens state)))
      (node 'sharp-complex start (if form (current-position state) +end+) form))))

(defun read-sharpsign-a (state start length)
  (declare (ignore length))
  (when (read-char* state #\a nil)
    (let ((form (read-parens state)))
      (node 'sharp-array start (if form (current-position state) +end+) form))))

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
      (node 'sharp-label start end (vector number children)))))

(defun read-sharpsign-sharpsign (state start number)
  (when (read-char* state #\#)
    (node 'sharp-reference start (if (and (integerp number)
                                          (<= 0 number))
                                     (current-position state)
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

(defreader read-quote ()
  "Read ` ' , ,@ and ,."
  (when-let* ((current-char (current-char state))
              (foundp (assoc current-char
                             '((#\' . quote)
                               (#\` . quasiquote)
                               (#\, . comma))
                             :test #'char=)))
    (incf (current-position state))
    ;; ,@ and ,.
    (when (eq (cdr foundp) 'comma)
      (when-let* ((current-char (current-char state))
                  (foundp* (assoc current-char
                                  '((#\. . comma-dot)
                                    (#\@ . comma-at))
                                  :test #'char=)))
        (incf (current-position state))
        (setf foundp foundp*)))
    (multiple-value-bind (end children)
        (read-any* state)
      (node (cdr foundp) start end children))))

(defreader read-punctuation ()
  "Read . or pagebreak (^L)"
  (when-let* ((current-char (current-char state))
              (foundp (assoc current-char
                              '((#\. . dot)
                                (#\page . page))
                              :test #'char=)))
    (prog1 (punctuation (cdr foundp) start)
      (incf (current-position state)))))

(defun read-quoted-string (state delimiter escape &optional validp)
  "Read strings delimited by DELIMITER where the single escape character
is ESCAPE. Optionally check if the characters is valid if VALIDP is
provided."
  (let ((start (current-position state)))
    (when (at= state (current-position state) delimiter)
      (loop
        ;; :for guard :upto 10
        :for pos :from (1+ (current-position state))
        :for c = (at state pos)
        :do
           ;; (format *debug-io* "~%~A ~A" pos c)
           (cond
             ((null c)
              (setf (current-position state) pos)
              (return (range start +end+)))
             ((char= c delimiter)
              (setf (current-position state) (1+ pos))
              (return (range start (1+ pos))))
             ((char= c escape) (incf pos))
             ((and validp
                   (not (funcall validp c)))
              (setf (current-position state) pos)
              (return (range start +end+))))))))

(defreader read-string ()
  "Read \"\""
  (when-let ((range (read-quoted-string state #\" #\\)))
    (node 'string (start range) (end range))))

(defun not-terminatingp (c)
  "Test whether a character is terminating. See
http://www.lispworks.com/documentation/HyperSpec/Body/02_ad.htm"
  (and c
       (not (position c '#.(concatenate 'string
                                         ";\"'(),`"
                                         +whitespaces+)
                      :test #'char=))))

(defun not-terminatingp-nor-escape (c)
  "Test whether a character is terminating or #\| or #\\. See
http://www.lispworks.com/documentation/HyperSpec/Body/02_ad.htm"
  (and c
       (not (position c '#.(concatenate 'string
                                         ";\"'(),`"
                                         "|\\"
                                         +whitespaces+)
                      :test #'char=))))

(defreader read-backslash ()
  (when (read-char* state #\\)
    (when (read-char* state)
      (range start (current-position state)))))

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
    :finally (return (unless (= start (current-position state))
                       (let ((end (if part (end part) +end+)))
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
  (let* ((pos (current-position state))
         (string (source state)))
    `(,pos
      ,(at state pos)
      ,(breeze.string:around string pos))))

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
        :do (return (parens start +end+ (ensure-nodes content)))
      :finally (return (parens start (current-position state)
                               (ensure-nodes content))))))

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
               read-quote
               read-punctuation
               read-token
               read-parens                ; recursion
               read-extraneous-closing-parens))
       (unless (donep state)
         (error "This is a bug: read-any read nothing and would return nil, but we're not done reading the file...~%~?"
                *state-control-string*
                (state-context state))))))


;;; Putting it all together

(defun parse (string &optional (state (make-state string)))
  "Parse a string, stop at the end, or when there's a parse error."
  (let ((result (make-array '(0) :adjustable t :fill-pointer t)))
    ;; TODO check if `string' starts with #!
    (loop
      ;; :for i :from 0
      :for node-start = (current-position state)
      :for node = (read-any state)
      ;; :when (< 9000 i) :do (error "Really? over 9000 top-level forms!? That must be a bug...")
      :when node
        :do (vector-push-extend node result)
      :while (and (valid-node-p node)
                  (not (donep state))))
    ;; When the input string is empty, we insert a 0-length whitespace
    ;; node at the root. This lets us uses the iterator's `value'
    ;; method without bound checks. This is a trade-off: we have a
    ;; slightly incorrect parse tree, but we avoid checks
    ;; everywhere...
    (when (zerop (length string))
      (vector-push-extend (whitespace 0 0) result))
    (setf (tree state) result))
  state)

(defun reparse (state)
  (loop
    :for node-start = (current-position state)
    :for node = (read-any state)
    ;; :when (< 9000 i) :do (error "Really? over 9000 top-level forms!? That must be a bug...")
    :when node
      :collect node
    :while (and (valid-node-p node)
                (not (donep state)))))

;; (parse "#2()")
;; (parse "(")

;; TODO This is a hot mess :P
(defun parse* (string)
  "Parse a string, tries to recover when something is not a valid parse."
  (loop
    :with state = (make-state string)
    :with unknown-start
    :for token-start = (current-position state)
    ;; :for guard :upto 1000               ; infinite loop guard
    :for token = (read-any state)

    ;; Loop invariant
    :when (= (current-position state)
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
           (setf unknown-start (current-position state)))
         ;; TODO For now we simply increment, but later we'll search
         ;; for synchornization points, hence the setf instead of an
         ;; incf
         (setf (current-position state) (1+ (current-position state)))

    :when (donep state)               ; Done
      :do (if unknown-start
              (return (append result `((unknown ,unknown-start
                                                ,(current-position state)))))
              (return result))))


;;; Node iterators

;; TODO create a mix-in class for "parser-state" (same for "name")
(defclass node-iterator (tree-iterator)
  ((parser-state
    ;; TODO rename to parser-state
    :initarg :state
    :type state
    ;; TODO rename to parser-state
    :accessor state))
  (:documentation "An iterator for parse-trees."))

(defmethod copy-iterator ((node-iterator node-iterator) &optional target)
  (declare (ignore target))
  (let ((iterator (call-next-method)))
    (setf (state iterator) (state node-iterator))
    iterator))

(defmethod print-object ((node-iterator node-iterator) stream)
  (print-unreadable-object
      (node-iterator stream :type t :identity nil)
    (with-slots (depth positions subtrees) node-iterator
      (format stream "~{~s~^, ~}"
              (list depth positions)))))

(defmethod children ((iterator node-iterator))
  (unless (donep iterator)
    (let* ((node (value iterator)))
      (when-let ((children (node-children node)))
        ;; (break "node: ~s children: ~s" node children)
        (typecase children
          (vector
           (cond
             ;; TODO would be nice not to have this special case
             ((sharp-label-node-p node) (aref children 1))
             (t children)))
          (node children))))))

;; TODO add tests
(defmethod make-node-iterator ((state state))
  (when (null (tree state))
    (error "Can't iterate on an empty parse tree."))
  (make-instance 'node-iterator
                 :root (tree state)
                 :state state))

;; TODO add tests
(defmethod make-node-iterator ((string string))
  (make-node-iterator (parse string)))

(defmethod source ((node-iterator node-iterator))
  (source (state node-iterator)))

(defmethod node-string ((node-iterator node-iterator))
  (node-content (state node-iterator) (value node-iterator)))


;;; goto point

(defun node-contains-position-p (node position)
  (let ((start (node-start node))
        (end (node-end node)))
    (and (<= start position)
         (or (no-end-p node)
             (< position end)))))

(defun node-range-contains-position-p (start-node end-node position)
  (check-type start-node node)
  (check-type end-node node)
  (let ((start (start start-node))
              (end (end end-node)))
          (and (<= start position)
               (or (no-end-p end-node)
                   (< position end)))))

(defun node-children-contains-position-p (node position)
  (when-let ((children (node-children node)))
    (typecase children
      (vector
       (when (sharp-label-node-p node)
         ;; special case: the "sharp-label" (#=) nodes have 2 children: 1
         ;; integer (the label) and one node for the labeled object.
         (unless (setf children (aref children 1))
           (return-from node-children-contains-position-p)))
       (node-range-contains-position-p
        (aref children 0)
        (aref children (1- (length children)))
        position))
      (node
       (node-contains-position-p children position)))))

(defmethod goto-position ((iterator node-iterator) position)
  ;; TODO this might not be necessary, as an optimization, when can
  ;; check if the current node contains the position. If yes, check if
  ;; there are children, if not, check the parent.
  ;;
  ;; TODO use binary search
  (cond
    ((<= (length (source iterator)) position)
     ;; TODO add a test for this case
     (setf position (1- (length (source iterator)))))
    ((donep iterator) (reset iterator))
    ((node-contains-position-p (root-node iterator) position)
     (loop :for d :from (slot-value iterator 'depth) :downto 0
           :if (node-contains-position-p (value-at-depth iterator d) position)
             :do (return)
           :else
             :do (pop-subtree iterator)))
    ;; TODO this could eazily be optimized by going backward (or bin search)
    ((< position (node-start (value iterator)))
     (reset iterator)))
  (loop
    :until (donep iterator)
    :for node = (value iterator)
    :do
    #++ (log-debug "============ depth: ~d positions: ~s donep: ~s node: ~s"
                   (slot-value iterator 'depth)
                   (slot-value iterator 'positions)
                   (donep iterator)
                   node)
        (cond
          ((= (start node) position)
           #++ (log-debug "stop: right at the start of a node")
           (return))
          ((node-contains-position-p node position)
           #++ (log-debug "node-contains-position-p ~a => T" node)
           (if (node-children node)
               (if (not (node-children-contains-position-p node position))
                   (progn
                     #++ (log-debug "stop: no need to recurse")
                     (return))
                   ;; this will recurse into the node's children
                   (go-down iterator))
               (progn
                 #++ (log-debug "stop: done!")
                 (return))))
          ((< position (node-end node))
           #++ (log-debug "stop: went too far! position: ~d node: ~s"
                          position node)
           (return))
          (t
           (next iterator))))
  iterator)



(defmethod value-at-depth ((iterator node-iterator) depth)
  (with-slots (positions subtrees) iterator
    (let ((pos (aref positions depth))
          (subtree (aref subtrees depth)))
      (etypecase subtree
        (vector (aref subtree pos))
        (t subtree)))))

(defmethod parent-node ((iterator node-iterator))
  (parent-value iterator))

(defmethod root-node ((iterator node-iterator))
  (root-value iterator))

(defmethod root-node-iterator ((iterator node-iterator))
  (let ((root (copy-iterator iterator)))
    (goto-root root)))

(defmethod start ((node-iterator node-iterator))
  "Get the start position of the current node."
  (start (value node-iterator)))

(defmethod end ((node-iterator node-iterator))
  "Get the end position of the current node."
  (end (value node-iterator)))

;; TODO tests
(defmethod firstp ((iterator node-iterator))
  "Is the current value the first one at the current depth?"
  (zerop (pos iterator)))

;; TODO tests
(defmethod lastp ((iterator node-iterator))
  "Is the current value the last one at the current depth?"
  (let ((pos (pos iterator))
        (subtree (subtree iterator)))
    (etypecase subtree
      (vector (= pos (1- (length subtree))))
      (t t))))

;; TODO tests
(defmethod previous-sibling ((iterator node-iterator))
  "Get the previous node at the same depth, or nil if there's is none."
  (unless (firstp iterator)
    (let ((pos (pos iterator))
          (subtree (subtree iterator)))
      (etypecase subtree
        (vector (aref subtree (1- pos)))
        (t nil)))))

;; TODO tests
(defmethod next-sibling ((iterator node-iterator))
  "Get the next node at the same depth, or nil if there's is none."
  (unless (lastp iterator)
    (let ((pos (pos iterator))
          (subtree (subtree iterator)))
      (etypecase subtree
        (vector (aref subtree (1+ pos)))
        (t nil)))))

;; TODO method "is point at start of form"
;; TODO method "is point at end of form"



#++
(defmethod crumbs ((iterator node-iterator))
  (loop :for i :from 0 :upto (depth iterator)
        :for node-at-depth = (value-at-depth iterator i)
        :for crumb-node = (or (first-node (node-children node-at-depth)) node-at-depth)
        :collect (if (= i (depth iterator))
                     (node-content (state iterator) node-at-depth)
                     (when crumb-node
                       (node-content (state iterator) crumb-node)))))

#++
(defmethod crumbs ((iterator node-iterator))
  (loop :for i :from 0 :below (depth iterator)
        :for node-at-depth = (value-at-depth iterator i)
        :for crumb-node = (or (first-node (node-children node-at-depth))
                              node-at-depth)
        :collect (when crumb-node
                   (node-content (state iterator) crumb-node))))

#++
(let* ((input "a (b c (d (e g)))")
       (state (parse input))
       (it (make-node-iterator state)))
  (loop :for i :below (length input)
        :do (goto-position it i)
        :collect (crumbs it)))


(defun type-path (node-iterator)
  (let ((depth (slot-value node-iterator 'depth)))
    (loop :for d :upto depth
          :for node = (value-at-depth node-iterator d)
          :collect (node-type node))))

#++
(let* ((input "a (b c (d (e g)))")
       (state (parse input))
       (it (make-node-iterator state)))
  (loop :for i :below (length input)
        :do (goto-position it i)
        :collect (type-path it)))

(defmethod add-offset ((iterator node-iterator) offset)
  (loop
    :until (donep iterator)
    :for node = (value iterator)
    :do (add-offset node offset)
    (next iterator)))


;; TODO add tests
(defmethod map-top-level-forms (function (state state))
  ;; TODO Recurse into forms that "preserves" top-level-ness:
  ;; progn, locally, macrolet, symbol-macrolet, eval-when
  (loop :with iterator = (make-node-iterator state)
        :until (donep iterator)
        :do (let ((node (value iterator)))
              (unless (whitespace-or-comment-node-p node)
                (funcall function iterator)))
            (incf (pos iterator))))


;;; Unparse (not exported, only used to test the parser!)

;; Should I pass the depth here too?
(defun write-node (node state stream)
  (when node
    (write-string (source state) stream
                  :start (start node)
                  :end (if (no-end-p node)
                           (length (source state))
                           (end node)))))

(defun %unparse (tree state stream depth transform)
  (etypecase tree
    (null)
    (vector
     (map nil (lambda (node)
                (%unparse (funcall transform node)
                          state stream (1+ depth)
                          transform))
          tree))
    (node
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
