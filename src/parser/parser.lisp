(cl:in-package #:cl-user)

(uiop:define-package #:breeze.parser
    (:documentation "A fast, lossless, robust and superficial reader for a superset of
common lisp.")
  (:use #:cl #:breeze.logging)
  (:use-reexport
   #:breeze.parse-tree
   #:breeze.parser-state
   #:breeze.iterator
   #:breeze.generics)
  (:import-from #:breeze.string
                #:+whitespaces+
                #:whitespacep)
  (:import-from #:alexandria
                #:when-let
                #:when-let*)
  (:export
   ;; top-level parsing
   #:parse))

(in-package #:breeze.parser)


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


;; 2025-10-07 only used in read-number
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
  (when (and (current-char= state #\#)
             (next-char= state #\|))
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
            (return (block-comment start +end+
                                   :errors "Unterminated block comment")))
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

(defun read-up-to-newline (state)
  (let ((newline (search #.(format nil "~%")
                         (source state)
                         :start2 (current-position state))))
    (setf (current-position state)
          (or newline (length (source state))))
    (current-position state)))

(defreader read-line-comment ()
  "Read ;"
  (when (read-char* state #\;)
    (line-comment start (read-up-to-newline state))))

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

;;; TODO in the following read-sharp-* functions, number should be
;;; renamed "prefix"

(defun read-sharp-backslash (state start number)
  (declare (ignore number))
  ;; TODO (if number) => invalid syntax
  (when (read-char* state #\\)
    (decf (current-position state))
    (let ((token (when (valid-position-p state (1+ (current-position state)))
                   (read-token state))))
      (sharp-char start (if token (current-position state) +end+)
                  token
                  :errors `(,@(unless token
                                ;; TODO error message
                                ()))))))

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

(defun %read-sharp-any (state start type)
  (multiple-value-bind (end children)
      (read-any* state)
    (make-instance type
                   :start start
                   :end end
                   :children children)))

(defun read-sharp-quote (state start number)
  (declare (ignore number))
  ;; TODO (if number) => invalid syntax
  (when (read-char* state #\')
    (%read-sharp-any state start 'sharp-function)))

(defun read-sharp-left-parens (state start number)
  (declare (ignore number))
  ;; N.B. we use current-char instead of read-char, because we don't
  ;; want to consume the left-parens right away.
  (when (current-char= state #\()
    (let ((form (read-parens state)))
      (sharp-vector start (if form (current-position state) +end+)
                    form
                    :errors `(,@(unless form
                                ;; TODO error message
                                ()))))))

(defun read-sharp-asterisk (state start length)
  (declare (ignore length))
  (when (read-char* state #\*)
    (multiple-value-bind (bits range)
        (read-number state 2)
      (declare (ignore range))
      ;; TODO check (- (cdr range) (car range)) <= length
      (sharp-bitvector start (current-position state) bits))))

(defun read-sharp-colon (state start number)
  (declare (ignore number))
  ;; TODO (if number) => invalid syntax
  (when (read-char* state #\:)
    (let* ((token-start (current-position state))
           (token (or (read-token state)
                      (token token-start token-start :name "")))
           (end (current-position state))
           (sharp-uninterned
             (sharp-uninterned start end token)))
      sharp-uninterned)))

(defun read-sharp-dot (state start number)
  (declare (ignore number))
  ;; TODO (if number) => invalid syntax
  (when (read-char* state #\.)
    (%read-sharp-any state start 'sharp-eval)))

(defun %read-sharp-number (state start type radix)
  "Read a number"
  (let ((n (read-number state radix)))
    (make-instance
     type
     :start start
     :end (if n (current-position state) +end+)
     :errors `(,@(unless n
                   ;; TODO error message
                   ())))))

(defun read-sharp-b (state start number)
  (declare (ignore number))
  ;; TODO (if number) => invalid syntax
  (when (read-char* state #\b nil)
    (%read-sharp-number state start 'sharp-binary 2)))

(defun read-sharp-o (state start number)
  (when (read-char* state #\o nil)
    (if number
        ;; TODO (if number) => invalid syntax
        (sharp-octal start +end+ nil
                     :errors `(;; TODO error message
                               ()))
        (%read-sharp-number state start 'sharp-octal 8))))

(defun read-sharp-x (state start number)
  (declare (ignore number))
  ;; TODO (if number) => invalid syntax
  (when (read-char* state #\x nil)
    (%read-sharp-number state start 'sharp-hexa 16)))

(defun read-sharp-r (state start radix)
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
       ;; TODO (DONE!) each node could have a list of errors
       ;; (diagnostics?)  attached, so we can have better feedback
       ;; than just "syntax error"
       ;;
       ;; TODO if we fail to parse this, it would be nice to tell the
       ;; caller (read-sharp-dispatching-reader-macro -> read-any)
       ;; where it would make sense to restart reading. I think it's
       ;; something doable for terminals (e.g. not read-parens)
       ;;
       ;; TODO instead of one sentinel value (+end+), we could use
       ;; negative positions... we could encode the "where to restart
       ;; reading" with this.
       ;;   - valid-node-p could use =(not (minusp 0))= to check if
       ;;     =(end node)= is considered valid.
       ;;   - -1 could still be used as "no-end"
       ;;   - any POS below -1 would mean "I think we can can restart
       ;;     parsing at position (- (1+ POS))
       ;; TODO write those in docs/*.org, like a normal human being
       (sharp-radix start +end+ nil nil :errors `(("Missing radix in #nR reader macro."))))
      ((not (<= 2 radix 36))
       ;; illegal radix for #R: <X>.
       (sharp-radix start +end+ nil radix
                    :errors `(("Illegal radix ~s, must be an integer between 2 and 36 (inclusively)." ,radix))))
      (t
       ;; TODO BUG: here we read an INTEGER, but it should be a RATIONAL
       (let ((n (read-number state radix)))
         (sharp-radix start (if n (current-position state) +end+) n radix
                      :errors `(,@(unless n
                                ;; TODO error message
                                ()))))))))

(defun read-sharp-c (state start number)
  (declare (ignore number))
  ;; TODO (if number) => invalid syntax
  (when (read-char* state #\c nil)
    (let ((form (read-parens state)))
      (sharp-complex start (if form (current-position state) +end+) form
                     :errors `(,@(unless form
                                   ;; TODO error message
                                   ()))))))

(defun read-sharp-a (state start length)
  (declare (ignore length))
  (when (read-char* state #\a nil)
    (let ((form (read-parens state)))
      (sharp-array start (if form (current-position state) +end+) form
                   :errors `(,@(unless form
                                 ;; TODO error message
                                 ()))))))

(defun read-sharp-s (state start number)
  (declare (ignore number))
  ;; TODO (if number) => invalid syntax
  (when (read-char* state #\s nil)
    (%read-sharp-any state start 'sharp-structure)))

(defun read-sharp-p (state start number)
  (declare (ignore number))
  ;; TODO (if number) => invalid syntax
  (when (read-char* state #\p nil)
    ;; TODO here we should expect a string
    (%read-sharp-any state start 'sharp-pathname)))

(defun read-sharp-equal (state start number)
  (when (read-char* state #\=)
    (multiple-value-bind (end children)
        (read-any* state)
      (sharp-label start end number children))))

(defun read-sharp-sharp (state start number)
  (when (read-char* state #\#)
    (let ((valid-number-p (and (integerp number)
                               (<= 0 number))))
      ;; TODO it would be great if we could check if there's a
      ;; corresponding "#=" before.
      (sharp-reference start (if valid-number-p
                                 (current-position state)
                                 +end+)
                       number
                       ;; TODO add more precise errors
                       :errors `(,@(unless (and number valid-number-p)
                                     `(("Invalid label ~s, it must be an unsigned decimal integer." ,number))))))))

(defun read-sharp-plus (state start number)
  (declare (ignore number))
  ;; TODO (if number) => invalid syntax
  (when (read-char* state #\+)
    (%read-sharp-any state start 'sharp-feature)))

(defun read-sharp-minus (state start number)
  (declare (ignore number))
  ;; TODO (if number) => invalid syntax
  (when (read-char* state #\-)
    (%read-sharp-any state start 'sharp-feature-not)))

;; TODO #) and #<any whitespace> are **invalid**
;; See https://www.lispworks.com/documentation/HyperSpec/Body/02_dh.htm
(defreader read-sharp-dispatching-reader-macro ()
  "Read reader macros #..."
  (when (read-char* state #\#)
    (let ((number (read-number state)))
      (if (null (current-char state))
          (sharp-unknown
           start +end+
           :errors '(("Unterminated dispatch character reader macro.")))
          (or
           (some
            (lambda (fn)
              (funcall fn state start number))
            '(read-sharp-backslash
              read-sharp-quote
              read-sharp-left-parens
              read-sharp-asterisk
              read-sharp-colon
              read-sharp-dot
              read-sharp-b
              read-sharp-o
              read-sharp-x
              read-sharp-r
              read-sharp-c
              read-sharp-a
              read-sharp-s
              read-sharp-p
              read-sharp-equal
              read-sharp-sharp
              read-sharp-plus
              read-sharp-minus
              ;; TODO read-sharp-space "# "
              ;; TODO read-sharp-less-than "#<"
              ))
           ;; Invalid syntax OR custom reader macro
           (sharp-unknown start +end+))))))

;; TODO this is a weird function name for what it does...
;; it reads things that are "quote-like"
(defreader read-quote ()
  "Read ` ' , ,@ and ,."
  (when-let* ((current-char (current-char state))
              (foundp (assoc current-char
                             '((#\' . quote-node)
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
      (make-instance (cdr foundp)
                     :start start
                     ;; TODO check if children is `no-end-p' (like an unclosed parens for example
                     :end end
                     :children children
                     :errors `(,@(unless children
                                ;; TODO error message
                                ()))))))

;; TODO this is also a weird function name... (its "historical")
(defreader read-punctuation ()
  "Read . or pagebreak (^L)"
  (when-let* ((current-char (current-char state))
              (foundp (assoc current-char
                              '((#\. . dot)
                                (#\page . page))
                              :test #'char=)))
    (prog1 (make-instance (cdr foundp) :start start :end (1+ start))
      (incf (current-position state)))))

(defun read-quoted-string (state delimiter escape &optional validp)
  "Read strings delimited by DELIMITER where the single escape character
is ESCAPE. Optionally check if the characters is valid if VALIDP is
provided."
  (let ((start (current-position state)))
    (when (at= state (current-position state) delimiter)
      (let ((out (make-string-output-stream)))
        (values
         (loop
           ;; :for guard :upto 10
                  :for pos :from (1+ (current-position state))
           :for c = (at state pos)
           :do
              ;; (format *debug-io* "~%~A ~A" pos c)
              (cond
                ((null c)
                 (setf (current-position state) pos)
                 ;; TODO somehow return an error from here
                 (return (range start +end+)))
                ((char= c delimiter)
                 (setf (current-position state) (1+ pos))
                 (return (range start (1+ pos))))
                ((char= c escape)
                 (incf pos)
                 (let ((escaped-char (current-char state)))
                   (when escaped-char
                     (write-char escaped-char out))))
                ((and validp
                      (not (funcall validp c)))
                 (setf (current-position state) pos)
                 ;; TODO somehow return an error from here
                 (return (range start +end+)))
                (t
                 (write-char c out))))
         (get-output-stream-string out))))))

(defreader read-string ()
  "Read \"\""
  (when-let ((range (read-quoted-string state #\" #\\)))
    (string-node (start range) (end range))))

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

(defreader read-token ()
  "Read one token."
  (let ((prefix (make-string-output-stream))
        (markers)
        (name (make-string-output-stream))
        end)
    (loop
      :for char = (current-char state)
      :while (not-terminatingp char)
      :do (cond
            ((char= char #\|)
             (multiple-value-bind (range string)
                 (read-quoted-string state #\| #\\)
               (write-string string name)
               (when (no-end-p range)
                 (setf end (end range))))
             (decf (slot-value (iterator state) 'position)))
            ((char= char #\\)
             (next (iterator state))
             (let ((escaped-char (current-char state)))
               (when escaped-char
                 (write-char escaped-char name))))
            ((char= char #\:)
             (write-string (get-output-stream-string name) prefix)
             (push (current-position state) markers))
            (t
             (write-char
              ;; TODO this assumes *read-case* is :upcase
              (char-upcase char)
              name)))
          (next (iterator state))
      :while (and (not-terminatingp (current-char state))
                  (not (donep state))))
    (let ((end (or end (current-position state))))
      (unless (= start end)
        (let* ((prefix (get-output-stream-string prefix))
               (prefix (unless (zerop (length prefix))
                         prefix))
               (name (get-output-stream-string name))
               (name (unless (and (zerop (length name))
                                          markers
                                          ;; the rightmost marker is
                                          ;; at the end of the token.
                                          (= (1+ (car markers)) end))
                       name))
               (valid-marker-p (or
                                (null markers)
                                (= 1 (length markers))
                                (and (= 2 (length markers))
                                     (= (1- (first markers))
                                        (second markers))))))
          (token start end
                 :package-prefix prefix
                 :package-marker markers
                 :name name
                 :errors `(,@(unless valid-marker-p
                               `(("Invalid package marker.")))
                           ,@(unless name
                               `(("Missing name after package marker."))))))))))


;; TODO Do something with this, to help error recovery, or at least
;; tell the user something.
(defreader read-extraneous-closing-parens ()
  (when (read-char* state #\))
    (extraneous-closing-parens
     start +end+
     :errors `(("Extraneous closing parenthesis.")))))


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
        :do (return (parens start +end+
                            (ensure-nodes content)
                            :errors `(,@(when (or (null el)
                                                  (no-end-p el))
                                        `(("Missing closing parenthesis."))))))
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
               read-sharp-dispatching-reader-macro
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

(defreader read-shebang ()
  (when (and (current-char= state #\#)
             (next-char= state #\!))
    (shebang start (read-up-to-newline state))))


;;; Putting it all together

(defun parse (string &optional (state (make-state string)))
  "Parse a string, stop at the end, or when there's a parse error."
  (let ((result (make-array '(0) :adjustable t :fill-pointer t)))
    ;; check if `string' starts with #!
    (when-let ((shebang (read-shebang state)))
      (vector-push-extend shebang result))
    (loop
      ;; :for i :from 0
      :for start = (current-position state)
      :for node = (read-any state)
      ;; :when (< 9000 i) :do (error "Really? over 9000 top-level forms!? That must be a bug...")
      :when node
        :do (vector-push-extend node result)

      :until
      #| we stop when we encounter a node that is not terminated. it
      would be possible to infer where to try to start parsing
      again. |#
      (or (null node) (no-end-p node) (donep state)))
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
    :for start = (current-position state)
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



;; TODO add tests
(defmethod make-node-iterator ((string string))
  (make-node-iterator (parse string)))


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
        (%unparse (children tree) state stream depth transform)
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
