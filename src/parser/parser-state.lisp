(cl:in-package #:cl-user)

(defpackage #:breeze.parser-state
  (:documentation "An object that represents a parser's state.")
  (:use #:cl #:breeze.iterator #:breeze.generics)
  (:import-from #:alexandria #:when-let)
  (:import-from #:breeze.string
                #:subseq-displaced)
  ;; Parsing state
  (:export #:state
           #:source
           #:pos
           #:tree
           #:make-state
           #:source-substring
           ;; TODO FIXME this symbol doesn't exist
           #:top-level-in-package)
  (:export #:current-position
           #:donep
           #:valid-position-p
           #:at
           #:at=
           #:at-equal
           #:current-char
           #:current-char=
           #:current-char-equal
           #:next-char
           #:next-char=
           #:next-char-equal))

(in-package #:breeze.parser-state)


;;; Parser state

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

#++
(with-input-from-string (in "(hi :world)")
  (let* ((iterator (make-stream-iterator in))
         (state (make-instance 'state
                          :source (slot-value iterator 'vector)
                          :iterator iterator)))
    (value iterator)
    (breeze.parser::read-any state t)))

#++
(let ((*readtable* (copy-readtable nil)))
  (set-dispatch-macro-character
   #\# #\`
   #'(lambda (stream subchar arg)
       (declare (ignorable subchar arg))

       (let* ((iterator (make-stream-iterator stream))
              (state (make-instance 'state
                                    :source (slot-value iterator 'vector)
                                    :iterator iterator))
              (node (nth-value 1 (breeze.parser::read-any state t))))
         (setf (tree state) node)
         state)))
  (read-from-string "#`32"))

(defmethod print-object ((state state) stream)
  (print-unreadable-object
      (state stream :type t :identity nil)
    (let ((excerpt (breeze.string:around (source state)
                                         (current-position state))))
      (format stream "~s ~d/~d"
              excerpt
              (length excerpt)
              (length (source state))))))



;;; Content and range

(defun source-substring (state start end)
  "Get a (displaced) substring of the state's source string."
  (subseq (source state) start (and (plusp end) end)))


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
  (cond
    ((valid-position-p state position)
     (char (source state) position))
    ((and
      (typep (iterator state) 'stream-iterator)
      (= position (length (source state))))
     (unless (donep state)
       (breeze.iterator::read-next-char (iterator state))))))

(defun at= (state position char)
  "Compare the character at POSITION in the STATE's source with the parameter CHAR and returns the CHAR if they're char=.
Returns nil if POSITION is invalid."
  (when-let ((c (at state position)))
    (and (char= c char) c)))

(defun at-equal (state position char)
  "Compare the character at POSITION in the STATE's source with the parameter CHAR and returns the CHAR if they're char-equal.
Returns nil if POSITION is invalid."
  (when-let ((c (at state position)))
    (and (char-equal c char) c)))

(defun current-char (state)
  "Get the character at the current STATE's position, without changing
the position."
  (at state (current-position state)))

(defun current-char= (state char)
  "Get the character at the current STATE's position, without changing
the position."
  (at= state (current-position state) char))

(defun current-char-equal (state char)
  "Get the character at the current STATE's position, without changing
the position."
  (at-equal state (current-position state) char))

(defun next-char (state &optional (n 1))
  "Peek at the next character to be read, without changing the
position."
  (at state (+ n (current-position state))))

(defun next-char= (state char &optional (n 1))
  "Peek at the next character to be read, without changing the
position."
  (at= state (+ n (current-position state)) char))

(defun next-char-equal (state char &optional (n 1))
  "Peek at the next character to be read, without changing the
position."
  (at-equal state (+ n (current-position state)) char))
