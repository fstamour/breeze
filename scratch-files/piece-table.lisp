
(defpackage #:breeze.piece-table
  (:documentation "A piece table data structure for efficient buffer editing")
  (:use #:cl)
  (:export piece
           piece-table
           make-piece-table
           write-piece-table
           write-piece-table-to-string))

(in-package #:breeze.piece-table)


#++
(defstruct (piece
            (:constructor piece (start length))
            :constructor
            (:predicate piecep))
  ;; TODI rename to "source"
  (string-index 0 :type (integer 0))
  (start 0 :type (integer 0))
  (length 0 :type (integer 0)))

(defclass piece ()
  ((source :type (integer 0)
           :initarg :source
           :initform 0
           :accessor source)
   (offset :type (integer 0)
          :initarg :offset
          :initform 0
          :accessor offset)
   (len :type (integer 0)
        :initarg :len
        :initform 0
        :accessor len)))

(defun piece (source offset len)
  (make-instance 'piece :source source :offset offset :len len))

(defmethod print-object ((piece piece) stream)
  (print-unreadable-object
      (piece stream :type t :identity nil)
    (format stream "src ~d offset ~d len ~d" (source piece) (offset piece) (len piece))))

(defclass piece-table ()
  ((strings
    :initform (make-array 0 :element-type 'string :adjustable t :fill-pointer 0)
    :accessor strings
    :documentation "Vector or strings, the first one is the original")
   (pieces
    :initform (make-array 0 :element-type 'piece :adjustable t :fill-pointer 0)
    :accessor pieces
    :documentation "Vector of pieces")))


(defmethod make-piece-table ((string string))
  (let ((pt (make-instance 'piece-table)))
    (vector-push-extend string (strings pt))
    (vector-push-extend (make-array 0
                                    :element-type 'character
                                    :adjustable t
                                    :fill-pointer 0)
                        (strings pt))
    (vector-push-extend (piece 0 0 (length string)) (pieces pt))
    ;; (push (piece 0 0 (length string)) (pieces pt))
    pt))


(declaim (inline number-of-strings))
(defun number-of-strings (pt)
  (length (strings pt)))

(declaim (inline nth-string))
(defun nth-string (pt nth)
  (aref (strings pt) nth))

(declaim (inline last-string))
(defun last-string (pt)
  (let* ((strings (strings pt)))
    (aref strings (1- (length strings)))))


(declaim (inline number-of-pieces))
(defun number-of-pieces (pt)
  (length (pieces pt)))

(declaim (inline nth-piece))
(defun nth-piece (pt nth)
  (aref (pieces pt) nth))

(declaim (inline last-piece))
(defun last-piece (pt)
  (let* ((pieces (pieces pt)))
    (aref pieces (1- (length pieces)))))

#++
(last-piece (make-piece-table "easdf"))

(defun write-piece-table (pt &optional (stream *standard-output*))
  (loop
    :for strings = (strings pt)
    :for piece :across (pieces pt)
    :for offset = (offset piece)
    :do (write-string (aref strings (source piece))
                      stream
                      :start offset
                      :end (+ offset (len piece)))))

(defun write-piece-table-to-string (pt)
  (with-output-to-string (out)
    (write-piece-table pt out)))

#++
(with-output-to-string (s)
  (write-piece-table (make-piece-table "hi") s))




;; for :across
(defun find-piece (pt pos)
  (loop
    :for i :from 0
    :for start = 0 :then end
    :for piece :across (pieces pt)
    :for end = (+ start (len piece))
    :when (<= start pos end)
      :return (values i start end)))

#++
(let ((pt (make-piece-table "hello")))
  (list
   (multiple-value-list (find-piece pt -1))
   (multiple-value-list (find-piece pt 0))
   (multiple-value-list (find-piece pt 5))
   (multiple-value-list (find-piece pt 7))))

(defun push-string (pt source string
                    &optional (start 0) (end (length string)))
  "Append STRING to to SOURCE-th strings in the piece-table PT.
Doesn't modify or add pieces.
Returns the offset of the start of the newly appended STRING."
  (let* ((buffer (aref (strings pt) 1))
         (offset (length buffer)))
    (with-output-to-string (out buffer)
      (write-string string out :start start :end end))
    offset))

(defun %append-text (pt string &optional (start 0))
  "Append STRING to PT, and create a new piece"
  (let* ((source 1))
    (vector-push-extend (piece source
                               (push-string pt 0 string start)
                               (length string))
                        (pieces pt))))

(defun %delete-end-of-last-piece (pt n)
  "Delete the last N character. Doesn't do any checks!"
  (let ((piece (last-piece pt)))
    (decf (len piece) n)
    (decf (fill-pointer (nth-string pt (source piece))) n)))

(defmacro without-fill-pointer ((s &optional
                                     (fill-pointer (gensym "fill-pointer"))
                                     (dimension (gensym "dimension")))
                                &body body)
  (alexandria:once-only (s)
    `(let ((,fill-pointer (fill-pointer ,s))
           (,dimension (array-dimension ,s 0)))
       (setf (fill-pointer ,s) ,dimension)
       (prog1
           (progn ,@body)
         (setf (fill-pointer ,s) ,fill-pointer)))))

(defun mismatch* (sequence-1 sequence-2
                  &key start2 end2 #| TODO from-ebd |#)
  "Like mismatch, but ignores the fill-pointer of sequence-2"
  (without-fill-pointer (sequence-2 fill-pointer dim)
    (let ((end2 (min end2 dim)))
      (mismatch sequence-1 sequence-2
                :start2 start2 :end2 end2)))
  #++
  (let* ((fill-pointer (fill-pointer sequence-2))
         (dim (array-dimension sequence-2 0))
         (end2 (min end2 dim)))
    (setf (fill-pointer sequence-2) dim)
    (let ((mismatch
            (mismatch sequence-1 sequence-2
                      :start2 start2 :end2 end2)))
      (setf (fill-pointer sequence-2) fill-pointer)
      mismatch)))


;; TODO replace piece by nth-piece (aka piece-index) that way, we can
;; use this function to append STRING in any pieces, regardless of its
;; position (right now, it assumes it's the last piece).
(defun append-text-to-piece (pt string piece)
  ;; Don't call this when (zerop (source piece))
  (let ((end (+ (offset piece) (len piece)))
        (buffer (nth-string pt (source piece))))
    (cond
      ;; there's no more text in the buffer
      ((= end (array-dimension buffer 0))
       (push-string pt (source piece) string)
       (incf (len piece) (length string)))
      (t
       (let ((mismatch
               (mismatch* string
                         buffer
                         :start2 end
                         :end2 (+ end (length string)))))
         (cond
           ((null mismatch)
            ;; the next part of the buffer match exactly
            (incf (len piece) (length string))
            (incf (fill-pointer buffer) (length string)))
           ((zerop mismatch)
            ;; there's no match at all: create a new piece
            (%append-text pt string))
           (t
            ;; there's some match, edit the existing piece and
            ;; possibly create a new one.
            #++
            (break "~s ~s ~s"
                   (+ (len piece) (offset piece))
                   (+ (len piece) (offset piece) mismatch)
                   (array-dimension buffer 0))
            (cond
              ;; if we're at the end
              ((= (+ (len piece) (offset piece) mismatch)
                  (array-dimension buffer 0))
               (incf (len piece) (length string))
               (incf (fill-pointer buffer) mismatch)
               ;; (break "~s ~s ~s" string mismatch (subseq string mismatch))
               (push-string pt (source piece) string mismatch))
              (t
               ;; otherwise, the current piece's end doesn't point to
               ;; the end of the buffer, but there's still some part
               ;; of STRING to append, a new piece must be created.
               (incf (len piece) (1- mismatch))
               (incf (fill-pointer buffer) (1- mismatch))
               (%append-text pt string mismatch))))))))))



(defmethod insert-text ((pt piece-table) string pos)
  (multiple-value-bind (rest start end)
      (find-piece pt pos)
    (declare (ignorable start end))
    (let* ((old-piece (car rest))
           (source (source old-piece))
           (output-string (aref (strings pt) 1))
           (offset (length output-string)))
      (with-output-to-string (out output-string)
        (write-string string out))
      ;; TODO this assumes we need to split the piece in 3
      (let* ((split-at (- pos (offset old-piece)))
             (piece-before (piece source (offset old-piece) split-at))
             (piece-after (piece source split-at (- (len old-piece)
                                                    split-at)))
             (new-piece (piece 1 offset (length string))))
        ;; TODO edit pt's pieces
        (values piece-before new-piece piece-after)))))

(let ((pt (make-piece-table "hello world")))
  (insert-text pt "small " 6))


TODO F11 => insert todo

TODO delete
TODO replace
