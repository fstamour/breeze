(in-package #:breeze.iterator)

(defclass stream-iterator (vector-iterator)
  ((stream
    :initform nil
    :initarg :stream
    :documentation "The source stream."))
  ;; Wordsoup!
  (:documentation "An adapter iterator for streams."))


(defun make-stream-iterator
    (stream &optional (buffer (make-array
                               '(0)
                               :element-type 'character
                               :adjustable t
                               :fill-pointer t)))
  (check-type stream stream)
  (assert (input-stream-p stream) (stream))
  (check-type buffer (vector character))
  (assert (adjustable-array-p buffer) (buffer))
  (make-instance 'stream-iterator
                 :stream stream
                 :vector buffer))

(defmethod donep ((iterator stream-iterator))
  (with-slots (position vector stream) iterator
    (if (< -1 position (length vector))
        nil
        (not (peek-char nil stream nil)))))

#+sbcl (declaim (sb-ext:maybe-inline read-next-char))
(defun read-next-char (stream-iterator)
  (with-slots (vector stream) stream-iterator
    (let ((char (read-char stream)))
      (vector-push-extend char vector)
      char)))

(defmethod value ((iterator stream-iterator))
  (declare (inline read-next-char))
  (with-slots (position vector stream) iterator
    (if (< -1 position (length vector))
        (aref vector position)
        (read-next-char iterator))))
