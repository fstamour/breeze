#|

I wrote this "stream" thingy long before the iterators, it was made
for and was part of the "egraph" code. It it used to enumerate the
terms of an egraph.

|#

(in-package #:breeze.iterator)


;;; Stream protocol

(defgeneric stream-get (stream))

(defgeneric stream-next (stream))

(defgeneric stream-done-p (stream))

(defun map-stream (fn stream &key (limit))
  (when stream
    (if limit
        (loop
          repeat limit
          until (stream-done-p stream)
          do (funcall fn (stream-get stream))
          do (stream-next stream))
        (loop
          until (stream-done-p stream)
          do (funcall fn (stream-get stream))
          do (stream-next stream)))))


;;; Stream constructors

(defun stream-sequence (seq)
  "Create a stream out of a sequence, the stream will repeat the sequence
infinitely, but will be considered \"done\" as soon as the iterator is
past the last element of the sequence."
  (let ((i 0)
        (l (length seq)))
    (if (zerop l)
        (lambda (&optional method)
          (ecase method
            ((nil) nil)
            (:next (progn (incf i) 0))
            (:done t)))
        (lambda (&optional method)
          (ecase method
            ((nil) (elt seq (mod i l)))
            (:next (progn (incf i)
                          (if (zerop l)
                              0
                              (zerop (mod i l)))))
            (:done (<= l i)))))))

(defun stream-constant (x)
  "Create a stream out of one value."
  (stream-sequence (vector x)))

(defmethod stream-get ((stream function))
  (funcall stream))

(defmethod stream-next ((stream function))
  (funcall stream :next))

(defmethod stream-done-p ((stream function))
  (funcall stream :done))

;; TODO filter-stream

(defun next-list-of-stream (streams)
  "Given a sequence of streams, advance the sequence as a whole."
  (etypecase streams
    (cons (loop
            for s in streams
            for nextp = (stream-next s)
            while nextp
            finally (return nextp)))
    (vector (loop
              for s across streams
              for nextp = (stream-next s)
              while nextp
              finally (return nextp)))))

(defun stream-product (streams)
  "Create a stream that produces the Cartesian product of all the STREAMS."
  (let ((donep nil)
        (streams (remove-if #'null streams)))
    (lambda (&optional method)
      (ecase method
        (:next (setf donep (next-list-of-stream streams)))
        (:done donep)
        ((nil) (map 'list 'stream-get streams))))))

(defun stream-sequence-product (sequences)
  "Create a stream that produces the Cartesian product of all the SEQUENCES."
  (stream-product
   (mapcar #'stream-sequence sequences)))

#++
(collect (stream-sequence-product '(#(a b) #(c d))))
;; => ((A C) (B C) (A D) (B D))

(defun stream-concat (streams)
  (if (zerop (length streams))
      (stream-sequence nil)
      (let* ((s (stream-sequence streams))
             (current (stream-get s)))
        (lambda (&optional method)
          (ecase method
            (:next (when (stream-next current)
                     (stream-next s)
                     (setf current (stream-get s))
                     (stream-done-p current)))
            (:done (or (stream-done-p current) (stream-done-p s)))
            ((nil) (stream-get current)))))))

#++
(collect
    (stream-concat (vector (stream-sequence '(a b)) (stream-constant 'c))))
;; => (A B C)

#++
(collect
    (stream-concat (vector (stream-sequence '()))))
;; => NIL


;;; Bridge: closure-based streams as iterators
;;
;; With these three methods, every stream-closure satisfies the iterator
;; protocol — so `collect' and `map-iterator' from interface.lisp work on
;; streams with no stream-specific helper.

(defmethod donep ((stream function))
  (stream-done-p stream))

(defmethod next ((stream function))
  (stream-next stream))

(defmethod value ((stream function))
  (stream-get stream))
