(in-package #:breeze.test.iterator)

(define-test+run stream-iterator
  (is equal '(#\H #\e #\l #\l #\o)
      (with-input-from-string (in "Hello")
        (collect (make-stream-iterator in))))
  ;; TODO test: is it donep after `collect'
  ;; TODO test: what happens if you call `value' once it's done
  (is equal "hulloooo"
      (with-input-from-string (in "hulloooo")
        (let ((it (make-stream-iterator in)))
          (collect it)
          (slot-value it 'vector))))
  ;; Empty stream — donep is true from the start.
  (true (with-input-from-string (in "")
          (donep (make-stream-iterator in)))))
