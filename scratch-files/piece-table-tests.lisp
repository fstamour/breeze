(defpackage #:breeze.tests.piece-table
  (:documentation "Test package for breeze.piece-table")
  (:use #:cl #:breeze.piece-table)
  (:import-from #:breeze.piece-table
                #:find-piece
                #:strings
                #:pieces
                #:offset
                #:start
                #:len
                #:number-of-strings
                #:nth-string
                #:number-of-pieces
                #:nth-piece
                #:last-piece
                #:push-string
                #:%append-text
                #:%delete-end-of-last-piece
                #:append-text-to-piece
                #:without-fill-pointer)
  (:import-from #:parachute
                #:finish
                #:define-test
                #:define-test+run
                #:is
                #:true
                #:false
                #:of-type))

(in-package #:breeze.tests.piece-table)

(defun check-piece-table (description
                          pt
                          expected-number-of-strings
                          expected-number-of-pieces
                          expected-string)
  (finish
   (progn
     (is = expected-number-of-pieces (number-of-pieces pt)
         "~a: should have ~s pieces got ~s"
         description expected-number-of-pieces (number-of-pieces pt))
     (is = expected-number-of-strings (number-of-strings pt)
         "~a: should have ~s strings got ~s"
         description expected-number-of-strings (number-of-strings pt))
     (let ((string (write-piece-table-to-string pt)))
       (is string= expected-string string
           "~a: should serialize to ~s got ~s instead"
           description expected-string string)))))

(define-test+run base
  (check-piece-table
   "hello"
   (make-piece-table "hello")
   2 1 "hello")
  (finish
   (let ((pt (make-piece-table "hello")))
     (%append-text pt " beautiful")
     (%append-text pt " world!")
     (check-piece-table
      "hello + %append x2"
      pt
      2 3 "hello beautiful world!"))))

(defun subseq-after-fill-pointer (s)
  (without-fill-pointer (s fill-pointer dimension)
    (subseq s fill-pointer dimension)))

(defun check-string (description s expected-before-fill-pointer
                     &optional expected-after-fill-pointer)
  (of-type string s "~a" description)
  (is string= expected-before-fill-pointer s
       "~a: the string was expected to be ~s got ~s"
       description expected-before-fill-pointer s)
  (when expected-after-fill-pointer
    (is string= expected-after-fill-pointer
        (subseq-after-fill-pointer s)
        "~a: TODO better description" description)))

(defun check-nth-string (description pt nth expected-before-fill-pointer
                         &optional expected-after-fill-pointer)
  (check-string description
                (nth-string pt nth)
                expected-before-fill-pointer
                expected-after-fill-pointer))

(define-test+run append-text-to-piece
  ;; Full match, perfect fit
  (finish
   (let* ((pt (make-piece-table "hello ")))
     (%append-text pt "aaaa")
     (check-piece-table
      "after appending \"aaa\""
      pt 2 2 "hello aaaa")
     (%delete-end-of-last-piece pt 2)
     (check-piece-table
      "after deleting the last 2 chars"
      pt 2 2 "hello aa")
     (check-nth-string
      "after deleting the last 2 chars"
      pt 1 "aa" "aa")
     (append-text-to-piece pt "aa" (last-piece pt))
     (check-piece-table
      "after appending \"aa\", which is exactly what just got deleted."
      pt 2 2 "hello aaaa")
     (check-nth-string
      "after appending \"aa\", which is exactly what just got deleted."
      pt 1 "aaaa" "")))
  ;; Full match, but the new string is longer than the end of the
  ;; buffer.
  (finish
   (let* ((pt (make-piece-table "hello ")))
     (%append-text pt "aaa")
     (check-piece-table
      "after appending \"aaa\""
      pt 2 2 "hello aaa")
     (%delete-end-of-last-piece pt 2)
     (check-nth-string
      "after deleting the last 2 chars"
      pt 1 "a" "aa")
     (append-text-to-piece pt "aabb" (last-piece pt))
     (check-piece-table
      "after appending \"aabb\""
      pt 2 2 "hello aaabb")
     (check-nth-string
      "after appending \"aabb\""
      pt 1 "aaabb"52))))
;; should be "hello aaabb" (with 3 "a"s)

;; Partial match
(let* ((pt (make-piece-table "hello ")))
  (%append-text pt "aaaa")
  (let ((piece (last-piece pt)))
    ;; Delete the last 2 chars
    (decf (len piece) 2)
    (decf (fill-pointer (nth-string pt 1)) 2)
    (append-text-to-piece pt "ab" piece))
  (write-piece-table-to-string pt))
;; => "hello aaab"

;; No match
(let* ((pt (make-piece-table "hello ")))
  (%append-text pt "aaaa")
  (let ((piece (last-piece pt)))
    ;; Delete the last 2 chars
    (decf (len piece) 2)
    (decf (fill-pointer (nth-string pt 1)) 2)
    (append-text-to-piece pt "bb" piece))
  (write-piece-table-to-string pt))
;; => "hello aabb"
;; piece was reused (edited length)
;; buffer was edited (not extended in this case)

;; Must extend
(let* ((pt (make-piece-table "hello ")))
  (%append-text pt "aaaa")
  (let ((piece (last-piece pt)))
    (append-text-to-piece pt "aa" piece))
  (write-piece-table-to-string pt))
;; => "hello aaaaaa"
