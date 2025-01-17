(in-package #:common-lisp-user)

(uiop:define-package #:breeze.test.string
    (:documentation "Tests for breeze.test.")
  (:mix #:cl #:alexandria #:breeze.string)
  (:import-from #:parachute
                #:define-test
                #:define-test+run
                #:is
                #:true
                #:false))

(in-package #:breeze.test.string)

(define-test optimal-string-alignment-distance)
(define-test indent-string)
(define-test print-comparison)

(define-test+run around
  ;; Not the best test, but it'll do.
  (is equalp
      '("abcdefg..."
        "abcdefg..."
        "abcdefg..."
        "abcdefg..."
        ".bcdefgh..."
        "..cdefghi..."
        "...defghij..."
        "...efghijk.."
        "...fghijkl."
        "...ghijklm"
        "...ghijklm"
        "...ghijklm"
        "...ghijklm"
        "...ghijklm")
      (loop :with string = "abcdefghijklm"
            :for i :upto (length string)
            :collect (around string i 3))))

#+(or)
(optimal-string-alignment-distance*
 "breeze.util"
 "breeze.utils"
 3)
