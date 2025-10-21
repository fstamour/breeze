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

(define-test optimal-string-alignment-distance*)

(define-test+run repeat-string)

(define-test+run split-by-newline)

(define-test indent-string)

(define-test print-comparison)

(define-test+run remove-parentheses)

(define-test+run summarize)

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

(define-test+run whitespacesp)

(define-test+run trim-whitespace)

(define-test+run symbol-package-qualified-name
  (is equal "COMMON-LISP:NULL" (symbol-package-qualified-name 'null)))

(define-test+run symbol-starts-with
  (progn
    (false (symbol-starts-with 'x #\?))
    (true (symbol-starts-with :? #\?))
    (true (symbol-starts-with :?x #\?))
    (true (symbol-starts-with '? #\?))
    (true (symbol-starts-with '?x #\?)))
  (progn
    (false (symbol-starts-with 'x "?"))
    (true (symbol-starts-with :? "?"))
    (true (symbol-starts-with :?x "?"))
    (true (symbol-starts-with '? "?"))
    (true (symbol-starts-with '?x "?"))))

(define-test+run without-prefix
  (is equal "a" (without-prefix "" "a"))
  (is equal "" (without-prefix "a" "a"))
  (is equal "quickfix" (without-prefix "breeze-" "breeze-quickfix"))
  (is equal "asdf" (without-prefix #\s "asdf"))
  (is equal "sdf" (without-prefix #\a "asdf")))

(define-test+run ensure-prefix
  (is equal "a" (ensure-prefix "" "a"))
  (is equal "*" (ensure-prefix "*" ""))
  (is equal "*a" (ensure-prefix "*" "a"))
  (is equal "*a" (ensure-prefix "*" "*a"))
  (is equal "*a*" (ensure-prefix "*" "a*"))
  (is equal "a" (ensure-prefix nil "a"))
  (is equal "*a" (ensure-prefix #\* "a")))

(define-test+run ensure-prefixes
  (is equal "a" (ensure-prefixes (list) "a"))
  (is equal "*a" (ensure-prefixes "*" "a"))
  (is equal "*a" (ensure-prefixes #\* "a"))
  (is equal "(setf " (ensure-prefixes (list "(" "setf" " ") ""))
  (is equal "(setf a)" (ensure-prefixes (list "(" "setf" " ") "(setf a)"))
  (is equal "(setf a)" (ensure-prefixes (list "(" "setf" " ") "setf a)"))
  (is equal "(setf a)" (ensure-prefixes (list "(" "setf" " ") "(a)"))
  (is equal "(setf a)" (ensure-prefixes (list "(" "setf" " ") "( a)"))
  (is equal "(setf a)" (ensure-prefixes (list "(" "setf" " ") " a)"))
  (is equal "(setf a)" (ensure-prefixes (list "(" "setf" " ") "a)"))
  (is equal "(setf a)" (ensure-prefixes (list "(" "setf" " ") (ensure-suffix ")" "a")))
  (is equal "(setf a)" (ensure-prefixes (list "(" "setf" " ") (ensure-suffix ")" "a)"))))

(define-test+run ensure-suffix
  (is equal "a" (ensure-suffix "" "a"))
  (is equal "*" (ensure-suffix "*" ""))
  (is equal "a*" (ensure-suffix "*" "a"))
  (is equal "a*" (ensure-suffix "*" "a*"))
  (is equal "*a*" (ensure-suffix "*" "*a")))

(define-test+run ensure-suffixes
  (is equal "a" (ensure-suffixes (list) "a"))
  (is equal "a>" (ensure-suffixes ">" "a"))
  (is equal " a)" (ensure-suffixes (list " " "a" ")") ""))
  (is equal "(setf a)" (ensure-suffixes (list " " "a" ")") "(setf a)"))
  (is equal "(setf a)" (ensure-suffixes (list " " "a" ")") "(setf a"))
  (is equal "(setf a)" (ensure-suffixes (list " " "a" ")") "(setf "))
  (is equal "(setf a)" (ensure-suffixes (list " " "a" ")") "(setf")))

(define-test+run ensure-circumfix
  (is equal "*" (ensure-circumfix "*" "")) ; Not sure if this should be the expected behaviour
  (is equal "*a*" (ensure-circumfix "*" "a"))
  (is equal "*a*" (ensure-circumfix "*" "*a"))
  (is equal "*a*" (ensure-circumfix "*" "a*"))
  (is equal "*a*" (ensure-circumfix "*" "*a*"))
  (is equal "(a)" (ensure-circumfix "(" "a" ")"))
  (is equal "(a)" (ensure-circumfix "(" "(a" ")"))
  (is equal "(a)" (ensure-circumfix "(" "a)" ")"))
  (is equal "(a)" (ensure-circumfix "(" "(a)" ")")))

(define-test+run ensure-circumfixes
  (is equal "" (ensure-circumfixes nil "" nil))
  (is equal "a" (ensure-circumfixes nil "a" nil))
  (is equal "(a)" (ensure-circumfixes "(" "a" ")"))
  (is equal "(setf )" (ensure-circumfixes '("(" "setf" " ") "" '(")")))
  (is equal "(setf a)" (ensure-circumfixes '("(" "setf" " ") "a" '(")")))
  (is equal "(setf a)" (ensure-circumfixes '("(" "setf" " ") "setf a" '(")")))
  (is equal "(setf a)" (ensure-circumfixes '("(" "setf" " ") "(setf a" '(")")))
  (is equal "(setf a)" (ensure-circumfixes '("(" "setf" " ") "(setf a)" '(")")))
  (is equal "(setf a)" (ensure-circumfixes '("(" "setf" " ") "setf a)" '(")")))
  (is equal "(setf a)" (ensure-circumfixes '("(" "setf" " ") " a)" '(")")))
  (is equal "(setf a)" (ensure-circumfixes '("(" "setf" " ") "a)" '(")"))))
