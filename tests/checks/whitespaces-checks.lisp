
(in-package #:breeze.test.lint)

(define-test+run "warn-mixed-indentation and warn-mixed-non-indentation-whitespace"
  (is equal '((4 7 :warning "Mixed spaces and tabs in indentation."))
      (test-lint "(foo~% ~Cbar)" #\Tab)
      "should warn about \"space then tab\" indentation.")
  (is equal '((4 7 :warning "Mixed spaces and tabs in indentation."))
      (test-lint "(foo~%~C bar)" #\Tab)
      "should warn about \"tab then space\" indentation.")
  (is equal '((4 8 :warning "Mixed spaces and tabs in indentation."))
      (test-lint "(foo~% ~C bar)" #\Tab)
      "should warn about \"tab then space\" indentation.")
  (false (test-lint "(foo~%  bar)")
         "should not warn if indentation is made up of only spaces.")
  (false (test-lint "(foo~%~C~Cbar)" #\Tab #\Tab)
         "should not warn if indentation is made up of only tabs")
  (false (test-lint "(foo~%bar)")
         "should not warn if there is no indentation.")
  (false (test-lint "(foo~%  ~%  bar)"))
  (false (test-lint "(foo~%~%  bar)"))
  (is equal '((4 11 :warning "Mixed spaces and tabs in whitespace."))
      (test-lint "(foo~% ~C ~%  bar)" #\Tab)
      "should warn when mixing tabs and space (outside of indentation)."))


;; TODO BUG "(length #1#foo)" is automatically fixed to "(length foo)" instead of "(length #1# foo)"
