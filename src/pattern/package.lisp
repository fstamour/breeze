(defpackage #:breeze.pattern
  (:documentation "Pattern matching")
  (:use #:cl #:breeze.iterator #:breeze.generics)
  (:import-from #:breeze.class-utils
                #:define-class)
  (:import-from #:breeze.string
                #:around
                #:symbol-starts-with)
  ;; re-exporting from breeze.generic
  (:export #:name
           #:named
           #:with-name)
  (:export #:compile-pattern)
  (:export #:pattern
           #:patterns
           #:match
           #:wildcard
           #:wildcardp
           #:simple-var
           #:svar
           #:multi-valued-p
           #:var
           #:varp
           #:name
           #:sym
           #:sym-package
           #:qualification
           #:wildp
           #:maybe
           #:zero-or-more
           #:one-or-more
           #:repetition
           #:repetitionp
           #:minimum
           #:maximum
           #:either
           #:eitherp
           #:slot-pattern
           #:slot-name
           #:object-pattern
           #:slots
           #:pattern-iterator
           #:make-pattern-iterator)
  ;; Working with match results
  (:export #:make-binding
           #:merge-substitutions
           #:find-binding
           #:binding
           #:multi-valued-binding
           #:add-binding
           #:from
           #:to
           #:substitutions
           #:substitutions-p
           #:merge-sets-of-substitutions
           #:pattern-substitute)
  (:export #:extract-names
           #:let-match)
  (:export #:make-rewrite
           #:rewrite-pattern
           #:rewrite-template))
