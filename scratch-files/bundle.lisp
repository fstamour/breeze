;;; Trying to bundle breeze's source code in one big file
#|

See also: remote-loading.el and remote-loading.lisp

Which makes me think: it would be nice to have some kind
to "transactions" when loading common lisp code. To avoid half-loaded
code.

EDIT before I commit: I didn't have much time and didn't get far. BUT,
I managed to figure out that (surprise surprise), the dependencies are
hard to handle correctly...

|#

;; Making sure the breeze system can be found.
(asdf:locate-system "breeze")

(defparameter *concat*
  (multiple-value-list
   (asdf:operate 'asdf:concatenate-source-op "breeze")))

(defparameter *bundle*
  (multiple-value-list
   (asdf:perform 'asdf:concatenate-source-op "breeze")))

(first *bundle*)
#P"/home/fstamour/.cache/common-lisp/sbcl-2.4.0-linux-x64/home/fstamour/dev/breeze/src/breeze--system.lisp"

(load (first *bundle*))
