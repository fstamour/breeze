;;; Work in progress
#|

Goal: be able to load breeze into a remote lisp image

Motivation: making breeze easier to use in more contexts

Expected issue: Loading breeze migth be easy... if we send the source
code through sly/slime's listener. But it probably won't be easy to do
so for all the dependencies...

Also, I already tried asdf's "bundle" feature... It's not great,
especially for third-party dependencies.

From a high-level point of view, I can think of 3 major things to
figure out:

- how to load _one_ file
- how to load a system
- which systems to load?

|#

(defpackage #:breeze.remote-loading
  (:documentation "Helper code to load breeze into a lisp image that doesn't have
breeze's code locally (i.e. a remote image).")
  (:use #:cl))

(in-package #:breeze.remote-loading)

#++ (progn
      (asdf:already-loaded-systems)
      (asdf:system-defsystem-depends-on)

      (defclass fake-load-op (asdf:load-op)
        ())

      (asdf:))
