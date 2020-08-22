#!/bin/sh

rlwrap sbcl --non-interactive --eval '(ql:quickload :breeze)' --eval '(load "tests/selftest.lisp")' --eval '(breeze.selftest:selftest)'
