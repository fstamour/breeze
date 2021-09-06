#!/bin/sh

rlwrap sbcl --non-interactive --eval '(ql:quickload :breeze)' --eval '(breeze.user:selftest)'
