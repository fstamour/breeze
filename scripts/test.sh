#!/bin/sh
#
# This script is used to run the tests
#

cd "$(git rev-parse --show-toplevel)"

sbcl --non-interactive --eval '(ql:quickload :breeze)' --eval '(breeze.user:selftest)'
# TODO make sure the exit-value is ok
# TODO use (asdf:operate 'asdf:test-op '#:breeze)
