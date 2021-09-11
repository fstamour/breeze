#!/bin/sh
#
# This script is used to run the tests
#

cd "$(git rev-parse --show-toplevel)"

sbcl --non-interactive --eval '(ql:quickload :breeze)' --eval '(breeze.user:selftest)'
