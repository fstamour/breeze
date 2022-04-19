#!/bin/sh
#
# This script is used to run the tests
#

cd "$(git rev-parse --show-toplevel)"

sbcl --non-interactive \
     --eval "(declaim (optimize (debug 3) (speed 0) (safety 3)))" \
     --eval "(ql:quickload '#:breeze/test)" \
     --eval "(asdf:test-system '#:breeze)"
# TODO make sure the exit-value is ok
