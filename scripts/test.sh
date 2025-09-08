#!/bin/sh
#
# This script is used to run the tests with sbcl
#

set -e

cd "$(git rev-parse --show-toplevel)"

# ASDF checks for warnings and errors when a file is compiled. The
# variables asdf:*compile-file-warnings-behaviour* and
# asdf:*compile-file-failure-behaviour* control the handling of any such
# events. The valid values for these variables are :error, :warn, and
# :ignore.

# TODO this loads stuff twice... because I used ql:quickload to ensure
# that the dependencies are downloaded before we compile again...

# TODO it would be nicer if the "lisp script" was in its own file

exec sbcl --noinform --non-interactive \
     --eval "(declaim (optimize (debug 3) (speed 0) (safety 3)))" \
     --eval "(asdf:load-asd (truename \"breeze.asd\"))" \
     --eval "(ql:quickload '#:breeze/test :verbose t)" \
     --eval "(setf asdf:*compile-file-warnings-behaviour* :error)" \
     --eval "(asdf:compile-system '#:breeze/test :force-not t :force t :verbose t)" \
     --eval "(breeze.test.main:run-breeze-tests :exitp t)"
