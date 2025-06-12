#!/bin/sh
#
# This script is used to compile breeze and its tests in order to show
# compilation errors and warnings
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

set -x
exec sbcl --noinform --non-interactive \
     --eval "(asdf:load-asd (truename \"breeze.asd\"))" \
     --eval "(ql:quickload '#:breeze/test :verbose t :explain t)" \
     --eval "(setf asdf:*compile-file-warnings-behaviour* :error)" \
     --eval "(asdf:compile-system '#:breeze/test :force t :verbose t)"
