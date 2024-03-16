#!/bin/sh
#
# This script is used to run the tests
#

set -e

cd "$(git rev-parse --show-toplevel)"

exec sbcl --noinform --non-interactive \
     --eval "(declaim (optimize (debug 3) (speed 0) (safety 3)))" \
     --eval "(asdf:load-asd (truename \"breeze.asd\"))" \
     --eval "(ql:quickload '#:breeze/test)" \
     --eval "(breeze.test.main:run-breeze-tests t)"
