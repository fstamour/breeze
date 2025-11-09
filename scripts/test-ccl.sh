#!/bin/sh
#
# This script is used to run the tests with sbcl
#

set -e

cd $(dirname $0)/../

exec ccl --batch \
     --eval "(load \"scripts/run-tests.lisp\")"
