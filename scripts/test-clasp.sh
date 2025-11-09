#!/bin/sh
#
# This script is used to run the tests with sbcl
#

set -e

cd "$(git rev-parse --show-toplevel)"

exec clasp --non-interactive  \
     --eval "(load \"scripts/run-tests.lisp\")"
