#!/bin/sh
#
# This script is used to run the emacs lisp integration tests
#

set -e

cd $(dirname $0)/../

exec emacs --batch \
     --load emacs/breeze.el \
     --load emacs/tests/test-helpers.el \
     --load emacs/tests/integration-tests.el \
     -f ert-run-tests-batch-and-exit
