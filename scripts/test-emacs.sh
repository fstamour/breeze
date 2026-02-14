#!/bin/sh
#
# This script is used to run the emacs lisp unit tests
#

set -e

cd $(dirname $0)/../

exec emacs --batch \
     --load emacs/breeze.el \
     --load emacs/tests/test-helpers.el \
     --load emacs/tests/no-listener.el \
     --load emacs/tests/unit-tests.el \
     -f ert-run-tests-batch-and-exit
