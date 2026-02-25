#!/bin/sh
#
# This script is used to generate or update part of the documentation
# (under docs/) using common lisp code. The rest of the documentation
# is generated using org-mode publishing system (into public/).
#
# This script (update-doc.sh) should be run before org-publish-project.sh
#

set -e

cd "$(git rev-parse --show-toplevel)"

mkdir -p public/

# This runs _all_ the tests, some of which generate or update files in docs/.
# TODO run only the relevant tests (currently in package #:breeze.test.documentation)
sbcl --noinform --non-interactive \
    --eval "(load \"scripts/run-tests.lisp\")"

cp docs/style.css public/
