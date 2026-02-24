#!/bin/sh
#
# This script is used to generate the documentation
#

set -e

cd $(dirname $0)/../

# This runs _all_ the tests, some of them generates documentation.
sbcl --noinform --non-interactive \
     --eval "(load \"scripts/run-tests.lisp\")"

# "test-helpers.el" is loaded for the "remove-hook" (becauze vc/git/etc)

# This converts the org-mode file to html
emacs -Q --batch \
      --load emacs/tests/test-helpers.el \
      --load scripts/org-publish-project.el \
      --kill
