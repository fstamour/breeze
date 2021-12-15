#!/usr/bin/env sh

# Stop on first error
set -e

# Move to repo's root
cd "$(git rev-parse --show-toplevel)"

termtosvg demo.svg -c 'emacs -nw -Q -l scripts/emacs-director/util/director-bootstrap.el -l scripts/demo.el'
# firefox demo.svg

tail demo.log
