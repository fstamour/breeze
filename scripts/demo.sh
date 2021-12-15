#!/usr/bin/env sh
#
# In another shell, run
# sbcl --load demo.lisp
#

# Stop on first error
set -e

# Move to repo's root
cd "$(git rev-parse --show-toplevel)"

demo_root=scripts/demo
mkdir -p scripts/demo

screencast="demo.svg"
command='emacs -nw -Q -l scripts/emacs-director/util/director-bootstrap.el -l scripts/demo.el -- asdf'
# ^^^ -- asdf is just an example of passing arguments (e.g. swank's
# port).

if true; then
    $command
else
    termtosvg "$screencast" -m 100 -c "$command"
    # firefox demo.svg
fi

tail $demo_root/demo.log
