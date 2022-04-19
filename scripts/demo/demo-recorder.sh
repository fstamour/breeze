#!/bin/sh
#
# This script is meant to be run as the main script _inside docker_
#


# sbcl --noinform --load breeze.asd \
#      --eval "(ql:quickload '(#:breeze #:swank))" \
#      --eval "(swank:create-server :dont-close t :port 40050)" \
#      &

# xvfb-run emacs -Q \
# 	 -l scripts/emacs-director/util/director-bootstrap.el \
# 	 -l scripts/demo.el -- &

export DISPLAY=:99

# -s "-screen 0 1280x800x32"

emacs -nw \
	 -l scripts/emacs-director/util/director-bootstrap.el \
	 -l scripts/demo.el

bash
