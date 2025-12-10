#!/bin/sh
#
# This script is used to generate the documentation
#

set -e

cd "$(git rev-parse --show-toplevel)"

mkdir -p public/

sbcl --noinform --non-interactive \
     --eval "(declaim (optimize (debug 3) (speed 0) (safety 3)))" \
     --eval "(asdf:load-asd (truename \"breeze.asd\"))" \
     --eval "(ql:quickload '#:breeze/doc)" \
     --eval "(ql:quickload '#:breeze/dogfood)" \
     --eval '(breeze.dogfood:generate-breeze-reference)'

cp docs/style.css public/
