#!/bin/sh
#
# This script is used to generate the documentation
#

cd "$(git rev-parse --show-toplevel)"

mkdir -p public/

sbcl --noinform --non-interactive \
     --eval "(declaim (optimize (debug 3) (speed 0) (safety 3)))" \
     --eval "(asdf:load-asd (truename \"breeze.asd\"))" \
     --eval "(ql:quickload '#:breeze)" \
     --eval '(breeze.documentation::generate-documentation)'

cp docs/style.css public/
