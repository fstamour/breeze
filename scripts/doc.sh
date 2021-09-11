#!/bin/sh
#
# This script is used to generate the documentation
#

cd "$(git rev-parse --show-toplevel)"

sbcl --non-interactive --eval '(ql:quickload :breeze)' --eval '(breeze.documentation::generate-documentation)'
