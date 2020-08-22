#!/bin/sh

# stop on first error
set -e

quicklisp init

./test.sh
