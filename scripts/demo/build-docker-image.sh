#!/bin/sh
#
# This script is used to build the docker image used to record the
# demos
#

set -e
set -x

cd "$(git rev-parse --show-toplevel)"

docker build -f scripts/demo/alpine.dockerfile \
       -t breeze-demo-recorder \
       "$@" \
       .
