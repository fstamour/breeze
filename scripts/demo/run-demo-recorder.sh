#!/bin/sh

docker run -it --rm \
       -v "$(git rev-parse --show-toplevel)":/breeze \
       breeze-demo-recorder
