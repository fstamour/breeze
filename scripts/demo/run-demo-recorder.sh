#!/bin/sh
       `#-v "$(git rev-parse --show-toplevel)":/breeze` \
docker run -it --rm breeze-demo-recorder
