#!/bin/sh
#
# This script is used to build the docker image used to record the
# demos
#

set -e
set -x

cd "$(git rev-parse --show-toplevel)"

if false; then
    # Build the base image with nix
    # nix build creates a tar file that is imported in docker.
    docker load < $(nix-build scripts/demo/docker.nix)

    docker build -f scripts/demo/dockerfile -t breeze-demo-recorder .
else
    # Build image on alpine
    docker build -f scripts/demo/alpine.dockerfile \
           -t breeze-demo-recorder-alpine \
           .
fi
