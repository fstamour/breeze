#!/usr/bin/env bash
#
# This script is used to build the docker image used to record the
# demos
#

set -euo pipefail
set -x

cd "$(git rev-parse --show-toplevel)"

docker_args=(
    build
    -f scripts/demo/alpine.dockerfile
    "$@"
)

if [ $# = 0 ]; then
    docker_args+=(
        --output type=tar,dest=demo.tar
    )
fi

docker_args+=( . )

DOCKER_BUILDKIT=1 docker "${docker_args[@]}"
