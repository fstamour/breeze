#!/usr/bin/env bash

# Stop on first error
set -e

# Move to repo's root
cd "$(git rev-parse --show-toplevel)"

output=scripts/demo/output
mkdir -p $output

for image in $(docker exec breeze-demo-recorder sh -c 'ls /breeze/scripts/demo/*.png'); do
    name=$(basename $image)
    docker cp breeze-demo-recorder:$image $output/$name
done
