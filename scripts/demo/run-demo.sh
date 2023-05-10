#!/usr/bin/env bash

set -euo pipefail

cat <<EOF |
FROM breeze-demo-recorder as demo
RUN scripts/demo/demo-recorder.sh run

FROM scratch
COPY --from=demo /breeze/scripts/demo/* /
EOF
DOCKER_BUILDKIT=1 docker build -f - --output type=tar,dest=demo.tar .
