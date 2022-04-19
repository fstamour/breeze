#!/usr/bin/env sh
#
# This is an example of how to take a bunch of capture made with tmux
# and assemble them with termtosvg
#
# usage: termtosvg output.svg -c ./animate.sh
#    or: termtosvg $demo_root -s -c ./animate.sh
#

# Stop on first error
set -e

# Move to repo's root
cd "$(git rev-parse --show-toplevel)"

demo_root=scripts/demo/

if [ ! -d "$demo_root" ]; then
    echo "Demo folder doesn't exits"
    exit 1
fi

for capture in $(echo $demo_root/*.capture | sort -n) ; do
    # echo "$capture"
    cat "$capture"
    sleep 0.5
done
