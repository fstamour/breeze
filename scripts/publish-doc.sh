#!/bin/sh
#
# Script to upload documentation to s3, effectively publishing it.
#

set -e

if [ "$(git rev-parse --abbrev-ref HEAD)" == "master" ]; then
    target=""
else
    target="dev/"
fi


cd "$(git rev-parse --show-toplevel)/docs"

files="index.html \
style.css"

for file in $files; do
    echo "Checking if \"$file\" exists..."
    test -f $file
done

for file in $files; do
    aws s3 cp --acl public-read $file s3://www.fstamour.com/breeze/$target
done
