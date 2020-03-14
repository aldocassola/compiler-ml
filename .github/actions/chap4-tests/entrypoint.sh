#!/bin/sh

cd ${GITHUB_WORKSPACE}/chap4
echo pwd is $(pwd)

# smlout=$(sml -m sources.cm)
status=$?

echo ::set-output name=compile-out::$(id; whoami; ls -la)
exit $status
