#!/bin/sh

tiger_tar_url=https://www.cs.princeton.edu/~appel/modern/ml/tiger.tar

cd ${GITHUB_WORKSPACE}/chap4
echo pwd is $(pwd)

if ! [ -d testcases ]; then
    (curl -s ${tiger_tar_url} | tar xf - tiger/testcases) || exit 1
fi

[ -d tiger/testcases ] || exit 1

allfiles="tiger/testcases/*.tig"

smlout=$( (echo CM.make \"sources.cm\"\;
for f in "$allfiles"; do \
    echo Parse.parse \"$f\"\;
done ) | sml 2> error.log)

status=$?

echo ::set-output name=compile-out::"$smlout"
echo ::set-output name=compile-err::"$(cat error.log)"
exit $status
