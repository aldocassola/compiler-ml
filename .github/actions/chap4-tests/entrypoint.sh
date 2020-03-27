#!/bin/sh

tiger_tar_url=https://www.cs.princeton.edu/~appel/modern/ml/tiger.tar
expected_fail=testcases/test49.tig

cd ${GITHUB_WORKSPACE}/chap4

if ! [ -d testcases ]; then
    (curl -s ${tiger_tar_url} | tar xf - tiger/testcases) || exit 1
fi

[ -d tiger/testcases ] || exit 1

allfiles="tiger/testcases/*.tig"

smlout=$((echo CM.make \"sources.cm\"\;
    echo map Parse.parse \[
    for f in $allfiles;
    do
        echo -n \"$f\",
    done | sed 's/,$//'; echo \]\;) | sml 2> error.log)

status=$?

echo ::set-output name=compile-out::"$smlout"
#echo ::set-output name=compile-err::"$(grep -v ${expected_fail} error.log)"
exit $status
