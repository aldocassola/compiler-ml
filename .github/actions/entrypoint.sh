#!/bin/sh

cd compiler-ml/chap4

smlout=$(smlnj -m sources.cm)
status=$?

echo ::set-output name=compile-out::$smlout

exit $status
