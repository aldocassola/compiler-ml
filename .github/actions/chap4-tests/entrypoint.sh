#!/bin/sh

echo pwd is $(pwd)
cd chap4

smlout=$(sml -m sources.cm)
status=$?

echo ::set-output name=compile-out::$smlout

exit $status
