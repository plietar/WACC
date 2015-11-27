#!/usr/bin/env bash

set -eux
TESTDIR=$1
BIN=$2

function runTest() {
    local dir=$1
    local expected=$2
    for f in $(find $dir -name '*.wacc' ); do
        echo Testing $f
        local ret=0
        $BIN $f || ret=$?
        [[ $ret -eq $expected ]] || (echo Failed; exit 1)
    done
}

runTest $TESTDIR/valid 0
runTest $TESTDIR/invalid/syntaxErr 100
runTest $TESTDIR/invalid/semanticErr 200

echo Done !

