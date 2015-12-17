#!/usr/bin/env bash
set -eux

if [[ $1 == --debug ]]
then debug=true; shift
else debug=false
fi

if [[ $1 == --with-runtime ]]
then runtime=true; shift
else runtime=false
fi

if [[ $1 == --with-gc ]]
then gc=true; shift
else gc=false
fi



dir=$(dirname $1)
filename=$(basename $1)
name=${filename%.*}

CFLAGS="-std=c99 -mcpu=arm1176jzf-s -mtune=arm1176jzf-s -D_GNU_SOURCE"
if $debug; then
    CFLAGS="$CFLAGS -g"
fi

RUNTIME_LIB="runtime"
RUNTIME_DIR="src/runtime"
GC_LIB="waccgc"
GC_DIR="src/GC"
LIBS=""
COMPILE_FLAGS=""

if $runtime; then
  COMPILE_FLAGS="$COMPILE_FLAGS --with-runtime"
  LIBS="$LIBS -L${RUNTIME_DIR} -l${RUNTIME_LIB}"
fi

if $gc; then
  COMPILE_FLAGS="$COMPILE_FLAGS --with-gc"
  LIBS="$LIBS -L${GC_DIR} -l${GC_LIB}"
fi

./compile $COMPILE_FLAGS ${1} -o dist/${name}.s

arm-linux-gnueabi-gcc $CFLAGS dist/${name}.s \
  $LIBS -o dist/$name

if $debug; then
    trap 'kill $(jobs -p)' EXIT
    qemu-arm -g 1234 -L /usr/arm-linux-gnueabi/ dist/$name < ${2:-/dev/stdin} &
    arm-linux-gnueabihf-gdb \
        -ex "set sysroot /usr/arm-linux-gnueabi" \
        -ex "target remote localhost:1234" dist/$name
else
    qemu-arm -L /usr/arm-linux-gnueabi/ dist/$name < ${2:-/dev/stdin}
fi

