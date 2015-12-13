#!/usr/bin/env bash
set -eux

if [[ $1 == --debug ]]
then debug=true; shift
else debug=false
fi

dir=$(dirname $1)
filename=$(basename $1)
name=${filename%.*}

CFLAGS="-std=c99 -mcpu=arm1176jzf-s -mtune=arm1176jzf-s -D_GNU_SOURCE"
if $debug; then
    CFLAGS="$CFLAGS -g"
fi

RUNTIME="src/runtime/main.c src/runtime/list.c src/runtime/task.c src/runtime/network.c src/runtime/async.c src/runtime/heap.c"

./compile ${1}
arm-linux-gnueabi-gcc $CFLAGS \
  -o $name ${name}.s ${RUNTIME}

if $debug; then
    trap 'kill $(jobs -p)' EXIT
    qemu-arm -g 1234 -L /usr/arm-linux-gnueabi/ $name &
    arm-linux-gnueabihf-gdb \
        -ex "set sysroot /usr/arm-linux-gnueabi" \
        -ex "target remote localhost:1234" $name
else
    qemu-arm -L /usr/arm-linux-gnueabi/ $name
fi

