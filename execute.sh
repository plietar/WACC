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

#RUNTIME="src/GC/GC.c src/GC/Page.c src/GC/PrintMethods.c src/runtime/main.c "
#RUNTIME+="src/runtime/list.c src/runtime/task.c src/runtime/network.c "
#RUNTIME+="src/runtime/async.c src/runtime/heap.c src/runtime/channel.c "
RUNTIME="runtime"
RUNTIME_DIR="src/runtime"

./compile --with-runtime ${1} -o dist/${name}.s
arm-linux-gnueabi-gcc $CFLAGS dist/${name}.s \
  -L${RUNTIME_DIR} -l${RUNTIME} -o dist/$name

if $debug; then
    trap 'kill $(jobs -p)' EXIT
    qemu-arm -g 1234 -L /usr/arm-linux-gnueabi/ dist/$name < ${2:-/dev/stdin} &
    arm-linux-gnueabihf-gdb \
        -ex "set sysroot /usr/arm-linux-gnueabi" \
        -ex "target remote localhost:1234" dist/$name
else
    qemu-arm -L /usr/arm-linux-gnueabi/ dist/$name < ${2:-/dev/stdin}
fi

