#!/usr/bin/env bash
set -eux

dir=$(dirname $1)
filename=$(basename $1)
name=${filename%.*}

RUNTIME="src/runtime/main.c src/runtime/list.c src/runtime/task.c"

./compile ${1}
arm-linux-gnueabi-gcc -std=c99 -mcpu=arm1176jzf-s -mtune=arm1176jzf-s \
  -o $name ${name}.s ${RUNTIME}
time qemu-arm -L /usr/arm-linux-gnueabi/ $name


