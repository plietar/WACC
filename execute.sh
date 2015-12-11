#!/usr/bin/env bash
set -eux

./compile $1/$2.wacc
arm-linux-gnueabi-gcc -o $2 -mcpu=arm1176jzf-s -mtune=arm1176jzf-s $2.s
qemu-arm -L /usr/arm-linux-gnueabi/ $2


