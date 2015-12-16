CC = arm-linux-gnueabi-gcc
CFLAGS = -std=c99 -Wall -mcpu=arm1176jzf-s -mtune=arm1176jzf-s -D_GNU_SOURCE

#OBJS =
#DEPS 

all: libruntime.a
libruntime.a: GC/GC.o GC/Page.o GC/PrintMethods.o async.o channel.o list.o main.o \
	network.o task.o heap.o
	ar rvs libruntime.a $^

%.o: %.c
	$(CC) -c $(CFLAGS) $< -o $@

.PHONY: all

