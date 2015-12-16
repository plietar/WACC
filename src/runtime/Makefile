CC = arm-linux-gnueabi-gcc
CFLAGS = -c -std=c99 -Wall -mcpu=arm1176jzf-s -mtune=arm1176jzf-s -D_GNU_SOURCE

all: libruntime.a
libruntime.a: GC.o Page.o PrintMethods.o async.o channel.o list.o main.o \
	network.o task.o
	ar rvs libruntime.a $^

GC.o:	GC/GC.c GC/GC.h
	$(CC) $(CFLAGS) $< -o $@
Page.o: GC/Page.c GC/Page.h
	$(CC) $(CFLAGS) $< -o $@
PrintMethods.o: GC/PrintMethods.c GC/PrintMethods.h
	$(CC) $(CFLAGS) $< -o $@
async.o: async.c async.h
	$(CC) $(CFLAGS) $< -o $@
channel.o: channel.c channel.h
	$(CC) $(CFLAGS) $< -o $@
list.o: list.c list.h
	$(CC) $(CFLAGS) $< -o $@
main.o : main.c
	$(CC) $(CFLAGS) $< -o $@
network.o: network.c network.h
	$(CC)  $(CFLAGS) $< -o $@
task.o: task.c task.h
	$(CC) $(CFLAGS) $< -o $@

.PHONY: all

