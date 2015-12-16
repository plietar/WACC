CC = arm-linux-gnueabi-gcc
CFLAGS = -std=c99 -Wall -mcpu=arm1176jzf-s -mtune=arm1176jzf-s -D_GNU_SOURCE

OBJS = GC/GC.o GC/Page.o GC/PrintMethods.o async.o channel.o list.o main.o \
       network.o task.o heap.o
DEPS = $(OBJS:.o=.d)

all: libruntime.a
libruntime.a: $(OBJS)
	ar rs $@ $^

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ \
	    -MMD -MT $@ -MF $*.d -MP \
	    $<

.PHONY: all clean

-include $(DEPS)
