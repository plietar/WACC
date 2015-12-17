CC = arm-linux-gnueabi-gcc
CFLAGS = -std=c99 -Wall -mcpu=arm1176jzf-s -mtune=arm1176jzf-s -D_GNU_SOURCE

OBJS = async.o channel.o list.o main.o \
       network.o task.o heap.o utils.o
DEPS = $(OBJS:.o=.d)

all: libruntime.a
libruntime.a: $(OBJS)
	ar rs $@ $^

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ \
	    -MMD -MT $@ -MF $*.d -MP \
	    $<
clean:
	rm -f *.d *.o libruntime.a

.PHONY: all clean

-include $(DEPS)
