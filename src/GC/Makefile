CC = arm-linux-gnueabi-gcc
CFLAGS = -std=c99 -Wall -mcpu=arm1176jzf-s -mtune=arm1176jzf-s -D_GNU_SOURCE

OBJS = GC.o Page.o PrintMethods.o 
DEPS = $(OBJS:.o=.d)

all: libwaccgc.a
libwaccgc.a: $(OBJS)
	ar rs $@ $^

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ \
	    -MMD -MT $@ -MF $*.d -MP \
	    $<
clean: 
	rm -f *.d *.o libwaccgc.a

.PHONY: all clean

-include $(DEPS)
