CFLAGS += -O -DMAIN
LDLIBS += -lncurses -lm

all:	 dcalc

dcalc: dcalc.o curse.o

clean:
	rm -f core *~ *.o dcalc
