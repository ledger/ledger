CODE =  amount.cc ledger.cc parse.cc gnucash.cc balance.cc
ifndef LIBRARY
CODE := $(CODE) main.cc
endif

OBJS = $(patsubst %.cc,%.o,$(CODE))

CFLAGS = -Wall -ansi -pedantic
DFLAGS = -g
INCS   = -I/usr/include/xmltok
LIBS   = -lgmpxx -lgmp -lpcre -lxmlparse

ifdef LIBRARY

CFLAGS := $(CFLAGS) -fpic

all: make.deps libledger.so ledger

libledger.so: $(OBJS)
	g++ $(CFLAGS) $(INCS) $(DFLAGS) -shared -fpic -o $@ $(OBJS) $(LIBS)

ledger: main.cc
	g++ $(INCS) $(DFLAGS) -o $@ main.cc -L. -lledger

else # LIBRARY

all: make.deps ledger

ledger: $(OBJS)
	g++ $(CFLAGS) $(INCS) $(DFLAGS) -o $@ $(OBJS) $(LIBS)

endif # LIBRARY

%.o: %.cc
	g++ $(CFLAGS) $(INCS) $(DFLAGS) -c -o $@ $<

clean:
	rm -f libledger.so ledger *.o

rebuild: clean deps all

deps: make.deps

make.deps: Makefile
	cc -M $(INCS) $(CODE) main.cc > $@

include make.deps
