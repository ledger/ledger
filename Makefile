CODE   = amount.cc ledger.cc parse.cc reports.cc
OBJS   = $(patsubst %.cc,%.o,$(CODE))
#CXX    = cc
CXX    = g++
CFLAGS = #-Wall -ansi -pedantic
DFLAGS = -O3 -fomit-frame-pointer
#DFLAGS = -g -DDEBUG=1
INCS   = -I/sw/include -I/usr/include/gcc/darwin/3.3/c++ -I/usr/include/gcc/darwin/3.3/c++/ppc-darwin
LIBS   = -L/sw/lib -lgmpxx -lgmp -lpcre

ifdef GNUCASH
CODE   := $(CODE)   gnucash.cc
CFLAGS := $(CFLAGS) -DREAD_GNUCASH=1
INCS   := $(INCS)   -I/usr/include/xmltok
LIBS   := $(LIBS)   -lxmlparse
endif

all: make.deps ledger ledger.info

install: all
	strip ledger
	cp ledger $(HOME)/bin

ledger: $(OBJS)
	$(CXX) $(CFLAGS) $(INCS) $(DFLAGS) -o $@ $(OBJS) $(LIBS)

ledger.info: ledger.texi
	makeinfo $<

%.o: %.cc
	$(CXX) $(CFLAGS) $(INCS) $(DFLAGS) -c -o $@ $<

clean:
	rm -f ledger *.o *.elc *~ .\#*

distclean fullclean: clean
	rm -f ledger.info README.html README.pdf *.elc make.deps

rebuild: clean deps all

deps: make.deps

make.deps: Makefile
	cc -M $(INCS) $(CODE) > $@

include make.deps
