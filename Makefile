CODE   = amount.cc ledger.cc parse.cc reports.cc
OBJS   = $(patsubst %.cc,%.o,$(CODE))
CFLAGS = -Wall -ansi -pedantic
DFLAGS = -O3 -fomit-frame-pointer
#DFLAGS = -g -DDEBUG=1
INCS   =
LIBS   = -lgmpxx -lgmp -lpcre

ifdef GNUCASH
CODE   := $(CODE)   gnucash.cc
CFLAGS := $(CFLAGS) -DREAD_GNUCASH=1
INCS   := $(INCS)   -I/usr/include/xmltok
LIBS   := $(LIBS)   -lxmlparse
endif

all: make.deps ledger ledger.info

ledger: $(OBJS)
	g++ $(CFLAGS) $(INCS) $(DFLAGS) -o $@ $(OBJS) $(LIBS)

ledger.info: ledger.texi
	makeinfo $<

%.o: %.cc
	g++ $(CFLAGS) $(INCS) $(DFLAGS) -c -o $@ $<

clean:
	rm -f ledger ledger.info *.o *~ .\#*

rebuild: clean deps all

deps: make.deps

make.deps: Makefile
	cc -M $(INCS) $(CODE) > $@

include make.deps
