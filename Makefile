define GNUCASH
true
endef

CODE =  amount.cc ledger.cc parse.cc \
	balance.cc register.cc equity.cc main.cc
ifdef GNUCASH
CODE := $(CODE) gnucash.cc
endif

OBJS = $(patsubst %.cc,%.o,$(CODE))

CFLAGS = -Wall -ansi -pedantic -DDEFAULT_COMMODITY="\"\$$\"" -DHUQUQULLAH=1
#DFLAGS = -O3 -fomit-frame-pointer
DFLAGS = -g # -pg
INCS   = -I/usr/include/xmltok
LIBS   = -lgmpxx -lgmp -lpcre
ifdef GNUCASH
LIBS  := $(LIBS) -lxmlparse
endif

all: make.deps ledger

ledger: $(OBJS)
	g++ $(CFLAGS) $(INCS) $(DFLAGS) -o $@ $(OBJS) $(LIBS)

%.o: %.cc
	g++ $(CFLAGS) $(INCS) $(DFLAGS) -c -o $@ $<

clean:
	rm -f libledger.so ledger *.o

rebuild: clean deps all

deps: make.deps

make.deps: Makefile
	cc -M $(INCS) $(CODE) > $@

include make.deps
