define GNUCASH
true
endef
define HUQUQ
true
endef

CODE =  amount.cc   \
	ledger.cc   \
	parse.cc    \
	balance.cc  \
	register.cc \
	equity.cc   \
	main.cc

OBJS = $(patsubst %.cc,%.o,$(CODE))

CFLAGS = -Wall -ansi -pedantic
#DFLAGS = -O3 -fomit-frame-pointer
DFLAGS = -g # -O2 # -pg
INCS   = -I/usr/include/xmltok
LIBS   = -lgmpxx -lgmp -lpcre

ifdef HUQUQ
CFLAGS := $(CFLAGS) -DHUQUQULLAH=1
endif

ifdef GNUCASH
CODE   := $(CODE) gnucash.cc
CFLAGS := $(CFLAGS) -DREAD_GNUCASH=1
LIBS   := $(LIBS) -lxmlparse
endif

all: make.deps ledger

ledger: $(OBJS)
	g++ $(CFLAGS) $(INCS) $(DFLAGS) -o $@ $(OBJS) $(LIBS)

%.o: %.cc
	g++ $(CFLAGS) $(INCS) $(DFLAGS) -c -o $@ $<

clean:
	rm -f ledger *.o

rebuild: clean deps all

deps: make.deps

make.deps: Makefile
	cc -M $(INCS) $(CODE) > $@

include make.deps
