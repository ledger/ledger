CODE   = account.cc  \
	 amount.cc   \
	 autoxact.cc \
	 balance.cc  \
	 binary.cc   \
	 config.cc   \
	 datetime.cc \
	 debug.cc    \
	 format.cc   \
	 ledger.cc   \
	 option.cc   \
	 parser.cc   \
	 qif.cc      \
	 quotes.cc   \
	 textual.cc  \
	 valexpr.cc  \
	 walk.cc

OBJS   = $(patsubst %.cc,%.o,$(CODE))

#CXX    = cc
CXX    = g++

CFLAGS = -Wall -ansi -pedantic
#DFLAGS = -O3 -fomit-frame-pointer -DDEBUG_LEVEL=0
DFLAGS = -g -DDEBUG_LEVEL=4
#DFLAGS = -g -DDEBUG_LEVEL=2 -pg

INCS   = -I/sw/include \
	 -I/usr/include/gcc/darwin/3.3/c++ \
	 -I/usr/include/gcc/darwin/3.3/c++/ppc-darwin
LIBS   = -L/sw/lib -lgmpxx -lgmp -lpcre

ifdef GNUCASH
CODE   := $(CODE)   gnucash.cc
OBJS   := $(OBJS)   gnucash.o
CFLAGS := $(CFLAGS) -DREAD_GNUCASH=1
INCS   := $(INCS)   -I/usr/include/httpd/xml
LIBS   := $(LIBS)   -L/sw/lib -lxmlparse
endif

all: make.deps ledger

docs: ledger.info ledger.pdf

libledger.a: $(OBJS)
	ar rv $@ $?
	ranlib $@

ledger: libledger.a main.o
	$(CXX) $(CFLAGS) $(INCS) $(DFLAGS) -o $@ main.o -L. -lledger $(LIBS)

valexpr: libledger.a valexpr.cc
	$(CXX) $(CFLAGS) $(INCS) $(DFLAGS) -DTEST -o $@ valexpr.cc \
		-L. -lledger $(LIBS)

ledger.info: ledger.texi
	makeinfo $<

ledger.pdf: ledger.texi
	texi2pdf $<

%.o: %.cc
	$(CXX) $(CFLAGS) $(INCS) $(DFLAGS) -c -o $@ $<

clean:
	rm -f ledger valexpr libledger.a *.o *.elc *~ .\#*
	rm -f *.aux *.cp *.fn *.ky *.log *.pg *.toc *.tp *.vr
	rm -f .gdb_history gmon.out out

distclean fullclean: clean
	rm -f *.info *.html *.pdf *.elc make.deps TAGS

rebuild: clean deps all

deps: make.deps

make.deps: Makefile
	cc -M $(INCS) $(CODE) main.cc > $@

include make.deps

# These next rules are for my own use.

install:
	make clean
	make DFLAGS="-O3 -fomit-frame-pointer -DDEBUG_LEVEL=0"
	cp ledger $(HOME)/bin
	strip $(HOME)/bin/ledger

VERSION = $(shell scripts/version)

dist:
	rm -fr /tmp/ledger-$(VERSION)
	rsync -av --exclude=".*" --exclude="TAGS" --exclude="version" \
	  --exclude="_darcs/" --exclude="ledger.dat" --exclude="CVS/" \
	  --exclude="1.7/" --exclude="*.out" --exclude="*~" \
	  $(shell pwd)/ /tmp/ledger-$(VERSION)
	(cd /tmp/ledger-$(VERSION) && \
	 make fullclean && make docs && \
	 make clean && rm make.deps && \
	 cat Makefile | sed 's/\/sw\//\/usr\/local\//g' > t && \
	 mv t Makefile && \
	 cat Makefile | sed 's/ -I\/usr\/include\/gcc.*//' > t && \
	 mv t Makefile && \
	 perl -ne 'print if 1 .. /^include make.deps/;' Makefile > t && \
	 mv t Makefile && \
	 cd /tmp && \
	 tar cvzf $(HOME)/Sites/ledger/ledger-$(VERSION).tar.gz \
		  ledger-$(VERSION))
