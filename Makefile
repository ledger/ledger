CODE   = amount.cc balance.cc account.cc ledger.cc \
	 constraint.cc item.cc expr.cc format.cc \
	 textual.cc binary.cc
OBJS   = $(patsubst %.cc,%.o,$(CODE))
#CXX    = cc
CXX    = g++
CFLAGS = -Wall -ansi -pedantic
#DFLAGS = -O3 -fomit-frame-pointer
DFLAGS = -g -DDEBUG=1
#DFLAGS = -g -pg
INCS   = -I/sw/include -I/usr/include/gcc/darwin/3.3/c++ -I/usr/include/gcc/darwin/3.3/c++/ppc-darwin
LIBS   = -L/sw/lib  -lgmpxx -lgmp -lpcre

ifdef GNUCASH
CODE   := $(CODE)   gnucash.cc
CFLAGS := $(CFLAGS) -DREAD_GNUCASH=1
INCS   := $(INCS)   -I/usr/include/httpd/xml
LIBS   := $(LIBS)   -L/sw/lib -lxmlparse
endif

all: make.deps ledger

docs: ledger.info ledger.pdf

install:
	make clean
	make DFLAGS="-O3 -fomit-frame-pointer"
	cp ledger $(HOME)/bin
	strip $(HOME)/bin/ledger

libledger.a: $(OBJS)
	ar rv $@ $?
	ranlib $@

ledger: libledger.a main.o
	$(CXX) $(CFLAGS) $(INCS) $(DFLAGS) -o $@ main.o -L. -lledger $(LIBS)

report: libledger.a report.cc
	$(CXX) $(CFLAGS) $(INCS) $(DFLAGS) -DTEST -o $@ report.cc \
		-L. -lledger $(LIBS)

ledger.info: ledger.texi
	makeinfo $<

ledger.pdf: ledger.texi
	texi2pdf $<

%.o: %.cc
	$(CXX) $(CFLAGS) $(INCS) $(DFLAGS) -c -o $@ $<

clean:
	rm -f ledger report libledger.a *.o *.elc *~ .\#*
	rm -f *.aux *.cp *.fn *.ky *.log *.pg *.toc *.tp *.vr
	rm -f .gdb_history

distclean fullclean: clean
	rm -f *.texi *.info *.html *.pdf *.elc make.deps TAGS

rebuild: clean deps all

deps: make.deps

make.deps: Makefile
	cc -M $(INCS) $(CODE) > $@

include make.deps

# These next rules are for my own use.

README.html: README
	(cd $(HOME)/src/muse && \
	 ./publish --html $(shell pwd)/README && \
	 mv README.html $(shell pwd))

ledger.texi: README
	(cd $(HOME)/src/muse && \
	 ./publish --texi $(shell pwd)/README && \
	 cat README.texi | sed 's/README\.info/ledger.info/g' \
	   > $(shell pwd)/ledger.texi && \
	 rm README.texi)

VERSION = $(shell scripts/version)

dist:
	rm -fr /tmp/ledger-$(VERSION)
	rsync -av --exclude=".*" --exclude="TAGS" --exclude="version" \
	  --exclude="_darcs/" --exclude="ledger.dat" \
	  $(shell pwd)/ /tmp/ledger-$(VERSION)
	(cd /tmp/ledger-$(VERSION) && \
	 make fullclean && \
	 make docs README.html && \
	 make clean && rm make.deps && \
	 cat Makefile | sed 's/\/sw\//\/usr\/local\//g' > t && \
	 mv t Makefile && \
	 cat Makefile | sed 's/ -I\/usr\/include\/gcc.*//' > t && \
	 mv t Makefile && \
	 perl -ne 'print if 1 .. /^include make.deps/;' Makefile > t && \
	 mv t Makefile && \
	 cd $(HOME)/Public && \
	 tar cvzf ledger-$(VERSION).tar.gz /tmp/ledger-$(VERSION))

publish: dist
	(cd $(HOME)/Public && \
	 ln -sf ledger-$(VERSION).tar.gz ledger.tar.gz && \
	 rm -fr /tmp/ledger-$(VERSION) && \
	 upload)
