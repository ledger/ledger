if USE_PCH
#BUILT_SOURCES = pchpic.h.gch pchnopic.h.gch
BUILT_SOURCES = pchnopic.h.gch
#CLEANFILES = pchpic.h.gch pchnopic.h.gch
CLEANFILES = pchnopic.h.gch
endif

lib_LTLIBRARIES = libledger.la

libledger_la_CXXFLAGS =
if USE_PCH
libledger_la_CXXFLAGS += -DUSE_PCH -Winvalid-pch -fpch-deps
endif
libledger_la_SOURCES = \
	amount.cc      \
	balance.cc     \
	value.cc       \
	datetime.cc    \
	valexpr.cc     \
	format.cc      \
	quotes.cc      \
	mask.cc        \
		       \
	trace.cc       \
	util.cc        \
		       \
	session.cc     \
	journal.cc     \
	parser.cc      \
	textual.cc     \
	binary.cc      \
	xml.cc	       \
	qif.cc	       \
		       \
	report.cc      \
	repitem.cc     \
	transform.cc   \
		       \
	dump.cc        \
	register.cc    \
	csv.cc         \
	derive.cc      \
	emacs.cc       \
	reconcile.cc
if HAVE_EXPAT
libledger_la_CXXFLAGS += -DHAVE_EXPAT=1
libledger_la_SOURCES += gnucash.cc
endif
if HAVE_XMLPARSE
libledger_la_CXXFLAGS += -DHAVE_XMLPARSE=1
libledger_la_SOURCES += gnucash.cc
endif
if HAVE_LIBOFX
libledger_la_CXXFLAGS += -DHAVE_LIBOFX=1
libledger_la_SOURCES += ofx.cc
endif
if HAVE_BOOST_PYTHON
libledger_la_CXXFLAGS += -DUSE_BOOST_PYTHON=1
libledger_la_SOURCES += py_eval.cc
endif
if DEBUG
libledger_la_CXXFLAGS += -DDEBUG_LEVEL=4
libledger_la_SOURCES  += debug.cc
endif
libledger_la_LDFLAGS = -release 3.0

pkginclude_HEADERS = \
	acconf.h     \
	amount.h     \
	balance.h    \
	binary.h     \
	csv.h	     \
	datetime.h   \
	debug.h	     \
	derive.h     \
	dump.h	     \
	emacs.h	     \
	error.h	     \
	format.h     \
	gnucash.h    \
	journal.h    \
	ledger.h     \
	mask.h	     \
	ofx.h	     \
	option.h     \
	parser.h     \
	py_eval.h    \
	pyfstream.h  \
	pyledger.h   \
	qif.h	     \
	quotes.h     \
	reconcile.h  \
	register.h   \
	repitem.h    \
	report.h     \
	session.h    \
	textual.h    \
	timing.h     \
	trace.h	     \
	transform.h  \
	util.h	     \
	valexpr.h    \
	value.h	     \
	xml.h

pchpic.h.gch: pch.h pchdata.h $(pkginclude_HEADERS)
	$(CXXCOMPILE) $(CXXFLAGS) -DPIC -o $@ pchdata.h

pchnopic.h.gch: pch.h pchdata.h $(pkginclude_HEADERS)
	$(CXXCOMPILE) $(CXXFLAGS) -o $@ pchdata.h

######################################################################

bin_PROGRAMS = ledger
ledger_CXXFLAGS =
ledger_SOURCES = option.cc main.cc
ledger_LDADD = $(LIBOBJS) libledger.la
if HAVE_EXPAT
ledger_CXXFLAGS += -DHAVE_EXPAT=1
endif
if HAVE_XMLPARSE
ledger_CXXFLAGS += -DHAVE_XMLPARSE=1
endif
if HAVE_LIBOFX
ledger_CXXFLAGS += -DHAVE_LIBOFX=1
endif
if HAVE_BOOST_PYTHON
ledger_CXXFLAGS += -DUSE_BOOST_PYTHON=1
endif
if DEBUG
ledger_CXXFLAGS += -DDEBUG_LEVEL=4
endif
ledger_LDFLAGS = -static	# for the sake of command-line speed

info_TEXINFOS = ledger.texi

######################################################################

lisp_LISP = ledger.el timeclock.el
dist_lisp_LISP = ledger.el timeclock.el

######################################################################

if HAVE_BOOST_PYTHON

noinst_PROGRAMS = ledger.so

ledger.so: pyledger.cc libledger.la
	CFLAGS="$(CPPFLAGS)" LDFLAGS="$(LDFLAGS) -L. -L.libs" \
	    python setup.py build --build-lib=.

install-exec-hook:
	CFLAGS="$(CPPFLAGS)" LDFLAGS="$(LDFLAGS) -L. -L.libs" \
	    python setup.py install --prefix=$(prefix)

endif

######################################################################

TESTS = alltests

CXXTEST_DIR = /usr/local/cxxtest
TESTGEN = $(CXXTEST_DIR)/cxxtestgen.py
TESTSUITES = tests/*.h

AM_CXXFLAGS =
if HAVE_EXPAT
AM_CXXFLAGS += -DHAVE_EXPAT=1
endif
if HAVE_XMLPARSE
AM_CXXFLAGS += -DHAVE_XMLPARSE=1
endif
if HAVE_LIBOFX
AM_CXXFLAGS += -DHAVE_LIBOFX=1
endif
if DEBUG
AM_CXXFLAGS += -DDEBUG_LEVEL=4
endif

alltests.cc: $(TESTSUITES)
	test -f $(TESTGEN) && python $(TESTGEN) -o $@ --error-printer $(TESTSUITES)

alltests: alltests.cc ledger
	$(CXXCOMPILE) -I$(CXXTEST_DIR) -lexpat -lgmp -lpcre -o $@ \
		alltests.cc -L. -L.libs -lledger

runtests: alltests
	DYLD_INSERT_LIBRARIES=/usr/lib/libgmalloc.dylib \
	LD_LIBRARY_PATH=.libs ./alltests && \
	DYLD_INSERT_LIBRARIES=/usr/lib/libgmalloc.dylib \
	LD_LIBRARY_PATH=.libs tests/regress && \
	DYLD_INSERT_LIBRARIES=/usr/lib/libgmalloc.dylib \
	LD_LIBRARY_PATH=.libs tests/regtest

verify: runtests
	python tests/runtests.py

######################################################################

all-clean: maintainer-clean
	rm -fr	*~ .*~ .\#* *.html *.info *.pdf *.a *.so *.o *.lo *.la \
		*.elc *.aux *.cp *.fn *.ky *.log *.pg *.toc *.tp *.vr \
		.gdb_history gmon.out h out TAGS ledger valexpr .deps \
		.libs build AUTHORS COPYING INSTALL Makefile acconf.h \
		acconf.h.in aclocal.m4 autom4te config.guess config.sub \
		configure depcomp install-sh libtool ltconfig ltmain.sh \
		missing stamp texinfo.tex Makefile.in mkinstalldirs \
		elisp-comp elc-stamp py-compile *.gch
