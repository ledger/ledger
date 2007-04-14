if USE_PCH
#BUILT_SOURCES = pchpic.h.gch pchnopic.h.gch
BUILT_SOURCES = pchnopic.h.gch
#CLEANFILES = pchpic.h.gch pchnopic.h.gch
CLEANFILES = pchnopic.h.gch
endif

######################################################################

#bin_PROGRAMS = xpath
#
#xpath_CXXFLAGS = -DTEST
#xpath_SOURCES = \
#	amount.cc      \
#	datetime.cc    \
#	quotes.cc      \
#	balance.cc     \
#	value.cc       \
#	mask.cc        \
#	xml.cc         \
#	xpath.cc       \
#	trace.cc       \
#	util.cc
#xpath_LDADD = $(LIBOBJS)
#if HAVE_EXPAT
#xpath_CXXFLAGS += -DHAVE_EXPAT=1
#endif
#if HAVE_XMLPARSE
#xpath_CXXFLAGS += -DHAVE_XMLPARSE=1
#endif
#if DEBUG
#xpath_CXXFLAGS += -DDEBUG_LEVEL=4
#xpath_SOURCES  += debug.cc
#endif
#xpath_LDFLAGS = -static	# for the sake of command-line speed

######################################################################

lib_LTLIBRARIES = libledger.la

libledger_la_CXXFLAGS =
if USE_PCH
libledger_la_CXXFLAGS += -DUSE_PCH -Winvalid-pch -fpch-deps
endif
libledger_la_SOURCES = \
	amount.cc      \
	quotes.cc      \
	balance.cc     \
	value.cc       \
	datetime.cc    \
	xml.cc         \
	xpath.cc       \
	mask.cc        \
	format.cc      \
		       \
	trace.cc       \
	util.cc        \
		       \
	session.cc     \
	journal.cc     \
	parser.cc      \
	textual.cc     \
	binary.cc      \
	xmlparse.cc    \
	qif.cc	       \
		       \
	report.cc      \
	transform.cc   \
		       \
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
	report.h     \
	session.h    \
	textual.h    \
	timing.h     \
	trace.h	     \
	transform.h  \
	util.h	     \
	value.h	     \
	xml.h        \
	xpath.h

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

#lisp_LISP = ledger.el timeclock.el
#dist_lisp_LISP = ledger.el timeclock.el

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

TESTS = UnitTests

check_PROGRAMS = $(TESTS)

UnitTests_SOURCES = tests/UnitTests.cc \
	tests/corelib/numerics/BasicAmountTest.cc

UnitTests_LDADD   = $(lib_LTLIBRARIES) -lcppunit
UnitTests_LDFLAGS = $(LIBADD_DL)

UnitTests_CXXFLAGS = -Itests
if HAVE_EXPAT
UnitTests_CXXFLAGS += -DHAVE_EXPAT=1
endif
if HAVE_XMLPARSE
UnitTests_CXXFLAGS += -DHAVE_XMLPARSE=1
endif
if HAVE_LIBOFX
UnitTests_CXXFLAGS += -DHAVE_LIBOFX=1
endif
if DEBUG
UnitTests_CXXFLAGS += -DDEBUG_LEVEL=4
endif

######################################################################

all: check

all-clean: maintainer-clean
	rm -fr	*~ .*~ .\#* *.html *.info *.pdf *.a *.so *.o *.lo *.la \
		*.elc *.aux *.cp *.fn *.ky *.log *.pg *.toc *.tp *.vr \
		.gdb_history gmon.out h out TAGS ledger valexpr .deps \
		.libs build AUTHORS COPYING INSTALL Makefile acconf.h \
		acconf.h.in aclocal.m4 autom4te config.guess config.sub \
		configure depcomp install-sh libtool ltconfig ltmain.sh \
		missing stamp texinfo.tex Makefile.in mkinstalldirs \
		elisp-comp elc-stamp py-compile *.gch UnitTests
