SUBDIRS = gdtoa

EXTRA_DIST = docs tests

dist-hook:
	rm -fr `find $(distdir) -name .svn`

lib_LTLIBRARIES = libledger.la
if HAVE_BOOST_PYTHON
lib_LTLIBRARIES += libpyledger.la
endif

libledger_la_CXXFLAGS = $(WARNFLAGS) -I$(top_builddir)/gdtoa
libledger_la_SOURCES = \
	amount.cc      \
	times.cc       \
	quotes.cc      \
	balance.cc     \
	value.cc       \
	xml.cc         \
	xpath.cc       \
	mask.cc        \
	format.cc      \
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
if DEBUG
libledger_la_CXXFLAGS += -DDEBUG_LEVEL=4
libledger_la_SOURCES  += debug.cc trace.cc
endif
if HAVE_BOOST_PYTHON
libledger_la_CXXFLAGS += -DUSE_BOOST_PYTHON=1
endif

libledger_la_LDFLAGS = -release 3.0


libpyledger_la_CXXFLAGS = -DUSE_BOOST_PYTHON=1
libpyledger_la_SOURCES = \
	py_eval.cc       \
	py_amount.cc

if DEBUG
libpyledger_la_CXXFLAGS += -DDEBUG_LEVEL=4
endif

libpyledger_la_LDFLAGS = -release 3.0


pkginclude_HEADERS = \
	amount.h     \
	times.h      \
	balance.h    \
	binary.h     \
	csv.h	     \
	debug.h	     \
	derive.h     \
	emacs.h	     \
	error.h	     \
	fdstream.hpp \
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
	xmlparse.h   \
	xpath.h

######################################################################

bin_PROGRAMS = ledger

ledger_CXXFLAGS =
ledger_SOURCES = option.cc main.cc
ledger_LDADD = $(LIBOBJS) libledger.la gdtoa/libgdtoa.la
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
ledger_LDADD += libpyledger.la
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

ledger_so_SOURCES = pyledger.cc

PYLIBS = pyledger ledger gdtoa boost_date_time boost_python gmp pcre

if HAVE_EXPAT
PYLIBS += expat
endif
if HAVE_XMLPARSE
PYLIBS += xmlparse xmltok
endif
if HAVE_LIBOFX
PYLIBS += ofx
endif

if DEBUG
DEBUG_LEVEL = 4
else
DEBUG_LEVEL = 0
endif

ledger.so: pyledger.cc libledger.la gdtoa/libgdtoa.la libpyledger.la
	CFLAGS="$(CPPFLAGS)" \
	LDFLAGS="$(LDFLAGS) -L. -L.libs -Lgdtoa -Lgdtoa/.libs" \
	PYLIBS="$(PYLIBS)" \
	DEBUG_LEVEL="$(DEBUG_LEVEL)" \
	    python setup.py build --build-lib=.

install-exec-hook:
	CFLAGS="$(CPPFLAGS)" \
	LDFLAGS="$(LDFLAGS) -L. -L.libs -Lgdtoa -Lgdtoa/.libs" \
	PYLIBS="$(PYLIBS)" \
	DEBUG_LEVEL="$(DEBUG_LEVEL)" \
	    python setup.py install --prefix=$(prefix)

endif

######################################################################

TESTS = UnitTests
if HAVE_BOOST_PYTHON
TESTS += PyUnitTests
endif

check_PROGRAMS = $(TESTS)

UnitTests_SOURCES = tests/UnitTests.cc \
	\
	tests/corelib/numerics/BasicAmount.cc \
	tests/corelib/numerics/CommodityAmount.cc \
	tests/corelib/numerics/Commodity.cc

UnitTests_LDADD   = $(lib_LTLIBRARIES) gdtoa/libgdtoa.la -lcppunit
UnitTests_LDFLAGS = $(LIBADD_DL)

UnitTests_CXXFLAGS = -I. -I$(srcdir)/tests
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

PyUnitTests_SOURCES =

PyUnitTests: PyUnitTests.py
	cp PyUnitTests.py PyUnitTests
	chmod 755 PyUnitTests

######################################################################

all: check

check-syntax:
	g++ -I. -Itests $(CPPFLAGS) $(UnitTests_CXXFLAGS) \
		-o /dev/null -S $(CHK_SOURCES)

all-clean: maintainer-clean
	rm -fr *~ .*~ .\#* *.html *.info *.pdf *.a *.so *.o *.lo *.la	     \
	       *.elc *.aux *.cp *.fn *.ky *.log *.pg *.toc *.tp *.vr	     \
	       .gdb_history gmon.out h out TAGS ledger valexpr .deps .libs   \
	       build INSTALL Makefile acconf.h acconf.h.in aclocal.m4	     \
	       autom4te config.guess config.sub configure depcomp install-sh \
	       libtool ltconfig ltmain.sh missing stamp texinfo.tex	     \
	       Makefile.in mkinstalldirs elisp-comp elc-stamp elc-temp	     \
	       py-compile
