SUBDIRS = gdtoa
BUILT_SOURCES =
CLEANFILES =


ESC_srcdir=`echo "$(srcdir)" | sed 's/\//\\\\\//g'`
ESC_builddir=`echo "$(top_builddir)" | sed 's/\//\\\\\//g'`
ESC_distdir=`echo "$(distdir)" | sed 's/\//\\\\\//g'`

EXTRA_DIST = docs tests ledger.pdf ledger.info

#(cd $(distdir)/docs; zip -r doxygen-html.zip html; rm -fr html)
dist-hook:
	rm -fr `find $(distdir) -name .svn`


lib_LTLIBRARIES = libledger.la
if HAVE_BOOST_PYTHON
lib_LTLIBRARIES += libpyledger.la
endif

AM_YFLAGS = -d
AM_LFLAGS = -o $(LEX_OUTPUT_ROOT).c

#WARNFLAGS  = -H -Wall -Wextra -Wfloat-equal -Wno-endif-labels
#WARNFLAGS += -Wcast-qual -Wcast-align -Wwrite-strings -Wconversion
#WARNFLAGS += -Wconversion -Wshorten-64-to-32 -Wsign-compare
#WARNFLAGS += -Wmissing-field-initializers -pedantic-errors

libledger_la_CPPFLAGS = -I$(top_builddir)/gdtoa
libledger_la_LDFLAGS  = -release 3.0

libledger_la_SOURCES = \
	utils.cc       \
	times.cc       \
	amount.cc      \
	quotes.cc      \
	balance.cc     \
	value.cc       \
	xml.cc         \
	xpath.cc       \
	mask.cc        \
	format.cc      \
		       \
	session.cc     \
	journal.cc     \
	textual.cc     \
	binary.cc      \
	xmlparse.cc    \
	qif.cc	       \
		       \
	report.cc      \
	transform.cc   \
		       \
	register.cc    \
	csv.cc         \
	derive.cc      \
	emacs.cc       \
	reconcile.cc

if HAVE_EXPAT
libledger_la_CPPFLAGS += -DHAVE_EXPAT=1
libledger_la_SOURCES += gnucash.cc
endif
if HAVE_XMLPARSE
libledger_la_CPPFLAGS += -DHAVE_XMLPARSE=1
libledger_la_SOURCES += gnucash.cc
endif
if HAVE_LIBOFX
libledger_la_CPPFLAGS += -DHAVE_LIBOFX=1
libledger_la_SOURCES += ofx.cc
endif
if DEBUG
libledger_la_CPPFLAGS += -DFULL_DEBUG
endif
if HAVE_BOOST_PYTHON
libledger_la_CPPFLAGS += -DUSE_BOOST_PYTHON=1
endif

if USE_PCH
libledger_la_CXXFLAGS = $(WARNFLAGS)
nodist_libledger_la_SOURCES = system.hh.gch

BUILT_SOURCES += system.hh.gch
CLEANFILES += system.hh.gch system.hh

$(top_builddir)/system.hh.gch: $(srcdir)/system.hh acconf.h $(srcdir)/fdstream.hpp
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) $(libledger_la_CPPFLAGS) \
		-o $@ $(srcdir)/system.hh
endif


libpyledger_la_CPPFLAGS = $(libledger_la_CPPFLAGS)
libpyledger_la_LDFLAGS = -release 3.0

libpyledger_la_SOURCES = \
	py_eval.cc       \
	py_amount.cc


pkginclude_HEADERS = \
	amount.h     \
	times.h      \
	balance.h    \
	binary.h     \
	context.h    \
	csv.h	     \
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
	register.h   \
	report.h     \
	session.h    \
	system.hh    \
	textual.h    \
	transform.h  \
	utils.h	     \
	value.h	     \
	xml.h        \
	xmlparse.h   \
	xpath.h

######################################################################

bin_PROGRAMS = ledger

ledger_CPPFLAGS = $(libledger_la_CPPFLAGS)
ledger_CXXFLAGS = $(WARNFLAGS)
ledger_SOURCES = option.cc main.cc
ledger_LDADD = $(LIBOBJS) libledger.la gdtoa/libgdtoa.la $(LEXLIB)
ledger_LDFLAGS = -static	# for the sake of command-line speed

if HAVE_BOOST_PYTHON
ledger_LDADD += libpyledger.la
endif

info_TEXINFOS = ledger.texi

######################################################################

dist_lisp_LISP = ledger.el timeclock.el

######################################################################

if HAVE_BOOST_PYTHON

noinst_PROGRAMS = ledger.so

CLEANFILES += ledger.so

clean-local:
	rm -fr build

ledger_so_SOURCES = pyledger.cc
ledger_so_DEPENDENCIES = libledger.la gdtoa/libgdtoa.la libpyledger.la

PYLIBS = pyledger ledger gdtoa boost_date_time boost_regex boost_python gmp

if HAVE_EXPAT
PYLIBS += expat
endif
if HAVE_XMLPARSE
PYLIBS += xmlparse xmltok
endif
if HAVE_LIBOFX
PYLIBS += ofx
endif

ledger.so: pyledger.cc libledger.la gdtoa/libgdtoa.la libpyledger.la
	SRCDIR="$(srcdir)" \
	CFLAGS="$(CPPFLAGS) -I$(srcdir) $(libledger_la_CPPFLAGS)" \
	LDFLAGS="$(LDFLAGS) -L. -L.libs -Lgdtoa -Lgdtoa/.libs" \
	PYLIBS="$(PYLIBS)" \
	    python $(srcdir)/setup.py build --build-lib=.

install-exec-hook:
	SRCDIR="$(srcdir)" \
	CFLAGS="$(CPPFLAGS) -I$(srcdir) $(libledger_la_CPPFLAGS)" \
	LDFLAGS="$(LDFLAGS) -L. -L.libs -Lgdtoa -Lgdtoa/.libs" \
	PYLIBS="$(PYLIBS)" \
	    python $(srcdir)/setup.py install --prefix=$(prefix)

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
	tests/corelib/numerics/DateTime.cc \
	tests/corelib/numerics/Commodity.cc

UnitTests_CPPFLAGS = -I$(srcdir)/tests $(libledger_la_CPPFLAGS)
UnitTests_LDFLAGS  = $(LIBADD_DL)
UnitTests_LDADD    = $(lib_LTLIBRARIES) gdtoa/libgdtoa.la -lcppunit

PyUnitTests_SOURCES = PyUnitTests.py

PyUnitTests: PyUnitTests.py
	cat $(srcdir)/PyUnitTests.py | sed "s/%srcdir%/$(ESC_srcdir)/g" \
	    | sed "s/%builddir%/$(ESC_builddir)/g" > PyUnitTests
	chmod 755 PyUnitTests

######################################################################

DISTCLEANFILES = Doxyfile.gen

alldocs: ledger.info ledger.pdf doxygen-docs

$(top_builddir)/Doxyfile.gen: $(srcdir)/Doxyfile
	cat $(srcdir)/Doxyfile | sed "s/%srcdir%/$(ESC_srcdir)/g" \
	    | sed "s/%builddir%/$(ESC_builddir)/g" > $@

doxygen-docs: $(top_builddir)/Doxyfile.gen
	doxygen $(top_builddir)/Doxyfile.gen

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
