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

libledger_la_CPPFLAGS = -I$(top_builddir)/gdtoa -I$(srcdir) -I$(srcdir)/src
libledger_la_LDFLAGS  = -release 3.0

libledger_la_SOURCES = \
	src/session.cc	 \
	src/journal.cc	 \
	src/amount.cc    \
	src/balance.cc   \
	src/value.cc     \
	src/binary.cc	 \
	src/qif.cc	 \
	src/textual.cc	 \
	src/quotes.cc	 \
	src/csv.cc	 \
	src/derive.cc	 \
	src/emacs.cc	 \
	src/format.cc	 \
	src/reconcile.cc \
	src/register.cc	 \
	src/report.cc	 \
	src/transform.cc \
	src/mask.cc	 \
	src/times.cc	 \
	src/utils.cc	 \
	src/xml.cc	 \
	src/xmlparse.cc	 \
	src/xpath.cc

if HAVE_EXPAT
libledger_la_CPPFLAGS += -DHAVE_EXPAT=1
libledger_la_SOURCES += src/gnucash.cc
endif
if HAVE_XMLPARSE
libledger_la_CPPFLAGS += -DHAVE_XMLPARSE=1
libledger_la_SOURCES += src/gnucash.cc
endif
if HAVE_LIBOFX
libledger_la_CPPFLAGS += -DHAVE_LIBOFX=1
libledger_la_SOURCES += src/ofx.cc
endif
if DEBUG
libledger_la_CPPFLAGS += -DFULL_DEBUG
endif
if HAVE_BOOST_PYTHON
libledger_la_CPPFLAGS += -DUSE_BOOST_PYTHON=1
libledger_la_SOURCES += src/pyinterp.cc
endif

if USE_PCH
libledger_la_CXXFLAGS = $(WARNFLAGS)
nodist_libledger_la_SOURCES = system.hh.gch

BUILT_SOURCES += system.hh.gch
CLEANFILES += system.hh.gch

$(top_builddir)/system.hh.gch: $(srcdir)/src/system.hh acconf.h
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) $(libledger_la_CPPFLAGS) \
		-o $@ $(srcdir)/src/system.hh
endif


libpyledger_la_CPPFLAGS = $(libledger_la_CPPFLAGS)
libpyledger_la_LDFLAGS = -release 3.0

libpyledger_la_SOURCES = \
	src/py_amount.cc


pkginclude_HEADERS = \
	src/amount.h     \
	src/balance.h    \
	src/binary.h     \
	src/context.h    \
	src/csv.h	 \
	src/derive.h     \
	src/emacs.h	 \
	src/error.h	 \
	src/fdstream.hpp \
	src/format.h     \
	src/gnucash.h    \
	src/journal.h    \
	src/ledger.h     \
	src/mask.h	 \
	src/ofx.h	 \
	src/option.h     \
	src/parser.h     \
	src/pyinterp.h   \
	src/pyfstream.h  \
	src/pyledger.h   \
	src/qif.h	 \
	src/quotes.h     \
	src/reconcile.h  \
	src/register.h   \
	src/report.h     \
	src/session.h    \
	src/system.hh    \
	src/textual.h    \
	src/times.h      \
	src/transform.h  \
	src/utils.h	 \
	src/value.h	 \
	src/xml.h        \
	src/xpath.h

######################################################################

bin_PROGRAMS = ledger

ledger_CPPFLAGS = $(libledger_la_CPPFLAGS)
ledger_CXXFLAGS = $(WARNFLAGS)
ledger_LDADD = $(LIBOBJS) libledger.la gdtoa/libgdtoa.la $(LEXLIB)
ledger_LDFLAGS = -static	# for the sake of command-line speed

ledger_SOURCES = \
	src/option.cc \
	src/main.cc

if HAVE_BOOST_PYTHON
ledger_LDADD += libpyledger.la
endif

info_TEXINFOS = docs/ledger.texi

######################################################################

dist_lisp_LISP = lisp/ledger.el lisp/timeclock.el

######################################################################

if HAVE_BOOST_PYTHON

noinst_PROGRAMS = ledger.so

CLEANFILES += ledger.so

clean-local:
	rm -fr build

ledger_so_SOURCES = src/pyledger.cc
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

ledger.so: src/pyledger.cc \
	   libledger.la gdtoa/libgdtoa.la libpyledger.la
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

nodist_UnitTests_SOURCES = tests/UnitTests.cc \
	\
	tests/numerics/BasicAmount.cc \
	tests/numerics/CommodityAmount.cc \
	tests/numerics/DateTime.cc \
	tests/numerics/Commodity.cc

UnitTests_CPPFLAGS = -I$(srcdir)/tests $(libledger_la_CPPFLAGS)
UnitTests_LDFLAGS  = $(LIBADD_DL)
UnitTests_LDADD    = $(lib_LTLIBRARIES) gdtoa/libgdtoa.la -lcppunit

PyUnitTests_SOURCES = tests/python/PyUnitTests.py

PyUnitTests: $(srcdir)/tests/python/PyUnitTests.py
	cat $(srcdir)/tests/python/PyUnitTests.py \
	    | sed "s/%srcdir%/$(ESC_srcdir)/g" \
	    | sed "s/%builddir%/$(ESC_builddir)/g" > $@
	chmod 755 $@

######################################################################

DISTCLEANFILES = Doxyfile.gen

alldocs: docs/ledger.info docs/ledger.pdf doxygen-docs

$(top_builddir)/Doxyfile.gen: $(srcdir)/docs/Doxyfile
	cat $(srcdir)/docs/Doxyfile \
	    | sed "s/%srcdir%/$(ESC_srcdir)/g" \
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
	       py-compile ylwrap compile
