SUBDIRS	      = gdtoa
BUILT_SOURCES =
CLEANFILES    =


ESC_srcdir=`echo "$(srcdir)" | sed 's/\//\\\\\//g'`
ESC_builddir=`echo "$(top_builddir)" | sed 's/\//\\\\\//g'`
ESC_distdir=`echo "$(distdir)" | sed 's/\//\\\\\//g'`

EXTRA_DIST = docs docs/ledger.pdf tests

#(cd $(distdir)/docs; zip -r doxygen-html.zip html; rm -fr html)
dist-hook:
	rm -fr `find $(distdir) -name .svn`


lib_LTLIBRARIES = libledger.la
if HAVE_BOOST_PYTHON
lib_LTLIBRARIES += libpyledger.la
endif

AM_YFLAGS = -d
AM_LFLAGS = -o $(LEX_OUTPUT_ROOT).c

WARNFLAGS  = -Wall -Wextra -Wfloat-equal -Wno-endif-labels
WARNFLAGS += -Wcast-qual -Wcast-align -Wwrite-strings -Wconversion
WARNFLAGS += -Wconversion -Wshorten-64-to-32 -Wsign-compare
WARNFLAGS += -Wmissing-field-initializers -pedantic-errors
WARNFLAGS += -Weffc++ -Wstrict-null-sentinel -Wold-style-cast
WARNFLAGS += -Woverloaded-virtual -Wsign-promo

libledger_la_CPPFLAGS = -I$(top_builddir)/gdtoa -I$(srcdir)/gdtoa \
			-I$(srcdir)/src #$(WARNFLAGS)
libledger_la_LDFLAGS  = -release $(PACKAGE_VERSION)

libledger_la_SOURCES = \
	src/utils.cc	 \
	src/times.cc	 \
	src/mask.cc	 \
	src/commodity.cc \
	src/amount.cc    \
	src/balance.cc   \
	src/value.cc     \
			 \
	src/session.cc	 \
	src/journal.cc	 \
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
libledger_la_CPPFLAGS += -DDEBUG_MODE
endif
if HAVE_BOOST_PYTHON
libledger_la_CPPFLAGS += -DUSE_BOOST_PYTHON=1
libledger_la_SOURCES += src/pyinterp.cc
endif

if USE_PCH
nodist_libledger_la_SOURCES = system.hh.gch

BUILT_SOURCES += system.hh.gch
CLEANFILES += system.hh.gch system.hh

$(top_builddir)/system.hh.gch: $(srcdir)/src/system.hh acconf.h
	echo "#include \"src/system.hh\"" > $(top_builddir)/system.hh
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) $(libledger_la_CPPFLAGS) \
		-o $@ $(srcdir)/src/system.hh
endif


libpyledger_la_CPPFLAGS = $(libledger_la_CPPFLAGS)
libpyledger_la_LDFLAGS  = -release $(PACKAGE_VERSION)

libpyledger_la_SOURCES = \
	src/py_utils.cc  \
	src/py_times.cc  \
	src/py_commodity.cc \
	src/py_amount.cc


pkginclude_HEADERS = \
	src/amount.h	     \
	src/balpair.h	     \
	src/balance.h	     \
	src/binary.h	     \
	src/commodity.h	     \
	src/context.h	     \
	src/csv.h	     \
	src/derive.h	     \
	src/emacs.h	     \
	src/fdstream.hpp     \
	src/flags.h	     \
	src/format.h	     \
	src/gnucash.h	     \
	src/journal.h	     \
	src/ledger.h	     \
	src/mask.h	     \
	src/ofx.h	     \
	src/option.h	     \
	src/parser.h	     \
	src/pyfstream.h	     \
	src/pyinterp.h	     \
	src/pyledger.h	     \
	src/pyutils.h	     \
	src/qif.h	     \
	src/quotes.h	     \
	src/reconcile.h	     \
	src/register.h	     \
	src/report.h	     \
	src/scoped_execute.h \
	src/session.h	     \
	src/system.hh	     \
	src/textual.h	     \
	src/times.h	     \
	src/transform.h	     \
	src/utils.h	     \
	src/value.h	     \
	src/xml.h	     \
	src/xpath.h

######################################################################

bin_PROGRAMS = ledger

ledger_CPPFLAGS = $(libledger_la_CPPFLAGS)
ledger_LDADD	= $(LIBOBJS) libledger.la gdtoa/libgdtoa.la $(LEXLIB)
ledger_LDFLAGS	= -static	# for the sake of command-line speed

ledger_SOURCES  = \
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

ledger_so_SOURCES = \
	src/pyledger.cc \
	src/py_utils.cc \
	src/py_times.cc \
	src/py_commodity.cc \
	src/py_amount.cc
ledger_so_DEPENDENCIES = libledger.la gdtoa/libgdtoa.la libpyledger.la

PYLIBS  = pyledger ledger gdtoa gmp
PYLIBS += boost_date_time$(BOOST_SUFFIX)  \
          boost_filesystem$(BOOST_SUFFIX) \
          boost_regex$(BOOST_SUFFIX)	  \
          boost_python$(BOOST_SUFFIX)

if HAVE_EXPAT
PYLIBS += expat
endif
if HAVE_XMLPARSE
PYLIBS += xmlparse xmltok
endif
if HAVE_LIBOFX
PYLIBS += ofx
endif

ledger.so: $(ledger_so_SOURCES) $(ledger_so_DEPENDENCIES)
	CFLAGS="$(CPPFLAGS) -I$(srcdir) $(libledger_la_CPPFLAGS)" \
	LDFLAGS="$(LDFLAGS) -L. -L.libs -Lgdtoa -Lgdtoa/.libs" \
	PYLIBS="$(PYLIBS)" SRCDIR="$(srcdir)" \
	    python $(srcdir)/setup.py build --build-lib=.

install-exec-hook:
	CFLAGS="$(CPPFLAGS) -I$(srcdir) $(libledger_la_CPPFLAGS)" \
	LDFLAGS="$(LDFLAGS) -L. -L.libs -Lgdtoa -Lgdtoa/.libs" \
	PYLIBS="$(PYLIBS)" SRCDIR="$(srcdir)" \
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
	tests/utility/t_utils.cc      \
	tests/utility/t_times.cc      \
	tests/numerics/t_commodity.cc \
	tests/numerics/t_amount.cc    \
	tests/numerics/t_balance.cc

UnitTests_CPPFLAGS = -I$(srcdir)/tests $(libledger_la_CPPFLAGS)
UnitTests_LDFLAGS  = $(LIBADD_DL)
UnitTests_LDADD    = $(lib_LTLIBRARIES) gdtoa/libgdtoa.la -lcppunit

PyUnitTests_SOURCES = tests/python/PyUnitTests.py

PyUnitTests): $(srcdir)/tests/python/PyUnitTests.py
	cat $(srcdir)/tests/python/PyUnitTests.py \
	    | sed "s/%srcdir%/$(ESC_srcdir)/g" \
	    | sed "s/%builddir%/$(ESC_builddir)/g" > $@
	chmod 755 $@

fullcheck: check
	MallocGuardEdges=1	 \
	MallocScribble=1	 \
	MallocPreScribble=1	 \
	MallocCheckHeapStart=100 \
	MallocCheckHeapEach=100	 \
	DYLD_INSERT_LIBRARIES=/usr/lib/libgmalloc.dylib \
	$(srcdir)/valgrind.sh $(top_builddir)/UnitTests --verify

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
