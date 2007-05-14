SUBDIRS	      = gdtoa
BUILT_SOURCES =
CLEANFILES    =
EXTRA_DIST    = LICENSE docs tests contrib scripts setup.py \
		acprep verify.sh run_verify.sh valgrind.sh \
		src/TODO # src/gnucash.cc src/ofx.cc

ESC_srcdir=`echo "$(srcdir)" | sed 's/\//\\\\\//g'`
ESC_builddir=`echo "$(top_builddir)" | sed 's/\//\\\\\//g'`
ESC_distdir=`echo "$(distdir)" | sed 's/\//\\\\\//g'`

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
	src/abbrev.cc	 \
	src/commodity.cc \
	src/amount.cc    \
	src/balance.cc   \
	src/value.cc     \
	src/document.cc	 \
	src/node.cc	 \
	src/xpath.cc     \
	src/builder.cc	 \
	src/journal.cc	 \
	src/textual.cc	 \
	src/binary.cc	 \
	src/transform.cc \
	src/report.cc	 \
	src/session.cc

#if HAVE_EXPAT
#libledger_la_CPPFLAGS += -DHAVE_EXPAT=1
#libledger_la_SOURCES += src/gnucash.cc
#endif
#if HAVE_XMLPARSE
#libledger_la_CPPFLAGS += -DHAVE_XMLPARSE=1
#libledger_la_SOURCES += src/gnucash.cc
#endif
#if HAVE_LIBOFX
#libledger_la_CPPFLAGS += -DHAVE_LIBOFX=1
#libledger_la_SOURCES += src/ofx.cc
#endif
if DEBUG
libledger_la_CPPFLAGS += -DDEBUG_MODE
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

libpyledger_la_SOURCES =    \
	src/pyinterp.cc	    \
	src/py_utils.cc	    \
	src/py_times.cc	    \
	src/py_commodity.cc \
	src/py_amount.cc


pkginclude_HEADERS = \
	src/abbrev.h	     \
	src/amount.h	     \
	src/balance.h	     \
	src/balpair.h	     \
	src/binary.h	     \
	src/builder.h	     \
	src/commodity.h	     \
	src/context.h	     \
	src/document.h	     \
	src/fdstream.hpp     \
	src/flags.h	     \
	src/format.h	     \
	src/journal.h	     \
	src/ledger.h	     \
	src/mask.h	     \
	src/node.h	     \
	src/option.h	     \
	src/parser.h	     \
	src/pyfstream.h	     \
	src/pyinterp.h	     \
	src/pyledger.h	     \
	src/pyutils.h	     \
	src/report.h	     \
	src/scoped_execute.h \
	src/session.h	     \
	src/system.hh	     \
	src/textual.h	     \
	src/times.h	     \
	src/transform.h	     \
	src/tuples.hpp	     \
	src/utils.h	     \
	src/value.h	     \
	src/xpath.h

###############################################################################

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

nodist_info_TEXINFOS = docs/ledger.texi

###############################################################################

dist_lisp_LISP = lisp/ledger.el lisp/timeclock.el

DISTCLEANFILES = ledger.elc timeclock.elc

###############################################################################

if HAVE_BOOST_PYTHON

noinst_PROGRAMS = ledger.so

CLEANFILES += ledger.so

clean-local:
	rm -fr build

ledger_so_SOURCES = \
	src/pyledger.cc \
	src/pyinterp.cc \
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

#if HAVE_EXPAT
#PYLIBS += expat
#endif
#if HAVE_XMLPARSE
#PYLIBS += xmlparse xmltok
#endif
#if HAVE_LIBOFX
#PYLIBS += ofx
#endif

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

###############################################################################

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

nodist_PyUnitTests_SOURCES = tests/python/PyUnitTests.py

# jww (2007-05-10): This rule will not be triggered on systems that
# define an EXEEXT.
PyUnitTests: $(srcdir)/tests/python/PyUnitTests.py
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
	$(srcdir)/valgrind.sh $(top_builddir)/UnitTests$(EXEEXT) --verify

###############################################################################

DISTCLEANFILES += Doxyfile.gen

alldocs: docs/ledger.info docs/ledger.pdf doxygen-docs

$(top_builddir)/Doxyfile.gen: $(srcdir)/docs/Doxyfile
	cat $(srcdir)/docs/Doxyfile \
	    | sed "s/%srcdir%/$(ESC_srcdir)/g" \
	    | sed "s/%builddir%/$(ESC_builddir)/g" > $@

doxygen-docs: $(top_builddir)/Doxyfile.gen
	doxygen $(top_builddir)/Doxyfile.gen

###############################################################################

clean-backupfiles:
	rm -fr *~  \
	       .*~ \
	       .\#*

clean-documentation:
	(cd docs; \
	rm -fr *.aux  \
	       *.cp   \
	       *.fn   \
	       *.info \
	       *.ky   \
	       *.log  \
	       *.pdf  \
	       *.pg   \
	       *.toc  \
	       *.tp   \
	       *.vr)

clean-buildproducts:
	rm -fr *.Plo  \
	       *.Po   \
	       *.a    \
	       *.elc  \
	       *.gcno \
	       *.gdca \
	       *.la   \
	       *.lo   \
	       *.o    \
	       *.so   \
	       .deps  \
	       .libs  \
	       build

clean-debugdata:
	rm -fr .gdb_history \
	       TAGS         \
	       gmon.out	    \
	       h	    \
	       out

clean-autoconf:
	(cd $(srcdir);
	rm -fr Makefile	     \
	       Makefile.in   \
	       acconf.h	     \
	       acconf.h.in   \
	       aclocal.m4    \
	       autom4te      \
	       compile	     \
	       config.guess  \
	       config.sub    \
	       configure     \
	       depcomp	     \
	       elc-stamp     \
	       elc-temp	     \
	       elisp-comp    \
	       install-sh    \
	       libtool	     \
	       ltconfig	     \
	       ltmain.sh     \
	       missing	     \
	       mkinstalldirs \
	       py-compile    \
	       stamp	     \
	       texinfo.tex   \
	       ylwrap)

all-clean: maintainer-clean    \
	   clean-buildproducts \
	   clean-backupfiles   \
	   clean-debugdata     \
	   clean-documentation \
	   clean-autoconf
