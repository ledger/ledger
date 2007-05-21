SUBDIRS	      = gdtoa
BUILT_SOURCES =
CLEANFILES    =
EXTRA_DIST    = LICENSE docs tests contrib scripts setup.py \
		acprep verify.sh run_verify.sh valgrind.sh \
		src/TODO # src/gnucash.cc src/ofx.cc

ESC_srcdir=`echo "$(srcdir)" | sed 's/\//\\\\\//g'`
ESC_builddir=`echo "$(top_builddir)" | sed 's/\//\\\\\//g'`
ESC_distdir=`echo "$(distdir)" | sed 's/\//\\\\\//g'`

dist-hook:
	rm -fr `find $(distdir) -name .svn`


lib_LTLIBRARIES = libledger.la

AM_YFLAGS = -d
AM_LFLAGS = -o $(LEX_OUTPUT_ROOT).c

WARNFLAGS  = -Wall #-pedantic-errors
#WARNFLAGS += -Wextra -Wfloat-equal -Wno-endif-labels -Wcast-qual
#WARNFLAGS += -Wcast-align -Wwrite-strings -Wconversion -Wconversion
#WARNFLAGS += -Wshorten-64-to-32 -Wsign-compare -Weffc++ -Wsign-promo
#WARNFLAGS += -Wmissing-field-initializers -Wstrict-null-sentinel
#WARNFLAGS += -Wold-style-cast -Woverloaded-virtual 

libledger_la_LDFLAGS  = -release $(PACKAGE_VERSION)
libledger_la_CPPFLAGS = $(WARNFLAGS) \
	-I$(top_builddir)/gdtoa \
	-I$(srcdir)/gdtoa \
	-I$(srcdir)/src \
	-I$(srcdir)/src/utility \
	-I$(srcdir)/src/numerics \
	-I$(srcdir)/src/data \
	-I$(srcdir)/src/traversal \
	-I$(srcdir)/src/driver

libledger_la_SOURCES = \
	src/utility/utils.cc	   \
	src/utility/times.cc	   \
	src/utility/binary.cc	   \
	src/utility/mask.cc	   \
	src/numerics/commodity.cc  \
	src/numerics/amount.cc	   \
	src/numerics/balance.cc	   \
	src/numerics/value.cc	   \
	src/data/document.cc	   \
	src/data/node.cc	   \
	src/data/textual.cc	   \
	src/data/compile.cc	   \
	src/data/journal.cc	   \
	src/data/jbuilder.cc	   \
	src/traversal/xpath.cc	   \
	src/traversal/transform.cc \
	src/traversal/abbrev.cc	   \
	src/driver/session.cc	   \
	src/driver/report.cc

if HAVE_BOOST_PYTHON
Python_SRC = \
	src/python/pyinterp.cc	   \
	src/python/pyledger.cc	   \
	src/python/py_amount.cc	   \
	src/python/py_commodity.cc \
	src/python/py_times.cc	   \
	src/python/py_utils.cc

libledger_la_SOURCES += $(Python_SRC)
libledger_la_CPPFLAGS += -I$(srcdir)/src/python
endif

if HAVE_LIBOFX
libledger_la_CPPFLAGS += -DHAVE_LIBOFX=1
libledger_la_SOURCES += src/ofx.cc
endif
if DEBUG
libledger_la_CPPFLAGS += -DDEBUG_MODE
endif

if USE_PCH
nodist_libledger_la_SOURCES = system.hh.gch

BUILT_SOURCES += system.hh.gch
CLEANFILES += system.hh.gch system.hh

$(top_builddir)/system.hh.gch: $(srcdir)/src/utility/system.hh acconf.h
	echo "#include \"src/utility/system.hh\"" > $(top_builddir)/system.hh
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) $(libledger_la_CPPFLAGS) \
		-o $@ $(srcdir)/src/utility/system.hh
endif


pkginclude_HEADERS = \
	src/ledger.h		  \
	src/utility/binary.h	  \
	src/utility/context.h	  \
	src/utility/flags.h	  \
	src/utility/mask.h	  \
	src/utility/pushvar.h	  \
	src/utility/times.h	  \
	src/utility/utils.h	  \
	src/numerics/amount.h	  \
	src/numerics/balance.h	  \
	src/numerics/balpair.h	  \
	src/numerics/commodity.h  \
	src/numerics/value.h	  \
	src/data/builder.h	  \
	src/data/compile.h	  \
	src/data/document.h	  \
	src/data/jbuilder.h	  \
	src/data/journal.h	  \
	src/data/node.h		  \
	src/data/parser.h	  \
	src/data/textual.h	  \
	src/traversal/abbrev.h	  \
	src/traversal/transform.h \
	src/traversal/xpath.h	  \
	src/driver/option.h	  \
	src/driver/report.h	  \
	src/driver/session.h	  \
	src/python/pyfstream.h	  \
	src/python/pyinterp.h	  \
	src/python/pyledger.h	  \
	src/python/pyutils.h

###############################################################################

bin_PROGRAMS = ledger

ledger_CPPFLAGS = $(libledger_la_CPPFLAGS)
ledger_LDADD	= $(LIBOBJS) libledger.la gdtoa/libgdtoa.la $(LEXLIB)
ledger_LDFLAGS	= -static	# for the sake of command-line speed
ledger_SOURCES  = src/driver/option.cc src/driver/main.cc

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

ledger_so_SOURCES = $(Python_SRC)
ledger_so_DEPENDENCIES = libledger.la gdtoa/libgdtoa.la

PYLIBS  = ledger gdtoa gmp \
	  boost_date_time$(BOOST_SUFFIX)  \
          boost_filesystem$(BOOST_SUFFIX) \
          boost_regex$(BOOST_SUFFIX)	  \
          boost_python$(BOOST_SUFFIX)

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
