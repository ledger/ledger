BUILT_SOURCES =
CLEANFILES    =
EXTRA_DIST    = LICENSE NEWS README test contrib doc python

# Files that are still only in the Ledger source tree (g200d919):
# 
#   amounts.cc
#   py_amount.cc
#   py_commodity.cc
#   py_times.cc
#   py_utils.cc
#   pyinterp.cc
#   pyledger.cc
#
#   pyfstream.h
#   pyinterp.h
#   pyledger.h
#   pyutils.h
#   tuples.hpp

ESC_srcdir=`echo "$(srcdir)" | sed 's/\//\\\\\//g'`
ESC_builddir=`echo "$(top_builddir)" | sed 's/\//\\\\\//g'`
ESC_distdir=`echo "$(distdir)" | sed 's/\//\\\\\//g'`

lib_LTLIBRARIES = libamounts.la libledger.la

libamounts_la_CPPFLAGS = -I$(srcdir)/src
libamounts_la_SOURCES  = \
	src/utils.cc       \
	src/times.cc       \
	src/mask.cc        \
	src/binary.cc      \
			   \
	src/amount.cc      \
	src/commodity.cc   \
	src/balance.cc     \
	src/value.cc       \
			   \
	src/token.cc       \
	src/parser.cc      \
	src/op.cc          \
	src/expr.cc        \
	src/scope.cc       \
	src/format.cc      \
	src/option.cc

if HAVE_EXPAT
libamounts_la_CPPFLAGS += -DHAVE_EXPAT=1
endif
if HAVE_XMLPARSE
libamounts_la_CPPFLAGS += -DHAVE_XMLPARSE=1
endif
if HAVE_LIBOFX
libamounts_la_CPPFLAGS += -DHAVE_LIBOFX=1
endif
if HAVE_BOOST_PYTHON
libamounts_la_CPPFLAGS += -DUSE_BOOST_PYTHON=1
endif
if DEBUG
libamounts_la_CPPFLAGS += -DDEBUG_MODE
endif

libledger_la_CPPFLAGS = $(libamounts_la_CPPFLAGS)
libledger_la_LDFLAGS  = -release 3.0.0
libledger_la_SOURCES  = \
	src/journal.cc     \
	src/entry.cc       \
	src/xact.cc        \
	src/account.cc     \
	src/iterators.cc   \
	src/compare.cc     \
			   \
	src/textual.cc     \
	src/cache.cc       \
	src/emacs.cc       \
	src/qif.cc	   \
	src/xml.cc	   \
	src/csv.cc         \
			   \
	src/session.cc     \
	src/report.cc      \
	src/filters.cc     \
	src/output.cc      \
	src/help.cc        \
			   \
	src/derive.cc      \
	src/reconcile.cc   \
	src/quotes.cc

if HAVE_EXPAT
libledger_la_SOURCES += src/gnucash.cc
endif
if HAVE_XMLPARSE
libledger_la_SOURCES += src/gnucash.cc
endif
if HAVE_LIBOFX
libledger_la_SOURCES += src/ofx.cc
endif

pkginclude_HEADERS =     \
	acconf.h         \
	src/system.hh    \
	src/fdstream.h   \
	src/utils.h	 \
	src/flags.h      \
	src/hooks.h      \
	src/pushvar.h    \
	src/error.h      \
	src/times.h	 \
	src/mask.h       \
	src/binary.h     \
			 \
	src/amount.h     \
	src/commodity.h  \
	src/balance.h    \
	src/balpair.h    \
	src/value.h      \
			 \
	src/token.h      \
	src/parser.h     \
	src/op.h         \
	src/expr.h       \
	src/scope.h      \
	src/predicate.h  \
	src/format.h     \
	src/option.h     \
			 \
	src/journal.h    \
	src/entry.h      \
	src/xact.h       \
	src/account.h    \
	src/iterators.h  \
	src/compare.h    \
			 \
	src/textual.h    \
	src/cache.h      \
	src/emacs.h      \
	src/qif.h	 \
	src/xml.h	 \
	src/csv.h        \
	src/gnucash.h    \
	src/ofx.h	 \
			 \
	src/session.h    \
	src/report.h     \
	src/handler.h    \
	src/filters.h    \
	src/output.h     \
	src/help.h       \
			 \
	src/derive.h     \
	src/reconcile.h  \
	src/quotes.h     \
			 \
	src/ledger.h

if USE_PCH
nodist_libledger_la_SOURCES = src/system.hh.gch

BUILT_SOURCES += src/system.hh.gch
CLEANFILES += src/system.hh.gch

$(srcdir)/src/system.hh.gch: $(srcdir)/src/system.hh $(top_builddir)/acconf.h
	$(CXX) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) \
		$(libledger_la_CPPFLAGS) \
		$(CPPFLAGS) $(AM_CXXFLAGS) $(CXXFLAGS) \
		-g -o $@ $(srcdir)/src/system.hh
endif

######################################################################

bin_PROGRAMS	= ledger

ledger_CPPFLAGS = $(libledger_la_CPPFLAGS)
ledger_SOURCES	= src/main.cc
ledger_LDADD	= $(LIBOBJS) libamounts.la libledger.la
ledger_LDFLAGS	= -static

info_TEXINFOS   = doc/ledger.texi

dist_lisp_LISP  = lisp/ledger.el lisp/timeclock.el

DISTCLEANFILES  = ledger.elc timeclock.elc

######################################################################

if HAVE_BOOST_PYTHON

noinst_PROGRAMS = amounts.so
nodist_amounts_so_SOURCES = amounts.c

CLEANFILES += amounts.so

clean-local:
	rm -fr build

PYLIBS = amounts gmp

amounts.so: python/amounts.cc libamounts.la
	CFLAGS="$(CPPFLAGS) $(DEFAULT_INCLUDES) $(libamounts_la_CPPFLAGS)" \
	LDFLAGS="$(LDFLAGS) -L. -L.libs" \
	PYLIBS="$(PYLIBS)" SRCDIR="$(srcdir)" \
	    python $(srcdir)/python/setup.py build --build-lib=.

install-exec-hook:
	CFLAGS="$(CPPFLAGS) $(DEFAULT_INCLUDES) $(libamounts_la_CPPFLAGS)" \
	LDFLAGS="$(LDFLAGS) -L. -L.libs" \
	PYLIBS="$(PYLIBS)" SRCDIR="$(srcdir)" \
	    python $(srcdir)/python/setup.py install --prefix=$(prefix)

endif

######################################################################

TESTS = UnitTests RegressionTests
if HAVE_BOOST_PYTHON
TESTS += PyUnitTests
endif

check_PROGRAMS = $(TESTS)

nodist_UnitTests_SOURCES = test/UnitTests.cc \
	\
	test/utility/t_utils.cc      \
	test/utility/t_times.cc      \
	test/numerics/t_commodity.cc \
	test/numerics/t_amount.cc    \
	test/numerics/t_balance.cc   \
	test/numerics/t_expr.cc

UnitTests_CPPFLAGS = -I$(srcdir)/test $(libledger_la_CPPFLAGS)
UnitTests_LDFLAGS  = $(LIBADD_DL)
UnitTests_LDADD    = $(lib_LTLIBRARIES) -lcppunit

nodist_PyUnitTests_SOURCES = python/test/PyUnitTests.py

# jww (2007-05-10): This rule will not be triggered on systems that
# define an EXEEXT.
PyUnitTests: $(srcdir)/python/test/PyUnitTests.py
	cat $(srcdir)/python/test/PyUnitTests.py \
	    | sed "s/%srcdir%/$(ESC_srcdir)/g" \
	    | sed "s/%builddir%/$(ESC_builddir)/g" > $@
	chmod 755 $@

RegressionTests_SOURCES = test/regress.py

RegressionTests: $(srcdir)/test/regress.py
	echo "python $(srcdir)/test/regress.py $(top_builddir)/ledger$(EXEEXT) $(srcdir)/test/regress" > $@
	chmod 755 $@

fullcheck: check
	MallocGuardEdges=1	 \
	MallocScribble=1	 \
	MallocPreScribble=1	 \
	MallocCheckHeapStart=100 \
	MallocCheckHeapEach=100	 \
	DYLD_INSERT_LIBRARIES=/usr/lib/libgmalloc.dylib \
	$(srcdir)/valgrind.sh $(top_builddir)/UnitTests$(EXEEXT) --verify

######################################################################

DISTCLEANFILES += Doxyfile.gen

alldocs: doc/ledger.info doc/ledger.pdf doxygen-docs

$(top_builddir)/Doxyfile.gen: $(srcdir)/doc/Doxyfile
	cat $(srcdir)/doc/Doxyfile \
	    | sed "s/%srcdir%/$(ESC_srcdir)/g" \
	    | sed "s/%builddir%/$(ESC_builddir)/g" > $@

doxygen: $(top_builddir)/Doxyfile.gen
	doxygen $(top_builddir)/Doxyfile.gen

# Makefile.am ends here
