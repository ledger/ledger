BUILT_SOURCES =
CLEANFILES    =
EXTRA_DIST    = LICENSE doc test contrib scripts setup.py \
		acprep valgrind.sh version

ESC_srcdir=`echo "$(srcdir)" | sed 's/\//\\\\\//g'`
ESC_builddir=`echo "$(top_builddir)" | sed 's/\//\\\\\//g'`
ESC_distdir=`echo "$(distdir)" | sed 's/\//\\\\\//g'`

dist-hook:
	rm -fr .git

lib_LTLIBRARIES = libamounts.la libledger.la

libamounts_la_CPPFLAGS =
libamounts_la_SOURCES = \
	amount.cc      \
	commodity.cc   \
	balance.cc     \
	value.cc       \
	times.cc       \
	utils.cc
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
libledger_la_SOURCES = \
	mask.cc        \
	token.cc       \
	parser.cc      \
	op.cc          \
	expr.cc        \
	scope.cc       \
	format.cc      \
		       \
	journal.cc     \
	report.cc      \
	session.cc     \
	walk.cc        \
		       \
	textual.cc     \
	binary.cc      \
	csv.cc         \
	emacs.cc       \
	qif.cc	       \
	xml.cc	       \
	derive.cc      \
	reconcile.cc   \
	option.cc
#	quotes.cc      this is currently not being included
if HAVE_EXPAT
libledger_la_SOURCES += gnucash.cc
endif
if HAVE_XMLPARSE
libledger_la_SOURCES += gnucash.cc
endif
if HAVE_LIBOFX
libledger_la_SOURCES += ofx.cc
endif

libledger_la_LDFLAGS = -release 2.7.0

pkginclude_HEADERS = \
	acconf.h     \
		     \
	amount.h     \
	commodity.h  \
	balance.h    \
	balpair.h    \
	value.h      \
	times.h	     \
	utils.h	     \
	error.h      \
		     \
	expr.h       \
	op.h         \
	parser.h     \
	token.h      \
	scope.h      \
	predicate.h  \
	format.h     \
	mask.h       \
		     \
	journal.h    \
	report.h     \
	session.h    \
	walk.h       \
		     \
	textual.h    \
	binary.h     \
	csv.h        \
	emacs.h      \
	gnucash.h    \
	qif.h	     \
	xml.h	     \
		     \
	derive.h     \
	reconcile.h  \
	quotes.h     \
	option.h     \
		     \
	ledger.h

if USE_PCH
nodist_libledger_la_SOURCES = system.hh.gch

BUILT_SOURCES += system.hh.gch
CLEANFILES += system.hh.gch system.hh

$(top_builddir)/system.hh.gch: $(srcdir)/system.hh $(top_builddir)/acconf.h
	echo "#include \"$(srcdir)/system.hh\"" > $(top_builddir)/system.hh
	$(CXX) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) \
		$(libledger_la_CPPFLAGS) \
		$(CPPFLAGS) $(AM_CXXFLAGS) $(CXXFLAGS) \
		-g -o $@ $(srcdir)/system.hh
endif

######################################################################

bin_PROGRAMS	= ledger

ledger_CPPFLAGS = $(libledger_la_CPPFLAGS)
ledger_SOURCES	= main.cc
ledger_LDADD	= $(LIBOBJS) libamounts.la libledger.la
ledger_LDFLAGS	= -static	# for the sake of command-line speed

info_TEXINFOS   = ledger.texi

dist_lisp_LISP  = ledger.el timeclock.el

DISTCLEANFILES  = ledger.elc timeclock.elc

######################################################################

if HAVE_BOOST_PYTHON

noinst_PROGRAMS = amounts.so

CLEANFILES += amounts.so

clean-local:
	rm -fr build

PYLIBS = amounts gmp

amounts.so: amounts.cc libamounts.la
	CFLAGS="$(CPPFLAGS) $(DEFAULT_INCLUDES) $(libamounts_la_CPPFLAGS)" \
	LDFLAGS="$(LDFLAGS) -L. -L.libs" \
	PYLIBS="$(PYLIBS)" SRCDIR="$(srcdir)" \
	    python $(srcdir)/setup.py build --build-lib=.

install-exec-hook:
	CFLAGS="$(CPPFLAGS) $(DEFAULT_INCLUDES) $(libamounts_la_CPPFLAGS)" \
	LDFLAGS="$(LDFLAGS) -L. -L.libs" \
	PYLIBS="$(PYLIBS)" SRCDIR="$(srcdir)" \
	    python $(srcdir)/setup.py install --prefix=$(prefix)

endif

######################################################################

TESTS = UnitTests
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

nodist_PyUnitTests_SOURCES = test/python/PyUnitTests.py

# jww (2007-05-10): This rule will not be triggered on systems that
# define an EXEEXT.
PyUnitTests: $(srcdir)/test/python/PyUnitTests.py
	cat $(srcdir)/test/python/PyUnitTests.py \
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

alldocs: ledger.info ledger.pdf doxygen-docs

$(top_builddir)/Doxyfile.gen: $(srcdir)/Doxyfile
	cat $(srcdir)/Doxyfile \
	    | sed "s/%srcdir%/$(ESC_srcdir)/g" \
	    | sed "s/%builddir%/$(ESC_builddir)/g" > $@

doxygen-docs: $(top_builddir)/Doxyfile.gen
	doxygen $(top_builddir)/Doxyfile.gen

######################################################################

clean-backupfiles:
	rm -fr *~  \
	       .*~ \
	       .\#*

clean-documentation:
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
	       *.vr

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
	rm -fr Makefile	     \
	       Makefile.in   \
	       acconf.h	     \
	       acconf.h.in   \
	       aclocal.m4    \
	       autom4te.cache \
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
	       ylwrap

scour:  maintainer-clean    \
	clean-buildproducts \
	clean-backupfiles   \
	clean-debugdata     \
	clean-documentation \
	clean-autoconf
