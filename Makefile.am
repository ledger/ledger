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
if HAVE_BOOST_PYTHON
libamounts_la_CPPFLAGS += -DUSE_BOOST_PYTHON=1
endif
if DEBUG
libamounts_la_CPPFLAGS += -DDEBUG_MODE
endif

libledger_la_CPPFLAGS =
libledger_la_SOURCES = \
	binary.cc      \
	config.cc      \
	csv.cc         \
	derive.cc      \
	emacs.cc       \
	format.cc      \
	journal.cc     \
	mask.cc        \
	option.cc      \
	parser.cc      \
	qif.cc	       \
	reconcile.cc   \
	report.cc      \
	startup.cc     \
	textual.cc     \
	valexpr.cc     \
	walk.cc        \
	xml.cc
#	quotes.cc      this is currently not being included
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
libledger_la_CPPFLAGS += -DDEBUG_MODE
endif
libledger_la_LDFLAGS = -release 2.6.0.90

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
		     \
	binary.h     \
	config.h     \
	csv.h        \
	derive.h     \
	emacs.h      \
	error.h      \
	format.h     \
	gnucash.h    \
	journal.h    \
	ledger.h     \
	mask.h       \
	option.h     \
	parser.h     \
	qif.h	     \
	quotes.h     \
	reconcile.h  \
	report.h     \
	textual.h    \
	valexpr.h    \
	walk.h       \
	xml.h

if USE_PCH
nodist_libledger_la_SOURCES = system.hh.gch

BUILT_SOURCES += system.hh.gch
CLEANFILES += system.hh.gch system.hh

$(top_builddir)/system.hh.gch: $(srcdir)/src/utility/system.hh acconf.h
	echo "#include \"src/utility/system.hh\"" > $(top_builddir)/system.hh
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) $(libledger_la_CPPFLAGS) \
		-o $@ $(srcdir)/src/utility/system.hh
endif

######################################################################

bin_PROGRAMS = ledger
ledger_CPPFLAGS =
ledger_SOURCES = main.cc
ledger_LDADD = $(LIBOBJS) libamounts.la libledger.la
if HAVE_EXPAT
ledger_CPPFLAGS += -DHAVE_EXPAT=1
endif
if HAVE_XMLPARSE
ledger_CPPFLAGS += -DHAVE_XMLPARSE=1
endif
if HAVE_LIBOFX
ledger_CPPFLAGS += -DHAVE_LIBOFX=1
endif
if DEBUG
ledger_CPPFLAGS += -DDEBUG_MODE
endif
ledger_LDFLAGS = -static	# for the sake of command-line speed

info_TEXINFOS = ledger.texi

######################################################################

dist_lisp_LISP = ledger.el timeclock.el

DISTCLEANFILES = ledger.elc timeclock.elc

######################################################################

if HAVE_BOOST_PYTHON

noinst_PROGRAMS = amounts.so

CLEANFILES += amounts.so

clean-local:
	rm -fr build

PYLIBS = amounts gmp

amounts.so: amounts.cc libamounts.la
	CFLAGS="$(CPPFLAGS) -I$(srcdir) $(libamounts_la_CPPFLAGS)" \
	LDFLAGS="$(LDFLAGS) -L. -L.libs" \
	PYLIBS="$(PYLIBS)" SRCDIR="$(srcdir)" \
	    python $(srcdir)/setup.py build --build-lib=.

install-exec-hook:
	CFLAGS="$(CPPFLAGS) -I$(srcdir) $(libamounts_la_CPPFLAGS)" \
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
	test/numerics/t_balance.cc

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
	(cd doc; \
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

all-clean: maintainer-clean    \
	   clean-buildproducts \
	   clean-backupfiles   \
	   clean-debugdata     \
	   clean-documentation \
	   clean-autoconf
