EXTRA_DIST = LICENSE scripts ledger.vim sample.dat

lib_LTLIBRARIES = libamounts.la libledger.la

libamounts_la_CXXFLAGS =
libamounts_la_SOURCES = \
	amount.cc      \
	balance.cc     \
	datetime.cc    \
	value.cc
if HAVE_BOOST_PYTHON
libamounts_la_CXXFLAGS += -DUSE_BOOST_PYTHON=1
endif
if DEBUG
libamounts_la_CXXFLAGS += -DDEBUG_LEVEL=4
libamounts_la_SOURCES  += debug.cc
endif

libledger_la_CXXFLAGS =
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
	quotes.cc      \
	reconcile.cc   \
	report.cc      \
	startup.cc     \
	textual.cc     \
	valexpr.cc     \
	walk.cc        \
	xml.cc
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
endif
libledger_la_LDFLAGS = -release 2.6.2

pkginclude_HEADERS = \
	acconf.h     \
		     \
	amount.h     \
	balance.h    \
	datetime.h   \
	value.h      \
	debug.h	     \
	util.h	     \
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
	ofx.h	     \
	quotes.h     \
	reconcile.h  \
	report.h     \
	textual.h    \
	timing.h     \
	valexpr.h    \
	walk.h       \
	xml.h

######################################################################

bin_PROGRAMS = ledger
ledger_CXXFLAGS =
ledger_SOURCES = main.cc
ledger_LDADD = $(LIBOBJS) libamounts.la libledger.la
if HAVE_EXPAT
ledger_CXXFLAGS += -DHAVE_EXPAT=1
endif
if HAVE_XMLPARSE
ledger_CXXFLAGS += -DHAVE_XMLPARSE=1
endif
if HAVE_LIBOFX
ledger_CXXFLAGS += -DHAVE_LIBOFX=1
endif
if DEBUG
ledger_CXXFLAGS += -DDEBUG_LEVEL=4
endif
ledger_LDFLAGS = -static	# for the sake of command-line speed

info_TEXINFOS = ledger.texi

######################################################################

lisp_LISP = ledger.el timeclock.el
dist_lisp_LISP = ledger.el timeclock.el

######################################################################

EXTRA_DIST += setup.py

if HAVE_BOOST_PYTHON

noinst_PROGRAMS    = amounts.so
amounts_so_SOURCES = amounts.cc fdstream.hpp

amounts.so: amounts.cc libamounts.la
	CFLAGS="$(CPPFLAGS) -I$(srcdir) $(libamounts_la_CPPFLAGS) $(AM_CXXFLAGS) $(CXXFLAGS)" \
	LDFLAGS="$(LDFLAGS) -L$(top_builddir) -L$(top_builddir)/.libs" \
	ARCHFLAGS="$(ARCHFLAGS)" SRCDIR="$(srcdir)" \
	    python setup.py build --build-lib=$(top_builddir)

install-exec-hook:
	CFLAGS="$(CPPFLAGS) -I$(srcdir) $(libamounts_la_CPPFLAGS) $(AM_CXXFLAGS) $(CXXFLAGS)" \
	LDFLAGS="$(LDFLAGS) -L$(top_builddir) -L$(top_builddir)/.libs" \
	ARCHFLAGS="$(ARCHFLAGS)" SRCDIR="$(srcdir)" \
	    python setup.py install --prefix=$(prefix) --root=$(DESTDIR)/

endif

######################################################################

TESTS = RegressionTests

DISTCLEANFILES = RegressionTests

RegressionTests:
	echo "exit 0" > $@
	chmod 755 $@

######################################################################

copy-sources:
	-mkdir /tmp/ledger
	rsync -av --delete $(srcdir)/.git/ /tmp/ledger/.git/
	(cd /tmp/ledger; git reset --hard HEAD; git clean -x -d -f)

release: copy-sources
	(cd /tmp/ledger; ./acprep && \
	 make -j3 \
	    CPPFLAGS="-I/usr/local/include -I/opt/local/include" \
	    LDFLAGS="-L/usr/local/lib -L/opt/local/lib" \
	    DISTCHECK_CONFIGURE_FLAGS="--disable-dependency-tracking")

release-distcheck: copy-sources
	(cd /tmp/ledger; ./acprep && \
	 make -j3 distcheck \
	    CPPFLAGS="-I/usr/local/include -I/opt/local/include" \
	    LDFLAGS="-L/usr/local/lib -L/opt/local/lib" \
	    DISTCHECK_CONFIGURE_FLAGS="--disable-dependency-tracking")

# Makefile.am ends here
