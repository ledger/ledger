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
	walk.cc
if HAVE_EXPAT
libledger_la_CXXFLAGS += -DHAVE_EXPAT=1
libledger_la_SOURCES += gnucash.cc xml.cc
endif
if HAVE_XMLPARSE
libledger_la_CXXFLAGS += -DHAVE_XMLPARSE=1
libledger_la_SOURCES += gnucash.cc xml.cc
endif
if HAVE_LIBOFX
libledger_la_CXXFLAGS += -DHAVE_LIBOFX=1
libledger_la_SOURCES += ofx.cc
endif
if DEBUG
libledger_la_CXXFLAGS += -DDEBUG_LEVEL=4
endif
libledger_la_LDFLAGS = -release 3.0

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

if HAVE_BOOST_PYTHON

noinst_PROGRAMS = amounts.so

amounts.so: amounts.cc libamounts.la
	CFLAGS="$(CPPFLAGS)" LDFLAGS="$(LDFLAGS) -L. -L.libs" \
	    python setup.py build --build-lib=.

install-exec-hook:
	CFLAGS="$(CPPFLAGS)" LDFLAGS="$(LDFLAGS) -L. -L.libs" \
	    python setup.py install --prefix=$(prefix)

endif

######################################################################

all-clean: maintainer-clean
	rm -fr	*~ .*~ .\#* *.html *.info *.pdf *.a *.so *.o *.lo *.la \
		*.elc *.aux *.cp *.fn *.ky *.log *.pg *.toc *.tp *.vr \
		.gdb_history gmon.out h out TAGS ledger valexpr .deps \
		.libs build AUTHORS COPYING INSTALL Makefile acconf.h \
		acconf.h.in aclocal.m4 autom4te config.guess config.sub \
		configure depcomp install-sh libtool ltconfig ltmain.sh \
		missing stamp texinfo.tex Makefile.in mkinstalldirs \
		elisp-comp elc-stamp py-compile
