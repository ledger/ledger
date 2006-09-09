lib_LTLIBRARIES = libledger.la

libledger_la_CXXFLAGS =
libledger_la_SOURCES = \
	amount.cc      \
	balance.cc     \
	datetime.cc    \
	value.cc       \
	valexpr.cc     \
	mask.cc        \
                       \
	binary.cc      \
	config.cc      \
	csv.cc         \
	derive.cc      \
	emacs.cc       \
	format.cc      \
	journal.cc     \
	option.cc      \
	parser.cc      \
	qif.cc	       \
	quotes.cc      \
	reconcile.cc   \
	repitem.cc     \
	report.cc      \
	session.cc     \
	startup.cc     \
	textual.cc     \
	trace.cc       \
	transform.cc   \
	util.cc        \
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
if HAVE_BOOST_PYTHON
libledger_la_CXXFLAGS += -DUSE_BOOST_PYTHON=1
libledger_la_SOURCES += py_eval.cc
endif
if DEBUG
libledger_la_CXXFLAGS += -DDEBUG_LEVEL=4
libledger_la_SOURCES  += debug.cc
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
	py_eval.h    \
	qif.h	     \
	quotes.h     \
	reconcile.h  \
	repitem.h    \
	report.h     \
	session.h    \
	textual.h    \
	timing.h     \
	trace.h      \
	transform.h  \
	valexpr.h    \
	walk.h       \
	xml.h

######################################################################

bin_PROGRAMS = ledger
ledger_CXXFLAGS =
ledger_SOURCES = main.cc
ledger_LDADD = $(LIBOBJS) libledger.la
if HAVE_EXPAT
ledger_CXXFLAGS += -DHAVE_EXPAT=1
endif
if HAVE_XMLPARSE
ledger_CXXFLAGS += -DHAVE_XMLPARSE=1
endif
if HAVE_LIBOFX
ledger_CXXFLAGS += -DHAVE_LIBOFX=1
endif
if HAVE_BOOST_PYTHON
ledger_CXXFLAGS += -DUSE_BOOST_PYTHON=1
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

noinst_PROGRAMS = ledger.so

ledger.so: pyledger.cc libledger.la
	CFLAGS="$(CPPFLAGS)" LDFLAGS="$(LDFLAGS) -L. -L.libs" \
	    python setup.py build --build-lib=.

install-exec-hook:
	CFLAGS="$(CPPFLAGS)" LDFLAGS="$(LDFLAGS) -L. -L.libs" \
	    python setup.py install --prefix=$(prefix)

endif

######################################################################

TESTS = alltests

CXXTEST_DIR = /usr/local/cxxtest
TESTGEN = $(CXXTEST_DIR)/cxxtestgen.py
TESTSUITES = tests/*.h

AM_CXXFLAGS =
if HAVE_EXPAT
AM_CXXFLAGS += -DHAVE_EXPAT=1
endif
if HAVE_XMLPARSE
AM_CXXFLAGS += -DHAVE_XMLPARSE=1
endif
if HAVE_LIBOFX
AM_CXXFLAGS += -DHAVE_LIBOFX=1
endif
if DEBUG
AM_CXXFLAGS += -DDEBUG_LEVEL=4
endif

alltests.cc: $(TESTSUITES)
	test -f $(TESTGEN) && python $(TESTGEN) -o $@ --error-printer $(TESTSUITES)

alltests: alltests.cc ledger
	$(CXXCOMPILE) -I$(CXXTEST_DIR) -lexpat -lgmp -lpcre -o $@ \
		alltests.cc -L. -L.libs -lledger

runtests: alltests
	LD_LIBRARY_PATH=.libs ./alltests && tests/regress && tests/regtest

verify: runtests
	python tests/runtests.py

VALEXPR_OBJS = amount.o balance.o datetime.o value.o mask.o debug.o py_eval.o
valexpr: $(VALEXPR_OBJS) valexpr.cc valexpr.h session.h
	$(CXXCOMPILE) -DTEST -DUSE_BOOST_PYTHON=1 -o $@ \
		valexpr.cc $(VALEXPR_OBJS) $(LIBS)

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
