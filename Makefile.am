lib_LTLIBRARIES = libledger.la
libledger_la_CXXFLAGS =
libledger_la_SOURCES = \
	amount.cc      \
	autoxact.cc    \
	balance.cc     \
	binary.cc      \
	config.cc      \
	datetime.cc    \
	derive.cc      \
	format.cc      \
	journal.cc     \
	option.cc      \
	parser.cc      \
	qif.cc	       \
	quotes.cc      \
	textual.cc     \
	valexpr.cc     \
	value.cc       \
	walk.cc
if READ_GNUCASH
libledger_la_SOURCES += gnucash.cc
endif
if HAVE_BOOST_PYTHON
libledger_la_CXXFLAGS += -DUSE_BOOST_PYTHON=1
libledger_la_SOURCES  += python.cc
libledger_la_LIBADD    = $(LIBOBJS) -lboost_python -lpython$(PYTHON_VERSION)
endif
if DEBUG
libledger_la_CXXFLAGS += -DDEBUG_LEVEL=4
libledger_la_SOURCES  += debug.cc
endif
libledger_la_LDFLAGS = -version-info 2:0

pkginclude_HEADERS = \
	acconf.h     \
	amount.h     \
	autoxact.h   \
	balance.h    \
	binary.h     \
	config.h     \
	datetime.h   \
	debug.h      \
	derive.h     \
	error.h      \
	format.h     \
	gnucash.h    \
	journal.h    \
	ledger.h     \
	option.h     \
	parser.h     \
	python.h     \
	qif.h	     \
	quotes.h     \
	textual.h    \
	timing.h     \
	util.h	     \
	valexpr.h    \
	value.h      \
	walk.h

######################################################################

bin_PROGRAMS = ledger
ledger_CXXFLAGS = $(global_CXXFLAGS)
ledger_SOURCES = main.cc
if HAVE_BOOST_PYTHON
ledger_LDADD = $(LIBOBJS) libledger.la -lboost_python -lpython$(PYTHON_VERSION)
else
ledger_LDADD = $(LIBOBJS) libledger.la
endif

info_TEXINFOS = ledger.texi

######################################################################

if HAVE_BOOST_PYTHON

noinst_PROGRAMS = ledger.so

ledger.so: python.cc libledger.la
	CFLAGS="$(CPPFLAGS)" LDFLAGS="$(LDFLAGS) -L. -L.libs" \
	    python setup.py build --build-lib=.

install-exec-hook:
	CFLAGS="$(CPPFLAGS)" LDFLAGS="$(LDFLAGS) -L. -L.libs" \
	    python setup.py install --install-base=$(DESTDIR) --root

endif

all-clean: maintainer-clean
	rm -fr	*~ .*~ .\#* *.html *.info *.pdf *.a *.so *.o *.lo *.la \
		*.elc *.aux *.cp *.fn *.ky *.log *.pg *.toc *.tp *.vr \
		.gdb_history gmon.out h out TAGS ledger valexpr .deps \
		.libs build AUTHORS COPYING INSTALL Makefile acconf.h \
		acconf.h.in aclocal.m4 autom4te config.guess config.sub \
		configure depcomp install-sh libtool ltconfig ltmain.sh \
		missing stamp texinfo.tex Makefile.in
