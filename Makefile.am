lib_LIBRARIES = libledger.a
libledger_a_CXXFLAGS =
libledger_a_SOURCES = \
	amount.cc      \
	balance.cc     \
	binary.cc      \
	config.cc      \
	datetime.cc    \
	derive.cc      \
	format.cc      \
	journal.cc     \
	mask.cc        \
	option.cc      \
	parser.cc      \
	qif.cc	       \
	quotes.cc      \
	reconcile.cc   \
	textual.cc     \
	valexpr.cc     \
	value.cc       \
	walk.cc
if HAVE_XMLPARSE
libledger_a_CXXFLAGS += -DHAVE_XMLPARSE=1
libledger_a_SOURCES += gnucash.cc xml.cc
endif
if HAVE_LIBOFX
libledger_a_CXXFLAGS += -DHAVE_LIBOFX=1
libledger_a_SOURCES += ofx.cc
endif
if HAVE_BOOST_PYTHON
libledger_a_CXXFLAGS += -DUSE_BOOST_PYTHON=1
libledger_a_SOURCES  += py_eval.cc
endif
if DEBUG
libledger_a_CXXFLAGS += -DDEBUG_LEVEL=4
libledger_a_SOURCES  += debug.cc
endif

pkginclude_HEADERS = \
	acconf.h     \
	amount.h     \
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
	mask.h       \
	option.h     \
	parser.h     \
	py_eval.h    \
	pyledger.h   \
	qif.h	     \
	quotes.h     \
	reconcile.h  \
	textual.h    \
	timing.h     \
	util.h	     \
	valexpr.h    \
	value.h      \
	walk.h       \
	xml.h

######################################################################

bin_PROGRAMS = ledger
ledger_CXXFLAGS =
ledger_SOURCES = main.cc
if HAVE_BOOST_PYTHON
ledger_CXXFLAGS += -DUSE_BOOST_PYTHON=1
ledger_LDADD = $(LIBOBJS) libledger.a -lboost_python -lpython$(PYTHON_VERSION)
else
ledger_LDADD = $(LIBOBJS) libledger.a
endif
if HAVE_XMLPARSE
ledger_CXXFLAGS += -DHAVE_XMLPARSE=1
ledger_LDADD += -lxmlparse -lxmltok
endif
if HAVE_LIBOFX
ledger_CXXFLAGS += -DHAVE_LIBOFX=1
ledger_LDADD += -lofx
endif
if DEBUG
ledger_CXXFLAGS += -DDEBUG_LEVEL=4
endif

info_TEXINFOS = ledger.texi

######################################################################

if HAVE_BOOST_PYTHON

noinst_PROGRAMS = ledger.so

if HAVE_XMLPARSE
HAVE_XMLPARSE_VALUE = true
else
HAVE_XMLPARSE_VALUE = false
endif

ledger.so: py_eval.cc libledger.a
	CFLAGS="$(CPPFLAGS)" LDFLAGS="$(LDFLAGS) -L." \
	HAVE_XMLPARSE="$(HAVE_XMLPARSE_VALUE)" \
	    python setup.py build --build-lib=.

install-exec-hook:
	CFLAGS="$(CPPFLAGS)" LDFLAGS="$(LDFLAGS) -L." \
	HAVE_XMLPARSE="$(HAVE_XMLPARSE_VALUE)" \
	    python setup.py install --prefix=$(prefix)

endif

all-clean: maintainer-clean
	rm -fr	*~ .*~ .\#* *.html *.info *.pdf *.a *.so *.o *.lo *.la \
		*.elc *.aux *.cp *.fn *.ky *.log *.pg *.toc *.tp *.vr *.pyc \
		.gdb_history gmon.out h out TAGS ledger valexpr .deps \
		build AUTHORS COPYING INSTALL Makefile aclocal.m4 autom4te \
		acconf.h acconf.h.in  config.guess config.sub configure \
		depcomp install-sh missing stamp texinfo.tex Makefile.in
