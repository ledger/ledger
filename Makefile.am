lib_LTLIBRARIES = libledger.la
libledger_la_CXXFLAGS =
libledger_la_SOURCES = \
	amount.cc      \
	balance.cc     \
	binary.cc      \
	config.cc      \
	datetime.cc    \
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
	startup.cc     \
	textual.cc     \
	valexpr.cc     \
	value.cc       \
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
libledger_la_SOURCES  += debug.cc
endif
libledger_la_LDFLAGS = -version-info 3:0

pkginclude_HEADERS = \
	acconf.h     \
	amount.h     \
	balance.h    \
	binary.h     \
	config.h     \
	datetime.h   \
	debug.h      \
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
ledger_LDADD = $(LIBOBJS) libledger.la
if HAVE_EXPAT
ledger_CXXFLAGS += -DHAVE_EXPAT=1
ledger_LDADD += -lexpat
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
ledger_LDFLAGS = -static	# for the sake of command-line speed

info_TEXINFOS = ledger.texi

######################################################################

lisp_LISP = ledger.el timeclock.el
dist_lisp_LISP = ledger.el timeclock.el

######################################################################

all-clean: maintainer-clean
	rm -fr	*~ .*~ .\#* *.html *.info *.pdf *.a *.so *.o *.lo *.la \
		*.elc *.aux *.cp *.fn *.ky *.log *.pg *.toc *.tp *.vr \
		.gdb_history gmon.out h out TAGS ledger valexpr .deps \
		.libs build AUTHORS COPYING INSTALL Makefile acconf.h \
		acconf.h.in aclocal.m4 autom4te config.guess config.sub \
		configure depcomp install-sh libtool ltconfig ltmain.sh \
		missing stamp texinfo.tex Makefile.in mkinstalldirs \
		elisp-comp elc-stamp
