lib_LTLIBRARIES = libledger.la
libledger_la_CXXFLAGS =
libledger_la_SOURCES = amount.cc balance.cc value.cc autoxact.cc \
	binary.cc config.cc datetime.cc format.cc journal.cc option.cc \
	parser.cc qif.cc quotes.cc textual.cc valexpr.cc walk.cc
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

bin_PROGRAMS += ledger.so

ledger.so: python.cc libledger.la
	CFLAGS="$(CPPFLAGS)" LDFLAGS="$(LDFLAGS) -L. -L.libs" \
	    python setup.py build --build-lib=.

install-exec-hook:
	CFLAGS="$(CPPFLAGS)" LDFLAGS="$(LDFLAGS) -L. -L.libs" \
	    python setup.py install --prefix=$(DESTDIR)

endif
