lib_LTLIBRARIES = libledger.la
libledger_la_SOURCES = account.cc amount.cc autoxact.cc balance.cc binary.cc \
	config.cc datetime.cc format.cc ledger.cc option.cc parser.cc qif.cc \
	quotes.cc textual.cc valexpr.cc value.cc walk.cc
if DEBUG
libledger_la_CXXFLAGS = -DDEBUG_LEVEL=4
libledger_la_SOURCES += debug.cc
else
if STANDALONE
libledger_a_CXXFLAGS = -DSGI_STL_USE_SINGLE_CLIENT_ALLOCATOR=1
endif
endif
if READ_GNUCASH
libledger_la_SOURCES += gnucash.cc
endif
libledger_la_LDFLAGS = -version-info 2:0

bin_PROGRAMS = ledger
if DEBUG
ledger_CXXFLAGS = -DDEBUG_LEVEL=4
else
if STANDALONE
ledger_CXXFLAGS = -DSGI_STL_USE_SINGLE_CLIENT_ALLOCATOR=1
endif
endif
ledger_SOURCES = main.cc
ledger_LDADD = $(LIBOBJS) libledger.la

info_TEXINFOS = ledger.texi
