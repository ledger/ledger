noinst_LIBRARIES = libledger.a
libledger_a_SOURCES = account.cc amount.cc autoxact.cc balance.cc binary.cc \
	config.cc datetime.cc format.cc ledger.cc option.cc parser.cc qif.cc \
	quotes.cc textual.cc valexpr.cc value.cc walk.cc
if DEBUG
libledger_a_CXXFLAGS = -DDEBUG_LEVEL=4
libledger_a_SOURCES += debug.cc
endif
if READ_GNUCASH
libledger_a_SOURCES += gnucash.cc
endif

bin_PROGRAMS = ledger
if DEBUG
ledger_CXXFLAGS = -DDEBUG_LEVEL=4 -DDO_CLEANUP
endif
ledger_SOURCES = main.cc
ledger_LDADD = $(LIBOBJS) libledger.a

info_TEXINFOS = ledger.texi
