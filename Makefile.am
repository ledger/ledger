noinst_LIBRARIES = libledger.a
libledger_a_CXXFLAGS = -DDEBUG_LEVEL=4 -DDO_CLEANUP
libledger_a_SOURCES = account.cc amount.cc autoxact.cc balance.cc binary.cc \
	config.cc datetime.cc debug.cc format.cc ledger.cc option.cc \
	parser.cc qif.cc quotes.cc textual.cc valexpr.cc value.cc walk.cc

bin_PROGRAMS = ledger
ledger_CXXFLAGS = -DDEBUG_LEVEL=4 -DDO_CLEANUP
ledger_SOURCES = main.cc
ledger_LDADD = $(LIBOBJS) libledger.a

info_TEXINFOS = ledger.texi
