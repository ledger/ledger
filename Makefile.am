lib_LTLIBRARIES = libledger.la
libledger_la_SOURCES = amount.cc autoxact.cc balance.cc binary.cc config.cc \
	datetime.cc format.cc journal.cc option.cc parser.cc qif.cc quotes.cc \
	textual.cc valexpr.cc value.cc walk.cc
if READ_GNUCASH
libledger_la_SOURCES += gnucash.cc
endif
if DEBUG
libledger_la_CXXFLAGS = -DDEBUG_LEVEL=4
libledger_la_SOURCES += debug.cc
endif
libledger_la_LDFLAGS = -version-info 2:0

######################################################################

bin_PROGRAMS = ledger
if DEBUG
ledger_CXXFLAGS = -DDEBUG_LEVEL=4
endif
ledger_SOURCES = main.cc
ledger_LDADD = $(LIBOBJS) libledger.la

nobase_include_HEADERS = \
	amount.h	 \
	autoxact.h	 \
	balance.h	 \
	binary.h	 \
	config.h	 \
	datetime.h	 \
	debug.h		 \
	error.h		 \
	format.h	 \
	gnucash.h	 \
	ledger.h	 \
	option.h	 \
	parser.h	 \
	qif.h		 \
	quotes.h	 \
	textual.h	 \
	valexpr.h	 \
	value.h		 \
	walk.h
info_TEXINFOS = ledger.texi

######################################################################

if HAVE_PYTHON
if HAVE_BOOST_PYTHON

noinst_LIBRARIES = libledger_bpy.a
libledger_bpy_a_SOURCES = amount.cc autoxact.cc balance.cc binary.cc config.cc \
	datetime.cc format.cc journal.cc option.cc parser.cc qif.cc quotes.cc \
	textual.cc valexpr.cc value.cc walk.cc
libledger_bpy_a_CXXFLAGS = -DUSE_BOOST_PYTHON=1
if READ_GNUCASH
libledger_bpy_a_SOURCES += gnucash.cc
endif
if DEBUG
libledger_bpy_a_CXXFLAGS += -DDEBUG_LEVEL=4
libledger_bpy_a_SOURCES += debug.cc
endif

bin_PROGRAMS += ledger.so

ledger.so: python.cc libledger_bpy.a
	CFLAGS="$(CPPFLAGS) -L." python setup.py build --build-lib=.

install-exec-hook:
	CFLAGS="$(CPPFLAGS) -L." python setup.py install --prefix=$(DESTDIR)

endif
endif
