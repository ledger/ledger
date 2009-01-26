VERSION = 3.0

if HAVE_GDTOA
SUBDIRS    = lib/gdtoa
endif
EXTRA_DIST = autogen.sh contrib

lib_LTLIBRARIES =    \
	libledger_util.la   \
	libledger_math.la   \
	libledger_expr.la   \
	libledger_data.la   \
	libledger_parse.la  \
	libledger_report.la \
	libledger_extra.la

lib_cppflags  = -I$(srcdir)/src -I$(srcdir)/lib \
		-I$(srcdir)/lib/utfcpp/source \
		-I$(srcdir)/lib/irrxml/src
if HAVE_GDTOA
lib_cppflags += -I$(top_builddir)/lib/gdtoa -I$(srcdir)/lib/gdtoa
endif
if HAVE_LIBOFX
lib_cppflags += -DHAVE_LIBOFX=1
endif
if DEBUG
lib_cppflags += -DDEBUG_MODE
endif

libledger_util_la_SOURCES =	 \
	src/utils.cc	 \
	src/times.cc	 \
	src/mask.cc	 \
	src/binary.cc	 \
	lib/sha1.cpp

libledger_util_la_CPPFLAGS = $(lib_cppflags)
libledger_util_la_LDFLAGS  = -release $(VERSION).0

libledger_math_la_SOURCES =	 \
	src/amount.cc    \
	src/commodity.cc \
	src/balance.cc   \
	src/balpair.cc   \
	src/value.cc

libledger_math_la_CPPFLAGS = $(lib_cppflags)
libledger_math_la_LDFLAGS  = -release $(VERSION).0

libledger_expr_la_SOURCES =	 \
	src/token.cc     \
	src/parser.cc    \
	src/op.cc        \
	src/expr.cc      \
	src/scope.cc     \
	src/format.cc    \
	src/option.cc

libledger_expr_la_CPPFLAGS  = $(lib_cppflags)
libledger_expr_la_LDFLAGS    = -release $(VERSION).0

libledger_data_la_SOURCES  =		  \
	src/item.cc		  \
	src/xact.cc		  \
	src/entry.cc		  \
	src/account.cc		  \
	src/journal.cc		  \
	src/iterators.cc	  \
	src/compare.cc

libledger_data_la_CPPFLAGS = $(lib_cppflags)
libledger_data_la_LDFLAGS  = -release $(VERSION).0

libledger_parse_la_SOURCES  =		  \
	src/textual.cc		  \
	src/cache.cc		  \
	src/xml.cc		  \
	src/csv.cc		  \
	src/emacs.cc		  \
	src/qif.cc		  \
	src/gnucash.cc		  \
	lib/irrxml/src/irrXML.cpp
if HAVE_LIBOFX
libledger_parse_la_SOURCES += src/ofx.cc
endif

libledger_parse_la_CPPFLAGS = $(lib_cppflags)
libledger_parse_la_LDFLAGS  = -release $(VERSION).0

libledger_report_la_SOURCES  =		  \
	src/session.cc		  \
	src/report.cc		  \
	src/filters.cc		  \
	src/output.cc		  \
	src/help.cc

libledger_report_la_CPPFLAGS = $(lib_cppflags)
libledger_report_la_LDFLAGS  = -release $(VERSION).0

libledger_extra_la_SOURCES  =		  \
	src/derive.cc		  \
	src/reconcile.cc	  \
	src/quotes.cc

libledger_extra_la_CPPFLAGS = $(lib_cppflags)
libledger_extra_la_LDFLAGS  = -release $(VERSION).0

pkginclude_HEADERS =			   \
	acconf.h			   \
	src/system.hh			   \
	src/utils.h			   \
	src/flags.h			   \
	src/hooks.h			   \
	src/pushvar.h			   \
	src/error.h			   \
	src/times.h			   \
	src/mask.h			   \
	src/binary.h			   \
					   \
	src/amount.h			   \
	src/commodity.h			   \
	src/balance.h			   \
	src/balpair.h			   \
	src/value.h			   \
					   \
	src/token.h			   \
	src/parser.h			   \
	src/op.h			   \
	src/expr.h			   \
	src/scope.h			   \
	src/predicate.h			   \
	src/format.h			   \
	src/option.h			   \
					   \
	src/item.h			   \
	src/xact.h			   \
	src/entry.h			   \
	src/account.h			   \
	src/journal.h			   \
	src/iterators.h			   \
	src/compare.h			   \
					   \
	src/textual.h			   \
	src/cache.h			   \
	src/xml.h			   \
	src/csv.h			   \
	src/emacs.h			   \
	src/qif.h			   \
	src/gnucash.h			   \
	src/ofx.h			   \
					   \
	src/session.h			   \
	src/report.h			   \
	src/handler.h			   \
	src/filters.h			   \
	src/output.h			   \
	src/help.h			   \
					   \
	src/derive.h			   \
	src/reconcile.h			   \
	src/quotes.h			   \
					   \
	src/ledger.h			   \
					   \
	python/pyledger.h		   \
	python/pyinterp.h                  \
					   \
	lib/fdstream.h			   \
	lib/sha1.h			   \
					   \
	lib/utfcpp/source/utf8.h	   \
	lib/utfcpp/source/utf8/checked.h   \
	lib/utfcpp/source/utf8/core.h	   \
	lib/utfcpp/source/utf8/unchecked.h \
					   \
	lib/irrxml/src/CXMLReaderImpl.h	   \
	lib/irrxml/src/fast_atof.h	   \
	lib/irrxml/src/heapsort.h	   \
	lib/irrxml/src/irrArray.h	   \
	lib/irrxml/src/irrString.h	   \
	lib/irrxml/src/irrTypes.h	   \
	lib/irrxml/src/irrXML.h

CLEANFILES =

if USE_PCH
nodist_libledger_util_la_SOURCES = src/system.hh.gch

BUILT_SOURCES = src/system.hh.gch
CLEANFILES   += src/system.hh.gch

$(srcdir)/src/system.hh.gch: $(srcdir)/src/system.hh $(top_builddir)/acconf.h
	$(CXX) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) \
	    $(lib_cppflags) $(CPPFLAGS) $(AM_CXXFLAGS) $(CXXFLAGS) \
	    -g -o $@ $(srcdir)/src/system.hh
endif

######################################################################

bin_PROGRAMS	= ledger

ledger_CPPFLAGS	= $(lib_cppflags)
if HAVE_BOOST_PYTHON
ledger_CPPFLAGS	+= -DHAVE_BOOST_PYTHON=1 -I$(srcdir)/python
endif
ledger_SOURCES	= src/main.cc
ledger_LDADD	= $(LIBOBJS) $(lib_LTLIBRARIES)
if HAVE_GDTOA
ledger_LDADD   += lib/gdtoa/libgdtoa.la
endif
if HAVE_BOOST_PYTHON
ledger_LDADD   += libledger_python.la
endif
ledger_LDFLAGS	= 

info_TEXINFOS	= doc/ledger.texi

dist_lisp_LISP	= lisp/ledger.el lisp/timeclock.el
ELCFILES	=
DISTCLEANFILES	= ledger.elc timeclock.elc
CLEANFILES     += TAGS

all_sources     = $(libledger_util_la_SOURCES)	  \
		  $(libledger_math_la_SOURCES)	  \
		  $(libledger_expr_la_SOURCES)	  \
		  $(libledger_data_la_SOURCES)	  \
		  $(libledger_parse_la_SOURCES)  \
		  $(libledger_report_la_SOURCES) \
		  $(libledger_extra_la_SOURCES)  \
		  $(libledger_python_la_SOURCES) \
		  python/pyledger.cc

all_files       = $(all_sources) $(pkginclude_HEADERS)

TAGS: $(all_sources)
	@etags $(all_sources)
	@echo TAGS rebuilt.

######################################################################

if HAVE_BOOST_PYTHON

lib_LTLIBRARIES += libledger_python.la

libledger_python_la_SOURCES =       \
	python/pyutils.h       \
	python/pyfstream.h     \
	python/py_utils.cc     \
	python/py_times.cc     \
	python/py_amount.cc    \
	python/py_commodity.cc \
	python/py_value.cc     \
	python/pyinterp.cc     \
	python/setup.py

libledger_python_la_CPPFLAGS = $(lib_cppflags) -I$(srcdir)/python

pyexec_PROGRAMS = ledger.so

clean-local:
	rm -fr build

uninstall-hook:
	rm -f $(DESTDIR)$(libdir)/Ledger-$(VERSION)-py$(PYTHON_VERSION).egg-info

if HAVE_GDTOA
install-exec-hook:
	BUILD_DIR=`cd $(top_builddir); pwd`; \
	SRC_DIR=`cd $(srcdir); pwd`; \
	(cd $(srcdir); \
	 CFLAGS="$(CPPFLAGS) -I$(srcdir) -I$$BUILD_DIR -I$$BUILD_DIR/lib/gdtoa $(libledger_python_la_CPPFLAGS) $(AM_CXXFLAGS) $(CXXFLAGS)" \
	 LDFLAGS="$(LDFLAGS) -L$$BUILD_DIR -L$$BUILD_DIR/.libs -L$$BUILD_DIR/lib/gdtoa -L$$BUILD_DIR/lib/gdtoa/.libs" \
	 ARCHFLAGS="$(ARCHFLAGS)" PYLIBS="$(PYLIBS)" VERSION="$(VERSION)" \
	     SRCDIR="$$SRC_DIR" $(PYTHON) python/setup.py install \
		--skip-build \
		--root=$(DESTDIR)/ \
		--prefix=$(prefix) \
		--install-lib=$(libdir) \
		--install-headers=$(pkgincludedir) \
		--install-data=$(datadir))
else
install-exec-hook:
	BUILD_DIR=`cd $(top_builddir); pwd`; \
	SRC_DIR=`cd $(srcdir); pwd`; \
	(cd $(srcdir); \
	 CFLAGS="$(CPPFLAGS) -I$(srcdir) -I$$BUILD_DIR $(libledger_python_la_CPPFLAGS) $(AM_CXXFLAGS) $(CXXFLAGS)" \
	 LDFLAGS="$(LDFLAGS) -L$$BUILD_DIR -L$$BUILD_DIR/.libs" \
	 ARCHFLAGS="$(ARCHFLAGS)" PYLIBS="$(PYLIBS)" VERSION="$(VERSION)" \
	     SRCDIR="$$SRC_DIR" $(PYTHON) python/setup.py install \
		--skip-build \
		--root=$(DESTDIR)/ \
		--prefix=$(prefix) \
		--install-lib=$(libdir) \
		--install-headers=$(pkgincludedir) \
		--install-data=$(datadir))
endif

ledger_so_SOURCES      = $(all_sources)
ledger_so_DEPENDENCIES = $(lib_LTLIBRARIES)

PYLIBS  = ledger_util ledger_math ledger_expr \
	  ledger_data ledger_parse	      \
	  ledger_report ledger_extra	      \
	  ledger_python			      \
	  gmp				      \
	  boost_system$(BOOST_SUFFIX)	      \
	  boost_date_time$(BOOST_SUFFIX)      \
          boost_filesystem$(BOOST_SUFFIX)     \
          boost_regex$(BOOST_SUFFIX)	      \
          boost_python$(BOOST_SUFFIX)

if HAVE_GDTOA
PYLIBS += gdtoa
endif
if HAVE_LIBOFX
PYLIBS += ofx
endif

if HAVE_GDTOA
ledger.so: $(ledger_so_SOURCES) $(ledger_so_DEPENDENCIES)
	BUILD_DIR=`cd $(top_builddir); pwd`; \
	SRC_DIR=`cd $(srcdir); pwd`; \
	(cd $(srcdir); \
	 CFLAGS="$(CPPFLAGS) -I$(srcdir) -I$$BUILD_DIR -I$$BUILD_DIR/lib/gdtoa $(libledger_python_la_CPPFLAGS) $(AM_CXXFLAGS) $(CXXFLAGS)" \
	 LDFLAGS="$(LDFLAGS) -L$$BUILD_DIR -L$$BUILD_DIR/.libs -L$$BUILD_DIR/lib/gdtoa -L$$BUILD_DIR/lib/gdtoa/.libs" \
	 ARCHFLAGS="$(ARCHFLAGS)" PYLIBS="$(PYLIBS)" VERSION="$(VERSION)" \
	     SRCDIR="$$SRC_DIR" $(PYTHON) python/setup.py build \
		--build-lib=$$BUILD_DIR/ --build-temp=$$BUILD_DIR/build -f)
else
ledger.so: $(ledger_so_SOURCES) $(ledger_so_DEPENDENCIES)
	BUILD_DIR=`cd $(top_builddir); pwd`; \
	SRC_DIR=`cd $(srcdir); pwd`; \
	(cd $(srcdir); \
	 CFLAGS="$(CPPFLAGS) -I$(srcdir) -I$$BUILD_DIR $(libledger_python_la_CPPFLAGS) $(AM_CXXFLAGS) $(CXXFLAGS)" \
	 LDFLAGS="$(LDFLAGS) -L$$BUILD_DIR -L$$BUILD_DIR/.libs" \
	 ARCHFLAGS="$(ARCHFLAGS)" PYLIBS="$(PYLIBS)" VERSION="$(VERSION)" \
	     SRCDIR="$$SRC_DIR" $(PYTHON) python/setup.py build \
		--build-lib=$$BUILD_DIR/ --build-temp=$$BUILD_DIR/build -f)
endif

endif

######################################################################

TESTS =

if HAVE_PYTHON
TESTS += RegressionTests
endif

if HAVE_CPPUNIT
TESTS +=	     \
	util_tests   \
	math_tests   \
	expr_tests   \
	data_tests   \
	parse_tests  \
	report_tests \
	extra_tests
endif

if HAVE_BOOST_PYTHON
TESTS += PyUnitTests
endif

check_PROGRAMS = $(TESTS)

util_tests_SOURCES =		 \
	test/UnitTests.cc	 \
	test/UnitTests.h	 \
	test/util_tests.cc	 \
	test/unit/t_utils.cc	 \
	test/unit/t_utils.h	 \
	test/unit/t_times.cc	 \
	test/unit/t_times.h

util_tests_CPPFLAGS = -I$(srcdir)/test $(lib_cppflags)
util_tests_LDADD    = libledger_util.la -lcppunit

math_tests_SOURCES =		 \
	test/UnitTests.cc	 \
	test/UnitTests.h	 \
	test/math_tests.cc	 \
	test/unit/t_commodity.cc \
	test/unit/t_commodity.h	 \
	test/unit/t_amount.cc	 \
	test/unit/t_amount.h	 \
	test/unit/t_balance.cc	 \
	test/unit/t_balance.h

math_tests_CPPFLAGS = -I$(srcdir)/test $(lib_cppflags)
math_tests_LDADD    = $(util_tests_LDADD) libledger_math.la
if HAVE_GDTOA
math_tests_LDADD   += lib/gdtoa/libgdtoa.la
endif

expr_tests_SOURCES =		 \
	test/UnitTests.cc	 \
	test/UnitTests.h	 \
	test/expr_tests.cc	 \
	test/unit/t_expr.cc	 \
	test/unit/t_expr.h

expr_tests_CPPFLAGS = -I$(srcdir)/test $(lib_cppflags)
expr_tests_LDADD    = $(math_tests_LDADD) libledger_expr.la

data_tests_SOURCES =		 \
	test/UnitTests.cc	 \
	test/UnitTests.h	 \
	test/data_tests.cc

data_tests_CPPFLAGS = -I$(srcdir)/test $(lib_cppflags)
data_tests_LDADD    = $(math_tests_LDADD) libledger_data.la

parse_tests_SOURCES =		 \
	test/UnitTests.cc	 \
	test/UnitTests.h	 \
	test/parse_tests.cc

parse_tests_CPPFLAGS = -I$(srcdir)/test $(lib_cppflags)
parse_tests_LDADD    = $(math_tests_LDADD) libledger_parse.la

report_tests_SOURCES =		 \
	test/UnitTests.cc	 \
	test/UnitTests.h	 \
	test/report_tests.cc

report_tests_CPPFLAGS = -I$(srcdir)/test $(lib_cppflags)
report_tests_LDADD    = $(math_tests_LDADD) libledger_report.la

extra_tests_SOURCES =		 \
	test/UnitTests.cc	 \
	test/UnitTests.h	 \
	test/extra_tests.cc

extra_tests_CPPFLAGS = -I$(srcdir)/test $(lib_cppflags)
extra_tests_LDADD    = $(math_tests_LDADD) libledger_extra.la

EXTRA_DIST += test/python

PyUnitTests_SOURCES = test/__init__.py test/PyUnitTests.py test/UnitTests.py

ESC_python=`echo "$(PYTHON)" | sed 's/\//\\\\\//g'`
ESC_srcdir=`echo "$(srcdir)" | sed 's/\//\\\\\//g'`
ESC_builddir=`echo "$(top_builddir)" | sed 's/\//\\\\\//g'`
ESC_distdir=`echo "$(distdir)" | sed 's/\//\\\\\//g'`

# jww (2007-05-10): This rule will not be triggered on systems that
# define an EXEEXT.
PyUnitTests: $(srcdir)/test/PyUnitTests.py
	cat $(srcdir)/test/PyUnitTests.py \
	    | sed "s/%python%/$(ESC_python)/" \
	    | sed "s/%srcdir%/$(ESC_srcdir)/g" \
	    | sed "s/%builddir%/$(ESC_builddir)/g" > $@
	chmod 755 $@

RegressionTests_SOURCES = test/regress.py

EXTRA_DIST += test/regress

RegressionTests: $(srcdir)/test/regress.py
	echo "$(PYTHON) $(srcdir)/test/regress.py $(top_builddir)/ledger$(EXEEXT) $(srcdir)/test/regress" > $@
	chmod 755 $@

fullcheck: $(TESTS)
	sh $(srcdir)/fullcheck $(top_builddir)/util_tests$(EXEEXT)   --verify
	sh $(srcdir)/fullcheck $(top_builddir)/math_tests$(EXEEXT)   --verify
	sh $(srcdir)/fullcheck $(top_builddir)/expr_tests$(EXEEXT)   --verify
	sh $(srcdir)/fullcheck $(top_builddir)/data_tests$(EXEEXT)   --verify
	sh $(srcdir)/fullcheck $(top_builddir)/parse_tests$(EXEEXT)  --verify
	sh $(srcdir)/fullcheck $(top_builddir)/report_tests$(EXEEXT) --verify
	sh $(srcdir)/fullcheck $(top_builddir)/extra_tests$(EXEEXT)  --verify

######################################################################

EXTRA_DIST += doc/LICENSE doc/NEWS doc/README
EXTRA_DIST += doc/Doxyfile doc/ledger.pdf doc/refman.pdf

DISTCLEANFILES += doc/ledger.info doc/ledger.pdf \
		  Doxyfile.gen doc/Doxyfile.bak doc/refman.pdf

dist-hook:
	find $(distdir) -name .DS_Store -delete
	find $(distdir) -name .localized -delete
	find $(distdir)/doc -name .dirstamp -delete
	rm -fr $(distdir)/doc/latex \
	       $(distdir)/doc/Doxyfile.bak \
	       $(distdir)/doc/Doxyfile.gen
	cp -pR $(srcdir)/doc/html $(distdir)/doc
	chmod u+w $(distdir)/README
	cp -p $(srcdir)/doc/README $(distdir)/README

distclean-local:
	rm -fr doc/html doc/latex
	rm -f doc/refman.pdf

$(top_builddir)/Doxyfile.gen: $(srcdir)/doc/Doxyfile
	cat $(srcdir)/doc/Doxyfile \
	    | sed "s/%srcdir%/$(ESC_srcdir)/g" \
	    | sed "s/%builddir%/$(ESC_builddir)/g" > $@

$(top_builddir)/doc/html/index.html: $(top_builddir)/Doxyfile.gen
	doxygen $(top_builddir)/Doxyfile.gen

# The intention with the following rules is that all of the Doxygen
# documentation (both HTML and PDF) is built locally before distcheck is
# run, since it's quite possible that the user will not have a complete
# TeX + Doxygen + dot environment on their own system.

$(top_builddir)/doc/refman.pdf: $(top_builddir)/doc/html/index.html
	(cd $(top_builddir)/doc/latex && make)
	cp $(top_builddir)/doc/latex/refman.pdf $@

docs: pdf $(top_builddir)/doc/refman.pdf

libs:
	@echo Building dependency libs and installing in /usr/local/stow ...
	git submodule update --init
	(cd lib; make)

report: all
	-rm -fr build
	lcov -d $(shell pwd) --zerocounters
	-mkdir doc/report
	lcov -c -i -d $(shell pwd) -o doc/report/ledger_base.info
	make fullcheck
	lcov -c -d $(shell pwd) --checksum -o doc/report/ledger_test.info
	lcov -a doc/report/ledger_base.info \
	     -a doc/report/ledger_test.info -o doc/report/ledger_total.info
	lcov --extract doc/report/ledger_total.info '*src/ledger/*' \
	     -o doc/report/ledger_cov.info
	genhtml -o doc/report doc/report/ledger_cov.info
	@echo Coverage reported generated\; now open doc/report/index.html

######################################################################

STAGING = /tmp/ledger

copy-sources:
	-mkdir $(STAGING)
	sudo rsync -av --delete --exclude=/.libs/ --exclude=/.deps/ \
	    --exclude=/plan/ --exclude=/2.6*/ --exclude=/archive/ \
	    $(srcdir)/ $(STAGING)/
	(cd $(STAGING); sudo git clean -x -d -f)

release: copy-sources
	(cd $(STAGING); ./acprep --release --build -j3)

release-distcheck: copy-sources
	(cd $(STAGING); ./acprep --release --build -j3 distcheck)

# Makefile.am ends here
