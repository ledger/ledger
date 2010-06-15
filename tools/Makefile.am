LIBVERSION	= $(shell echo $(PACKAGE_VERSION) | sed 's/[-abgrc].*//')
ACLOCAL_AMFLAGS = -I m4
dist_man_MANS	= doc/ledger.1
SUBDIRS		= po intl
EXTRA_DIST	= autogen.sh config.rpath contrib src/system.hh.in
DISTCLEANFILES	= .timestamp

lib_LTLIBRARIES =				\
	libledger_report.la			\
	libledger_data.la			\
	libledger_expr.la			\
	libledger_math.la			\
	libledger_util.la

lib_cppflags  = -I$(srcdir)/src -I$(srcdir)/lib	\
		-I$(srcdir)/lib/utfcpp/source

libledger_util_la_SOURCES =			\
	src/stream.cc				\
	src/mask.cc				\
	src/times.cc				\
	src/error.cc				\
	src/utils.cc				\
	src/accum.cc				\
	lib/sha1.cpp

libledger_util_la_CPPFLAGS = $(lib_cppflags)
libledger_util_la_LDFLAGS  = -release $(LIBVERSION)

libledger_math_la_SOURCES  =			\
	src/balance.cc				\
	src/quotes.cc				\
	src/pool.cc				\
	src/annotate.cc				\
	src/commodity.cc			\
	src/amount.cc

libledger_math_la_CPPFLAGS = $(lib_cppflags)
libledger_math_la_LDFLAGS  = -release $(LIBVERSION)

libledger_expr_la_SOURCES  =			\
	src/option.cc				\
	src/format.cc				\
	src/query.cc				\
	src/predicate.cc			\
	src/scope.cc				\
	src/expr.cc				\
	src/op.cc				\
	src/parser.cc				\
	src/token.cc				\
	src/value.cc

libledger_expr_la_CPPFLAGS  = $(lib_cppflags)
libledger_expr_la_LDFLAGS   = -release $(LIBVERSION)

libledger_data_la_SOURCES   =			\
	src/lookup.cc				\
	src/compare.cc				\
	src/iterators.cc			\
	src/timelog.cc				\
	src/textual.cc				\
	src/temps.cc				\
	src/journal.cc				\
	src/archive.cc				\
	src/account.cc				\
	src/xact.cc				\
	src/post.cc				\
	src/item.cc

libledger_data_la_CPPFLAGS   = $(lib_cppflags)
libledger_data_la_LDFLAGS    = -release $(LIBVERSION)

libledger_report_la_SOURCES  =			\
	src/stats.cc				\
	src/generate.cc				\
	src/csv.cc				\
	src/convert.cc				\
	src/draft.cc				\
	src/emacs.cc				\
	src/xml.cc				\
	src/print.cc				\
	src/output.cc				\
	src/precmd.cc				\
	src/chain.cc				\
	src/filters.cc				\
	src/report.cc				\
	src/session.cc

libledger_report_la_CPPFLAGS = $(lib_cppflags)
libledger_report_la_LDFLAGS  = -release $(LIBVERSION)

pkginclude_HEADERS =				\
	src/utils.h				\
	src/flags.h				\
	src/error.h				\
	src/times.h				\
	src/mask.h				\
	src/stream.h				\
	src/pstream.h				\
	src/unistring.h				\
	src/accum.h				\
						\
	src/amount.h				\
	src/commodity.h				\
	src/annotate.h				\
	src/pool.h				\
	src/quotes.h				\
	src/balance.h				\
						\
	src/value.h				\
	src/token.h				\
	src/parser.h				\
	src/op.h				\
	src/exprbase.h				\
	src/expr.h				\
	src/scope.h				\
	src/predicate.h				\
	src/query.h				\
	src/format.h				\
	src/option.h				\
						\
	src/item.h				\
	src/post.h				\
	src/xact.h				\
	src/account.h				\
	src/journal.h				\
	src/temps.h				\
	src/archive.h				\
	src/timelog.h				\
	src/iterators.h				\
	src/compare.h				\
	src/lookup.h				\
						\
	src/session.h				\
	src/report.h				\
	src/filters.h				\
	src/chain.h				\
	src/precmd.h				\
	src/csv.h				\
	src/convert.h				\
	src/draft.h				\
	src/generate.h				\
	src/stats.h				\
	src/print.h				\
	src/output.h				\
	src/xml.h				\
	src/emacs.h				\
						\
	src/global.h				\
						\
	src/pyinterp.h				\
						\
	lib/sha1.h				\
	lib/gettext.h				\
						\
	lib/utfcpp/source/utf8.h		\
	lib/utfcpp/source/utf8/checked.h	\
	lib/utfcpp/source/utf8/core.h		\
	lib/utfcpp/source/utf8/unchecked.h

nodist_libledger_util_la_SOURCES = system.hh

BUILT_SOURCES = system.hh
CLEANFILES    = system.hh

system.hh: src/system.hh.in
	cp -p $< $@

if USE_PCH
nodist_libledger_util_la_SOURCES += system.hh.gch

BUILT_SOURCES += system.hh.gch
CLEANFILES    += system.hh.gch

system.hh.gch: system.hh
	-rm -f $@
	$(CXX) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) \
	    $(lib_cppflags) $(CPPFLAGS) $(AM_CXXFLAGS) $(CXXFLAGS) \
	    -o $@ $<
endif

######################################################################

bin_PROGRAMS	= ledger

ledger_CPPFLAGS	= $(lib_cppflags)
if HAVE_BOOST_PYTHON
ledger_CPPFLAGS	+= -I$(srcdir)/python
endif
ledger_SOURCES	= src/main.cc src/global.cc
ledger_LDADD	= $(LIBOBJS) $(lib_LTLIBRARIES) $(INTLLIBS)
ledger_LDFLAGS	= -static

info_TEXINFOS	= doc/ledger.texi

dist_lisp_LISP	= lisp/ledger.el lisp/timeclock.el
ELCFILES	=
DISTCLEANFILES += ledger.elc timeclock.elc

all_sources	= $(libledger_util_la_SOURCES)		\
		  $(libledger_math_la_SOURCES)		\
		  $(libledger_expr_la_SOURCES)		\
		  $(libledger_data_la_SOURCES)		\
		  $(libledger_report_la_SOURCES)	\
		  $(libledger_python_la_SOURCES)	\
		  src/pyledger.cc

all_files	= $(all_sources) $(pkginclude_HEADERS)

######################################################################

if HAVE_BOOST_PYTHON

lib_LTLIBRARIES += libledger_python.la

libledger_python_la_SOURCES =			\
	src/pyutils.h				\
	src/pyfstream.h				\
	src/py_account.cc			\
	src/py_amount.cc			\
	src/py_balance.cc			\
	src/py_commodity.cc			\
	src/py_expr.cc				\
	src/py_format.cc			\
	src/py_item.cc				\
	src/py_journal.cc			\
	src/py_post.cc				\
	src/py_times.cc				\
	src/py_utils.cc				\
	src/py_value.cc				\
	src/py_xact.cc				\
	src/pyinterp.cc

libledger_python_la_CPPFLAGS = $(lib_cppflags) -I$(srcdir)/python

pyexec_LTLIBRARIES     = ledger.la

ledger_la_CPPFLAGS     = $(libledger_python_la_CPPFLAGS)
ledger_la_SOURCES      = src/pyledger.cc
ledger_la_DEPENDENCIES = $(lib_LTLIBRARIES)
ledger_la_LDFLAGS      = -avoid-version -module
ledger_la_LIBADD       = $(LIBOBJS) $(lib_LTLIBRARIES)

pkgpython_PYTHON       = python/__init__.py python/server.py

endif

######################################################################

TESTS = RegressTests BaselineTests ManualTests ConfirmTests GenerateTests

if HAVE_CPPUNIT
TESTS +=	     \
	UtilTests   \
	MathTests   \
	ExprTests   \
	DataTests   \
	ReportTests
endif

if HAVE_BOOST_PYTHON
TESTS += PyUnitTests
endif

check_PROGRAMS = $(TESTS)

UtilTests_SOURCES =		 \
	test/UnitTests.cc	 \
	test/UnitTests.h	 \
	test/UtilTests.cc	 \
	test/unit/t_utils.cc	 \
	test/unit/t_utils.h	 \
	test/unit/t_times.cc	 \
	test/unit/t_times.h

UtilTests_CPPFLAGS = -I$(srcdir)/test $(lib_cppflags)
UtilTests_LDADD	   = libledger_util.la -lcppunit

MathTests_SOURCES =		 \
	test/UnitTests.cc	 \
	test/UnitTests.h	 \
	test/MathTests.cc	 \
	test/unit/t_commodity.cc \
	test/unit/t_commodity.h	 \
	test/unit/t_amount.cc	 \
	test/unit/t_amount.h	 \
	test/unit/t_balance.cc	 \
	test/unit/t_balance.h

MathTests_CPPFLAGS = -I$(srcdir)/test $(lib_cppflags)
MathTests_LDADD	   = libledger_math.la $(UtilTests_LDADD)

ExprTests_SOURCES =		 \
	test/UnitTests.cc	 \
	test/UnitTests.h	 \
	test/ExprTests.cc	 \
	test/unit/t_expr.cc	 \
	test/unit/t_expr.h

ExprTests_CPPFLAGS = -I$(srcdir)/test $(lib_cppflags)
ExprTests_LDADD	   = libledger_expr.la $(MathTests_LDADD)

DataTests_SOURCES =		 \
	test/UnitTests.cc	 \
	test/UnitTests.h	 \
	test/DataTests.cc

DataTests_CPPFLAGS = -I$(srcdir)/test $(lib_cppflags)
DataTests_LDADD	   = libledger_data.la $(ExprTests_LDADD)

ReportTests_SOURCES =		 \
	test/UnitTests.cc	 \
	test/UnitTests.h	 \
	test/ReportTests.cc

ReportTests_CPPFLAGS = -I$(srcdir)/test $(lib_cppflags)
ReportTests_LDADD    = libledger_report.la $(DataTests_LDADD)

all_tests_sources =				\
	$(UtilTests_SOURCES)			\
	$(MathTests_SOURCES)			\
	$(ExprTests_SOURCES)			\
	$(DataTests_SOURCES)			\
	$(ReportTests_SOURCES)

PyUnitTests_SOURCES = test/PyUnitTests.py

all_py_tests_sources = \
	$(patsubst test/unit/%.cc,$(top_builddir)/test/python/%.py, \
		   $(filter test/unit/t_%.cc,$(all_tests_sources)))

test/python/%.py: test/unit/%.cc test/convert.py
	$(PYTHON) $(srcdir)/test/convert.py $< $@

test/python/UnitTests.py: $(all_py_tests_sources)
	@echo "from unittest import TextTestRunner, TestSuite" > $@
	@for file in $$(ls $(srcdir)/test/unit/*.cc); do \
	    base=$$(basename $$file); \
	    base=$$(echo $$base | sed 's/\.cc//'); \
	    echo "import $$base" >> $@; \
	done
	@echo "suites = [" >> $@
	@for file in $$(ls $(srcdir)/test/unit/*.cc); do \
	    base=$$(basename $$file); \
	    base=$$(echo $$base | sed 's/\.cc//'); \
	    echo "    $$base.suite()," >> $@; \
	done
	@echo "]" >> $@
	@echo "TextTestRunner().run(TestSuite(suites))" >> $@

ledger_python = $(top_builddir)/ledger$(EXEEXT) python

ESC_python=`echo "$(ledger_python)" | sed 's/\//\\\\\//g'`
ESC_srcdir=`echo "$(srcdir)" | sed 's/\//\\\\\//g'`
ESC_builddir=`echo "$(top_builddir)" | sed 's/\//\\\\\//g'`
ESC_distdir=`echo "$(distdir)" | sed 's/\//\\\\\//g'`

# jww (2007-05-10): This rule will not be triggered on systems that
# define an EXEEXT.
PyUnitTests: test/PyUnitTests.py test/python/UnitTests.py
	@cat $(srcdir)/test/PyUnitTests.py \
	    | sed "s/%python%/$(ESC_python)/" \
	    | sed "s/%srcdir%/$(ESC_srcdir)/g" \
	    | sed "s/%builddir%/$(ESC_builddir)/g" > $@
	chmod 755 $@

RegressTests_SOURCES = test/RegressTests.py

EXTRA_DIST += test/regress test/convert.py test/LedgerHarness.py

RegressTests: $(srcdir)/test/RegressTests.py
	echo "$(PYTHON) $(srcdir)/test/RegressTests.py -j$(JOBS) $(top_builddir)/ledger$(EXEEXT) $(srcdir) $(srcdir)/test/regress \"\$$@\"" > $@
	chmod 755 $@

BaselineTests_SOURCES = test/RegressTests.py

EXTRA_DIST += test/baseline

BaselineTests: $(srcdir)/test/RegressTests.py
	echo "$(PYTHON) $(srcdir)/test/RegressTests.py -j$(JOBS) $(top_builddir)/ledger$(EXEEXT) $(srcdir) $(srcdir)/test/baseline \"\$$@\"" > $@
	chmod 755 $@

ManualTests_SOURCES = test/RegressTests.py

EXTRA_DIST += test/manual

ManualTests: $(srcdir)/test/RegressTests.py
	echo "$(PYTHON) $(srcdir)/test/RegressTests.py -j$(JOBS) $(top_builddir)/ledger$(EXEEXT) $(srcdir) $(srcdir)/test/manual \"\$$@\"" > $@
	chmod 755 $@

ConfirmTests_SOURCES = test/ConfirmTests.py

EXTRA_DIST += test/input

test/input/mondo.dat: test/input/standard.dat
	@for i in 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 ; do \
	    for j in 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 ; do \
		cat $< >> $@; \
	    done; \
	done

ConfirmTests: $(srcdir)/test/ConfirmTests.py
	echo "$(PYTHON) $(srcdir)/test/ConfirmTests.py $(top_builddir)/ledger$(EXEEXT) $(srcdir) $(srcdir)/test/input \"\$$@\"" > $@
	chmod 755 $@

GenerateTests_SOURCES = test/GenerateTests.py

GenerateTests: $(srcdir)/test/GenerateTests.py
	echo "$(PYTHON) $(srcdir)/test/GenerateTests.py -j$(JOBS) $(top_builddir)/ledger$(EXEEXT) $(srcdir) 1 ${1:-20} \"\$$@\"" > $@
	chmod 755 $@

FULLCHECK=$(srcdir)/test/fullcheck.sh

if HAVE_CPPUNIT
cppunittests: check
	@sh $(FULLCHECK) $(top_builddir)/UtilTests$(EXEEXT)   --verify \
	    2>&1 | grep -v '^GuardMalloc:'
	@sh $(FULLCHECK) $(top_builddir)/MathTests$(EXEEXT)   --verify \
	    2>&1 | grep -v '^GuardMalloc:'
	@sh $(FULLCHECK) $(top_builddir)/ExprTests$(EXEEXT)   --verify \
	    2>&1 | grep -v '^GuardMalloc:'
	@sh $(FULLCHECK) $(top_builddir)/DataTests$(EXEEXT)   --verify \
	    2>&1 | grep -v '^GuardMalloc:'
	@sh $(FULLCHECK) $(top_builddir)/ReportTests$(EXEEXT) --verify \
	    2>&1 | grep -v '^GuardMalloc:'
else
cppunittests: check
	@test 1 -eq 1
endif

fullcheck: cppunittests
	@$(top_builddir)/RegressTests  --verify
	@$(top_builddir)/BaselineTests --verify
	@$(top_builddir)/ManualTests   --verify
	@$(top_builddir)/ConfirmTests  --verify
	@$(top_builddir)/GenerateTests 20 --verify
	@$(top_builddir)/RegressTests  --gmalloc
	@$(top_builddir)/BaselineTests --gmalloc
	@$(top_builddir)/ManualTests   --gmalloc
	@$(top_builddir)/ConfirmTests  --gmalloc
	@$(top_builddir)/GenerateTests 10000
#	@$(top_builddir)/GenerateTests --gmalloc

######################################################################

EXTRA_DIST += doc/README doc/NEWS doc/ledger.pdf
EXTRA_DIST += doc/LICENSE doc/LICENSE-sha1 doc/LICENSE-utfcpp
if USE_DOXYGEN
EXTRA_DIST += doc/Doxyfile doc/refman.pdf
endif

DISTCLEANFILES += doc/ledger.info doc/ledger.pdf
if USE_DOXYGEN
DISTCLEANFILES += Doxyfile.gen doc/Doxyfile.bak doc/refman.pdf
endif

if USE_DOXYGEN
dist-hook-doxygen:
	find $(distdir)/doc -name .dirstamp -delete
	rm -fr $(distdir)/doc/latex \
	       $(distdir)/doc/Doxyfile.bak \
	       $(distdir)/doc/Doxyfile.gen
	cp -pR doc/html $(distdir)/doc
else
dist-hook-doxygen:
	@test 1 -eq 1
endif

dist-hook: dist-hook-doxygen
	find $(distdir) -name .DS_Store -delete
	find $(distdir) -name .localized -delete
	rm -f $(distdir)/README.textile
	cp -p $(srcdir)/doc/README $(distdir)/README

if USE_DOXYGEN
distclean-local-doxygen:
	rm -fr doc/html doc/latex
	rm -f doc/refman.pdf
else
distclean-local-doxygen:
	@test 1 -eq 1
endif

distclean-local: distclean-local-doxygen
	rm -fr test/python

if USE_DOXYGEN
ESC_top_builddir=`cd $(top_builddir); pwd | sed 's/\//\\\\\//g'`

Doxyfile.gen: doc/Doxyfile
	cat $< | sed "s/%srcdir%/$(ESC_srcdir)/g" \
	       | sed "s/%builddir%/$(ESC_top_builddir)/g" > $@

doc/html/index.html: Doxyfile.gen $(all_files)
	BUILD_DIR=`cd $(top_builddir); pwd`; \
	(cd $(srcdir); doxygen $$BUILD_DIR/Doxyfile.gen)

# The intention with the following rules is that all of the Doxygen
# documentation (both HTML and PDF) is built locally before distcheck is
# run, since it's quite possible that the user will not have a complete
# TeX + Doxygen + dot environment on their own system.

doc/refman.pdf: doc/html/index.html
	(cd doc/latex && make)
	cp doc/latex/refman.pdf $@

docs: pdf doc/refman.pdf
else
docs: pdf
endif

libs:
	@echo Building dependency libs and installing in /usr/local/stow ...
	git submodule update --init
	(cd lib; make)

report: all
	lcov -d $(shell pwd) --zerocounters
	if [ -d doc/report ]; then rm -fr doc/report; fi
	-mkdir doc/report
	lcov -c -i -d $(shell pwd) -o doc/report/ledger_base.info
	make check
	lcov -c -d $(shell pwd) --checksum -o doc/report/ledger_test.info
	lcov -a doc/report/ledger_base.info \
	     -a doc/report/ledger_test.info -o doc/report/ledger_total.info
	lcov --extract doc/report/ledger_total.info '*/src/*' \
	     -o doc/report/ledger_cov.info
	genhtml -o doc/report doc/report/ledger_cov.info
	@echo Coverage reported generated\; now open doc/report/index.html

LEDGER_BINARY  = $(top_builddir)/ledger$(EXEEXT)
SPEEDTEST_DAT  = $(srcdir)/test/input/speed-test.dat
SPEEDTEST_ARGS = -v --decimal-comma -o /dev/null reg 2eb75f84 1eede0cb

speedtest:
	@$(LEDGER_BINARY) -f $(SPEEDTEST_DAT) $(SPEEDTEST_ARGS)
	@$(LEDGER_BINARY) -f $(SPEEDTEST_DAT) $(SPEEDTEST_ARGS) -V
	@$(LEDGER_BINARY) -f $(SPEEDTEST_DAT) $(SPEEDTEST_ARGS) -X €

# Makefile.am ends here
