VERSION		= 3.0
ACLOCAL_AMFLAGS = -I m4
dist_man_MANS	= doc/ledger.1
SUBDIRS         = po intl
EXTRA_DIST	= config.rpath autogen.sh contrib

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
libledger_util_la_LDFLAGS  = -release $(VERSION).0

libledger_math_la_SOURCES =			\
	src/value.cc				\
	src/balance.cc				\
	src/commodity.cc			\
	src/amount.cc

libledger_math_la_CPPFLAGS = $(lib_cppflags)
libledger_math_la_LDFLAGS  = -release $(VERSION).0

libledger_expr_la_SOURCES =			\
	src/option.cc				\
	src/format.cc				\
	src/predicate.cc			\
	src/scope.cc				\
	src/interactive.cc			\
	src/expr.cc				\
	src/op.cc				\
	src/parser.cc				\
	src/token.cc

libledger_expr_la_CPPFLAGS  = $(lib_cppflags)
libledger_expr_la_LDFLAGS    = -release $(VERSION).0

libledger_data_la_SOURCES  =			\
	src/compare.cc				\
	src/iterators.cc			\
	src/timelog.cc				\
	src/textual.cc				\
	src/journal.cc				\
	src/account.cc				\
	src/xact.cc				\
	src/post.cc				\
	src/item.cc

libledger_data_la_CPPFLAGS = $(lib_cppflags)
libledger_data_la_LDFLAGS  = -release $(VERSION).0

libledger_report_la_SOURCES  =			\
	src/quotes.cc				\
	src/derive.cc				\
	src/emacs.cc                            \
	src/output.cc				\
	src/precmd.cc				\
	src/chain.cc				\
	src/filters.cc				\
	src/report.cc				\
	src/session.cc

libledger_report_la_CPPFLAGS = $(lib_cppflags)
libledger_report_la_LDFLAGS  = -release $(VERSION).0

pkginclude_HEADERS =				\
	$(top_builddir)/config.h		\
	src/system.hh				\
	src/utils.h				\
	src/flags.h				\
	src/hooks.h				\
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
	src/balance.h				\
	src/value.h				\
						\
	src/token.h				\
	src/parser.h				\
	src/op.h				\
	src/expr.h				\
	src/scope.h				\
	src/interactive.h			\
	src/predicate.h				\
	src/format.h				\
	src/option.h				\
						\
	src/item.h				\
	src/post.h				\
	src/xact.h				\
	src/account.h				\
	src/journal.h				\
	src/timelog.h				\
	src/iterators.h				\
	src/compare.h				\
						\
	src/session.h				\
	src/report.h				\
	src/filters.h				\
	src/chain.h				\
	src/precmd.h				\
	src/derive.h				\
	src/output.h				\
	src/emacs.h				\
	src/quotes.h				\
						\
	src/global.h				\
	src/ledger.h				\
						\
	src/pyledger.h				\
	src/pyinterp.h				\
						\
	lib/fdstream.h				\
	lib/sha1.h				\
	lib/gettext.h				\
						\
	lib/utfcpp/source/utf8.h		\
	lib/utfcpp/source/utf8/checked.h	\
	lib/utfcpp/source/utf8/core.h		\
	lib/utfcpp/source/utf8/unchecked.h

CLEANFILES =

if USE_PCH
nodist_libledger_util_la_SOURCES = src/system.hh.gch

BUILT_SOURCES = src/system.hh.gch
CLEANFILES   += $(srcdir)/src/system.hh.gch

$(srcdir)/src/system.hh.gch: $(srcdir)/src/system.hh $(top_builddir)/config.h
	$(CXX) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) \
	    $(lib_cppflags) $(CPPFLAGS) $(AM_CXXFLAGS) $(CXXFLAGS) \
	    -g -o $@ $(srcdir)/src/system.hh
endif

######################################################################

bin_PROGRAMS	= ledger

ledger_CPPFLAGS	= $(lib_cppflags)
if HAVE_BOOST_PYTHON
ledger_CPPFLAGS	+= -I$(srcdir)/python
endif
ledger_SOURCES	= src/main.cc src/global.cc
ledger_LDADD	= $(LIBOBJS) $(lib_LTLIBRARIES) $(INTLLIBS)
ledger_LDFLAGS	= 

info_TEXINFOS	= doc/ledger.texi

dist_lisp_LISP	= lisp/ledger.el lisp/timeclock.el
ELCFILES	=
DISTCLEANFILES	= ledger.elc timeclock.elc
CLEANFILES     += TAGS

all_sources     = $(libledger_util_la_SOURCES)		\
		  $(libledger_math_la_SOURCES)		\
		  $(libledger_expr_la_SOURCES)		\
		  $(libledger_data_la_SOURCES)		\
		  $(libledger_report_la_SOURCES)	\
		  $(libledger_python_la_SOURCES)	\
		  src/pyledger.cc

all_files       = $(all_sources) $(pkginclude_HEADERS)

TAGS: $(all_files)
	@BUILD_DIR=`cd $(top_builddir); pwd`; \
	(cd $(srcdir); etags -l c++ -o $$BUILD_DIR/TAGS $(all_files) 2> /dev/null)
	@echo TAGS rebuilt.

######################################################################

if HAVE_BOOST_PYTHON

lib_LTLIBRARIES += libledger_python.la

libledger_python_la_SOURCES =			\
	src/pyutils.h				\
	src/pyfstream.h				\
	src/py_amount.cc			\
	src/py_balance.cc			\
	src/py_chain.cc				\
	src/py_commodity.cc			\
	src/py_expr.cc				\
	src/py_flags.cc				\
	src/py_format.cc			\
	src/py_global.cc			\
	src/py_item.cc				\
	src/py_journal.cc			\
	src/py_post.cc				\
	src/py_report.cc			\
	src/py_scope.cc				\
	src/py_session.cc			\
	src/py_timelog.cc			\
	src/py_times.cc				\
	src/py_utils.cc				\
	src/py_value.cc				\
	src/py_xact.cc				\
	src/pyinterp.cc

libledger_python_la_CPPFLAGS = $(lib_cppflags) -I$(srcdir)/python

pyexec_LTLIBRARIES = ledger.la

ledger_la_CPPFLAGS     = $(libledger_python_la_CPPFLAGS)
ledger_la_SOURCES      = src/pyledger.cc
ledger_la_DEPENDENCIES = $(lib_LTLIBRARIES)
ledger_la_LDFLAGS      = -avoid-version -module
ledger_la_LIBADD       = $(LIBOBJS) $(lib_LTLIBRARIES) $(INTLLIBS)

pkgpython_PYTHON = python/__init__.py		\
	python/hello.py				\
	python/interp.py			\
	python/server.py

endif

######################################################################

TESTS =

if HAVE_PYTHON
TESTS += RegressionTests BaselineTests
endif

if HAVE_CPPUNIT
TESTS +=	     \
	util_tests   \
	math_tests   \
	expr_tests   \
	data_tests   \
	report_tests
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
math_tests_LDADD    = libledger_math.la $(util_tests_LDADD)

expr_tests_SOURCES =		 \
	test/UnitTests.cc	 \
	test/UnitTests.h	 \
	test/expr_tests.cc	 \
	test/unit/t_expr.cc	 \
	test/unit/t_expr.h

expr_tests_CPPFLAGS = -I$(srcdir)/test $(lib_cppflags)
expr_tests_LDADD    = libledger_expr.la $(math_tests_LDADD)

data_tests_SOURCES =		 \
	test/UnitTests.cc	 \
	test/UnitTests.h	 \
	test/data_tests.cc

data_tests_CPPFLAGS = -I$(srcdir)/test $(lib_cppflags)
data_tests_LDADD    = libledger_data.la $(expr_tests_LDADD)

report_tests_SOURCES =		 \
	test/UnitTests.cc	 \
	test/UnitTests.h	 \
	test/report_tests.cc

report_tests_CPPFLAGS = -I$(srcdir)/test $(lib_cppflags)
report_tests_LDADD    = libledger_report.la $(data_tests_LDADD)

all_tests_sources =				\
	$(util_tests_SOURCES)			\
	$(math_tests_SOURCES)			\
	$(expr_tests_SOURCES)			\
	$(data_tests_SOURCES)			\
	$(report_tests_SOURCES)

PyUnitTests_SOURCES = test/__init__.py test/PyUnitTests.py

all_py_tests_sources = \
	$(patsubst test/unit/%.cc,$(top_builddir)/test/python/%.py, \
		   $(filter test/unit/t_%.cc,$(all_tests_sources)))

$(top_builddir)/test/python/%.py: $(srcdir)/test/unit/%.cc \
				  $(srcdir)/test/convert.py
	$(PYTHON) $(srcdir)/test/convert.py $< $@

$(top_builddir)/test/python/UnitTests.py: $(all_py_tests_sources)
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

ESC_python=`echo "$(PYTHON)" | sed 's/\//\\\\\//g'`
ESC_srcdir=`echo "$(srcdir)" | sed 's/\//\\\\\//g'`
ESC_builddir=`echo "$(top_builddir)" | sed 's/\//\\\\\//g'`
ESC_distdir=`echo "$(distdir)" | sed 's/\//\\\\\//g'`

# jww (2007-05-10): This rule will not be triggered on systems that
# define an EXEEXT.
PyUnitTests: $(srcdir)/test/PyUnitTests.py test/python/UnitTests.py
	@cat $(srcdir)/test/PyUnitTests.py \
	    | sed "s/%python%/$(ESC_python)/" \
	    | sed "s/%srcdir%/$(ESC_srcdir)/g" \
	    | sed "s/%builddir%/$(ESC_builddir)/g" > $@
	chmod 755 $@

RegressionTests_SOURCES = test/regress.py

EXTRA_DIST += test/regress test/convert.py

RegressionTests: $(srcdir)/test/regress.py
	echo "$(PYTHON) $(srcdir)/test/regress.py $(top_builddir)/ledger$(EXEEXT) $(srcdir)/test/regress" > $@
	chmod 755 $@

BaselineTests_SOURCES = test/regress.py

EXTRA_DIST += test/baseline

BaselineTests: $(srcdir)/test/regress.py
	echo "$(PYTHON) $(srcdir)/test/regress.py $(top_builddir)/ledger$(EXEEXT) $(srcdir)/test/baseline" > $@
	chmod 755 $@

FULLCHECK=$(srcdir)/tools/fullcheck

if HAVE_CPPUNIT
fullcheck: $(TESTS)
	sh $(FULLCHECK) $(top_builddir)/util_tests$(EXEEXT)   --verify
	sh $(FULLCHECK) $(top_builddir)/math_tests$(EXEEXT)   --verify
	sh $(FULLCHECK) $(top_builddir)/expr_tests$(EXEEXT)   --verify
	sh $(FULLCHECK) $(top_builddir)/data_tests$(EXEEXT)   --verify
	sh $(FULLCHECK) $(top_builddir)/report_tests$(EXEEXT) --verify
else
fullcheck: check
	@test 1 -eq 1
endif

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
	cp -pR doc/html $(distdir)/doc
	rm -f $(distdir)/README.textile
	cp -p $(srcdir)/doc/README $(distdir)/README

distclean-local:
	rm -fr doc/html doc/latex
	rm -f doc/refman.pdf

ESC_top_builddir=`cd $(top_builddir); pwd | sed 's/\//\\\\\//g'`

$(top_builddir)/Doxyfile.gen: $(srcdir)/doc/Doxyfile
	cat $(srcdir)/doc/Doxyfile \
	    | sed "s/%srcdir%/$(ESC_srcdir)/g" \
	    | sed "s/%builddir%/$(ESC_top_builddir)/g" > $@

$(top_builddir)/doc/html/index.html: $(top_builddir)/Doxyfile.gen $(all_files)
	BUILD_DIR=`cd $(top_builddir); pwd`; \
	(cd $(srcdir); doxygen $$BUILD_DIR/Doxyfile.gen)

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

sloc:
	sloccount $(srcdir)/src $(srcdir)/python $(srcdir)/lisp $(srcdir)/test

######################################################################

STAGING = /tmp/ledger

copy-sources:
	-mkdir -p $(STAGING)
	rsync -av --delete --exclude=/.libs/ --exclude=/.deps/ \
	    --exclude=/plan/ --exclude=/2.6*/ --exclude=/archive/ \
	    $(srcdir)/ $(STAGING)/
	-(cd $(STAGING); git clean -x -d -f)

release: copy-sources
	(cd $(STAGING); \
	 nice -n 20 ./acprep --release --opt --build -j3 clean all)

release-distcheck: copy-sources
	(cd $(STAGING); \
	 nice -n 20 ./acprep --release --build -j3 clean distcheck)

benchmark: release
	PATH=$(PATH):$(srcdir)/tools \
	    $(srcdir)/tools/speedcmp 50 $(STAGING)/ledger

# Makefile.am ends here
