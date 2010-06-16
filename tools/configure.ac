# -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.61)

m4_include([version.m4])

AC_INIT([ledger],[VERSION_NUMBER],[johnw@newartisans.com])

AC_CONFIG_AUX_DIR([.])
AM_INIT_AUTOMAKE([dist-bzip2 foreign])
AC_CONFIG_MACRO_DIR([m4])

AC_CONFIG_SRCDIR([src/main.cc])
AC_CONFIG_HEADER([config.h])

# Use AM_SILENT_RULES if present
m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])])

# Checks for programs.
AC_USE_SYSTEM_EXTENSIONS
AC_PROG_CXX
AC_PROG_MAKE_SET
AC_PROG_LIBTOOL
AM_GNU_GETTEXT
AM_GNU_GETTEXT_VERSION([0.17])

# Checks for emacs lisp path
AM_PATH_LISPDIR

# Check for options
AC_ARG_ENABLE(debug,
  [  --enable-debug          Turn on debugging],
  [case "${enableval}" in
    yes) debug=true ;;
    no)  debug=false ;;
    *) AC_MSG_ERROR(bad value ${enableval} for --enable-debug) ;;
  esac],[debug=false])

if [ test x$debug = xtrue ]; then
  AC_DEFINE([DEBUG_MODE], [1], [Whether debugging is enabled])
fi
AM_CONDITIONAL(DEBUG, test x$debug = xtrue)

AC_ARG_ENABLE(pch,
  [  --enable-pch            Use GCC 4.x pre-compiled headers],
  [case "${enableval}" in
    yes) pch=true ;;
    no)  pch=false ;;
    *) AC_MSG_ERROR(bad value ${enableval} for --enable-pch) ;;
  esac],[pch=false])

if [ test x$pch = xtrue ]; then
  AC_DEFINE([USE_PCH], [1], [Whether pre-compiled headers are being used])
fi
AM_CONDITIONAL(USE_PCH, test x$pch = xtrue)

AC_ARG_ENABLE(doxygen,
  [  --enable-doxygen        Turns on generation of code documentation],
  [case "${enableval}" in
    yes) doxygen=true ;;
    no)  doxygen=false ;;
    *) AC_MSG_ERROR(bad value ${enableval} for --enable-doxygen) ;;
  esac],[doxygen=false])

AM_CONDITIONAL(USE_DOXYGEN, test x$doxygen = xtrue)

AC_ARG_ENABLE(cache,
  [  --enable-cache           Enable use of the --cache option],
  [case "${enableval}" in
    yes) cache=true ;;
    no)  cache=false ;;
    *) AC_MSG_ERROR(bad value ${enableval} for --enable-cache) ;;
  esac],[cache=false])

AM_CONDITIONAL(USE_CACHE_OPTION, test x$cache = xtrue)

AC_ARG_ENABLE(python,
  [  --enable-python          Turn on Python support (experimental)],
  [case "${enableval}" in
    yes) python=true ;;
    no)  python=false ;;
    *) AC_MSG_ERROR(bad value ${enableval} for --enable-python) ;;
  esac],[python=false])

AC_ARG_WITH(boost-suffix,
  [  --with-boost-suffix=X   Append X to the Boost library names],
  [BOOST_SUFFIX="${withval}"],
  [BOOST_SUFFIX=""])

AC_SUBST([BOOST_SUFFIX], $BOOST_SUFFIX)

# check if UNIX pipes are available
AC_CACHE_CHECK(
  [if pipes can be used],
  [pipes_avail_cv_],
  [AC_LANG_PUSH(C++)
   AC_LINK_IFELSE(
     [AC_LANG_PROGRAM(
        [[#include <sys/types.h>
          #include <sys/wait.h>
          #include <unistd.h>
          #include <stdlib.h>
          #include <string.h>
          #include <stdio.h>]],
        [[int status, pfd[2];
          status = pipe(pfd);
          status = fork();
          if (status < 0) {
            ;
          } else if (status == 0) {
            char *arg0 = NULL;

            status = dup2(pfd[0], STDIN_FILENO);

            close(pfd[1]);
            close(pfd[0]);

            execlp("", arg0, (char *)0);
            perror("execl");
            exit(1);
          } else {
            close(pfd[0]);
          }]])],
     [pipes_avail_cv_=true],
     [pipes_avail_cv_=false])
   AC_LANG_POP])

if [test x$pipes_avail_cv_ = xtrue ]; then
  AC_DEFINE([HAVE_UNIX_PIPES], [1], [Whether UNIX pipes are available])
fi

# check for gmp
AC_CACHE_CHECK(
  [if GMP is available],
  [libgmp_avail_cv_],
  [libgmp_save_libs=$LIBS
   LIBS="-lgmp $LIBS"
   AC_LANG_PUSH(C++)
   AC_LINK_IFELSE([AC_LANG_PROGRAM([[#include <gmp.h>]], [[mpz_t bar;
      mpz_init(bar);
      mpz_clear(bar);]])],[libgmp_avail_cv_=true],[libgmp_avail_cv_=false])
   AC_LANG_POP
   LIBS=$libgmp_save_libs])

if [test x$libgmp_avail_cv_ = xtrue ]; then
  LIBS="-lgmp $LIBS"
else
  AC_MSG_FAILURE("Could not find gmp library (set CPPFLAGS and LDFLAGS?)")
fi

# check for mpfr
AC_CACHE_CHECK(
  [if MPFR is available],
  [libmpfr_avail_cv_],
  [libmpfr_save_libs=$LIBS
   LIBS="-lmpfr $LIBS"
   AC_LANG_PUSH(C++)
   AC_LINK_IFELSE([AC_LANG_PROGRAM([[#include <mpfr.h>]], [[mpfr_t bar;
      mpfr_init(bar);
      char * buf = NULL;
      mpfr_asprintf(&buf, "%Rf", bar);
      mpfr_clear(bar);]])],[libmpfr_avail_cv_=true],[libmpfr_avail_cv_=false])
   AC_LANG_POP
   LIBS=$libmpfr_save_libs])

if [test x$libmpfr_avail_cv_ = xtrue ]; then
  LIBS="-lmpfr $LIBS"
else
  AC_MSG_FAILURE("Could not find mpfr library 2.4.0 or higher (set CPPFLAGS and LDFLAGS?)")
fi

# check for edit
AC_CACHE_CHECK(
  [if libedit is available],
  [libedit_avail_cv_],
  [libedit_save_libs=$LIBS
   LIBS="-ledit $LIBS"
   AC_LANG_PUSH(C++)
   AC_LINK_IFELSE([AC_LANG_PROGRAM(
    [[#include <stdlib.h>
      #include <stdio.h>
      #include <editline/readline.h>]],
    [[rl_readline_name = const_cast<char *>("foo");
      char * line = readline(const_cast<char *>("foo: "));
      free(line);]])],[libedit_avail_cv_=true],[libedit_avail_cv_=false])
   AC_LANG_POP
   LIBS=$libedit_save_libs])

if [test x$libedit_avail_cv_ = xtrue ]; then
  LIBS="-ledit $LIBS"
  AC_DEFINE([HAVE_LIBEDIT], [1], [If the libedit library is available])
fi

# check for boost_regex
AC_CACHE_CHECK(
  [if boost_regex is available],
  [boost_regex_avail_cv_],
  [boost_regex_save_libs=$LIBS
   LIBS="-lboost_regex$BOOST_SUFFIX $LIBS"
   AC_LANG_PUSH(C++)
   AC_LINK_IFELSE(
     [AC_LANG_PROGRAM(
       [[#include <boost/regex.hpp>]],
       [[boost::regex foo_regexp("Hello, world!");]])],
     [boost_regex_avail_cv_=true],
     [boost_regex_avail_cv_=false])
   AC_LANG_POP
   LIBS=$boost_regex_save_libs])

if [test x$boost_regex_avail_cv_ = xtrue ]; then
  LIBS="-lboost_regex$BOOST_SUFFIX $LIBS"
else
  AC_MSG_FAILURE("Could not find boost_regex library (set CPPFLAGS and LDFLAGS?)")
fi

AC_CACHE_CHECK(
  [if boost_regex w/ICU is available],
  [boost_regex_icu_avail_cv_],
  [boost_regex_icu_save_libs=$LIBS
   LIBS="-licuuc $LIBS"
   AC_LANG_PUSH(C++)
   AC_LINK_IFELSE(
     [AC_LANG_PROGRAM(
       [[#include <boost/regex/icu.hpp>
         using namespace boost;]],
       [[std::string text = "Активы";
         u32regex r = make_u32regex("активы", regex::perl | regex::icase);
         return u32regex_search(text, r) ? 0 : 1;]])],
     [boost_regex_icu_avail_cv_=true],
     [boost_regex_icu_avail_cv_=false])
   AC_LANG_POP
   LIBS=$boost_regex_icu_save_libs])

if [test x$boost_regex_icu_avail_cv_ = xtrue ]; then
  AC_DEFINE([HAVE_BOOST_REGEX_UNICODE], [1], [If the boost_regex library w/ICU is available])
  LIBS="-licuuc $LIBS"
fi

# check for boost_date_time
AC_CACHE_CHECK(
  [if boost_date_time is available],
  [boost_date_time_cpplib_avail_cv_],
  [boost_date_time_save_libs=$LIBS
   LIBS="-lboost_date_time$BOOST_SUFFIX $LIBS"
   AC_LANG_PUSH(C++)
   AC_LINK_IFELSE(
     [AC_LANG_PROGRAM(
        [[#include <boost/date_time/posix_time/posix_time.hpp>
          #include <boost/date_time/gregorian/gregorian.hpp>
          #include <boost/date_time/local_time_adjustor.hpp>
          #include <boost/date_time/time_duration.hpp>

          using namespace boost::posix_time;
          using namespace boost::date_time;

          #include <ctime>

          inline ptime time_to_system_local(const ptime& when) {
            struct std::tm tm_gmt = to_tm(when);
            return from_time_t(mktime(&tm_gmt));
          }]],
        [[ptime t10 = ptime(boost::gregorian::from_string("2007-01-15"),
                            ptime::time_duration_type());

          ptime t12 = time_to_system_local(t10);

          return t10 != t12;]])],
     [boost_date_time_cpplib_avail_cv_=true],
     [boost_date_time_cpplib_avail_cv_=false])
   AC_LANG_POP
   LIBS=$boost_date_time_save_libs])

if [test x$boost_date_time_cpplib_avail_cv_ = xtrue ]; then
  LIBS="-lboost_date_time$BOOST_SUFFIX $LIBS"
else
  AC_MSG_FAILURE("Could not find boost_date_time library (set CPPFLAGS and LDFLAGS?)")
fi

# check for boost_filesystem
AC_CACHE_CHECK(
  [if boost_filesystem is available],
  [boost_filesystem_cpplib_avail_cv_],
  [boost_filesystem_save_libs=$LIBS
   LIBS="-lboost_filesystem$BOOST_SUFFIX -lboost_system$BOOST_SUFFIX $LIBS"
   AC_LANG_PUSH(C++)
   AC_LINK_IFELSE(
     [AC_LANG_PROGRAM(
        [[#include <boost/filesystem/path.hpp>]],
        [[boost::filesystem::path this_path("Hello");]])],
     [boost_filesystem_cpplib_avail_cv_=true],
     [boost_filesystem_cpplib_avail_cv_=false])
   AC_LANG_POP
   LIBS=$boost_filesystem_save_libs])

if [test x$boost_filesystem_cpplib_avail_cv_ = xtrue ]; then
  LIBS="-lboost_filesystem$BOOST_SUFFIX -lboost_system$BOOST_SUFFIX $LIBS"
else
  AC_MSG_FAILURE("Could not find boost_filesystem library (set CPPFLAGS and LDFLAGS?)")
fi

# check for boost_iostreams
AC_CACHE_CHECK(
  [if boost_iostreams is available],
  [boost_iostreams_cpplib_avail_cv_],
  [boost_iostreams_save_libs=$LIBS
   LIBS="-lboost_iostreams$BOOST_SUFFIX -lboost_system$BOOST_SUFFIX $LIBS"
   AC_LANG_PUSH(C++)
   AC_LINK_IFELSE(
     [AC_LANG_PROGRAM(
        [[#include <boost/iostreams/device/file_descriptor.hpp> 
          #include <boost/iostreams/stream.hpp>]],
        [[namespace io = boost::iostreams; 
          typedef io::stream<io::file_descriptor_sink> ofdstream; 
          ofdstream outstream(1);]])],
     [boost_iostreams_cpplib_avail_cv_=true],
     [boost_iostreams_cpplib_avail_cv_=false])
   AC_LANG_POP
   LIBS=$boost_iostreams_save_libs])

if [test x$boost_iostreams_cpplib_avail_cv_ = xtrue ]; then
  LIBS="-lboost_iostreams$BOOST_SUFFIX -lboost_system$BOOST_SUFFIX $LIBS"
else
  AC_MSG_FAILURE("Could not find boost_iostreams library (set CPPFLAGS and LDFLAGS?)")
fi

# check for boost_serialization
#AC_CACHE_CHECK(
#  [if boost_serialization is available],
#  [boost_serialization_cpplib_avail_cv_],
#  [boost_serialization_save_libs=$LIBS
#   LIBS="-lboost_serialization$BOOST_SUFFIX -lboost_system$BOOST_SUFFIX $LIBS"
#   AC_LANG_PUSH(C++)
#   AC_LINK_IFELSE(
#     [AC_LANG_PROGRAM(
#        [[#include <boost/archive/binary_oarchive.hpp>
#          #include <iostream>
#          struct foo {
#            int a;
#            template<class Archive>
#            void serialize(Archive & ar, const unsigned int) {
#              ar & a;
#            }
#          };]],
#        [[boost::archive::binary_oarchive oa(std::cout);
#          foo x;
#          oa << x;]])],
#     [boost_serialization_cpplib_avail_cv_=true],
#     [boost_serialization_cpplib_avail_cv_=false])
#   AC_LANG_POP
#   LIBS=$boost_serialization_save_libs])
#
#if [test x$boost_serialization_cpplib_avail_cv_ = xtrue -a x$cache = xtrue]; then
#  AC_DEFINE([HAVE_BOOST_SERIALIZATION], [1], [Whether Boost.Serialization is available])
#  LIBS="-lboost_serialization$BOOST_SUFFIX $LIBS"
#fi
#AM_CONDITIONAL(HAVE_BOOST_SERIALIZATION, test x$boost_serialization_cpplib_avail_cv_ = xtrue -a x$cache = xtrue)
AM_CONDITIONAL(HAVE_BOOST_SERIALIZATION, false)

# check for Python
if [ test x$python = xtrue ]; then
  AM_PATH_PYTHON(2.4,, :)
  if [test "$PYTHON" != :]; then
    AM_CONDITIONAL(HAVE_PYTHON, true)
    AC_CACHE_CHECK(
      [if boost_python is available],
      [boost_python_cpplib_avail_cv_],
      [boost_python_save_libs=$LIBS
       LIBS="-lboost_python$BOOST_SUFFIX -lpython$PYTHON_VERSION $LIBS"
       AC_LANG_PUSH(C++)
       AC_LINK_IFELSE(
         [AC_LANG_PROGRAM(
            [[#include <boost/python.hpp>
              using namespace boost::python;
              class foo {};
              BOOST_PYTHON_MODULE(samp) {
                class_< foo > ("foo") ;
              }]],
            [[return 0]])],
         [boost_python_cpplib_avail_cv_=true],
         [boost_python_cpplib_avail_cv_=false])
       AC_LANG_POP
       LIBS=$boost_python_save_libs])
  
    if [ test x$boost_python_cpplib_avail_cv_ = xtrue ]; then
      AC_DEFINE([HAVE_BOOST_PYTHON], [1], [Whether Boost.Python is available])
      LIBS="-lboost_python$BOOST_SUFFIX -lpython$PYTHON_VERSION $LIBS"
    fi
  else
    AM_CONDITIONAL(HAVE_PYTHON, false)
  fi
  AM_CONDITIONAL(HAVE_BOOST_PYTHON, test x$boost_python_cpplib_avail_cv_ = xtrue)
else
  AM_CONDITIONAL(HAVE_PYTHON, false)
  AM_CONDITIONAL(HAVE_BOOST_PYTHON, false)
fi

# check for CppUnit
AC_CACHE_CHECK(
  [if cppunit is available],
  [cppunit_avail_cv_],
  [cppunit_save_libs=$LIBS
   LIBS="-lcppunit $LIBS"
   AC_LANG_PUSH(C++)
   AC_LINK_IFELSE(
     [AC_LANG_PROGRAM(
        [[#include <cppunit/CompilerOutputter.h>
          #include <cppunit/TestResult.h>
          #include <cppunit/TestResultCollector.h>
          #include <cppunit/TestRunner.h>
          #include <cppunit/TextTestProgressListener.h>
          #include <cppunit/BriefTestProgressListener.h>
          #include <cppunit/XmlOutputter.h>
          #include <cppunit/extensions/TestFactoryRegistry.h>]],
        [[CPPUNIT_NS::TestResult controller;
          CPPUNIT_NS::TestResultCollector result;]])],
     [cppunit_avail_cv_=true],
     [cppunit_avail_cv_=false])
   AC_LANG_POP
   LIBS=$cppunit_save_libs])

AM_CONDITIONAL(HAVE_CPPUNIT, test x$cppunit_avail_cv_ = xtrue)

# Checks for header files.
AC_HEADER_STDC
AC_HEADER_STAT
AC_CHECK_HEADERS([langinfo.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_TYPE_SIZE_T
AC_STRUCT_TM

# Checks for library functions.
#AC_FUNC_MKTIME
#AC_FUNC_STAT
#AC_FUNC_STRFTIME
AC_CHECK_FUNCS([access realpath getpwuid getpwnam isatty])

# Pepare the Makefiles
AC_CONFIG_FILES([Makefile po/Makefile.in intl/Makefile])
AC_OUTPUT
