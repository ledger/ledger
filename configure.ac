#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.59)
AC_INIT(ledger, 2.0, johnw@netartisans.com)
AM_INIT_AUTOMAKE(ledger, 2.0)
AC_CONFIG_SRCDIR([main.cc])
AC_CONFIG_HEADER([acconf.h])

# Checks for programs.
AC_PROG_CXX
AC_PROG_MAKE_SET
AC_PROG_RANLIB

# Checks for libraries.
AC_CHECK_LIB([gmp], [__gmpz_add])
AC_CHECK_LIB([pcre], [pcre_compile])
AC_CHECK_LIB([xmlparse], [XML_ParserCreate])

# Checks for header files.
AC_STDC_HEADERS
AC_HAVE_HEADERS(sys/stat.h)

# Checks for typedefs, structures, and compiler characteristics.
AC_HEADER_STDBOOL
AC_TYPE_SIZE_T
AC_STRUCT_TM

# Checks for library functions.
#AC_FUNC_ERROR_AT_LINE
AC_HEADER_STDC
#AC_FUNC_MALLOC
#AC_FUNC_MKTIME
#AC_FUNC_STAT
#AC_FUNC_STRFTIME
AC_CHECK_FUNCS([memset strchr strstr])

AC_CONFIG_FILES([Makefile])
AC_OUTPUT
