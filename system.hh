#ifndef _SYSTEM_HH
#define _SYSTEM_HH

/**
 * @file   system.hh
 * @author John Wiegley
 * @date   Mon Apr 23 03:43:05 2007
 * 
 * @brief  All system headers needed by Ledger.
 *
 * These are collected here so that a pre-compiled header can be made.
 * None of these header files (with the exception of acconf.h, when
 * configure is re-run) are expected to change.
 */

#include "acconf.h"

#if defined(__GNUG__) && __GNUG__ < 3
#define _XOPEN_SOURCE
#endif

#include <algorithm>
#include <exception>
#include <iostream>
#include <streambuf>
#include <iomanip>
#include <fstream>
#include <sstream>
#include <iterator>
#include <list>
#include <map>
#include <memory>
#include <new>
#include <stack>
#include <string>
#include <vector>

#if defined(__GNUG__) && __GNUG__ < 3
namespace std {
  inline ostream & right (ostream & i) {
    i.setf(i.right, i.adjustfield);
    return i;
  }
  inline ostream & left (ostream & i) {
    i.setf(i.left, i.adjustfield);
    return i;
  }
}
#endif

#include <cassert>
#include <cctype>
#include <cstdarg>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <ctime>

#if defined __FreeBSD__ && __FreeBSD__ <= 4
// FreeBSD has a broken isspace macro, so don't use it
#undef isspace(c)
#endif

#include <sys/stat.h>

#ifdef HAVE_UNIX_PIPES
#include <sys/types.h>
#include <sys/wait.h>
#include "fdstream.hpp"
#endif

#ifdef WIN32
#include <io.h>
#else
#include <unistd.h>
#endif

#if defined(HAVE_GETPWUID) || defined(HAVE_GETPWNAM)
#include <pwd.h>
#endif

#if defined(HAVE_NL_LANGINFO)
#include <langinfo.h>
#endif

#include <gmp.h>

#define HAVE_GDTOA 1
#ifdef HAVE_GDTOA
#include "gdtoa/gdtoa.h"
#endif

extern "C" {
#if defined(HAVE_EXPAT)
#include <expat.h>		// expat XML parser
#elif defined(HAVE_XMLPARSE)
#include <xmlparse.h>		// expat XML parser
#endif
}

#if defined(HAVE_LIBOFX)
#include <libofx.h>
#endif

#endif // _SYSTEM_HH
