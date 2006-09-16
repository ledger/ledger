#ifndef _PCH_H
#define _PCH_H

#include "ledger.h"

#if defined(__GNUG__) && __GNUG__ < 3
#define _XOPEN_SOURCE
#endif

#include <deque>
#include <iterator>
#include <list>
#include <map>
#include <string>

#include <algorithm>
#include <exception>
#include <memory>

#include <fstream>
#include <iostream>
#include <istream>
#include <sstream>

#include <cctype>
#include <cstdarg>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <ctime>

#include <gmp.h>
#include <pcre.h>

#include <sys/stat.h>
#include <unistd.h>		// for the `write' method

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

extern "C" {
#if defined(HAVE_EXPAT)
#include <expat.h>           // expat XML parser
#elif defined(HAVE_XMLPARSE)
#include <xmlparse.h>        // expat XML parser
#endif
}

#if defined(HAVE_GETPWUID) || defined(HAVE_GETPWNAM)
#include <pwd.h>
#endif

#ifdef HAVE_LIBOFX
#include <libofx.h>
#endif

#ifdef USE_BOOST_PYTHON
#include <boost/python.hpp>
#include <boost/python/exception_translator.hpp>
#include <boost/python/detail/api_placeholder.hpp>
#include <boost/python/suite/indexing/map_indexing_suite.hpp>
#include <Python.h>
#endif

#endif // _PCH_H
