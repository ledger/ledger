/*
 * Copyright (c) 2003-2009, John Wiegley.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * - Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 *
 * - Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in the
 *   documentation and/or other materials provided with the distribution.
 *
 * - Neither the name of New Artisans LLC nor the names of its
 *   contributors may be used to endorse or promote products derived from
 *   this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

/**
 * @addtogroup util
 */

/**
 * @file   system.hh
 * @author John Wiegley
 *
 * @ingroup util
 *
 * @brief Brief
 *
 * Long.
 */
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

#include "config.h"

#if defined(__GNUG__) && __GNUG__ < 3
#define _XOPEN_SOURCE
#endif

#include <algorithm>
#include <exception>
#include <typeinfo>
#include <stdexcept>
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
#include <set>
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

typedef std::size_t istream_pos_type;
typedef std::size_t ostream_pos_type;

#else // ! (defined(__GNUG__) && __GNUG__ < 3)

typedef std::istream::pos_type istream_pos_type;
typedef std::ostream::pos_type ostream_pos_type;

#endif

#include <cassert>
#include <cctype>
#include <cstdarg>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <ctime>
#include <csignal>

#if defined __FreeBSD__ && __FreeBSD__ <= 4
// FreeBSD has a broken isspace macro, so don't use it
#undef isspace(c)
#endif

#include <sys/stat.h>
#ifdef WIN32
#include <io.h>
#else
#include <unistd.h>
#endif
#if defined(HAVE_GETPWUID) || defined(HAVE_GETPWNAM)
#include <pwd.h>
#endif

#ifdef HAVE_UNIX_PIPES
#include <sys/types.h>
#include <sys/wait.h>
#include "fdstream.h"
#endif
#if defined(HAVE_GETTEXT)
#include "gettext.h"
#define _(str) gettext(str)
#else
#define _(str) str
#endif

#include <gmp.h>
#include <mpfr.h>
#include "sha1.h"
#include "utf8.h"

#ifdef HAVE_LIBEDIT
#include <editline/readline.h>
#endif

#include <boost/algorithm/string/classification.hpp>
#include <boost/algorithm/string/predicate.hpp>
#include <boost/any.hpp>
#include <boost/bind.hpp>
#include <boost/cast.hpp>
#include <boost/current_function.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/filesystem/convenience.hpp>
#include <boost/filesystem/exception.hpp>
#include <boost/filesystem/fstream.hpp>
#include <boost/filesystem/operations.hpp>
#include <boost/filesystem/path.hpp>
#include <boost/foreach.hpp>
#include <boost/function.hpp>
#include <boost/intrusive_ptr.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/operators.hpp>
#include <boost/optional.hpp>
#include <boost/ptr_container/ptr_list.hpp>
#include <boost/random/mersenne_twister.hpp>
#include <boost/random/uniform_int.hpp>
#include <boost/random/uniform_real.hpp>
#include <boost/random/variate_generator.hpp>
#include <boost/regex.hpp>
#include <boost/variant.hpp>
#include <boost/version.hpp>

#if defined(HAVE_BOOST_PYTHON)

#include <boost/python.hpp>

#include <boost/python/detail/wrap_python.hpp>
#include <datetime.h>

#include <boost/python/exception_translator.hpp>
#include <boost/python/implicit.hpp>
#include <boost/python/args.hpp>
#include <boost/python/module.hpp>
#include <boost/python/def.hpp>
#include <boost/python/to_python_converter.hpp>
#include <boost/python/module_init.hpp>

#endif // HAVE_BOOST_PYTHON

#endif // _SYSTEM_HH
