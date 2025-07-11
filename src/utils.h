/*
 * Copyright (c) 2003-2023, John Wiegley.  All rights reserved.
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
 * @defgroup util General utilities
 */

/**
 * @file   utils.h
 * @author John Wiegley
 *
 * @ingroup util
 *
 * @brief General utility facilities used by Ledger
 */
#pragma once

#include <boost/uuid/detail/sha1.hpp>
#include <ledger.hh>

#define TIMERS_ON   1

/**
 * @name Forward declarations
 */
/*@{*/

namespace ledger {
  using namespace boost;

  typedef std::string string;
  typedef std::list<string> strings_list;

  typedef posix_time::ptime         ptime;
  typedef ptime::time_duration_type time_duration;
  typedef gregorian::date           date;
  typedef gregorian::date_duration  date_duration;
  typedef posix_time::seconds       seconds;

  typedef boost::filesystem::path             path;
  typedef boost::filesystem::ifstream         ifstream;
  typedef boost::filesystem::ofstream         ofstream;
  typedef boost::filesystem::filesystem_error filesystem_error;
}

/*@}*/

/**
 * @name Assertions
 */
/*@{*/

#ifdef assert
#undef assert
#endif

#if !NO_ASSERTS

namespace ledger {
  void debug_assert(const string& reason, const string& func,
                    const string& file, std::size_t line);
}

#define assert(x)                                               \
  ((x) ? ((void)0) : debug_assert(#x, BOOST_CURRENT_FUNCTION,   \
                                  __FILE__, __LINE__))

#else // !NO_ASSERTS

#define assert(x) ((void)(x))

#endif // !NO_ASSERTS

/*@}*/

/**
 * @name Verification (i.e., heavy asserts)
 */
/*@{*/

#if VERIFY_ON

namespace ledger {

extern bool verify_enabled;

#define VERIFY(x)   if (ledger::verify_enabled) { assert(x); }
#define DO_VERIFY() ledger::verify_enabled

void initialize_memory_tracing();
void shutdown_memory_tracing();

std::size_t current_memory_size();
std::size_t current_objects_size();

void trace_ctor_func(void * ptr, const char * cls_name, const char * args,
                     std::size_t cls_size);
void trace_dtor_func(void * ptr, const char * cls_name, std::size_t cls_size);

#define TRACE_CTOR(cls, args) \
  (DO_VERIFY() ? \
   ledger::trace_ctor_func(this, #cls, args, sizeof(cls)) : ((void)0))
#define TRACE_DTOR(cls) \
  (DO_VERIFY() ? \
   ledger::trace_dtor_func(this, #cls, sizeof(cls)) : ((void)0))

void report_memory(std::ostream& out, bool report_all = false);

} // namespace ledger

#else // ! VERIFY_ON

#define VERIFY(x)
#define DO_VERIFY() false
#define TRACE_CTOR(cls, args)
#define TRACE_DTOR(cls)

#endif // VERIFY_ON

#define IF_VERIFY() if (DO_VERIFY())

/*@}*/

/**
 * @name String wrapper
 *
 * This string type is a wrapper around std::string that allows us to trace
 * constructor and destructor calls.  It also makes ledger's use of strings a
 * unique type, that the Boost.Python code can use as the basis for
 * transparent Unicode conversions.
 */
/*@{*/

namespace ledger {

extern string empty_string;

strings_list split_arguments(const char * line);

inline string to_string(long num) {
  std::ostringstream buf;
  buf << num;
  return buf.str();
}

inline string to_string(std::size_t num) {
  std::ostringstream buf;
  buf << num;
  return buf.str();
}

inline string lowered(const string& str) {
  string tmp(str);
  to_lower(tmp);
  return tmp;
}

inline string operator+(const char * left, const string& right) {
  return string(left) + right;
}

} // namespace ledger

/*@}*/

/**
 * @name Tracing and logging
 */
/*@{*/

namespace ledger {

enum log_level_t {
  LOG_OFF = 0,
  LOG_CRIT,
  LOG_FATAL,
  LOG_ASSERT,
  LOG_ERROR,
  LOG_VERIFY,
  LOG_WARN,
  LOG_INFO,
  LOG_EXCEPT,
  LOG_DEBUG,
  LOG_TRACE,
  LOG_ALL
};

extern log_level_t        _log_level;
extern std::ostream *     _log_stream;
extern std::ostringstream _log_buffer;

void logger_func(log_level_t level);

#define LOGGER(cat) \
    static const char * const _this_category = cat

#if TRACING_ON

extern uint16_t _trace_level;

#define SHOW_TRACE(lvl) \
  (ledger::_log_level >= ledger::LOG_TRACE && lvl <= ledger::_trace_level)
#define TRACE(lvl, msg) \
  (SHOW_TRACE(lvl) ? \
   ((ledger::_log_buffer << msg), \
    ledger::logger_func(ledger::LOG_TRACE)) : (void)0)

#else // TRACING_ON

#define SHOW_TRACE(lvl) false
#define TRACE(lvl, msg)

#endif // TRACING_ON

#if DEBUG_ON

extern optional<std::string>       _log_category;
#if HAVE_BOOST_REGEX_UNICODE
  extern optional<boost::u32regex> _log_category_re;
#else
  extern optional<boost::regex>    _log_category_re;
#endif

inline bool category_matches(const char * cat) {
  if (_log_category) {
    if (! _log_category_re) {
      _log_category_re =
#if HAVE_BOOST_REGEX_UNICODE
        boost::make_u32regex(_log_category->c_str(),
                             boost::regex::perl | boost::regex::icase);
#else
        boost::regex(_log_category->c_str(),
                     boost::regex::perl | boost::regex::icase);
#endif
    }
#if HAVE_BOOST_REGEX_UNICODE
    return boost::u32regex_search(cat, *_log_category_re);
#else
    return boost::regex_search(cat, *_log_category_re);
#endif
  }
  return false;
}

#define SHOW_DEBUG(cat) \
  (ledger::_log_level >= ledger::LOG_DEBUG && ledger::category_matches(cat))
#define SHOW_DEBUG_() SHOW_DEBUG(_this_category)

#define DEBUG(cat, msg) \
  (SHOW_DEBUG(cat) ? \
   ((ledger::_log_buffer << msg), \
    ledger::logger_func(ledger::LOG_DEBUG)) : (void)0)
#define DEBUG_(msg) DEBUG(_this_category, msg)

#else // DEBUG_ON

#define SHOW_DEBUG(cat) false
#define SHOW_DEBUG_()   false
#define DEBUG(cat, msg)
#define DEBUG_(msg)

#endif // DEBUG_ON

#define LOG_MACRO(level, msg) \
  (ledger::_log_level >= level ? \
   ((ledger::_log_buffer << msg), ledger::logger_func(level)) : (void)0)

#define SHOW_INFO()     (ledger::_log_level >= ledger::LOG_INFO)
#define SHOW_WARN()     (ledger::_log_level >= ledger::LOG_WARN)
#define SHOW_ERROR()    (ledger::_log_level >= ledger::LOG_ERROR)
#define SHOW_FATAL()    (ledger::_log_level >= ledger::LOG_FATAL)
#define SHOW_CRITICAL() (ledger::_log_level >= ledger::LOG_CRIT)

#define INFO(msg)      LOG_MACRO(ledger::LOG_INFO, msg)
#define WARN(msg)      LOG_MACRO(ledger::LOG_WARN, msg)
#define ERROR(msg)     LOG_MACRO(ledger::LOG_ERROR, msg)
#define FATAL(msg)     LOG_MACRO(ledger::LOG_FATAL, msg)
#define CRITICAL(msg)  LOG_MACRO(ledger::LOG_CRIT, msg)
#define EXCEPTION(msg) LOG_MACRO(ledger::LOG_EXCEPT, msg)

} // namespace ledger

#define IF_TRACE(lvl) if (SHOW_TRACE(lvl))
#define IF_DEBUG(cat) if (SHOW_DEBUG(cat))
#define IF_DEBUG_()   if (SHOW_DEBUG_())
#define IF_INFO()     if (SHOW_INFO())
#define IF_WARN()     if (SHOW_WARN())
#define IF_ERROR()    if (SHOW_ERROR())
#define IF_FATAL()    if (SHOW_FATAL())
#define IF_CRITICAL() if (SHOW_CRITICAL())

/*@}*/

/**
 * @name Timers
 * This allows log xacts to specify cumulative time spent.
 */
/*@{*/

#if TIMERS_ON

namespace ledger {

void start_timer(const char * name, log_level_t lvl);
void stop_timer(const char * name);
void finish_timer(const char * name);

#if TRACING_ON
#define TRACE_START(name, lvl, msg) \
  (SHOW_TRACE(lvl) ? \
   ((ledger::_log_buffer << msg), \
    ledger::start_timer(#name, ledger::LOG_TRACE)) : ((void)0))
#define TRACE_STOP(name, lvl) \
  (SHOW_TRACE(lvl) ? ledger::stop_timer(#name) : ((void)0))
#define TRACE_FINISH(name, lvl) \
  (SHOW_TRACE(lvl) ? ledger::finish_timer(#name) : ((void)0))
#else
#define TRACE_START(name, lvl, msg)
#define TRACE_STOP(name, lvl)
#define TRACE_FINISH(name, lvl)
#endif

#if DEBUG_ON
#define DEBUG_START(name, cat, msg) \
  (SHOW_DEBUG(cat) ? \
   ((ledger::_log_buffer << msg), \
    ledger::start_timer(#name, ledger::LOG_DEBUG)) : ((void)0))
#define DEBUG_START_(name, msg) \
  DEBUG_START_(name, _this_category, msg)
#define DEBUG_STOP(name, cat) \
  (SHOW_DEBUG(cat) ? ledger::stop_timer(#name) : ((void)0))
#define DEBUG_STOP_(name) \
  DEBUG_STOP_(name, _this_category)
#define DEBUG_FINISH(name, cat) \
  (SHOW_DEBUG(cat) ? ledger::finish_timer(#name) : ((void)0))
#define DEBUG_FINISH_(name) \
  DEBUG_FINISH_(name, _this_category)
#else
#define DEBUG_START(name, cat, msg)
#define DEBUG_START_(name, msg)
#define DEBUG_STOP(name)
#define DEBUG_FINISH(name)
#endif

#define INFO_START(name, msg) \
  (SHOW_INFO() ? \
   ((ledger::_log_buffer << msg), \
    ledger::start_timer(#name, ledger::LOG_INFO)) : ((void)0))
#define INFO_STOP(name) \
  (SHOW_INFO() ? stop_timer(#name) : ((void)0))
#define INFO_FINISH(name) \
  (SHOW_INFO() ? finish_timer(#name) : ((void)0))

} // namespace ledger

#else // !TIMERS_ON

#define TRACE_START(lvl, msg, name)
#define TRACE_STOP(name, lvl)
#define TRACE_FINISH(name, lvl)

#define DEBUG_START(name, msg)
#define DEBUG_START_(name, cat, msg)
#define DEBUG_STOP(name)
#define DEBUG_FINISH(name)

#define INFO_START(name, msg)
#define INFO_STOP(name)
#define INFO_FINISH(name)

#endif // TIMERS_ON

/*@}*/

/*
 * These files define the other internal facilities.
 */

#include "error.h"

enum caught_signal_t {
  NONE_CAUGHT,
  INTERRUPTED,
  PIPE_CLOSED
};

extern caught_signal_t caught_signal;

void sigint_handler(int sig);
void sigpipe_handler(int sig);

inline void check_for_signal() {
  switch (caught_signal) {
  case NONE_CAUGHT:
    break;
  case INTERRUPTED:
    throw std::runtime_error(_("Interrupted by user (use Control-D to quit)"));
  case PIPE_CLOSED:
    throw std::runtime_error(_("Pipe terminated"));
  }
}

/**
 * @name General utility functions
 */
/*@{*/

#define foreach BOOST_FOREACH
using std::unique_ptr;

namespace ledger {

template <typename T, typename U>
inline T& downcast(U& object) {
  return *polymorphic_downcast<T *>(&object);
}

path resolve_path(const path& pathname);

inline const string& either_or(const string& first,
                               const string& second) {
  return first.empty() ? second : first;
}

inline char * skip_ws(char * ptr) {
  while (*ptr == ' ' || *ptr == '\t' || *ptr == '\n')
    ptr++;
  return ptr;
}

inline char * trim_ws(char * ptr) {
  std::size_t len = std::strlen(ptr);
  int i = int(len) - 1;
  while (i >= 0 && (ptr[i] == ' ' || ptr[i] == '\t' || ptr[i] == '\n'))
    ptr[i--] = '\0';
  return skip_ws(ptr);
}

inline char * next_element(char * buf, bool variable = false) {
  for (char * p = buf; *p; p++) {
    if (! (*p == ' ' || *p == '\t'))
      continue;

    if (! variable) {
      *p = '\0';
      return skip_ws(p + 1);
    }
    else if (*p == '\t') {
      *p = '\0';
      return skip_ws(p + 1);
    }
    else if (*(p + 1) == ' ') {
      *p = '\0';
      return skip_ws(p + 2);
    }
  }
  return NULL;
}

inline int peek_next_nonws(std::istream& in) {
  int c = in.peek();
  while (in.good() && ! in.eof() && std::isspace(c)) {
    in.get();
    c = in.peek();
  }
  return c;
}

#define READ_INTO(str, targ, size, var, cond) {         \
    char * _p = targ;                                   \
    var = str.peek();                                   \
    while (str.good() && ! str.eof() && var != '\n' &&  \
           (cond) && _p - targ < size) {                \
      var = str.get();                                  \
      if (str.eof())                                    \
        break;                                          \
      if (var == '\\') {                                \
        var = str.get();                                \
        if (in.eof())                                   \
          break;                                        \
        switch (var) {                                  \
        case 'b': var = '\b'; break;                    \
        case 'f': var = '\f'; break;                    \
        case 'n': var = '\n'; break;                    \
        case 'r': var = '\r'; break;                    \
        case 't': var = '\t'; break;                    \
        case 'v': var = '\v'; break;                    \
        default: break;                                 \
        }                                               \
      }                                                 \
      *_p++ = var;                                      \
      var = str.peek();                                 \
    }                                                   \
    *_p = '\0';                                         \
  }

#define READ_INTO_(str, targ, size, var, idx, cond) {   \
    char * _p = targ;                                   \
    var = str.peek();                                   \
    while (str.good() && ! str.eof() && var != '\n' &&  \
           (cond) && _p - targ < size) {                \
      var = str.get();                                  \
      if (str.eof())                                    \
        break;                                          \
      idx++;                                            \
      if (var == '\\') {                                \
        var = str.get();                                \
        if (in.eof())                                   \
          break;                                        \
        switch (var) {                                  \
        case 'b': var = '\b'; break;                    \
        case 'f': var = '\f'; break;                    \
        case 'n': var = '\n'; break;                    \
        case 'r': var = '\r'; break;                    \
        case 't': var = '\t'; break;                    \
        case 'v': var = '\v'; break;                    \
        default: break;                                 \
        }                                               \
        idx++;                                          \
      }                                                 \
      *_p++ = var;                                      \
      var = str.peek();                                 \
    }                                                   \
    *_p = '\0';                                         \
  }

inline string digest_to_hex(
  const boost::uuids::detail::sha1::digest_type& message_digest,
  size_t len = sizeof(boost::uuids::detail::sha1::digest_type) * 2
) {
  std::ostringstream buf;
  buf.setf(std::ios_base::hex, std::ios_base::basefield);
  buf.fill('0');

  // sha1::digest_type is an array type and may change between Boost versions
  const size_t count = std::min(
    sizeof(message_digest) / sizeof(message_digest[0]),
    (len - 1) / (sizeof(message_digest[0]) * 2) + 1
  );
  for(size_t i = 0; i < count; i++) {
    buf.width(sizeof(message_digest[i]) * 2);
    buf << (unsigned int)message_digest[i];
  }
  string hex = buf.str();
  hex.resize(len, '0'); // in case a partial element is requested
  return hex;
}

inline string sha1sum(
  const string& str,
  size_t len = sizeof(boost::uuids::detail::sha1::digest_type) * 2
) {
	static boost::uuids::detail::sha1 sha;
  boost::uuids::detail::sha1::digest_type message_digest;

	sha.reset();
  sha.process_bytes(str.c_str(), str.length());
  sha.get_digest(message_digest);
  return digest_to_hex(message_digest, len);
}

extern const string version;

enum hash_type_t {
  NO_HASHES = 0,
  HASH_SHA512 = 1,
  HASH_SHA512_Half = 2
};

} // namespace ledger

/*@}*/
