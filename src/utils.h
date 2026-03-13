/*
 * Copyright (c) 2003-2025, John Wiegley.  All rights reserved.
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
 * @brief General utility facilities used by Ledger.
 *
 * This header is the foundation of Ledger's infrastructure layer.  It provides:
 *
 * - **Type aliases** for commonly used Boost and STL types (date, path, string)
 * - **Assertion macros** (`assert()`) that throw rather than abort
 * - **Memory verification** via `TRACE_CTOR`/`TRACE_DTOR` for leak detection
 * - **Logging and tracing** at multiple severity levels (`LOG_TRACE` through
 *   `LOG_CRIT`), controlled by `--debug` and `--trace` command-line options
 * - **Cumulative timers** (`TRACE_START`/`TRACE_STOP`/`TRACE_FINISH`) for
 *   profiling parsing, report generation, and other phases
 * - **Signal handling** for graceful interruption via Ctrl-C
 * - **String utilities** for whitespace manipulation and argument splitting
 * - **SHA-1 hashing** used to generate stable identifiers for transactions
 *
 * Nearly every source file in Ledger includes this header (typically via
 * `system.hh`), making it the shared vocabulary for the entire codebase.
 */
#pragma once

#include <boost/uuid/detail/sha1.hpp>
#include <ledger.hh>

#define TIMERS_ON 1 ///< Enable cumulative timer instrumentation

/**
 * @name Forward declarations and type aliases
 *
 * These aliases provide a uniform vocabulary throughout the codebase,
 * insulating Ledger source files from the particular Boost and STL
 * namespaces used for dates, paths, and strings.
 */
/*@{*/

namespace ledger {
using namespace boost;

using string = std::string;             ///< Default string type
using string_view = std::string_view;   ///< Non-owning string view
using strings_list = std::list<string>; ///< List of strings (e.g., split arguments)

using ptime = posix_time::ptime;                 ///< Point in time (date + time)
using time_duration = ptime::time_duration_type; ///< Duration between two time points
using date = gregorian::date;                    ///< Calendar date (year/month/day)
using date_duration = gregorian::date_duration;  ///< Duration between two dates
using seconds = posix_time::seconds;             ///< Duration in whole seconds

using path = std::filesystem::path;                         ///< Filesystem path
using ifstream = std::ifstream;                             ///< Input file stream
using ofstream = std::ofstream;                             ///< Output file stream
using filesystem_error = std::filesystem::filesystem_error; ///< Filesystem error
} // namespace ledger

/*@}*/

/**
 * @name Assertions
 *
 * Ledger replaces the standard `assert()` macro with a version that throws
 * `assertion_failed` (a `std::logic_error` subclass) instead of calling
 * `abort()`.  This allows the test harness and interactive sessions to
 * report assertion failures as catchable exceptions with file/line context.
 *
 * Compile with `NO_ASSERTS=1` to elide all assertion checks for release
 * builds (the expression is still evaluated for side effects).
 */
/*@{*/

#ifdef assert
#undef assert
#endif

#if !NO_ASSERTS

namespace ledger {
/// Throws assertion_failed with context about the failing expression.
void debug_assert(const string& reason, const string& func, const string& file, std::size_t line);
} // namespace ledger

/// Asserts that @p x is true; throws assertion_failed on failure.
#define assert(x) ((x) ? ((void)0) : debug_assert(#x, BOOST_CURRENT_FUNCTION, __FILE__, __LINE__))

#else // !NO_ASSERTS

#define assert(x) ((void)(x))

#endif // !NO_ASSERTS

/*@}*/

/**
 * @name Verification (i.e., heavy asserts) and memory tracing
 *
 * The verification subsystem provides two capabilities:
 *
 * 1. **Heavy assertions** (`VERIFY(x)`) that are only evaluated when
 *    verification is globally enabled at runtime.  These are more expensive
 *    than normal assertions and are used for internal consistency checks.
 *
 * 2. **Object lifetime tracing** (`TRACE_CTOR`/`TRACE_DTOR`).  When enabled,
 *    every constructor/destructor call is recorded so that at shutdown the
 *    system can report leaked objects and their allocation sites.  This is
 *    Ledger's own lightweight leak detector, independent of tools like
 *    AddressSanitizer.
 *
 * Enable with `--verify` at runtime.  Compile with `VERIFY_ON=0` to remove
 * all verification overhead from release builds.
 */
/*@{*/

#if VERIFY_ON

namespace ledger {

extern bool verify_enabled; ///< Runtime toggle for verification checks

/// Assert @p x only when verification is enabled.
#define VERIFY(x)                                                                                  \
  if (ledger::verify_enabled) {                                                                    \
    assert(x);                                                                                     \
  }
/// Returns true if verification is currently active.
#define DO_VERIFY() ledger::verify_enabled

/// Allocate internal tracking maps for memory and object tracing.
void initialize_memory_tracing();
/// Print leak report (if any) and deallocate tracking maps.
void shutdown_memory_tracing();

/// Returns total bytes currently tracked in live memory allocations.
std::size_t current_memory_size();
/// Returns total bytes currently tracked in live traced objects.
std::size_t current_objects_size();

/// Record construction of an object at @p ptr for class @p cls_name.
void trace_ctor_func(void* ptr, const char* cls_name, const char* args, std::size_t cls_size);
/// Record destruction of an object at @p ptr for class @p cls_name.
void trace_dtor_func(void* ptr, const char* cls_name, std::size_t cls_size);

/// Trace construction of the current object (`this`).  Place in every
/// constructor body of traced classes.  The @p args string describes
/// which constructor overload was called (e.g. "copy", "const string&").
#define TRACE_CTOR(cls, args)                                                                      \
  (DO_VERIFY() ? ledger::trace_ctor_func(this, #cls, args, sizeof(cls)) : ((void)0))
/// Trace destruction of the current object (`this`).  Place in the
/// destructor body of every traced class.
#define TRACE_DTOR(cls) (DO_VERIFY() ? ledger::trace_dtor_func(this, #cls, sizeof(cls)) : ((void)0))

/// Print a memory and object report to @p out.
/// @param report_all  If true, include total (not just live) counts.
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

extern string empty_string; ///< Shared empty string instance, avoids repeated allocations

/// Split a command line into a list of arguments, respecting single and
/// double quoting as well as backslash escapes.  Used by the interactive
/// command parser and Python bindings.
strings_list split_arguments(const char* line);

/// Convert a long integer to its decimal string representation.
inline string to_string(long num) {
  std::ostringstream buf;
  buf << num;
  return buf.str();
}

/// Convert a size_t value to its decimal string representation.
inline string to_string(std::size_t num) {
  std::ostringstream buf;
  buf << num;
  return buf.str();
}

/// Return a lower-cased copy of @p str (using Boost's locale-aware to_lower).
inline string lowered(const string& str) {
  string tmp(str); // NOLINT(bugprone-unused-local-non-trivial-variable)
  to_lower(tmp);
  return tmp;
}

inline string operator+(const char* left, const string& right) {
  return string(left) + right;
}

} // namespace ledger

/*@}*/

/**
 * @name Tracing and logging
 *
 * Ledger has a multi-level logging system controlled by the `--verbose`,
 * `--debug CATEGORY`, and `--trace LEVEL` command-line options.  Each log
 * message is stamped with elapsed time (and, when verification is active,
 * current memory/object sizes).
 *
 * - **TRACE** messages track fine-grained execution flow.  They are
 *   controlled by a numeric level: `--trace 5` shows levels 1-5.
 * - **DEBUG** messages are filtered by a regex-matched category string
 *   (e.g. `--debug "history.find"`).  Source files declare their default
 *   category with `LOGGER("category.name")`.
 * - **INFO** through **CRITICAL** are unconditionally formatted when
 *   the log level is high enough.
 *
 * The `IF_TRACE`, `IF_DEBUG`, etc. macros allow guarding expensive
 * argument construction so it is skipped when the message would not
 * be emitted.
 */
/*@{*/

namespace ledger {

/// Severity levels for the logging system, from most to least severe.
/// The global `_log_level` must be >= a message's level for it to appear.
enum log_level_t : uint8_t {
  LOG_OFF = 0, ///< Logging disabled
  LOG_CRIT,    ///< Critical errors (unrecoverable)
  LOG_FATAL,   ///< Fatal errors
  LOG_ASSERT,  ///< Assertion failures
  LOG_ERROR,   ///< Recoverable errors
  LOG_VERIFY,  ///< Verification-level messages
  LOG_WARN,    ///< Warnings (default threshold)
  LOG_INFO,    ///< Informational messages (`--verbose`)
  LOG_EXCEPT,  ///< Exception details
  LOG_DEBUG,   ///< Debug messages (`--debug CATEGORY`)
  LOG_TRACE,   ///< Fine-grained tracing (`--trace LEVEL`)
  LOG_ALL      ///< Show everything
};

extern log_level_t _log_level;                      ///< Current global log level threshold
extern std::ostream* _log_stream;                   ///< Destination stream (defaults to stderr)
extern thread_local std::ostringstream _log_buffer; ///< Per-thread buffer for message assembly

/// Flush the thread-local `_log_buffer` to `_log_stream` with timestamp
/// and level prefix.  Called by all logging macros after building the message.
void logger_func(log_level_t level);

/// Declare the default debug category for the current translation unit.
/// Used by `DEBUG_()` and `SHOW_DEBUG_()` to avoid repeating the category.
#define LOGGER(cat) static const char* const _this_category = cat

#if TRACING_ON

extern uint16_t _trace_level; ///< Maximum trace level enabled (set by `--trace N`)

/// True if trace messages at the given numeric level should be emitted.
#define SHOW_TRACE(lvl) (ledger::_log_level >= ledger::LOG_TRACE && (lvl) <= ledger::_trace_level)
#define TRACE(lvl, ...)                                                                            \
  do {                                                                                             \
    if (SHOW_TRACE(lvl)) {                                                                         \
      ledger::_log_buffer << __VA_ARGS__;                                                          \
      ledger::logger_func(ledger::LOG_TRACE);                                                      \
    }                                                                                              \
  } while (false)

#else // TRACING_ON

#define SHOW_TRACE(lvl) false
#define TRACE(lvl, msg)

#endif // TRACING_ON

#if DEBUG_ON

extern optional<std::string> _log_category;
#if HAVE_BOOST_REGEX_UNICODE
extern optional<boost::u32regex> _log_category_re;
#else
extern optional<boost::regex> _log_category_re;
#endif

/// Test whether the given debug category matches the user-specified
/// `--debug` regex pattern.  The match is case-insensitive.
inline bool category_matches(const char* cat) {
  if (_log_category) {
    if (!_log_category_re) {
      _log_category_re =
#if HAVE_BOOST_REGEX_UNICODE
          boost::make_u32regex(_log_category->c_str(), boost::regex::perl | boost::regex::icase);
#else
          boost::regex(_log_category->c_str(), boost::regex::perl | boost::regex::icase);
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

#define SHOW_DEBUG(cat) (ledger::_log_level >= ledger::LOG_DEBUG && ledger::category_matches(cat))
#define SHOW_DEBUG_() SHOW_DEBUG(_this_category)

#define DEBUG(cat, ...)                                                                            \
  do {                                                                                             \
    if (SHOW_DEBUG(cat)) {                                                                         \
      ledger::_log_buffer << __VA_ARGS__;                                                          \
      ledger::logger_func(ledger::LOG_DEBUG);                                                      \
    }                                                                                              \
  } while (false)
#define DEBUG_(...) DEBUG(_this_category, __VA_ARGS__)

#else // DEBUG_ON

#define SHOW_DEBUG(cat) false
#define SHOW_DEBUG_() false
#define DEBUG(cat, msg)
#define DEBUG_(msg)

#endif // DEBUG_ON

#define LOG_MACRO(level, ...)                                                                      \
  do {                                                                                             \
    if (ledger::_log_level >= (level)) {                                                           \
      ledger::_log_buffer << __VA_ARGS__;                                                          \
      ledger::logger_func(level);                                                                  \
    }                                                                                              \
  } while (false)

#define SHOW_INFO() (ledger::_log_level >= ledger::LOG_INFO) ///< True if INFO messages are visible
#define SHOW_WARN() (ledger::_log_level >= ledger::LOG_WARN) ///< True if WARN messages are visible
#define SHOW_ERROR()                                                                               \
  (ledger::_log_level >= ledger::LOG_ERROR) ///< True if ERROR messages are visible
#define SHOW_FATAL()                                                                               \
  (ledger::_log_level >= ledger::LOG_FATAL) ///< True if FATAL messages are visible
#define SHOW_CRITICAL()                                                                            \
  (ledger::_log_level >= ledger::LOG_CRIT) ///< True if CRITICAL messages are visible

#define INFO(msg) LOG_MACRO(ledger::LOG_INFO, msg)        ///< Log an informational message
#define WARN(msg) LOG_MACRO(ledger::LOG_WARN, msg)        ///< Log a warning message
#define ERROR(msg) LOG_MACRO(ledger::LOG_ERROR, msg)      ///< Log an error message
#define FATAL(msg) LOG_MACRO(ledger::LOG_FATAL, msg)      ///< Log a fatal error message
#define CRITICAL(msg) LOG_MACRO(ledger::LOG_CRIT, msg)    ///< Log a critical error message
#define EXCEPTION(msg) LOG_MACRO(ledger::LOG_EXCEPT, msg) ///< Log exception details

} // namespace ledger

#define IF_TRACE(lvl) if (SHOW_TRACE(lvl))
#define IF_DEBUG(cat) if (SHOW_DEBUG(cat))
#define IF_DEBUG_() if (SHOW_DEBUG_())
#define IF_INFO() if (SHOW_INFO())
#define IF_WARN() if (SHOW_WARN())
#define IF_ERROR() if (SHOW_ERROR())
#define IF_FATAL() if (SHOW_FATAL())
#define IF_CRITICAL() if (SHOW_CRITICAL())

/*@}*/

/**
 * @name Cumulative timers
 *
 * Timers measure cumulative wall-clock time spent in named regions of code.
 * They appear in log output as, for example, `[INFO] Parsing journal: (42ms)`.
 *
 * - `start_timer` / `stop_timer` accumulate elapsed time across multiple
 *   start/stop pairs (useful for functions called repeatedly).
 * - `finish_timer` logs the total accumulated time and removes the timer.
 *
 * The `TRACE_START`/`DEBUG_START`/`INFO_START` macros combine the level
 * check with the timer start so that the timer description is only built
 * when the corresponding log level is active.
 */
/*@{*/

#if TIMERS_ON

namespace ledger {

/// Begin or resume the named timer at the given log level.
void start_timer(const char* name, log_level_t lvl);
/// Pause the named timer, accumulating elapsed time since last start.
void stop_timer(const char* name);
/// Log the total accumulated time for the named timer, then remove it.
void finish_timer(const char* name);

#if TRACING_ON
#define TRACE_START(name, lvl, ...)                                                                \
  do {                                                                                             \
    if (SHOW_TRACE(lvl)) {                                                                         \
      ledger::_log_buffer << __VA_ARGS__;                                                          \
      ledger::start_timer(#name, ledger::LOG_TRACE);                                               \
    }                                                                                              \
  } while (false)
#define TRACE_STOP(name, lvl) (SHOW_TRACE(lvl) ? ledger::stop_timer(#name) : ((void)0))
#define TRACE_FINISH(name, lvl) (SHOW_TRACE(lvl) ? ledger::finish_timer(#name) : ((void)0))
#else
#define TRACE_START(name, lvl, msg)
#define TRACE_STOP(name, lvl)
#define TRACE_FINISH(name, lvl)
#endif

#if DEBUG_ON
#define DEBUG_START(name, cat, ...)                                                                \
  do {                                                                                             \
    if (SHOW_DEBUG(cat)) {                                                                         \
      ledger::_log_buffer << __VA_ARGS__;                                                          \
      ledger::start_timer(#name, ledger::LOG_DEBUG);                                               \
    }                                                                                              \
  } while (false)
#define DEBUG_START_(name, msg) DEBUG_START_(name, _this_category, msg)
#define DEBUG_STOP(name, cat) (SHOW_DEBUG(cat) ? ledger::stop_timer(#name) : ((void)0))
#define DEBUG_STOP_(name) DEBUG_STOP_(name, _this_category)
#define DEBUG_FINISH(name, cat) (SHOW_DEBUG(cat) ? ledger::finish_timer(#name) : ((void)0))
#define DEBUG_FINISH_(name) DEBUG_FINISH_(name, _this_category)
#else
#define DEBUG_START(name, cat, msg)
#define DEBUG_START_(name, msg)
#define DEBUG_STOP(name)
#define DEBUG_FINISH(name)
#endif

#define INFO_START(name, ...)                                                                      \
  do {                                                                                             \
    if (SHOW_INFO()) {                                                                             \
      ledger::_log_buffer << __VA_ARGS__;                                                          \
      ledger::start_timer(#name, ledger::LOG_INFO);                                                \
    }                                                                                              \
  } while (false)
#define INFO_STOP(name) (SHOW_INFO() ? stop_timer(#name) : ((void)0))
#define INFO_FINISH(name) (SHOW_INFO() ? finish_timer(#name) : ((void)0))

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

/**
 * @name Signal handling
 *
 * Ledger installs handlers for SIGINT (Ctrl-C) and SIGPIPE (broken pipe,
 * e.g. when a pager exits early).  The handlers set a global flag that is
 * polled at safe points via `check_for_signal()`, which throws an exception
 * to unwind the current operation cleanly.
 */
/*@{*/

/// Tracks which signal, if any, has been caught since the last check.
enum caught_signal_t : uint8_t {
  NONE_CAUGHT, ///< No signal pending
  INTERRUPTED, ///< SIGINT received (user pressed Ctrl-C)
  PIPE_CLOSED  ///< SIGPIPE received (pager or pipe consumer exited)
};

extern caught_signal_t caught_signal; ///< Global signal flag, set by signal handlers

void sigint_handler(int sig);  ///< SIGINT handler -- sets caught_signal to INTERRUPTED
void sigpipe_handler(int sig); ///< SIGPIPE handler -- sets caught_signal to PIPE_CLOSED

/// Poll for a pending signal and throw an appropriate exception if one
/// has been caught.  Called at natural "safe points" during processing
/// (e.g. between transactions) to allow clean interruption.
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

/*@}*/

/**
 * @name General utility functions
 *
 * Low-level helpers for path resolution, string manipulation, hashing,
 * and type-safe downcasting.  These are used pervasively by the parser,
 * expression engine, and report formatters.
 */
/*@{*/

using std::unique_ptr;

namespace ledger {

/// Safely downcast a reference from base type @p U to derived type @p T.
/// Uses Boost's `polymorphic_downcast` which asserts correctness in debug
/// builds and is a static_cast in release builds.
template <typename T, typename U>
inline T& downcast(U& object) {
  return *polymorphic_downcast<T*>(&object);
}

/// Resolve a filesystem path, expanding `~` to the user's home directory
/// and normalizing the result.  On Cygwin, also converts Windows-style
/// drive letter paths (e.g. `C:\...`) to POSIX paths.
path resolve_path(const path& pathname);

/// Return @p first if non-empty, otherwise @p second.
inline const string& either_or(const string& first, const string& second) {
  return first.empty() ? second : first;
}

/// Advance @p ptr past any leading whitespace (space, tab, newline).
inline char* skip_ws(char* ptr) {
  while (*ptr == ' ' || *ptr == '\t' || *ptr == '\n')
    ptr++;
  return ptr;
}

/// Strip trailing whitespace from the string at @p ptr (in-place),
/// then skip leading whitespace and return a pointer to the first
/// non-whitespace character.
inline char* trim_ws(char* ptr) {
  std::size_t len = std::strlen(ptr);
  int i = int(len) - 1;
  while (i >= 0 && (ptr[i] == ' ' || ptr[i] == '\t' || ptr[i] == '\n'))
    ptr[i--] = '\0';
  return skip_ws(ptr);
}

/// Find the next whitespace-delimited element in @p buf.  Null-terminates
/// the current element and returns a pointer to the start of the next one.
///
/// When @p variable is true, elements are separated by either a tab or two
/// consecutive spaces (matching Ledger's journal format where a single space
/// may appear within a payee name, but two spaces or a tab separates fields).
///
/// @return Pointer to next element, or nullptr if there is none.
inline char* next_element(char* buf, bool variable = false) {
  for (char* p = buf; *p; p++) {
    if (!(*p == ' ' || *p == '\t'))
      continue;

    if (!variable) { // NOLINT(bugprone-branch-clone)
      *p = '\0';
      return skip_ws(p + 1);
    } else if (*p == '\t') {
      *p = '\0';
      return skip_ws(p + 1);
    } else if (*(p + 1) == ' ') {
      *p = '\0';
      return skip_ws(p + 2);
    }
  }
  return nullptr;
}

/// Consume whitespace from @p in and return the next non-whitespace
/// character (via peek, so it remains in the stream).
inline int peek_next_nonws(std::istream& in) {
  int c = in.peek();
  while (in.good() && !in.eof() && std::isspace(static_cast<unsigned char>(c))) {
    in.get();
    c = in.peek();
  }
  return c;
}

/// Read characters from stream @p str into buffer @p targ (max @p size bytes),
/// storing each character in @p var, continuing while @p cond is true.
/// Handles backslash escape sequences (\b, \f, \n, \r, \t, \v).
/// Stops at newline or end-of-stream.
#define READ_INTO(str, targ, size, var, cond)                                                      \
  {                                                                                                \
    char* _p = targ;                                                                               \
    (var) = (str).peek();                                                                          \
    while ((str).good() && !(str).eof() && (var) != '\n' && (cond) && _p - (targ) < (size)) {      \
      (var) = (str).get();                                                                         \
      if ((str).eof())                                                                             \
        break;                                                                                     \
      if ((var) == '\\') {                                                                         \
        (var) = (str).get();                                                                       \
        if (in.eof())                                                                              \
          break;                                                                                   \
        switch (var) {                                                                             \
        case 'b':                                                                                  \
          (var) = '\b';                                                                            \
          break;                                                                                   \
        case 'f':                                                                                  \
          (var) = '\f';                                                                            \
          break;                                                                                   \
        case 'n':                                                                                  \
          (var) = '\n';                                                                            \
          break;                                                                                   \
        case 'r':                                                                                  \
          (var) = '\r';                                                                            \
          break;                                                                                   \
        case 't':                                                                                  \
          (var) = '\t';                                                                            \
          break;                                                                                   \
        case 'v':                                                                                  \
          (var) = '\v';                                                                            \
          break;                                                                                   \
        default:                                                                                   \
          break;                                                                                   \
        }                                                                                          \
      }                                                                                            \
      *_p++ = var;                                                                                 \
      (var) = (str).peek();                                                                        \
    }                                                                                              \
    *_p = '\0';                                                                                    \
  }

/// Like READ_INTO but also increments a column index @p idx for each
/// character consumed, including both bytes of an escape sequence.
/// Used by the parser to track column positions for error reporting.
#define READ_INTO_(str, targ, size, var, idx, cond)                                                \
  {                                                                                                \
    char* _p = targ;                                                                               \
    (var) = (str).peek();                                                                          \
    while ((str).good() && !(str).eof() && (var) != '\n' && (cond) && _p - (targ) < (size)) {      \
      (var) = (str).get();                                                                         \
      if ((str).eof())                                                                             \
        break;                                                                                     \
      (idx)++;                                                                                     \
      if ((var) == '\\') {                                                                         \
        (var) = (str).get();                                                                       \
        if (in.eof())                                                                              \
          break;                                                                                   \
        switch (var) {                                                                             \
        case 'b':                                                                                  \
          (var) = '\b';                                                                            \
          break;                                                                                   \
        case 'f':                                                                                  \
          (var) = '\f';                                                                            \
          break;                                                                                   \
        case 'n':                                                                                  \
          (var) = '\n';                                                                            \
          break;                                                                                   \
        case 'r':                                                                                  \
          (var) = '\r';                                                                            \
          break;                                                                                   \
        case 't':                                                                                  \
          (var) = '\t';                                                                            \
          break;                                                                                   \
        case 'v':                                                                                  \
          (var) = '\v';                                                                            \
          break;                                                                                   \
        default:                                                                                   \
          break;                                                                                   \
        }                                                                                          \
        (idx)++;                                                                                   \
      }                                                                                            \
      *_p++ = var;                                                                                 \
      (var) = (str).peek();                                                                        \
    }                                                                                              \
    *_p = '\0';                                                                                    \
  }

/// Convert a SHA-1 digest to a hexadecimal string.
/// @param message_digest  The raw SHA-1 digest (array of unsigned ints).
/// @param len             Number of hex characters to produce (default: full digest).
inline string digest_to_hex(const boost::uuids::detail::sha1::digest_type& message_digest,
                            size_t len = sizeof(boost::uuids::detail::sha1::digest_type) * 2) {
  std::ostringstream buf;
  buf.setf(std::ios_base::hex, std::ios_base::basefield);
  buf.fill('0');

  // sha1::digest_type is an array type and may change between Boost versions
  const size_t count = std::min(sizeof(message_digest) / sizeof(message_digest[0]),
                                (len - 1) / (sizeof(message_digest[0]) * 2) + 1);
  for (size_t i = 0; i < count; i++) {
    buf.width(sizeof(message_digest[i]) * 2);
    buf << (unsigned int)message_digest[i];
  }
  string hex = buf.str();
  hex.resize(len, '0'); // in case a partial element is requested
  return hex;
}

/// Compute the SHA-1 hash of @p str and return it as a hex string.
/// Used to generate stable identifiers for automated transactions
/// and other content-addressed data.
/// @param len  Number of hex characters to return (default: full 40-char hash).
inline string sha1sum(const string& str,
                      size_t len = sizeof(boost::uuids::detail::sha1::digest_type) * 2) {
  boost::uuids::detail::sha1 sha;
  boost::uuids::detail::sha1::digest_type message_digest;

  sha.reset();
  sha.process_bytes(str.c_str(), str.length());
  sha.get_digest(message_digest);
  return digest_to_hex(message_digest, len);
}

// Forward-declare the SHA-512 implementation (defined in sha512.cc).
extern "C" unsigned char* SHA512(void* data, unsigned int data_len, unsigned char* digest);

/// Compute the SHA-512 hash of @p str and return the first 256 bits as a
/// 64-character lowercase hex string.  Used to generate stable content-
/// addressed identifiers (e.g. for CSV lines during import) with a stronger
/// hash than SHA-1.
inline string sha512_256sum(const string& str) {
  // SHA-512 produces 64 bytes; we keep only the first 32 (256 bits).
  static constexpr std::size_t DIGEST_BYTES = 64;
  static constexpr std::size_t HALF_BYTES = 32;
  unsigned char digest[DIGEST_BYTES];
  SHA512(const_cast<char*>(str.c_str()), static_cast<unsigned int>(str.size()), digest);
  std::ostringstream buf;
  buf << std::hex << std::setfill('0');
  for (std::size_t i = 0; i < HALF_BYTES; ++i)
    buf << std::setw(2) << static_cast<unsigned int>(digest[i]);
  return buf.str();
}

/// Selects which hash algorithm to use for transaction checksums.
enum hash_type_t : uint8_t {
  NO_HASHES = 0,       ///< No hashing
  HASH_SHA512 = 1,     ///< Full SHA-512
  HASH_SHA512_Half = 2 ///< Truncated SHA-512 (first half)
};

} // namespace ledger

/*@}*/
