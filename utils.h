#ifndef _UTILS_H
#define _UTILS_H

#include <system.hh>

#if defined __FreeBSD__ && __FreeBSD__ <=4
// FreeBSD has a broken isspace macro, so don't use it
#undef isspace(c)
#endif

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

namespace ledger {

// jww (2007-04-23): Need to clean up the following include files.  I
// want to following services:
//
//   error reporting via exceptions
//   error context stack and display (copy-by-value)
//   logging (always on, but with user-settable levels)
//   assert (always on, unless the users asks for them off)
//   timing of critical areas (and warning on variance from expectation)
//   debugging (optionally on)
//   verification (optionally on, like debugging but silent)
//   memory tracing and debugging (and watching for threshholds)

#import "error.h"

/**********************************************************************
 *
 * Default values
 */

#if defined(FULL_DEBUG)
#define VERIFY_ON  1
#define TRACING_ON 1
#define DEBUG_ON   1
#define TIMERS_ON  1
#elif defined(NO_DEBUG)
#define NO_ASSERTS 1
#define NO_LOGGING 1
#else
#define TIMERS_ON  1
#endif

/**********************************************************************
 *
 * Assertions
 */

#ifdef assert
#undef assert
#endif

#if ! defined(NO_ASSERTS)
#define ASSERTS_ON 1
#endif
#if defined(ASSERTS_ON)

#include <boost/current_function.hpp>

void debug_assert(const ledger::string& reason,
		  const ledger::string& func,
		  const ledger::string& file,
		  unsigned long		line);

#define assert(x)						\
  ((x) ? ((void)0) : debug_assert(#x, BOOST_CURRENT_FUNCTION,	\
				  __FILE__, __LINE__)

#endif // ASSERTS_ON

/**********************************************************************
 *
 * Verification (basically, very slow asserts)
 */

#if defined(VERIFY_ON)

extern bool verify_enabled;

#define verify(x) (verify_enabled ? assert(x) : ((void)0))

extern int	     new_calls;
extern unsigned long new_size;

void * operator new(std::size_t) throw(std::bad_alloc);
void * operator new[](std::size_t) throw(std::bad_alloc);
void   operator delete(void*) throw();
void   operator delete[](void*) throw();
void * operator new(std::size_t, const std::nothrow_t&) throw();
void * operator new[](std::size_t, const std::nothrow_t&) throw();
void   operator delete(void*, const std::nothrow_t&) throw();
void   operator delete[](void*, const std::nothrow_t&) throw();

typedef std::multimap<void *, std::string>      live_objects_map;
typedef std::pair<void *, std::string>          live_objects_pair;
typedef std::pair<unsigned int, std::size_t>    count_size_pair;
typedef std::map<std::string, count_size_pair>  object_count_map;
typedef std::pair<std::string, count_size_pair> object_count_pair;

extern live_objects_map live_objects;
extern object_count_map	live_count;
extern object_count_map	ctor_count;
extern object_count_map	object_count;

bool trace_ctor(void * ptr, const char * cls_name, const char * args,
		std::size_t cls_size);
bool trace_dtor(void * ptr, const char * cls_name, std::size_t cls_size);

#define trace_ctor(cls, args) \
  verify(trace_ctor(this, #cls, args, sizeof(cls)))
#define trace_dtor(cls) \
  verify(trace_dtor(this, #cls, sizeof(cls)))

void report_memory(std::ostream& out);

#if ! defined(USE_BOOST_PYTHON)

class string : public std::string
{
public:
  string();
  string(const string& str);
  string(const std::string& str);
  string(const int len, char x);
  string(const char * str);
  string(const char * str, const char * end);
  string(const string& str, int x);
  string(const string& str, int x, int y);
  string(const char * str, int x);
  string(const char * str, int x, int y);
  ~string();
};

inline string operator+(const string& __lhs, const string& __rhs)
{
  string __str(__lhs);
  __str.append(__rhs);
  return __str;
}

string operator+(const char* __lhs, const string& __rhs);
string operator+(char __lhs, const string& __rhs); 

inline string operator+(const string& __lhs, const char* __rhs)
{
  string __str(__lhs);
  __str.append(__rhs);
  return __str;
}

inline string operator+(const string& __lhs, char __rhs)
{
  typedef string		__string_type;
  typedef string::size_type	__size_type;
  __string_type __str(__lhs);
  __str.append(__size_type(1), __rhs);
  return __str;
}

inline bool operator==(const string& __lhs, const string& __rhs)
{ return __lhs.compare(__rhs) == 0; }

inline bool operator==(const char* __lhs, const string& __rhs)
{ return __rhs.compare(__lhs) == 0; }

inline bool operator==(const string& __lhs, const char* __rhs)
{ return __lhs.compare(__rhs) == 0; }

inline bool operator!=(const string& __lhs, const string& __rhs)
{ return __rhs.compare(__lhs) != 0; }

inline bool operator!=(const char* __lhs, const string& __rhs)
{ return __rhs.compare(__lhs) != 0; }

inline bool operator!=(const string& __lhs, const char* __rhs)
{ return __lhs.compare(__rhs) != 0; }

#endif

#else // ! VERIFY_ON

#define verify(x)
#define trace_ctor(cls, args)
#define trace_dtor(cls)

#endif // VERIFY_ON

/**********************************************************************
 *
 * Logging
 */

#if ! defined(NO_LOGGING)
#define LOGGING_ON 1
#endif
#if defined(LOGGING_ON)

// Logging

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

extern log_level_t	  _log_level;
extern unsigned int	  _trace_level;
extern std::string	  _log_category;
extern std::ostream *	  _log_stream;
extern std::ostringstream _log_buffer;

#define category(cat)					\
    static const char * const _this_category = (cat)

bool logger(log_level_t level);

#if defined(TRACING_ON)
#define trace(lvl, msg)					\
  (_log_level >= LOG_TRACE && lvl >= _trace_level ?	\
   ((_log_buffer << msg), logger(LOG_TRACE)) : false)
#else
#define trace(lvl, msg)
#endif

#if defined(DEBUG_ON)
#define debug_(cat, msg)				\
  (_log_level >= LOG_DEBUG &&				\
   (_log_category == cat ||				\
    _log_category.find(cat ".") == 0) ?			\
   ((_log_buffer << msg), logger(LOG_DEBUG)) : false)
#define debug(msg) debug_(_this_category, msg)
#else
#define debug_(cat, msg)
#define debug(msg)
#endif
   
#define info_(cat, msg)					\
  (_log_level >= LOG_INFO &&				\
   (_log_category == cat ||				\
    _log_category.find(cat ".") == 0) ?			\
   ((_log_buffer << msg), logger(LOG_INFO)) : false)
#define info(msg) info_(_this_category, msg)

#define log_macro(level, msg)				\
  (_log_level >= level ?				\
   ((_log_buffer << msg), logger(level)) : false)

#define warn(msg)     log_macro(LOG_WARN, msg)
#define error(msg)    log_macro(LOG_ERROR, msg)
#define fatal(msg)    log_macro(LOG_FATAL, msg)
#define critical(msg) log_macro(LOG_CRIT, msg)

#else // ! LOGGING_ON

#define category(cat)
#define trace(lvl, msg)
#define debug(msg)
#define debug_(cat, msg)
#define info(msg)
#define info_(cat, msg)
#define warn(msg)
#define error(msg)
#define fatal(msg)
#define critical(msg)

/**********************************************************************
 *
 * Timers (allows log entries to specify cumulative time spent)
 */

#if defined(LOGGING_ON) && defined(TIMERS_ON)

struct timer_t {
  std::clock_t count;
  std::string  message;
};

typedef std::map<std::string, timer_t> timing_map;
typedef timing_map::value_type         timing_pair;

void start_timer(const char * name);
void stop_timer(const char * name);
void finish_timer(const char * name);

#if defined(TRACING_ON)
#define trace_start(name, lvl, msg)		\
  (trace(lvl, msg) && start_timer(name))
#define trace_stop(name)   stop_timer(name)
#define trace_finish(name) finish_timer(name)
#else
#define trace_start(name, lvl, msg)
#define trace_stop(name)
#define trace_finish(name)
#endif

#if defined(DEBUG_ON)
#define debug_start(name, msg)			\
  (debug(msg) && start_timer(name))
#define debug_start_(name, cat, msg)		\
  (debug_(cat, msg) && start_timer(name))
#define debug_stop(name)   stop_timer(name)
#define debug_finish(name) finish_timer(name)
#else
#define debug_start(name, msg)
#define debug_start_(name, cat, msg)
#define debug_stop(name)
#define debug_finish(name)
#endif

#define info_start(name, msg)			\
  (info(msg) && start_timer(name))
#define info_start_(name, cat, msg)
#define info_stop(name)   stop_timer(name)
#define info_finish(name) finish_timer(name)

#else // ! (LOGGING_ON && TIMERS_ON)

#define trace_start(lvl, msg, name)
#define trace_stop(name)
#define trace_finish(name)

#define debug_start(name, msg)
#define debug_start_(name, cat, msg)
#define debug_stop(name)
#define debug_finish(name)

#define info_start(name, msg)
#define info_start_(name, cat, msg)
#define info_stop(name)
#define info_finish(name)

#endif // TIMERS_ON

/**********************************************************************
 *
 * Debug macros
 */

#if defined(DEBUG_ON)

#define if_debug_(cat)				\
  if (_log_level >= LOG_DEBUG &&		\
      (_log_category == cat ||			\
       _log_category.find(cat ".") == 0))
#define if_debug() if_debug_(_this_category)
  
#else // ! DEBUG_ON

#define if_debug(cat) if (false)

#endif // DEBUG_ON

// } namespace ledger

#endif // _UTILS_H
