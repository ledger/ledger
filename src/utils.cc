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

#include "utils.h"

/**********************************************************************
 *
 * Assertions
 */

#if defined(ASSERTS_ON)

namespace ledger {

DECLARE_EXCEPTION(assertion_failed, std::logic_error);

void debug_assert(const string& reason,
		  const string& func,
		  const string& file,
		  std::size_t	line)
{
  std::ostringstream buf;
  buf << "Assertion failed in \"" << file << "\", line " << line
      << ": " << func << ": " << reason;
  throw assertion_failed(buf.str());
}

} // namespace ledger

#endif

/**********************************************************************
 *
 * Verification (basically, very slow asserts)
 */

#if defined(VERIFY_ON)

namespace ledger {

bool verify_enabled = false;

typedef std::pair<std::string, std::size_t>     allocation_pair;
typedef std::map<void *, allocation_pair>       live_memory_map;
typedef std::multimap<void *, allocation_pair>  live_objects_map;

typedef std::pair<std::size_t, std::size_t>     count_size_pair;
typedef std::map<std::string, count_size_pair>  object_count_map;

static live_memory_map  * live_memory	     = NULL;
static object_count_map * live_memory_count  = NULL;
static object_count_map * total_memory_count = NULL;

static bool memory_tracing_active = false;

static live_objects_map * live_objects	     = NULL;
static object_count_map * live_object_count  = NULL;
static object_count_map * total_object_count = NULL;
static object_count_map * total_ctor_count   = NULL;

void initialize_memory_tracing()
{
  memory_tracing_active = false;

  live_memory	     = new live_memory_map;
  live_memory_count  = new object_count_map;
  total_memory_count = new object_count_map;

  live_objects	     = new live_objects_map;
  live_object_count  = new object_count_map;
  total_object_count = new object_count_map;
  total_ctor_count   = new object_count_map;

  memory_tracing_active = true;
}

void shutdown_memory_tracing()
{
  memory_tracing_active = false;

  if (live_objects) {
    IF_DEBUG("memory.counts")
      report_memory(std::cerr, true);
    else IF_DEBUG("memory.counts.live")
      report_memory(std::cerr);
    else if (live_objects->size() > 0)
      report_memory(std::cerr);
  }

  checked_delete(live_memory);        live_memory	 = NULL;
  checked_delete(live_memory_count);  live_memory_count	 = NULL;
  checked_delete(total_memory_count); total_memory_count = NULL;

  checked_delete(live_objects);       live_objects	 = NULL;
  checked_delete(live_object_count);  live_object_count	 = NULL;
  checked_delete(total_object_count); total_object_count = NULL;
  checked_delete(total_ctor_count);   total_ctor_count	 = NULL;
}

inline void add_to_count_map(object_count_map& the_map,
			     const char * name, std::size_t size)
{
  object_count_map::iterator k = the_map.find(name);
  if (k != the_map.end()) {
    (*k).second.first++;
    (*k).second.second += size;
  } else {
    std::pair<object_count_map::iterator, bool> result =
      the_map.insert(object_count_map::value_type(name, count_size_pair(1, size)));
    VERIFY(result.second);
  }
}

std::size_t current_memory_size()
{
  std::size_t memory_size = 0;

  foreach (const object_count_map::value_type& pair, *live_memory_count)
    memory_size += pair.second.second;

  return memory_size;
}

static void trace_new_func(void * ptr, const char * which, std::size_t size)
{
  memory_tracing_active = false;

  if (! live_memory) return;

  live_memory->insert
    (live_memory_map::value_type(ptr, allocation_pair(which, size)));

  add_to_count_map(*live_memory_count, which, size);
  add_to_count_map(*total_memory_count, which, size);
  add_to_count_map(*total_memory_count, "__ALL__", size);

  memory_tracing_active = true;
}

static void trace_delete_func(void * ptr, const char * which)
{
  memory_tracing_active = false;

  if (! live_memory) return;

  // Ignore deletions of memory not tracked, since it's possible that
  // a user (like boost) allocated a block of memory before memory
  // tracking began, and then deleted it before memory tracking ended.
  // If it really is a double-delete, the malloc library on OS/X will
  // notify me.

  live_memory_map::iterator i = live_memory->find(ptr);
  if (i == live_memory->end())
    return;

  std::size_t size = (*i).second.second;
  VERIFY((*i).second.first == which);

  live_memory->erase(i);

  object_count_map::iterator j = live_memory_count->find(which);
  VERIFY(j != live_memory_count->end());

  (*j).second.second -= size;
  if (--(*j).second.first == 0)
    live_memory_count->erase(j);

  memory_tracing_active = true;
}

} // namespace ledger

void * operator new(std::size_t size) throw (std::bad_alloc) {
  void * ptr = std::malloc(size);
  if (DO_VERIFY() && ledger::memory_tracing_active)
    ledger::trace_new_func(ptr, "new", size);
  return ptr;
}
void * operator new(std::size_t size, const std::nothrow_t&) throw() {
  void * ptr = std::malloc(size);
  if (DO_VERIFY() && ledger::memory_tracing_active)
    ledger::trace_new_func(ptr, "new", size);
  return ptr;
}
void * operator new[](std::size_t size) throw (std::bad_alloc) {
  void * ptr = std::malloc(size);
  if (DO_VERIFY() && ledger::memory_tracing_active)
    ledger::trace_new_func(ptr, "new[]", size);
  return ptr;
}
void * operator new[](std::size_t size, const std::nothrow_t&) throw() {
  void * ptr = std::malloc(size);
  if (DO_VERIFY() && ledger::memory_tracing_active)
    ledger::trace_new_func(ptr, "new[]", size);
  return ptr;
}
void   operator delete(void * ptr) throw() {
  if (DO_VERIFY() && ledger::memory_tracing_active)
    ledger::trace_delete_func(ptr, "new");
  std::free(ptr);
}
void   operator delete(void * ptr, const std::nothrow_t&) throw() {
  if (DO_VERIFY() && ledger::memory_tracing_active)
    ledger::trace_delete_func(ptr, "new");
  std::free(ptr);
}
void   operator delete[](void * ptr) throw() {
  if (DO_VERIFY() && ledger::memory_tracing_active)
    ledger::trace_delete_func(ptr, "new[]");
  std::free(ptr);
}
void   operator delete[](void * ptr, const std::nothrow_t&) throw() {
  if (DO_VERIFY() && ledger::memory_tracing_active)
    ledger::trace_delete_func(ptr, "new[]");
  std::free(ptr);
}

namespace ledger {

inline void report_count_map(std::ostream& out, object_count_map& the_map)
{
  foreach (object_count_map::value_type& pair, the_map)
    out << "  " << std::right << std::setw(12) << pair.second.first
	<< "  " << std::right << std::setw(7) << pair.second.second
	<< "  " << std::left  << pair.first
	<< std::endl;
}

std::size_t current_objects_size()
{
  std::size_t objects_size = 0;

  foreach (const object_count_map::value_type& pair, *live_object_count)
    objects_size += pair.second.second;

  return objects_size;
}

void trace_ctor_func(void * ptr, const char * cls_name, const char * args,
		     std::size_t cls_size)
{
  memory_tracing_active = false;

  if (! live_objects) return;

  static char name[1024];
  std::strcpy(name, cls_name);
  std::strcat(name, "(");
  std::strcat(name, args);
  std::strcat(name, ")");

  DEBUG("memory.debug", "TRACE_CTOR " << ptr << " " << name);

  live_objects->insert
    (live_objects_map::value_type(ptr, allocation_pair(cls_name, cls_size)));

  add_to_count_map(*live_object_count, cls_name, cls_size);
  add_to_count_map(*total_object_count, cls_name, cls_size);
  add_to_count_map(*total_object_count, "__ALL__", cls_size);
  add_to_count_map(*total_ctor_count, name, cls_size);

  memory_tracing_active = true;
}

void trace_dtor_func(void * ptr, const char * cls_name, std::size_t cls_size)
{
  if (! live_objects) return;

  memory_tracing_active = false;

  DEBUG("memory.debug", "TRACE_DTOR " << ptr << " " << cls_name);

  live_objects_map::iterator i = live_objects->find(ptr);
  if (i == live_objects->end()) {
    std::cerr << "Attempting to delete " << ptr << " a non-living " << cls_name
	      << std::endl;
    memory_tracing_active = true;
    return;
  }

  std::size_t ptr_count = live_objects->count(ptr);
  for (std::size_t x = 0; x < ptr_count; x++, i++) {
    if ((*i).second.first == cls_name) {
      live_objects->erase(i);
      break;
    }
  }

  object_count_map::iterator k = live_object_count->find(cls_name);
  if (k == live_object_count->end()) {
    std::cerr << "Failed to find " << cls_name << " in live object counts"
	      << std::endl;
    memory_tracing_active = true;
    return;
  }

  (*k).second.second -= cls_size;
  if (--(*k).second.first == 0)
    live_object_count->erase(k);

  memory_tracing_active = true;
}

void report_memory(std::ostream& out, bool report_all)
{
  if (! live_memory) return;

  if (live_memory_count->size() > 0) {
    out << "NOTE: There may be memory held by Boost "
	<< "and libstdc++ after ledger::shutdown()" << std::endl;
    out << "Live memory count:" << std::endl;
    report_count_map(out, *live_memory_count);
  }

  if (live_memory->size() > 0) {
    out << "Live memory:" << std::endl;

    foreach (const live_memory_map::value_type& pair, *live_memory)
      out << "  " << std::right << std::setw(12) << pair.first
	  << "  " << std::right << std::setw(7) << pair.second.second
	  << "  " << std::left  << pair.second.first
	  << std::endl;
  }

  if (report_all && total_memory_count->size() > 0) {
    out << "Total memory counts:" << std::endl;
    report_count_map(out, *total_memory_count);
  }

  if (live_object_count->size() > 0) {
    out << "Live object count:" << std::endl;
    report_count_map(out, *live_object_count);
  }

  if (live_objects->size() > 0) {
    out << "Live objects:" << std::endl;

    foreach (const live_objects_map::value_type& pair, *live_objects)
      out << "  " << std::right << std::setw(12) << pair.first
	  << "  " << std::right << std::setw(7) << pair.second.second
	  << "  " << std::left  << pair.second.first
	  << std::endl;
  }

  if (report_all) {
    if (total_object_count->size() > 0) {
      out << "Total object counts:" << std::endl;
      report_count_map(out, *total_object_count);
    }

    if (total_ctor_count->size() > 0) {
      out << "Total constructor counts:" << std::endl;
      report_count_map(out, *total_ctor_count);
    }
  }
}


string::string() : std::string() {
  TRACE_CTOR(string, "");
}
string::string(const string& str) : std::string(str) {
  TRACE_CTOR(string, "copy");
}
string::string(const std::string& str) : std::string(str) {
  TRACE_CTOR(string, "const std::string&");
}
string::string(size_type len, char x) : std::string(len, x) {
  TRACE_CTOR(string, "size_type, char");
}
string::string(const char * str) : std::string(str) {
  TRACE_CTOR(string, "const char *");
}
string::string(const char * str, const char * end) : std::string(str, end) {
  TRACE_CTOR(string, "const char *, const char *");
}
string::string(const string& str, int x) : std::string(str, x) {
  TRACE_CTOR(string, "const string&, int");
}
string::string(const string& str, int x, int y) : std::string(str, x, y) {
  TRACE_CTOR(string, "const string&, int, int");
}
string::string(const char * str, int x) : std::string(str, x) {
  TRACE_CTOR(string, "const char *, int");
}
string::string(const char * str, int x, int y) : std::string(str, x, y) {
  TRACE_CTOR(string, "const char *, int, int");
}
string::~string() throw() {
  TRACE_DTOR(string);
}

} // namespace ledger

#endif // VERIFY_ON

ledger::string empty_string("");

/**********************************************************************
 *
 * Logging
 */

#if defined(LOGGING_ON)

namespace ledger {

log_level_t	   _log_level  = LOG_WARN;
std::ostream *	   _log_stream = &std::cerr;
std::ostringstream _log_buffer;

#if defined(TRACING_ON)
uint8_t		   _trace_level;
#endif

static inline void stream_memory_size(std::ostream& out, std::size_t size)
{
  if (size < 1024)
    out << size << 'b';
  else if (size < (1024 * 1024))
    out << (double(size) / 1024.0) << 'K';
  else if (size < (1024 * 1024 * 1024))
    out << (double(size) / (1024.0 * 1024.0)) << 'M';
  else
    out << (double(size) / (1024.0 * 1024.0 * 1024.0)) << 'G';
}

static bool  logger_has_run = false;
static ptime logger_start;

bool logger_func(log_level_t level)
{
  if (! logger_has_run) {
    logger_has_run = true;
    logger_start   = CURRENT_TIME();

    IF_VERIFY()
      *_log_stream << "   TIME  OBJSZ  MEMSZ" << std::endl;
  }

  *_log_stream << std::right << std::setw(5)
	       << (CURRENT_TIME() - logger_start).total_milliseconds() << "ms";

#if defined(VERIFY_ON)
  IF_VERIFY() {
    *_log_stream << std::right << std::setw(6) << std::setprecision(3);
    stream_memory_size(*_log_stream, current_objects_size());
    *_log_stream << std::right << std::setw(6) << std::setprecision(3);
    stream_memory_size(*_log_stream, current_memory_size());
  }
#endif

  *_log_stream << "  " << std::left << std::setw(7);

  switch (level) {
  case LOG_CRIT:   *_log_stream << "[CRIT]"; break;
  case LOG_FATAL:  *_log_stream << "[FATAL]"; break;
  case LOG_ASSERT: *_log_stream << "[ASSRT]"; break;
  case LOG_ERROR:  *_log_stream << "[ERROR]"; break;
  case LOG_VERIFY: *_log_stream << "[VERFY]"; break;
  case LOG_WARN:   *_log_stream << "[WARN]"; break;
  case LOG_INFO:   *_log_stream << "[INFO]"; break;
  case LOG_EXCEPT: *_log_stream << "[EXCPT]"; break;
  case LOG_DEBUG:  *_log_stream << "[DEBUG]"; break;
  case LOG_TRACE:  *_log_stream << "[TRACE]"; break;

  case LOG_OFF:
  case LOG_ALL:
    assert(false);
    break;
  }

  *_log_stream << ' ' << _log_buffer.str() << std::endl;
  _log_buffer.str("");

  return true;
}

} // namespace ledger

#if defined(DEBUG_ON)

namespace ledger {

optional<std::string> _log_category;

struct __maybe_enable_debugging {
  __maybe_enable_debugging() {
    const char * p = std::getenv("LEDGER_DEBUG");
    if (p != NULL) {
      _log_level    = LOG_DEBUG;
      _log_category = p;
    }
  }
} __maybe_enable_debugging_obj;

} // namespace ledger

#endif // DEBUG_ON
#endif // LOGGING_ON

/**********************************************************************
 *
 * Timers (allows log xacts to specify cumulative time spent)
 */

#if defined(LOGGING_ON) && defined(TIMERS_ON)

namespace ledger {

struct timer_t
{
  log_level_t	level;
  ptime		begin;
  time_duration	spent;
  std::string	description;
  bool		active;

  timer_t(log_level_t _level, std::string _description)
    : level(_level), begin(CURRENT_TIME()),
      spent(time_duration(0, 0, 0, 0)),
      description(_description), active(true) {}
};

typedef std::map<std::string, timer_t> timer_map;

static timer_map timers;

void start_timer(const char * name, log_level_t lvl)
{
#if defined(VERIFY_ON)
  memory_tracing_active = false;
#endif

  timer_map::iterator i = timers.find(name);
  if (i == timers.end()) {
    timers.insert(timer_map::value_type(name, timer_t(lvl, _log_buffer.str())));
  } else {
    assert((*i).second.description == _log_buffer.str());
    (*i).second.begin  = CURRENT_TIME();
    (*i).second.active = true;
  }
  _log_buffer.str("");

#if defined(VERIFY_ON)
  memory_tracing_active = true;
#endif
}

void stop_timer(const char * name)
{
#if defined(VERIFY_ON)
  memory_tracing_active = false;
#endif

  timer_map::iterator i = timers.find(name);
  assert(i != timers.end());

  (*i).second.spent += CURRENT_TIME() - (*i).second.begin;
  (*i).second.active = false;

#if defined(VERIFY_ON)
  memory_tracing_active = true;
#endif
}

void finish_timer(const char * name)
{
#if defined(VERIFY_ON)
  memory_tracing_active = false;
#endif

  timer_map::iterator i = timers.find(name);
  if (i == timers.end())
    return;

  time_duration spent = (*i).second.spent;
  if ((*i).second.active) {
    spent = CURRENT_TIME() - (*i).second.begin;
    (*i).second.active = false;
  }

  _log_buffer << (*i).second.description << ' ';

  bool need_paren =
    (*i).second.description[(*i).second.description.size() - 1] != ':';

  if (need_paren)
    _log_buffer << '(';

  _log_buffer << spent.total_milliseconds() << "ms";

  if (need_paren)
    _log_buffer << ')';

  logger_func((*i).second.level);

  timers.erase(i);

#if defined(VERIFY_ON)
  memory_tracing_active = true;
#endif
}

} // namespace ledger

#endif // LOGGING_ON && TIMERS_ON

/**********************************************************************
 *
 * Signal handlers
 */

caught_signal_t caught_signal = NONE_CAUGHT;

void sigint_handler(int)
{
  caught_signal = INTERRUPTED;
}

void sigpipe_handler(int)
{
  caught_signal = PIPE_CLOSED;
}

/**********************************************************************
 *
 * General utility functions
 */

namespace ledger {

path expand_path(const path& pathname)
{
  if (pathname.empty())
    return pathname;

  std::string       path_string = pathname.string();
  const char *	    pfx = NULL;
  string::size_type pos = path_string.find_first_of('/');

  if (path_string.length() == 1 || pos == 1) {
    pfx = std::getenv("HOME");
#ifdef HAVE_GETPWUID
    if (! pfx) {
      // Punt. We're trying to expand ~/, but HOME isn't set
      struct passwd * pw = getpwuid(getuid());
      if (pw)
	pfx = pw->pw_dir;
    }
#endif
  }
#ifdef HAVE_GETPWNAM
  else {
    string user(path_string, 1, pos == string::npos ?
		string::npos : pos - 1);
    struct passwd * pw = getpwnam(user.c_str());
    if (pw)
      pfx = pw->pw_dir;
  }
#endif

  // if we failed to find an expansion, return the path unchanged.

  if (! pfx)
    return pathname;

  string result(pfx);

  if (pos == string::npos)
    return result;

  if (result.length() == 0 || result[result.length() - 1] != '/')
    result += '/';

  result += path_string.substr(pos + 1);

  return result;
}

path resolve_path(const path& pathname)
{
  path temp = pathname;
  if (temp.string()[0] == '~')
    temp = expand_path(temp);
  temp.normalize();
  return temp;
}

} // namespace ledger
