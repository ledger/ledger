#include "utils.h"
#include "times.h"

/**********************************************************************
 *
 * Assertions
 */

#if defined(ASSERTS_ON)

namespace ledger {

void debug_assert(const string& reason,
		  const string& func,
		  const string& file,
		  unsigned long	line)
{
  std::ostringstream buf;
  buf << "Assertion failed in \"" << file << "\", line " << line
      << ": " << reason;
  throw exception(buf.str(), context());
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
typedef std::pair<void *, allocation_pair>      live_memory_pair;
typedef std::multimap<void *, allocation_pair>  live_objects_map;
typedef std::pair<void *, allocation_pair>      live_objects_pair;
typedef std::pair<unsigned int, std::size_t>    count_size_pair;
typedef std::map<std::string, count_size_pair>  object_count_map;
typedef std::pair<std::string, count_size_pair> object_count_pair;

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
    IF_DEBUG_("memory.counts")
      report_memory(std::cerr, true);
    else
      IF_DEBUG_("memory.counts.live")
	report_memory(std::cerr);
    else if (live_objects->size() > 0)
      report_memory(std::cerr);
  }

  delete live_memory;        live_memory	= NULL;
  delete live_memory_count;  live_memory_count	= NULL;
  delete total_memory_count; total_memory_count = NULL;

  delete live_objects;       live_objects	= NULL;
  delete live_object_count;  live_object_count	= NULL;
  delete total_object_count; total_object_count = NULL;
  delete total_ctor_count;   total_ctor_count	= NULL;
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
      the_map.insert(object_count_pair(name, count_size_pair(1, size)));
    VERIFY(result.second);
  }
}

std::size_t current_memory_size()
{
  std::size_t memory_size = 0;
  
  for (object_count_map::const_iterator i = live_memory_count->begin();
       i != live_memory_count->end();
       i++)
    memory_size += (*i).second.second;

  return memory_size;
}

static void trace_new_func(void * ptr, const char * which, std::size_t size)
{
  memory_tracing_active = false;

  if (! live_memory) return;

  live_memory->insert(live_memory_pair(ptr, allocation_pair(which, size)));

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
  for (object_count_map::iterator i = the_map.begin();
       i != the_map.end();
       i++)
    out << "  " << std::right << std::setw(12) << (*i).second.first
	<< "  " << std::right << std::setw(12) << (*i).second.second
	<< "  " << std::left  << (*i).first
	<< std::endl;
}

std::size_t current_objects_size()
{
  std::size_t objects_size = 0;
  
  for (object_count_map::const_iterator i = live_object_count->begin();
       i != live_object_count->end();
       i++)
    objects_size += (*i).second.second;

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

  DEBUG_("verify.memory", "TRACE_CTOR " << ptr << " " << name);

  live_objects->insert(live_objects_pair(ptr, allocation_pair(cls_name, cls_size)));

  add_to_count_map(*live_object_count, cls_name, cls_size);
  add_to_count_map(*total_object_count, cls_name, cls_size);
  add_to_count_map(*total_object_count, "__ALL__", cls_size);
  add_to_count_map(*total_ctor_count, name, cls_size);

  memory_tracing_active = true;
}

void trace_dtor_func(void * ptr, const char * cls_name, std::size_t cls_size)
{
  memory_tracing_active = false;

  if (! live_objects) return;

  DEBUG_("ledger.trace.debug", "TRACE_DTOR " << ptr << " " << cls_name);

  live_objects_map::iterator i = live_objects->find(ptr);
  VERIFY(i != live_objects->end());

  int ptr_count = live_objects->count(ptr);
  for (int x = 0; x < ptr_count; x++, i++) {
    if ((*i).second.first == cls_name) {
      live_objects->erase(i);
      break;
    }
  }

  object_count_map::iterator k = live_object_count->find(cls_name);
  VERIFY(k != live_object_count->end());

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

    for (live_memory_map::const_iterator i = live_memory->begin();
	 i != live_memory->end();
	 i++)
      out << "  " << std::right << std::setw(7) << (*i).first
	  << "  " << std::right << std::setw(7) << (*i).second.second
	  << "  " << std::left  << (*i).second.first
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

    for (live_objects_map::const_iterator i = live_objects->begin();
	 i != live_objects->end();
	 i++)
      out << "  " << std::right << std::setw(7) << (*i).first
	  << "  " << std::right << std::setw(7) << (*i).second.second
	  << "  " << std::left  << (*i).second.first
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

#if ! defined(USE_BOOST_PYTHON)

string::string() : std::string() {
  TRACE_CTOR(string, "");
}
string::string(const string& str) : std::string(str) {
  TRACE_CTOR(string, "const string&");
}
string::string(const std::string& str) : std::string(str) {
  TRACE_CTOR(string, "const std::string&");
}
string::string(const int len, char x) : std::string(len, x) {
  TRACE_CTOR(string, "const int, char");
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
string::~string() {
  TRACE_DTOR(string);
}

#endif

} // namespace ledger

#endif // VERIFY_ON

/**********************************************************************
 *
 * Logging
 */

#if defined(LOGGING_ON)

namespace ledger {

log_level_t	   _log_level;
std::ostream *	   _log_stream = &std::cerr;
std::ostringstream _log_buffer;

#if defined(TRACING_ON)
unsigned int	   _trace_level;
#endif

#ifdef BOOST_DATE_TIME_HAS_HIGH_PRECISION_CLOCK
#define CURRENT_TIME() boost::posix_time::microsec_clock::universal_time()
#else
#define CURRENT_TIME() boost::posix_time::second_clock::universal_time()
#endif

static inline void stream_memory_size(std::ostream& out, std::size_t size)
{
  if (size < 1024)
    out << size << 'b';
  else if (size < (1024 * 1024))
    out << (double(size) / 1024.0) << 'K';
  else if (size < (1024 * 1024 * 1024))
    out << (double(size) / (1024.0 * 1024.0)) << 'M';
  else if (size < (1024 * 1024 * 1024 * 1024))
    out << (double(size) / (1024.0 * 1024.0 * 1024.0)) << 'G';
  else
    assert(false);
}

static bool  logger_has_run = false;
static ptime logger_start;

bool logger_func(log_level_t level)
{
  unsigned long appender = 0;

  if (! logger_has_run) {
    logger_has_run = true;
    logger_start   = CURRENT_TIME();

    IF_VERIFY()
      *_log_stream << " TIME  OBJSZ  MEMSZ" << std::endl;

    appender = (logger_start - now).total_milliseconds();
  }

  *_log_stream << std::right << std::setw(5)
	       << (CURRENT_TIME() - logger_start).total_milliseconds();

  IF_VERIFY() {
    *_log_stream << std::right << std::setw(6) << std::setprecision(3);
    stream_memory_size(*_log_stream, current_objects_size());
    *_log_stream << std::right << std::setw(6) << std::setprecision(3);
    stream_memory_size(*_log_stream, current_memory_size());
  }

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

  *_log_stream << ' ' << _log_buffer.str();

  if (appender)
    *_log_stream << " (" << appender << "ms startup)";

  *_log_stream << std::endl;

  _log_buffer.str("");

  return true;
}

} // namespace ledger

#if defined(DEBUG_ON)

#include <boost/regex.hpp>

namespace ledger {

std::string _log_category;

} // namespace ledger

#endif // DEBUG_ON
#endif // LOGGING_ON

/**********************************************************************
 *
 * Timers (allows log entries to specify cumulative time spent)
 */

#if defined(LOGGING_ON) && defined(TIMERS_ON)

namespace ledger {

struct timer_t {
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

typedef std::map<std::string, timer_t>  timer_map;
typedef std::pair<std::string, timer_t> timer_pair;

static timer_map timers;

void start_timer(const char * name, log_level_t lvl)
{
#if defined(VERIFY_ON)
  memory_tracing_active = false;
#endif

  timer_map::iterator i = timers.find(name);
  if (i == timers.end()) {
    timers.insert(timer_pair(name, timer_t(lvl, _log_buffer.str())));
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
 * Exception handling
 */

namespace ledger {

std::ostringstream _exc_buffer;

} // namespace ledger

/**********************************************************************
 *
 * General utility functions
 */

namespace ledger {

string expand_path(const string& path)
{
  if (path.length() == 0 || path[0] != '~')
    return path;

  const char * pfx = NULL;
  string::size_type pos = path.find_first_of('/');

  if (path.length() == 1 || pos == 1) {
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
    string user(path, 1, pos == string::npos ?
		     string::npos : pos - 1);
    struct passwd * pw = getpwnam(user.c_str());
    if (pw)
      pfx = pw->pw_dir;
  }
#endif

  // if we failed to find an expansion, return the path unchanged.

  if (! pfx)
    return path;

  string result(pfx);

  if (pos == string::npos)
    return result;

  if (result.length() == 0 || result[result.length() - 1] != '/')
    result += '/';

  result += path.substr(pos + 1);

  return result;
}

string resolve_path(const string& path)
{
  if (path[0] == '~')
    return expand_path(path);
  return path;
}

} // namespace ledger
