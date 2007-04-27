#include "utils.h"

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
#if defined(FULL_DEBUG)
  bool verify_enabled = true;
#else
  bool verify_enabled = false;
#endif

  int		new_calls = 0;
  unsigned long	new_size  = 0;
}

void * operator new(std::size_t size) throw (std::bad_alloc) {
  void * ptr = std::malloc(size);
  ledger::new_calls++;
  ledger::new_size += size;
  return ptr;
}
void * operator new[](std::size_t size) throw (std::bad_alloc) {
  void * ptr = std::malloc(size);
  ledger::new_calls++;
  ledger::new_size += size;
  return ptr;
}
void * operator new(std::size_t size, const std::nothrow_t&) throw() {
  void * ptr = std::malloc(size);
  ledger::new_calls++;
  ledger::new_size += size;
  return ptr;
}
void * operator new[](std::size_t size, const std::nothrow_t&) throw() {
  void * ptr = std::malloc(size);
  ledger::new_calls++;
  ledger::new_size += size;
  return ptr;
}
void   operator delete(void * ptr) throw() {
  std::free(ptr);
}
void   operator delete[](void * ptr) throw() {
  std::free(ptr);
}
void   operator delete(void * ptr, const std::nothrow_t&) throw() {
  std::free(ptr);
}
void   operator delete[](void * ptr, const std::nothrow_t&) throw() {
  std::free(ptr);
}

namespace ledger {

live_objects_map live_objects;
object_count_map ctor_count;
object_count_map object_count;
object_count_map live_count;

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
    assert(result.second);
  }
}

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

bool trace_ctor_func(void * ptr, const char * cls_name, const char * args,
		     std::size_t cls_size)
{
  static char name[1024];
  std::strcpy(name, cls_name);
  std::strcat(name, "(");
  std::strcat(name, args);
  std::strcat(name, ")");

  DEBUG_("ledger.trace.debug", "TRACE_CTOR " << ptr << " " << name);

  live_objects.insert(live_objects_pair(ptr, cls_name));

  add_to_count_map(ctor_count, name, cls_size);
  add_to_count_map(object_count, cls_name, cls_size);
  add_to_count_map(object_count, "__ALL__", cls_size);
  add_to_count_map(live_count, cls_name, cls_size);

  return true;
}

bool trace_dtor_func(void * ptr, const char * cls_name, std::size_t cls_size)
{
  DEBUG_("ledger.trace.debug", "TRACE_DTOR " << ptr << " " << cls_name);

  live_objects_map::iterator i = live_objects.find(ptr);
  if (i == live_objects.end()) {
    std::cerr << "Destruction of unknown object of type " << cls_name
	      << " " << ptr << std::endl;
    assert(0);
    return false;
  }

  int ptr_count = live_objects.count(ptr);
  for (int x = 0; x < ptr_count; x++, i++) {
    if ((*i).second == cls_name) {
      live_objects.erase(i);
      break;
    }
  }

  object_count_map::iterator k = live_count.find(cls_name);
  if (k == live_count.end()) {
    std::cerr << "Destruction of unregistered class " << cls_name
	      << std::endl;;
    assert(0);
    return false;
  }

  (*k).second.second -= cls_size;
  if (--(*k).second.first == 0)
    live_count.erase(k);

  return true;
}

void report_memory(std::ostream& out)
{
  if (live_count.size() > 0) {
    out << "Live object counts:" << std::endl;
    report_count_map(out, live_count);
  }

  if (live_objects.size() > 0) {
    out << "Live objects:" << std::endl;

    for (live_objects_map::iterator i = live_objects.begin();
	 i != live_objects.end();
	 i++)
      out << "  " << std::right << std::setw(7) << (*i).first
	  << "  " << std::left  << (*i).second
	  << std::endl;
  }

  if (object_count.size() > 0) {
    out << "Object counts:" << std::endl;
    report_count_map(out, object_count);
  }

  if (ctor_count.size() > 0) {
    out << "Constructor counts:" << std::endl;
    report_count_map(out, ctor_count);
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

#if defined(LOGGING_ON) && defined(DEBUG_ON)

#include <boost/regex.hpp>

namespace ledger {

log_level_t	   _log_level;
unsigned int	   _trace_level;
std::string	   _log_category;
std::ostream *	   _log_stream = &std::cerr;
std::ostringstream _log_buffer;

bool logger_func(log_level_t level)
{
  _log_buffer.str("");
}

} // namespace ledger

#endif // LOGGING_ON && DEBUG_ON

/**********************************************************************
 *
 * Timers (allows log entries to specify cumulative time spent)
 */

#if defined(LOGGING_ON) && defined(TIMERS_ON)

namespace ledger {

void start_timer(const char * name)
{
#if 0
  begin = std::clock();
#endif
}

void stop_timer(const char * name)
{
#if 0
  cumulative += std::clock() - begin;
#endif
}

void finish_timer(const char * name)
{
#if 0
  DEBUG_(cls.c_str(), file << ":" << line << ": "
	 << category << " = "
	 << (double(cumulative) / double(CLOCKS_PER_SEC)) << "s");
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
