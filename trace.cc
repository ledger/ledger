#include "trace.h"
#include "debug.h"
#include "timing.h"
#include "times.h"
#include "acconf.h"

namespace ledger {

bool trace_alloc_mode;
bool trace_class_mode;

void trace(const string& cat, const string& str)
{
  std::cerr << boost::posix_time::to_simple_string(now) << " "
	    << cat << ": " << str << std::endl;
}

void trace_push(const string& cat, const string& str,
		timing_t& timer)
{
  timer.start();
  trace(cat, str);
}

void trace_pop(const string& cat, const string& str,
	       timing_t& timer)
{
  timer.stop();
  std::ostringstream out;
  out << str << ": " << (double(timer.cumulative) / double(CLOCKS_PER_SEC)) << "s";
  trace(cat, out.str());
}

live_objects_map live_objects;
object_count_map ctor_count;
object_count_map object_count;
object_count_map live_count;

bool tracing_active = false;

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

bool trace_ctor(void * ptr, const char * cls_name, const char * args,
		std::size_t cls_size)
{
  if (! tracing_active)
    return true;

  if (trace_class_mode && cls_name[0] == '_')
    return true;
  if (trace_alloc_mode && cls_name[0] != '_')
    return true;
  
  static char name[1024];
  std::strcpy(name, cls_name);
  std::strcat(name, "(");
  std::strcat(name, args);
  std::strcat(name, ")");

  DEBUG_PRINT("ledger.trace.debug",
	      "trace_ctor " << ptr << " " << name);

  live_objects.insert(live_objects_pair(ptr, cls_name));

  add_to_count_map(ctor_count, name, cls_size);
  add_to_count_map(object_count, cls_name, cls_size);
  add_to_count_map(object_count, "__ALL__", cls_size);
  add_to_count_map(live_count, cls_name, cls_size);

  return true;
}

bool trace_dtor(void * ptr, const char * cls_name, std::size_t cls_size)
{
  if (! tracing_active)
    return true;

  if (trace_class_mode && cls_name[0] == '_')
    return true;
  if (trace_alloc_mode && cls_name[0] != '_')
    return true;
  
  DEBUG_PRINT("ledger.trace.debug", "trace_dtor " << ptr << " " << cls_name);

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

#if 0 && DEBUG_LEVEL >= 4 && ! defined(USE_BOOST_PYTHON)

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
