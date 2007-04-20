#include "trace.h"
#include "debug.h"
#include "timing.h"
#include "times.h"
#include "acconf.h"

namespace ledger {

bool trace_mode;

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

bool trace_ctor(void * ptr, const char * name)
{
  if (! tracing_active)
    return true;

  DEBUG_PRINT("ledger.trace.debug", "trace_ctor " << ptr << " " << name);

  const char * pos = std::strchr(name, '(');
  static char cls_name[1024];
  std::strncpy(cls_name, name, pos - name);
  cls_name[pos - name] = '\0';

  live_objects.insert(live_objects_pair(ptr, cls_name));

  object_count_map::iterator i = ctor_count.find(name);
  if (i != ctor_count.end()) {
    (*i).second++;
  } else {
    std::pair<object_count_map::iterator, bool> result
      = ctor_count.insert(object_count_pair(name, 1));
    if (! result.second) {
      tracing_active = false;
      return false;
    }
  }

  object_count_map::iterator j = object_count.find(cls_name);
  if (j != object_count.end()) {
    (*j).second++;
  } else {
    std::pair<object_count_map::iterator, bool> result
      = object_count.insert(object_count_pair(cls_name, 1));
    if (! result.second) {
      tracing_active = false;
      return false;
    }
  }

  object_count_map::iterator k = live_count.find(cls_name);
  if (k != live_count.end()) {
    (*k).second++;
  } else {
    std::pair<object_count_map::iterator, bool> result
      = live_count.insert(object_count_pair(cls_name, 1));
    if (! result.second) {
      tracing_active = false;
      return false;
    }
  }

  return true;
}

bool trace_dtor(void * ptr, const char * name)
{
  if (! tracing_active)
    return true;

  DEBUG_PRINT("ledger.trace.debug", "trace_dtor " << ptr << " " << name);

  live_objects_map::iterator i = live_objects.find(ptr);
  if (i == live_objects.end()) {
    std::cerr << "Destruction of unknown object " << name << " " << ptr
	      << std::endl;;
    tracing_active = false;
    return false;
  }

  const char * cls_name = name;

  int ptr_count = live_objects.count(ptr);
  for (int x = 0; x < ptr_count; x++) {
    if ((*i).second == cls_name) {
      live_objects.erase(i);
      break;
    } else {
      i++;
    }
  }

  object_count_map::iterator k = live_count.find(name);
  if (k == live_count.end()) {
    std::cerr << "Destruction of unregistered class " << name
	      << std::endl;;
    tracing_active = false;
    return false;
  }
  if (--(*k).second == 0)
    live_count.erase(k);

  return true;
}

void report_memory(std::ostream& out)
{
  if (live_count.size() > 0)
    out << "Live object counts:" << std::endl;

  for (object_count_map::iterator i = live_count.begin();
       i != live_count.end();
       i++) {
    out << "  ";
    out << std::right;
    out.width(7);
    out << (*i).second << "  " << (*i).first << std::endl;
  }

  DEBUG_IF("ledger.trace.verbose") {
    if (live_objects.size() > 0)
      out << "Live objects:" << std::endl;

    for (live_objects_map::iterator i = live_objects.begin();
	 i != live_objects.end();
	 i++) {
      out << "  ";
      out << std::right;
      out.width(7);
      out << (*i).first << "  " << (*i).second << std::endl;
    }
  }

  if (object_count.size() > 0)
    out << "Object counts:" << std::endl;

  for (object_count_map::iterator i = object_count.begin();
       i != object_count.end();
       i++) {
    out << "  ";
    out << std::right;
    out.width(7);
    out << (*i).second << "  " << (*i).first << std::endl;
  }

  if (ctor_count.size() > 0)
    out << "Constructor counts:" << std::endl;

  for (object_count_map::iterator i = ctor_count.begin();
       i != ctor_count.end();
       i++) {
    out << "  ";
    out << std::right;
    out.width(7);
    out << (*i).second << "  " << (*i).first << std::endl;
  }
}

#if DEBUG_LEVEL >= 4

string::string() : std::string() {
  TRACE_CTOR("string()");
}
string::string(const string& str) : std::string(str) {
  TRACE_CTOR("string(const string&)");
}
string::string(const std::string& str) : std::string(str) {
  TRACE_CTOR("string(const std::string&)");
}
string::string(const int len, char x) : std::string(len, x) {
  TRACE_CTOR("string(const int, char)");
}
string::string(const char * str) : std::string(str) {
  TRACE_CTOR("string(const char *)");
}
string::string(const char * str, const char * end) : std::string(str, end) {
  TRACE_CTOR("string(const char *, const char *)");
}
string::string(const string& str, int x) : std::string(str, x) {
  TRACE_CTOR("string(const string&, int)");
}
string::string(const string& str, int x, int y) : std::string(str, x, y) {
  TRACE_CTOR("string(const string&, int, int)");
}
string::string(const char * str, int x) : std::string(str, x) {
  TRACE_CTOR("string(const char *, int)");
}
string::string(const char * str, int x, int y) : std::string(str, x, y) {
  TRACE_CTOR("string(const char *, int, int)");
}
string::~string() {
  TRACE_DTOR("string");
}

#endif

} // namespace ledger
