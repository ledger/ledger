#ifndef _TRACE_H
#define _TRACE_H

#include <string>
#include <map>

namespace ledger {

class timing_t;

extern bool trace_alloc_mode;
extern bool trace_class_mode;

#if 0 && DEBUG_LEVEL >= 4 && ! defined(USE_BOOST_PYTHON)
class string;
#else
typedef std::string string;
#endif

void trace(const string& cat, const string& str);
void trace_push(const string& cat, const string& str, timing_t& timer);
void trace_pop(const string& cat, const string& str, timing_t& timer);

#ifndef TRACE
#define TRACE(cat, msg)  if (trace_class_mode) trace(#cat, msg)
#define TRACE_(cat, msg) if (trace_class_mode) trace(#cat, msg)

#define TRACE_PUSH(cat, msg)					\
  timing_t timer_ ## cat(#cat);					\
  if (trace_class_mode) trace_push(#cat, msg, timer_ ## cat)

#define TRACE_POP(cat, msg)					\
  if (trace_class_mode) trace_pop(#cat, msg, timer_ ## cat)
#endif

typedef std::multimap<void *, std::string>      live_objects_map;
typedef std::pair<void *, std::string>          live_objects_pair;
typedef std::pair<unsigned int, std::size_t>    count_size_pair;
typedef std::map<std::string, count_size_pair>  object_count_map;
typedef std::pair<std::string, count_size_pair> object_count_pair;

extern live_objects_map live_objects;
extern object_count_map	live_count;
extern object_count_map	ctor_count;
extern object_count_map	object_count;

extern bool tracing_active;

bool trace_ctor(void * ptr, const char * cls_name, const char * args,
		std::size_t cls_size);
bool trace_dtor(void * ptr, const char * cls_name, std::size_t cls_size);

void report_memory(std::ostream& out);

#if 1
#ifndef TRACE_CTOR
#define TRACE_CTOR(cls, args) \
  CONFIRM(ledger::trace_ctor(this, #cls, args, sizeof(cls)))
#define TRACE_DTOR(cls) \
  CONFIRM(ledger::trace_dtor(this, #cls, sizeof(cls)))
#endif
#else
#define TRACE_CTOR(cls, args)
#define TRACE_DTOR(cls)
#endif

#if 0 && DEBUG_LEVEL >= 4 && ! defined(USE_BOOST_PYTHON)

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

} // namespace ledger

#endif // _TRACE_H
