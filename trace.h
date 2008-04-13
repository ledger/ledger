#ifndef _TRACE_H
#define _TRACE_H

#include "timing.h"

#include <string>
#include <map>

namespace ledger {

extern bool trace_mode;

void trace(const std::string& cat, const std::string& str);
void trace_push(const std::string& cat, const std::string& str,
		timing_t& timer);
void trace_pop(const std::string& cat, const std::string& str,
	       timing_t& timer);

#define TRACE(cat, msg)  if (trace_mode) trace(#cat, msg)
#define TRACE_(cat, msg) if (trace_mode) trace(#cat, msg)

#define TRACE_PUSH(cat, msg)					\
  timing_t timer_ ## cat(#cat);					\
  if (trace_mode) trace_push(#cat, msg, timer_ ## cat)

#define TRACE_POP(cat, msg)					\
  if (trace_mode) trace_pop(#cat, msg, timer_ ## cat)

typedef std::multimap<void *, std::string> live_objects_map;
typedef std::pair<void *, std::string>	   live_objects_pair;
typedef std::map<std::string, int>	   object_count_map;
typedef std::pair<std::string, int>	   object_count_pair;

extern live_objects_map live_objects;
extern object_count_map	ctor_count;
extern object_count_map	object_count;
extern object_count_map	live_count;

extern bool tracing_active;

bool trace_ctor(void * ptr, const std::string& name);
bool trace_dtor(void * ptr, const std::string& name);

void report_memory(std::ostream& out);

#define TRACE_CTOR(cls) CONFIRM(ledger::trace_ctor(this, cls))
#define TRACE_DTOR(cls) CONFIRM(ledger::trace_dtor(this, cls))

} // namespace ledger

#endif // _TRACE_H
