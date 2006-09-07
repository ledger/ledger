#ifndef _TRACE_H
#define _TRACE_H

#include "timing.h"

#include <string>

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

} // namespace ledger

#endif // _TRACE_H
