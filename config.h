#ifndef _CONFIG_H
#define _CONFIG_H

#include "ledger.h"
#include "timing.h"

#include <iostream>
#include <memory>
#include <list>

namespace ledger {

class config_t
{
 public:
  std::string init_file;
  std::string data_file;
  std::string cache_file;
  std::string price_db;

  std::string balance_format;
  std::string register_format;
  std::string wide_register_format;
  std::string plot_amount_format;
  std::string plot_total_format;
  std::string print_format;
  std::string write_hdr_format;
  std::string write_xact_format;
  std::string equity_format;
  std::string prices_format;
  std::string pricesdb_format;

  std::string date_input_format;

  std::string account;
  std::string pager;

  unsigned long pricing_leeway;

  bool download_quotes;
  bool use_cache;
  bool cache_dirty;
  bool debug_mode;
  bool verbose_mode;
  bool trace_mode;

  config_t();
};

//////////////////////////////////////////////////////////////////////

std::string resolve_path(const std::string& path);

//////////////////////////////////////////////////////////////////////

void trace(const std::string& cat, const std::string& str);
void trace_push(const std::string& cat, const std::string& str,
		timing_t& timer);
void trace_pop(const std::string& cat, const std::string& str,
	       timing_t& timer);

#define TRACE(cat, msg) if (config.trace_mode) trace(#cat, msg)
#define TRACE_(cat, msg) if (trace_mode) trace(#cat, msg)

#define TRACE_PUSH(cat, msg)					\
  timing_t timer_ ## cat(#cat);					\
  if (config.trace_mode) trace_push(#cat, msg, timer_ ## cat)
#define TRACE_PUSH_(cat, msg)					\
  timing_t timer_ ## cat(#cat);					\
  if (trace_mode) trace_push(#cat, msg, timer_ ## cat)

#define TRACE_POP(cat, msg)					\
  if (config.trace_mode) trace_pop(#cat, msg, timer_ ## cat)
#define TRACE_POP_(cat, msg)					\
  if (trace_mode) trace_pop(#cat, msg, timer_ ## cat)

} // namespace ledger

#endif // _CONFIG_H
