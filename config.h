#ifndef _CONFIG_H
#define _CONFIG_H

#include "ledger.h"

namespace ledger {

class config_t
{
 public:
  path init_file;
  path data_file;
  path cache_file;
  path price_db;

  string balance_format;
  string register_format;
  string wide_register_format;
  string plot_amount_format;
  string plot_total_format;
  string print_format;
  string write_hdr_format;
  string write_xact_format;
  string equity_format;
  string prices_format;
  string pricesdb_format;

  string date_input_format;

  string account;
  string pager;

  unsigned long pricing_leeway;

  bool download_quotes;
  bool use_cache;
  bool cache_dirty;
  bool debug_mode;
  bool verbose_mode;
  bool trace_mode;

  config_t();
};

} // namespace ledger

#endif // _CONFIG_H
