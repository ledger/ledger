#ifndef _AUTOXACT_H
#define _AUTOXACT_H

#include "ledger.h"

#include <iostream>
#include <memory>

namespace ledger {

extern const std::string bal_fmt;
extern const std::string reg_fmt;
extern const std::string plot_value_fmt;
extern const std::string plot_total_fmt;
extern const std::string print_fmt;
extern const std::string equity_fmt;

struct config_t
{
  strings_list  price_settings;
  std::string   init_file;
  std::string   data_file;
  std::string   cache_file;
  std::string   price_db;
  std::string   output_file;
  std::string   account;
  std::string   predicate;
  std::string   display_predicate;
  std::string   interval_text;
  std::string   format_string;
  std::string   date_format;
  std::string   sort_string;
  std::string   value_expr;
  std::string   total_expr;
  unsigned long pricing_leeway;
  bool		show_collapsed;
  bool		show_subtotal;
  bool		show_related;
  bool		show_inverted;
  bool		show_empty;
  bool		days_of_the_week;
  bool		show_revalued;
  bool		show_revalued_only;
  bool		download_quotes;

  config_t();
};

extern config_t * config;

void option_help(std::ostream& out);

} // namespace ledger

#endif // _CONFIG_H
