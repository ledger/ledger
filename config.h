#ifndef _CONFIG_H
#define _CONFIG_H

#include "ledger.h"
#include "option.h"
#include "valexpr.h"
#include "datetime.h"
#include "format.h"

#include <iostream>
#include <memory>

namespace ledger {

extern std::string bal_fmt;
extern std::string reg_fmt;
extern std::string plot_value_fmt;
extern std::string plot_total_fmt;
extern std::string print_fmt;
extern std::string equity_fmt;

struct config_t
{
  // These options can all be set used text fields.

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
  bool		show_all_related;
  bool		show_inverted;
  bool		show_empty;
  bool		days_of_the_week;
  bool		show_revalued;
  bool		show_revalued_only;
  bool		download_quotes;

  // These settings require processing of the above.

  bool          use_cache;
  bool          cache_dirty;
  interval_t    report_interval;
  std::time_t   interval_begin;
  format_t      format;
  format_t      nformat;

  std::auto_ptr<value_expr_t> sort_order;
  std::auto_ptr<std::ostream> output_stream;

  config_t();

  void process_options(const std::string&     command,
		       strings_list::iterator arg,
		       strings_list::iterator args_end);
};

extern config_t config;

void option_help(std::ostream& out);

struct declared_option_handler : public option_handler {
  declared_option_handler(const std::string& label,
			  const std::string& opt_chars) {
    register_option(label, opt_chars, *this);
  }
};

#define OPT_BEGIN(tag, chars)						\
  static struct opt_ ## tag ## _handler					\
      : public declared_option_handler {				\
    opt_ ## tag ## _handler() : declared_option_handler(#tag, chars) {}	\
    virtual void operator()(const char * optarg)

#define OPT_END(tag) } opt_ ## tag ## _handler_obj

} // namespace ledger

#endif // _CONFIG_H
