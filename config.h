#ifndef _CONFIG_H
#define _CONFIG_H

#include "journal.h"
#include "option.h"
#include "valexpr.h"
#include "datetime.h"
#include "format.h"
#include "parser.h"

#include <iostream>
#include <memory>
#include <list>

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
  std::string   report_interval;
  std::string   format_string;
  std::string   balance_format;
  std::string   register_format;
  std::string   plot_value_format;
  std::string   plot_total_format;
  std::string   print_format;
  std::string   equity_format;
  std::string   date_format;
  std::string   sort_string;
  std::string   amount_expr;
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
  bool          use_cache;
  bool          cache_dirty;

  config_t();
  config_t(const config_t&) {
    assert(0);
  }

  void process_options(const std::string&     command,
		       strings_list::iterator arg,
		       strings_list::iterator args_end);
};

extern config_t		   config;
extern std::list<option_t> config_options;

void option_help(std::ostream& out);

// Parse what ledger data can be determined from the config settings
void parse_ledger_data(journal_t * journal,
		       parser_t *  text_parser,
		       parser_t *  cache_parser = NULL);

struct declared_option_handler : public option_handler {
  declared_option_handler(const std::string& label,
			  const std::string& opt_chars) {
    add_option_handler(config_options, label, opt_chars, *this);
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
