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
  // These options can all be set used text fields.

  strings_list  price_settings;
  std::string   init_file;
  std::string   data_file;
  std::string   cache_file;
  std::string   price_db;
  std::string   output_file;
  std::string   account;
  std::string   predicate;
  std::string   secondary_predicate;
  std::string   display_predicate;
  std::string   report_period;
  std::string   report_period_sort;
  std::string   format_string;
  std::string   balance_format;
  std::string   register_format;
  std::string   wide_register_format;
  std::string   csv_register_format;
  std::string   plot_amount_format;
  std::string   plot_total_format;
  std::string   print_format;
  std::string   write_hdr_format;
  std::string   write_xact_format;
  std::string   equity_format;
  std::string   prices_format;
  std::string   pricesdb_format;
  std::string   date_format;
  std::string   sort_string;
  std::string   amount_expr;
  std::string   total_expr;
  std::string   descend_expr;
  std::string   forecast_limit;
  std::string   reconcile_balance;
  std::string   reconcile_date;
  std::string   pager;
  unsigned long budget_flags;
  unsigned long pricing_leeway;
  int           head_entries;
  int           tail_entries;
  bool		show_collapsed;
  bool		show_subtotal;
  bool		show_totals;
  bool		show_related;
  bool		show_all_related;
  bool		show_inverted;
  bool		show_empty;
  bool		days_of_the_week;
  bool		by_payee;
  bool		comm_as_payee;
  bool		show_revalued;
  bool		show_revalued_only;
  bool		download_quotes;
  bool          use_cache;
  bool          cache_dirty;
  bool          debug_mode;
  bool          verbose_mode;
  bool          trace_mode;
  bool          keep_price;
  bool          keep_date;
  bool          keep_tag;
  bool          entry_sort;
  bool          sort_all;

  config_t() {
    reset();
  }
  config_t(const config_t&) {
    assert(0);
  }
  void reset();

  void regexps_to_predicate(const std::string& command,
			    std::list<std::string>::const_iterator begin,
			    std::list<std::string>::const_iterator end,
			    const bool account_regexp	       = false,
			    const bool add_account_short_masks = false,
			    const bool logical_and             = true);

  bool process_option(const std::string& opt, const char * arg = NULL);
  void process_arguments(int argc, char ** argv, const bool anywhere,
			 std::list<std::string>& args);
  void process_environment(char ** envp, const std::string& tag);

  void process_options(const std::string&     command,
		       strings_list::iterator arg,
		       strings_list::iterator args_end);

  item_handler<transaction_t> *
  chain_xact_handlers(const std::string& command,
		      item_handler<transaction_t> * base_formatter,
		      journal_t * journal,
		      account_t * master,
		      std::list<item_handler<transaction_t> *>& ptrs);
};

#define CONFIG_OPTIONS_SIZE 93
extern option_t config_options[CONFIG_OPTIONS_SIZE];

void option_help(std::ostream& out);

#define OPT_BEGIN(tag, chars)			\
    void opt_ ## tag(const char * optarg)

#define OPT_END(tag)

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
