#ifndef _REPORT_H
#define _REPORT_H

#include "ledger.h"

#include <iostream>
#include <memory>
#include <list>

namespace ledger {

class report_t
{
 public:
  path output_file;

  string predicate;
  string secondary_predicate;
  string display_predicate;
  string report_period;
  string report_period_sort;
  string format_string;
  string sort_string;
  string amount_expr;
  string total_expr;
  string descend_expr;
  string forecast_limit;
  string reconcile_balance;
  string reconcile_date;
  string date_output_format;

  unsigned long budget_flags;

  int head_entries;
  int tail_entries;

  bool show_collapsed;
  bool show_subtotal;
  bool show_totals;
  bool show_related;
  bool show_all_related;
  bool show_inverted;
  bool show_empty;
  bool days_of_the_week;
  bool by_payee;
  bool comm_as_payee;
  bool code_as_payee;
  bool show_revalued;
  bool show_revalued_only;
  bool keep_price;
  bool keep_date;
  bool keep_tag;
  bool entry_sort;
  bool sort_all;

  report_t();

  void regexps_to_predicate(const string& command,
			    std::list<string>::const_iterator begin,
			    std::list<string>::const_iterator end,
			    const bool account_regexp	       = false,
			    const bool add_account_short_masks = false,
			    const bool logical_and             = true);

  void process_options(const string&     command,
		       strings_list::iterator arg,
		       strings_list::iterator args_end);

  item_handler<transaction_t> *
  chain_xact_handlers(const string& command,
		      item_handler<transaction_t> * base_formatter,
		      journal_t * journal,
		      account_t * master,
		      std::list<item_handler<transaction_t> *>& ptrs);
};

} // namespace ledger

#endif // _REPORT_H
