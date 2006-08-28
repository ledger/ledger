#ifndef _REPORT_H
#define _REPORT_H

#include "ledger.h"
#include "timing.h"

#include <iostream>
#include <memory>
#include <list>

namespace ledger {

class report_t
{
 public:
  std::string output_file;
  std::string predicate;
  std::string secondary_predicate;
  std::string display_predicate;
  std::string report_period;
  std::string report_period_sort;
  std::string format_string;
  std::string sort_string;
  std::string amount_expr;
  std::string total_expr;
  std::string descend_expr;
  std::string forecast_limit;
  std::string reconcile_balance;
  std::string reconcile_date;
  std::string date_output_format;

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

  void regexps_to_predicate(const std::string& command,
			    std::list<std::string>::const_iterator begin,
			    std::list<std::string>::const_iterator end,
			    const bool account_regexp	       = false,
			    const bool add_account_short_masks = false,
			    const bool logical_and             = true);

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

class repitem_t
{
public:
  repitem_t * next;

  union {
    transaction_t * xact;
    entry_t *	    entry;
    account_t *	    account_ptr;
  };

  enum kind_t { XACT, ENTRY, ACCOUNT };
  kind_t kind;

  bool istemp; // if item pointer is a temporary; assert that its journal pointer is NULL

  repitem_t * contents;
  repitem_t ** content_next_ptr;
  repitem_t * children;
  repitem_t ** child_next_ptr;

  mutable value_t _total;
  mutable value_t _value;
  mutable value_t _sort_value;
  unsigned short  flags;
  unsigned int    parents_elided;
  account_t *	  reported_account;
  datetime_t	  reported_date;

  repitem_t() : next(NULL), istemp(false),
		contents(NULL), content_next_ptr(NULL),
		children(NULL), child_next_ptr(NULL),
		flags(0), parents_elided(0), reported_account(NULL) {}

  virtual ~repitem_t();

  void add_total(value_t& val) const;
  void add_value(value_t& val) const;
  void add_sort_value(value_t& val) const;

  datetime_t date() const;
  datetime_t effective_date() const;
  datetime_t actual_date() const;

  account_t * account() const;

  bool valid() const;

  static repitem_t * wrap_item(transaction_t * txact);
  static repitem_t * wrap_item(entry_t * tentry);
  static repitem_t * wrap_item(account_t * taccount);

  repitem_t * add_content(repitem_t * item);
  repitem_t * add_child(repitem_t * item);

  static repitem_t * fake_transaction(account_t * taccount);
  static repitem_t * fake_entry(const datetime_t& date,
				const std::string& payee);

  void populate_entries(entries_list& entries);
  void populate_entries(entries_list& entries,
			const value_expr_t * filter);

  void populate_account(account_t& acct, repitem_t * item);
  void populate_accounts(entries_list& entries);
  void populate_accounts(entries_list& entries,
			 const value_expr_t * filter);

  void print_tree(std::ostream& out, int depth = 0);
};

} // namespace ledger

#endif // _REPORT_H
