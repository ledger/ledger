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
  repitem_t * parent;
  repitem_t * next;
  repitem_t * prev;

  union {
    transaction_t * xact;
    entry_t *	    entry;
    account_t *	    account_ptr;
  };

  enum kind_t { XACT, ENTRY, ACCOUNT };
  kind_t kind;

  bool istemp; // if item pointer is a temporary; assert that its
	       // journal pointer is NULL

  repitem_t * contents;
  repitem_t * last_content;
  repitem_t * children;
  repitem_t * last_child;

  mutable value_t _total;
  mutable value_t _value;
  mutable value_t _sort_value;
  unsigned short  flags;
  unsigned int    parents_elided;
  account_t *	  reported_account;
  datetime_t	  reported_date;

  repitem_t()
    : parent(NULL), next(NULL), prev(NULL), istemp(false),
      contents(NULL), last_content(NULL),
      children(NULL), last_child(NULL),
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

class transform_t {
 public:
  virtual void walk_items(repitem_t * items) = 0;
};

typedef std::list<transform_t *> transform_queue_list;

extern transform_queue_list pre_transform_queue;
extern transform_queue_list transform_queue;
extern transform_queue_list post_transform_queue;

void apply_transform_queue(repitem_t * items);

class split_transform : public transform_t {
  // --split breaks entry with two or more transactions into what
  // seems like two entries each with one transaction -- even though
  // it is the same entry being reported in both cases.  This is
  // useful before sorting, for exampel, in order to sort by
  // transaction instead of by entry.
 public:
  virtual void walk_items(repitem_t * items);
};

class check_transform : public transform_t {
  // --check checks the validity of the item list.
 public:
  virtual void walk_items(repitem_t * items);
};

class merge_transform : public transform_t {
  // --merge is the opposite of --split: any adjacent transactions
  // which share the same entry will be merged into a group of
  // transactions under one reported entry.
 public:
  virtual void walk_items(repitem_t * items);
};

class combine_transform : public transform_t {
  // --combine EXPR combines all transactions matching EXPR so that
  // they appear within the same virtual entry (whose date will span
  // the earliest to the latest of those entries, and whose payee name
  // will show the terminating date or a label that is characteristic
  // of the set).
 public:
  virtual void walk_items(repitem_t * items);
};

class group_transform : public transform_t {
  // --group groups all transactions that affect the same account
  // within an entry, so that they appear as a single transaction.
 public:
  virtual void walk_items(repitem_t * items);
};

class collapse_transform : public transform_t {
  // --collapse makes all transactions within an entry appear as a
  // single transaction, even if they affect different accounts.  The
  // fictitous account "<total>" is used to represent the final sum,
  // if multiple accounts are involved.
 public:
  virtual void walk_items(repitem_t * items);
};

class subtotal_transform : public transform_t {
  // --subtotal will combine the transactions from all entries into
  // one giant entry.  When used in conjunction with --group, the
  // affect is very similar to a regular balance report.
 public:
  virtual void walk_items(repitem_t * items);
};

} // namespace ledger

#endif // _REPORT_H
