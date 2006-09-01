#ifndef _REPITEM_H
#define _REPITEM_H

#include "journal.h"
#include "datetime.h"
#include "session.h"

#include <iostream>
#include <memory>
#include <list>

namespace ledger {

class report_t;

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
    report_t *	    report_ptr;
  };

  enum kind_t { XACT, ENTRY, ACCOUNT, REPORT };
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
  report_t * report() const;

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

#endif // _REPITEM_H
