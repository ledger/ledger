#ifndef _REPITEM_H
#define _REPITEM_H

#include "session.h"
#include "journal.h"
#include "valexpr.h"

#include <iostream>
#include <memory>
#include <list>

namespace ledger {

class report_t;

class repitem_t : public valexpr_t::scope_t
{
public:
  repitem_t * parent;
  repitem_t * next;
  repitem_t * prev;

  union {
    transaction_t * xact;
    entry_t *	    entry;
    account_t *	    account_ptr;
    journal_t *	    journal;
    session_t *	    session;
  };

  enum kind_t { TRANSACTION, ENTRY, ACCOUNT, JOURNAL, SESSION } kind;

  bool istemp; // if item pointer is a temporary; assert that its
	       // journal pointer is NULL

  repitem_t * contents;
  repitem_t * last_content;
  repitem_t * children;
  repitem_t * last_child;

  mutable value_t _total;
  mutable value_t _value;
  mutable value_t _sort_value;

  unsigned short flags;
  unsigned int   parents_elided;
  account_t *	 reported_account;
  datetime_t	 reported_date;

  repitem_t(kind_t _kind, repitem_t * owner = NULL)
    : valexpr_t::scope_t(owner), kind(_kind),
      parent(NULL), next(NULL), prev(NULL), istemp(false),
      contents(NULL), last_content(NULL),
      children(NULL), last_child(NULL), flags(0),
      parents_elided(0), reported_account(NULL) {
    TRACE_CTOR("repitem_t(valexpr_t::scope_t *)");
    if (owner) {
      if (kind == TRANSACTION)
	owner->add_content(this);
      else
	owner->add_child(this);
    }
  }

  virtual ~repitem_t();

  void add_value(value_t& val);
  void add_sort_value(value_t& val);
  void add_total(value_t& val);

  void get_amount(value_t& result) { add_value(result); }

  datetime_t date() const;
  datetime_t effective_date() const;
  datetime_t actual_date() const;

  void get_date(value_t& result) { result = date(); }
  void get_edate(value_t& result) { result = effective_date(); }
  void get_rdate(value_t& result) { result = actual_date(); }

  account_t * account() const;

  bool valid() const;

  static repitem_t * wrap(transaction_t * txact, repitem_t * owner = NULL);
  static repitem_t * wrap(entry_t * tentry, repitem_t * owner = NULL,
			  bool deep = true);
  static repitem_t * wrap(account_t * taccount, repitem_t * owner = NULL,
			  bool deep = true);
  static repitem_t * wrap(journal_t * tjournal, repitem_t * owner = NULL,
			  bool deep = true);
  static repitem_t * wrap(session_t * tsession, bool deep = true);

  repitem_t * add_content(repitem_t * item);
  repitem_t * add_child(repitem_t * item);

  static repitem_t * fake_transaction(account_t * taccount);
  static repitem_t * fake_entry(const datetime_t& edate,
				const datetime_t& rdate,
				const std::string& payee);

  void populate_account(account_t& acct, repitem_t * item);
  void populate_accounts(entries_list& entries);
  void populate_accounts(entries_list& entries,
			 const valexpr_t& filter);

  void print_tree(std::ostream& out, int depth = 0);

  void calc(const std::string& expr, value_t& result) {
    calc(valexpr_t(expr), result);
  }
  void calc(const valexpr_t& expr, value_t& result) {
    expr.calc(result, this);
  }

  bool test(const std::string& expr) {
    return test(valexpr_t(expr));
  }
  bool test(const valexpr_t& expr) {
    value_t truth;
    calc(expr, truth);
    return truth;
  }

  //
  // Scope members
  //

  virtual void define(const std::string& name, valexpr_t::node_t * def) {
    // Pass any definitions up to our parent
    parent->define(name, def);
  }

  virtual valexpr_t::node_t * lookup(const std::string& name) {
    const char * p = name.c_str();
    switch (*p) {
    case 'a':
      switch (*++p) {
      case 'm':
	return MAKE_FUNCTOR(repitem_t, get_amount);
      }
      break;
    }
    return parent ? parent->lookup(name) : NULL;
  }
};

} // namespace ledger

#endif // _REPITEM_H
