#ifndef _WALK_H
#define _WALK_H

#include "ledger.h"
#include "balance.h"
#include "valexpr.h"
#include "datetime.h"

#include <iostream>
#include <deque>

namespace ledger {

template <typename T>
struct item_handler {
  item_handler * handler;

 public:
  item_handler() : handler(NULL) {}
  item_handler(item_handler * _handler) : handler(_handler) {}
  virtual ~item_handler() {}
  virtual void close() {
    flush();
    if (handler) {
      delete handler;
      handler = NULL;
    }
  }
  virtual void flush() {
    if (handler)
      handler->flush();
  }
  virtual void operator()(T * item) = 0;
};

template <typename T>
struct compare_items {
  const value_expr_t * sort_order;

  compare_items(const value_expr_t * _sort_order)
    : sort_order(_sort_order) {
    assert(sort_order);
  }

  bool operator()(const T * left, const T * right) const {
    assert(left);
    assert(right);
    balance_t left_result;
    sort_order->compute(left_result, details_t(left));
    balance_t right_result;
    sort_order->compute(right_result, details_t(right));
    return left_result < right_result;
  }
};

//////////////////////////////////////////////////////////////////////
//
// Transaction handlers
//

typedef std::deque<transaction_t *> transactions_deque;
typedef std::deque<entry_t *>       entries_deque;

inline void walk_transactions(transactions_list::iterator begin,
			      transactions_list::iterator end,
			      item_handler<transaction_t>& handler) {
  for (transactions_list::iterator i = begin; i != end; i++)
    handler(*i);
}

inline void walk_transactions(transactions_list& list,
			      item_handler<transaction_t>& handler) {
  walk_transactions(list.begin(), list.end(), handler);
}

inline void walk_transactions(transactions_deque::iterator begin,
			      transactions_deque::iterator end,
			      item_handler<transaction_t>& handler) {
  for (transactions_deque::iterator i = begin; i != end; i++)
    handler(*i);
}

inline void walk_transactions(transactions_deque& deque,
			      item_handler<transaction_t>& handler) {
  walk_transactions(deque.begin(), deque.end(), handler);
}

inline void walk_entries(entries_list::iterator       begin,
			 entries_list::iterator       end,
			 item_handler<transaction_t>& handler) {
  for (entries_list::iterator i = begin; i != end; i++)
    walk_transactions((*i)->transactions, handler);
}

inline void walk_entries(entries_list& list,
			 item_handler<transaction_t>& handler) {
  walk_entries(list.begin(), list.end(), handler);
}

//////////////////////////////////////////////////////////////////////

class ignore_transactions : public item_handler<transaction_t>
{
 public:
  virtual void operator()(transaction_t * xact) {}
};

class clear_display_flags : public item_handler<transaction_t>
{
 public:
  virtual void operator()(transaction_t * xact) {
    xact->dflags = 0;
  }
};

class add_to_account_value : public item_handler<transaction_t>
{
 public:
  virtual void operator()(transaction_t * xact) {
    xact->account->value += *xact;
  }
};

class sort_transactions : public item_handler<transaction_t>
{
  transactions_deque   transactions;
  const value_expr_t * sort_order;

 public:
  sort_transactions(item_handler<transaction_t> * handler,
		    const value_expr_t * _sort_order)
    : item_handler<transaction_t>(handler),
      sort_order(_sort_order) {}
  virtual ~sort_transactions() { close(); }

  virtual void flush();
  virtual void operator()(transaction_t * xact) {
    transactions.push_back(xact);
  }
};

class filter_transactions : public item_handler<transaction_t>
{
  item_predicate<transaction_t> pred;

 public:
  filter_transactions(item_handler<transaction_t> * handler,
		      const std::string& predicate)
    : item_handler<transaction_t>(handler), pred(predicate) {}

  virtual ~filter_transactions() { close(); }

  virtual void operator()(transaction_t * xact) {
    if (pred(xact))
      (*handler)(xact);
  }
};

class calc_transactions : public item_handler<transaction_t>
{
  transaction_t * last_xact;
  const bool      inverted;

 public:
  calc_transactions(item_handler<transaction_t> * handler,
		    const bool _inverted = false)
    : item_handler<transaction_t>(handler),
      last_xact(NULL), inverted(_inverted) {}

  virtual ~calc_transactions() { close(); }

  virtual void operator()(transaction_t * xact);
};

class collapse_transactions : public item_handler<transaction_t>
{
  balance_pair_t     subtotal;
  unsigned int       count;
  entry_t *          last_entry;
  transaction_t *    last_xact;
  account_t *        totals_account;
  transactions_deque xact_temps;

 public:
  collapse_transactions(item_handler<transaction_t> * handler)
    : item_handler<transaction_t>(handler), count(0),
      last_entry(NULL), last_xact(NULL) {
    totals_account = new account_t(NULL, "<Total>");
  }

  virtual ~collapse_transactions() {
    close();
    delete totals_account;
    for (transactions_deque::iterator i = xact_temps.begin();
	 i != xact_temps.end();
	 i++)
      delete *i;
  }

  virtual void flush() {
    if (subtotal)
      report_cumulative_subtotal();
    item_handler<transaction_t>::flush();
  }

  void report_cumulative_subtotal();

  virtual void operator()(transaction_t * xact) {
    // If we've reached a new entry, report on the subtotal
    // accumulated thus far.

    if (last_entry && last_entry != xact->entry)
      report_cumulative_subtotal();

    subtotal += *xact;
    count++;

    last_entry = xact->entry;
    last_xact  = xact;
  }
};

class changed_value_transactions : public item_handler<transaction_t>
{
  // This filter requires that calc_transactions be used at some point
  // later in the chain.

  bool		     changed_values_only;
  transaction_t *    last_xact;
  entries_deque      entry_temps;
  transactions_deque xact_temps;

 public:
  changed_value_transactions(item_handler<transaction_t> * handler,
			     bool _changed_values_only)
    : item_handler<transaction_t>(handler),
      changed_values_only(_changed_values_only), last_xact(NULL) {}

  virtual ~changed_value_transactions() {
    close();

    for (entries_deque::iterator i = entry_temps.begin();
	 i != entry_temps.end();
	 i++)
      delete *i;

    for (transactions_deque::iterator i = xact_temps.begin();
	 i != xact_temps.end();
	 i++)
      delete *i;
  }

  virtual void flush() {
    (*this)(NULL);
    item_handler<transaction_t>::flush();
  }

  virtual void operator()(transaction_t * xact);
};

class subtotal_transactions : public item_handler<transaction_t>
{
  typedef std::map<account_t *, balance_pair_t>  balances_map;
  typedef std::pair<account_t *, balance_pair_t> balances_pair;

 protected:
  std::time_t	     start;
  std::time_t	     finish;
  balances_map	     balances;
  entries_deque      entry_temps;
  transactions_deque xact_temps;

 public:
  subtotal_transactions(item_handler<transaction_t> * handler)
    : item_handler<transaction_t>(handler) {}

  virtual ~subtotal_transactions() {
    close();

    for (entries_deque::iterator i = entry_temps.begin();
	 i != entry_temps.end();
	 i++)
      delete *i;

    for (transactions_deque::iterator i = xact_temps.begin();
	 i != xact_temps.end();
	 i++)
      delete *i;
  }

  void flush(const char * spec_fmt);

  virtual void flush() {
    flush(NULL);
  }
  virtual void operator()(transaction_t * xact);
};

class interval_transactions : public subtotal_transactions
{
  std::time_t     begin;
  interval_t      interval;
  transaction_t * last_xact;

 public:
  interval_transactions(item_handler<transaction_t> * handler,
			const interval_t& _interval,
			const std::time_t _begin = 0)
    : subtotal_transactions(handler), begin(_begin),
      interval(_interval), last_xact(NULL) {}

  virtual ~interval_transactions() {
    start  = begin;
    finish = interval.increment(begin);
  }

  virtual void operator()(transaction_t * xact);
};

class dow_transactions : public subtotal_transactions
{
  transactions_deque days_of_the_week[7];

 public:
  dow_transactions(item_handler<transaction_t> * handler)
    : subtotal_transactions(handler) {}

  virtual void flush();
  virtual void operator()(transaction_t * xact) {
    struct std::tm * desc = std::gmtime(&xact->entry->date);
    days_of_the_week[desc->tm_wday].push_back(xact);
  }
};

class related_transactions : public item_handler<transaction_t>
{
  bool also_matching;

 public:
  related_transactions(item_handler<transaction_t> * handler,
		       const bool _also_matching = false)
    : item_handler<transaction_t>(handler),
      also_matching(_also_matching) {}

  virtual ~related_transactions() { close(); }

  virtual void operator()(transaction_t * xact) {
    for (transactions_list::iterator i = xact->entry->transactions.begin();
	 i != xact->entry->transactions.end();
	 i++)
      if (! ((*i)->dflags & TRANSACTION_HANDLED) &&
	  (*i == xact ? also_matching :
	   ! ((*i)->flags & TRANSACTION_AUTO))) {
	(*i)->dflags |= TRANSACTION_HANDLED;
	(*handler)(*i);
      }
  }
};


//////////////////////////////////////////////////////////////////////
//
// Account walking functions
//

inline void sum_accounts(account_t * account) {
  for (accounts_map::iterator i = account->accounts.begin();
       i != account->accounts.end();
       i++) {
    sum_accounts((*i).second);
    account->total += (*i).second->total;
  }
  account->total += account->value;
}

typedef std::deque<account_t *> accounts_deque;

inline void sort_accounts(account_t *	       account,
			  const value_expr_t * sort_order,
			  accounts_deque&      accounts) {
  for (accounts_map::iterator i = account->accounts.begin();
       i != account->accounts.end();
       i++)
    accounts.push_back((*i).second);

  std::stable_sort(accounts.begin(), accounts.end(),
		   compare_items<account_t>(sort_order));
}

inline void walk_accounts(account_t *		   account,
			  item_handler<account_t>& handler,
			  const value_expr_t *     sort_order = NULL) {
  handler(account);

  if (sort_order) {
    accounts_deque accounts;
    sort_accounts(account, sort_order, accounts);
    for (accounts_deque::const_iterator i = accounts.begin();
	 i != accounts.end();
	 i++)
      walk_accounts(*i, handler, sort_order);
  } else {
    for (accounts_map::const_iterator i = account->accounts.begin();
	 i != account->accounts.end();
	 i++)
      walk_accounts((*i).second, handler);
  }
}

} // namespace ledger

#endif // _WALK_H
