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
  virtual ~item_handler() {}
  virtual void flush() {}
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

struct ignore_transactions : public item_handler<transaction_t>
{
  virtual void operator()(transaction_t * xact) {}
};

struct clear_display_flags : public item_handler<transaction_t>
{
  virtual void operator()(transaction_t * xact) {
    xact->dflags = 0;
  }
};

class sort_transactions : public item_handler<transaction_t>
{
  transactions_deque transactions;
  const value_expr_t *     sort_order;

  item_handler<transaction_t> * handler;

 public:
  sort_transactions(item_handler<transaction_t> * _handler,
		    const value_expr_t * _sort_order)
    : sort_order(_sort_order), handler(_handler) {}

  virtual ~sort_transactions() {
    flush();
    handler->flush();
    delete handler;
  }

  virtual void flush() {
    std::stable_sort(transactions.begin(), transactions.end(),
		     compare_items<transaction_t>(sort_order));

    for (transactions_deque::iterator i = transactions.begin();
	 i != transactions.end();
	 i++)
      (*handler)(*i);

    transactions.clear();

    handler->flush();
  }

  virtual void operator()(transaction_t * xact) {
    transactions.push_back(xact);
  }
};

class filter_transactions : public item_handler<transaction_t>
{
  item_predicate<transaction_t> pred;

  item_handler<transaction_t> * handler;

 public:
  filter_transactions(item_handler<transaction_t> * _handler,
		      const std::string& predicate)
    : pred(predicate), handler(_handler) {}

  virtual ~filter_transactions() {
    handler->flush();
    delete handler;
  }

  virtual void operator()(transaction_t * xact) {
    if (pred(xact))
      (*handler)(xact);
  }
};

class calc_transactions : public item_handler<transaction_t>
{
  transaction_t * last_xact;
  const bool      inverted;

  item_handler<transaction_t> * handler;

 public:
  calc_transactions(item_handler<transaction_t> * _handler,
		    const bool _inverted = false)
    : last_xact(NULL), inverted(_inverted), handler(_handler) {}

  virtual ~calc_transactions() {
    handler->flush();
    delete handler;
  }

  virtual void operator()(transaction_t * xact);
};

class collapse_transactions : public item_handler<transaction_t>
{
  balance_pair_t  subtotal;
  unsigned int    count;
  entry_t *       last_entry;
  transaction_t * last_xact;

  item_handler<transaction_t> * handler;

  account_t *        totals_account;
  transactions_deque xact_temps;

 public:
  collapse_transactions(item_handler<transaction_t> * _handler)
    : count(0), last_entry(NULL), last_xact(NULL),
      handler(_handler) {
    totals_account = new account_t(NULL, "<Total>");
  }

  virtual ~collapse_transactions() {
    flush();
    handler->flush();

    delete handler;
    delete totals_account;

    for (transactions_deque::iterator i = xact_temps.begin();
	 i != xact_temps.end();
	 i++)
      delete *i;
  }

  virtual void flush() {
    if (subtotal)
      report_cumulative_subtotal();
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

// This filter requires that calc_transactions be used.

class changed_value_transactions : public item_handler<transaction_t>
{
  transaction_t * last_xact;

  item_handler<transaction_t> * handler;

  entries_deque      entry_temps;
  transactions_deque xact_temps;

 public:
  changed_value_transactions(item_handler<transaction_t> * _handler)
    : last_xact(NULL), handler(_handler) {}

  virtual ~changed_value_transactions() {
    flush();
    handler->flush();
    delete handler;

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
  }

  virtual void operator()(transaction_t * xact);
};

typedef std::map<account_t *, balance_pair_t>  balances_map;
typedef std::pair<account_t *, balance_pair_t> balances_pair;

class subtotal_transactions : public item_handler<transaction_t>
{
 protected:
  std::time_t  start;
  std::time_t  finish;
  balances_map balances;

  item_handler<transaction_t> * handler;

  entries_deque      entry_temps;
  transactions_deque xact_temps;

 public:
  subtotal_transactions(item_handler<transaction_t> * _handler)
    : handler(_handler) {}

  virtual ~subtotal_transactions() {
    flush();
    handler->flush();
    delete handler;

    for (entries_deque::iterator i = entry_temps.begin();
	 i != entry_temps.end();
	 i++)
      delete *i;

    for (transactions_deque::iterator i = xact_temps.begin();
	 i != xact_temps.end();
	 i++)
      delete *i;
  }

  virtual void flush();
  virtual void operator()(transaction_t * xact);
};

class interval_transactions : public subtotal_transactions
{
  std::time_t     begin;
  interval_t      interval;
  transaction_t * last_xact;

 public:
  interval_transactions(item_handler<transaction_t> * _handler,
			std::time_t _begin, const interval_t& _interval)
    : subtotal_transactions(_handler),
      begin(_begin), interval(_interval), last_xact(NULL) {}

  virtual ~interval_transactions() {
    start  = begin;
    finish = interval.increment(begin);
  }

  virtual void operator()(transaction_t * xact) {
    if (std::difftime(xact->entry->date, interval.increment(begin)) > 0) {
      if (last_xact) {
	start  = begin;
	finish = interval.increment(begin);
	flush();
      }

      begin = interval.increment(begin);
      std::time_t temp;
      while (std::difftime(xact->entry->date,
			   temp = interval.increment(begin)) > 0)
	begin = temp;
    }

    subtotal_transactions::operator()(xact);

    last_xact = xact;
  }
};

class related_transactions : public item_handler<transaction_t>
{
  bool also_matching;

  item_handler<transaction_t> * handler;

 public:
  related_transactions(item_handler<transaction_t> * _handler,
		       bool _also_matching = false)
    : also_matching(_also_matching), handler(_handler) {}

  virtual ~related_transactions() {
    handler->flush();
    delete handler;
  }

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
  // jww (2004-08-11): do transaction dflags need to be cleared first?
  for (entries_list::iterator i = begin; i != end; i++)
    walk_transactions((*i)->transactions, handler);
}

inline void walk_entries(entries_list& list,
			 item_handler<transaction_t>& handler) {
  walk_entries(list.begin(), list.end(), handler);
}


//////////////////////////////////////////////////////////////////////
//
// Account handlers
//

typedef std::deque<account_t *> accounts_deque;

struct add_to_account_value : public item_handler<transaction_t>
{
  virtual void operator()(transaction_t * xact) {
    xact->account->value += *xact;
  }
};

#if 0

class format_accounts : public item_handler<account_t>
{
};

class filter_accounts : public item_handler<account_t>
{
  item_handler<account_t> * handler;

 public:
  filter_accounts(item_handler<account_t> * _handler)
    : handler(_handler) {}

  virtual ~filter_accounts() {
    handler->flush();
    delete handler;
  }

  virtual void flush() {}

  virtual void operator()(account_t * account) {
  }
};

class sort_accounts : public item_handler<account_t>
{
  value_expr_t * sort_order;

  item_handler<account_t> * handler;

 public:
  sort_accounts(item_handler<account_t> * _handler,
		value_expr_t * _sort_order)
    : sort_order(_sort_order), handler(_handler) {}

  virtual ~sort_accounts() {
    handler->flush();
    delete handler;
  }

  virtual void flush() {}

  virtual void operator()(account_t * account) {
    accounts_deque accounts;

    for (accounts_map::iterator i = account->accounts.begin();
	 i != account->accounts.end();
	 i++)
      accounts.push_back((*i).second);

    std::stable_sort(accounts.begin(), accounts.end(),
		     compare_items<account_t>(sort_order));
  }
};

class balance_accounts : public item_handler<account_t>
{
  item_handler<account_t> * handler;

 public:
  balance_accounts(item_handler<account_t> * _handler)
    : handler(_handler) {}

  virtual ~balance_accounts() {
    handler->flush();
    delete handler;
  }

  virtual void flush() {
    if (format_account::disp_subaccounts_p(top)) {
      std::string end_format = "--------------------\n";
      format.reset(end_format + f);
      format.format_elements(std::cout, details_t(top));
    }
  }

  virtual void operator()(account_t * account) {
  }
};

#endif

inline void sort_accounts(account_t *	  account,
			  accounts_deque& accounts,
			  const value_expr_t *  sort_order)
{
  for (accounts_map::iterator i = account->accounts.begin();
       i != account->accounts.end();
       i++)
    accounts.push_back((*i).second);

  std::stable_sort(accounts.begin(), accounts.end(),
		   compare_items<account_t>(sort_order));
}

inline void walk__accounts_sorted(account_t * account,
				  item_handler<account_t>& handler,
				  const value_expr_t * sort_order)
{
  handler(account);

  accounts_deque accounts;
  sort_accounts(account, accounts, sort_order);

  for (accounts_deque::const_iterator i = accounts.begin();
       i != accounts.end();
       i++)
    walk__accounts_sorted(*i, handler, sort_order);
}

inline void sum__accounts(account_t * account)
{
  for (accounts_map::iterator i = account->accounts.begin();
       i != account->accounts.end();
       i++) {
    sum__accounts((*i).second);
    account->total += (*i).second->total;
  }
  account->total += account->value;
}

inline void walk__accounts(account_t * account,
			   item_handler<account_t>& handler)
{
  handler(account);

  for (accounts_map::const_iterator i = account->accounts.begin();
       i != account->accounts.end();
       i++)
    walk__accounts((*i).second, handler);
}

inline void walk_accounts(account_t *		       account,
			  item_handler<account_t>&     handler,
			  const bool		       calc_subtotals,
			  const value_expr_t *         sort_order = NULL)
{
  if (calc_subtotals)
    sum__accounts(account);

  if (sort_order)
    walk__accounts_sorted(account, handler, sort_order);
  else
    walk__accounts(account, handler);
}

} // namespace ledger

#endif // _WALK_H
