#ifndef _WALK_H
#define _WALK_H

#include "ledger.h"
#include "balance.h"
#include "valexpr.h"

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
  const node_t * sort_order;

  compare_items(const node_t * _sort_order)
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
// Several default handlers
//

typedef std::deque<transaction_t *> transactions_deque;
typedef std::deque<entry_t *>       entries_deque;

struct ignore_transaction : public item_handler<transaction_t>
{
  virtual void operator()(transaction_t * xact) {}
};

class collect_transactions : public item_handler<transaction_t>
{
  transactions_deque& transactions;

 public:
  collect_transactions(transactions_deque& _transactions)
    : transactions(_transactions) {}

  virtual void operator()(transaction_t * xact) {
    transactions.push_back(xact);
  }
};

inline void sort_transactions(transactions_deque& transactions,
			      const node_t *	  sort_order)
{
  std::stable_sort(transactions.begin(), transactions.end(),
		   compare_items<transaction_t>(sort_order));
}

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

    if (last_entry && last_entry != xact->entry) {
      report_cumulative_subtotal();
      subtotal = 0;
      count    = 0;
    }

    subtotal += *xact;
    count++;

    last_entry = xact->entry;
    last_xact  = xact;
  }
};

// This filter requires that calc_transactions be used.

class changed_value_transactions : public item_handler<transaction_t>
{
  entry_t         modified_entry;
  transaction_t   modified_xact;
  transaction_t * last_xact;

  item_handler<transaction_t> * handler;

 public:
  changed_value_transactions(item_handler<transaction_t> * _handler)
    : modified_xact(&modified_entry, NULL), last_xact(NULL),
      handler(_handler) {
    assert(handler);
    modified_entry.payee = "Commodities revalued";
  }

  virtual ~changed_value_transactions() {
    flush();
    handler->flush();
    delete handler;
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

class interval_transactions : public item_handler<transaction_t>
{
  std::time_t     start;
  unsigned long   months;
  unsigned long   seconds;
  transaction_t * last_xact;

  item_handler<transaction_t> * handler;

 public:
  interval_transactions(item_handler<transaction_t> * _handler,
			std::time_t _start, unsigned long _months,
			unsigned long _seconds)
    : start(_start), months(_months), seconds(_seconds),
      last_xact(NULL), handler(_handler) {}

  virtual ~interval_transactions() {
    flush();
  }

  virtual void flush() {
    handler->flush();
  }
  virtual void operator()(transaction_t * xact) {
    if (std::difftime(xact->entry->date, start + seconds) > 0) {
      if (last_xact)
	handler->flush();
      start += seconds;
      while (std::difftime(xact->entry->date, start + seconds) > 0)
	start += seconds;
    }

    (*handler)(xact);

    last_xact = xact;
  }
};

//////////////////////////////////////////////////////////////////////

#define MATCHING_TRANSACTIONS 0x01
#define OTHER_TRANSACTIONS    0x02

inline void handle_transaction(transaction_t * xact,
			       item_handler<transaction_t>& handler,
			       unsigned int flags)
{
  for (transactions_list::iterator i = xact->entry->transactions.begin();
       i != xact->entry->transactions.end();
       i++)
    if ((! (flags & OTHER_TRANSACTIONS) ||
	 ! ((*i)->flags & TRANSACTION_AUTO)) &&
	! ((*i)->dflags & TRANSACTION_HANDLED) &&
	(*i == xact ?
	 (flags & MATCHING_TRANSACTIONS) : (flags & OTHER_TRANSACTIONS))) {
      (*i)->dflags |= TRANSACTION_HANDLED;
      handler(*i);
    }
}

inline void walk_entries(entries_list::iterator begin,
			 entries_list::iterator end,
			 item_handler<transaction_t>& handler,
			 const std::string&	predicate,
			 unsigned int		flags)
{
  item_predicate<transaction_t> pred(predicate);

  for (entries_list::iterator i = begin; i != end; i++)
    for (transactions_list::iterator j = (*i)->transactions.begin();
	 j != (*i)->transactions.end();
	 j++)
      if (pred(*j))
	handle_transaction(*j, handler, flags);
}

inline void walk_entries(entries_list::iterator begin,
			 entries_list::iterator end,
			 item_handler<transaction_t>& handler)
{
  for (entries_list::iterator i = begin; i != end; i++)
    for (transactions_list::iterator j = (*i)->transactions.begin();
	 j != (*i)->transactions.end();
	 j++)
      handler(*j);
}

struct clear_flags : public item_handler<transaction_t>
{
  virtual void operator()(transaction_t * xact) {
    xact->dflags = 0;
  }
};

inline void clear_transaction_display_flags(entries_list::iterator begin,
					    entries_list::iterator end)
{
  clear_flags handler;
  walk_entries(begin, end, handler);
}

inline void walk_transactions(transactions_list::iterator begin,
			      transactions_list::iterator end,
			      item_handler<transaction_t>& handler)
{
  for (transactions_list::iterator i = begin; i != end; i++)
    handler(*i);
}

inline void walk_transactions(transactions_deque::iterator begin,
			      transactions_deque::iterator end,
			      item_handler<transaction_t>& handler)
{
  for (transactions_deque::iterator i = begin; i != end; i++)
    handler(*i);
}

typedef std::deque<account_t *> accounts_deque;

inline void sort_accounts(account_t *	  account,
			  accounts_deque& accounts,
			  const node_t *  sort_order)
{
  for (accounts_map::iterator i = account->accounts.begin();
       i != account->accounts.end();
       i++)
    accounts.push_back((*i).second);

  std::stable_sort(accounts.begin(), accounts.end(),
		   compare_items<account_t>(sort_order));
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

inline void walk__accounts_sorted(account_t * account,
				  item_handler<account_t>& handler,
				  const node_t * sort_order)
{
  handler(account);

  accounts_deque accounts;

  for (accounts_map::const_iterator i = account->accounts.begin();
       i != account->accounts.end();
       i++)
    accounts.push_back((*i).second);

  std::stable_sort(accounts.begin(), accounts.end(),
		   compare_items<account_t>(sort_order));

  for (accounts_deque::const_iterator i = accounts.begin();
       i != accounts.end();
       i++)
    walk__accounts_sorted(*i, handler, sort_order);
}

inline void for_each_account(account_t * account,
			     item_handler<account_t>& handler)
{
  handler(account);

  for (accounts_map::iterator i = account->accounts.begin();
       i != account->accounts.end();
       i++)
    walk__accounts((*i).second, handler);
}

void calc__accounts(account_t * account,
		    const item_predicate<transaction_t>& pred,
		    unsigned int flags);

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

inline void walk_accounts(account_t *	     account,
			  item_handler<account_t>& handler,
			  const std::string& predicate,
			  unsigned int	     flags,
			  const bool	     calc_subtotals,
			  const node_t *     sort_order = NULL)
{
  item_predicate<transaction_t> pred(predicate);

  calc__accounts(account, pred, flags);
  if (calc_subtotals)
    sum__accounts(account);

  if (sort_order)
    walk__accounts_sorted(account, handler, sort_order);
  else
    walk__accounts(account, handler);
}

} // namespace ledger

#endif // _WALK_H
