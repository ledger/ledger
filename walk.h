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
  virtual void operator()(T * item) const = 0;
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

typedef std::deque<transaction_t *> transactions_deque;

class collect_transactions : public item_handler<transaction_t> {
  transactions_deque& transactions;

 public:
  collect_transactions(transactions_deque& _transactions)
    : transactions(_transactions) {}

  virtual void operator()(transaction_t * xact) const {
    transactions.push_back(xact);
  }
};

inline void sort_transactions(transactions_deque& transactions,
			      const node_t *	  sort_order)
{
  std::stable_sort(transactions.begin(), transactions.end(),
		   compare_items<transaction_t>(sort_order));
}

struct ignore_transaction : public item_handler<transaction_t> {
  virtual void operator()(transaction_t * xact) const {}
};

#define MATCHING_TRANSACTIONS 0x01
#define OTHER_TRANSACTIONS    0x02

inline void handle_transaction(transaction_t * xact,
			       const item_handler<transaction_t>& handler,
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
			 const item_handler<transaction_t>& handler,
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
			 const item_handler<transaction_t>& handler)
{
  for (entries_list::iterator i = begin; i != end; i++)
    for (transactions_list::iterator j = (*i)->transactions.begin();
	 j != (*i)->transactions.end();
	 j++)
      handler(*j);
}

struct clear_flags : public item_handler<transaction_t> {
  virtual void operator()(transaction_t * xact) const {
    xact->dflags = 0;
  }
};

inline void clear_transaction_display_flags(entries_list::iterator begin,
					    entries_list::iterator end)
{
  walk_entries(begin, end, clear_flags());
}

inline void walk_transactions(transactions_list::iterator begin,
			      transactions_list::iterator end,
			      const item_handler<transaction_t>& handler)
{
  for (transactions_list::iterator i = begin; i != end; i++)
    handler(*i);
}

inline void walk_transactions(transactions_deque::iterator begin,
			      transactions_deque::iterator end,
			      const item_handler<transaction_t>& handler)
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
			   const item_handler<account_t>& handler)
{
  handler(account);

  for (accounts_map::const_iterator i = account->accounts.begin();
       i != account->accounts.end();
       i++)
    walk__accounts((*i).second, handler);
}

inline void walk__accounts_sorted(account_t * account,
				  const item_handler<account_t>& handler,
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
			     const item_handler<account_t>& handler)
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
			  const item_handler<account_t>& handler,
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
