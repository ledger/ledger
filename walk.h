#ifndef _WALK_H
#define _WALK_H

#include "ledger.h"
#include "balance.h"
#include "valexpr.h"

#include <iostream>
#include <deque>

namespace ledger {

template <typename T>
class item_predicate
{
  const node_t * predicate;

 public:
  item_predicate(const node_t * _predicate) : predicate(_predicate) {}

  bool operator()(const T * item) const {
    if (predicate) {
      balance_t result;
      predicate->compute(result, details_t(item));
      return result;
    } else {
      return true;
    }
  }
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

class collect_transactions
{
  transactions_deque& transactions;

 public:
  collect_transactions(transactions_deque& _transactions)
    : transactions(_transactions) {}

  void operator()(transaction_t * xact) {
    transactions.push_back(xact);
  }
};

inline void sort_transactions(transactions_deque& transactions,
			      const node_t *	  sort_order)
{
  std::stable_sort(transactions.begin(), transactions.end(),
		   compare_items<transaction_t>(sort_order));
}

class ignore_transaction
{
 public:
  void operator()(transaction_t * xact) const {}
};

#define MATCHING_TRANSACTIONS 0x01
#define OTHER_TRANSACTIONS    0x02

template <typename Function>
void handle_transaction(transaction_t * xact, Function functor,
			item_predicate<transaction_t>& pred_functor,
			unsigned int flags)
{
  if ((flags & MATCHING_TRANSACTIONS) &&
      ! (xact->flags & TRANSACTION_HANDLED)) {
    xact->flags |= TRANSACTION_HANDLED;
    if (pred_functor(xact)) {
      xact->flags |= TRANSACTION_DISPLAYED;
      functor(xact);
    }
  }

  if (flags & OTHER_TRANSACTIONS)
    for (transactions_list::iterator i = xact->entry->transactions.begin();
	 i != xact->entry->transactions.end();
	 i++) {
      if (*i == xact || ((*i)->flags & (TRANSACTION_AUTO |
					TRANSACTION_HANDLED)))
	continue;

      (*i)->flags |= TRANSACTION_HANDLED;
      if (pred_functor(xact)) {
	xact->flags |= TRANSACTION_DISPLAYED;
	functor(*i);
      }
    }
}

template <typename Function>
void walk_entries(entries_list::iterator begin,
		  entries_list::iterator end,
		  Function	 functor,
		  const node_t * predicate,
		  unsigned int	 flags,
		  const node_t * display_predicate = NULL)
{
  item_predicate<transaction_t> pred_functor(predicate);
  item_predicate<transaction_t> disp_pred_functor(display_predicate);

  for (entries_list::iterator i = begin; i != end; i++)
    for (transactions_list::iterator j = (*i)->transactions.begin();
	 j != (*i)->transactions.end();
	 j++)
      if (pred_functor(*j))
	handle_transaction(*j, functor, disp_pred_functor, flags);
}

template <typename Function>
void walk_entries(entries_list::iterator begin,
		  entries_list::iterator end, Function functor)
{
  for (entries_list::iterator i = begin; i != end; i++)
    for (transactions_list::iterator j = (*i)->transactions.begin();
	 j != (*i)->transactions.end();
	 j++)
      functor(*j);
}

class clear_flags
{
 public:
  void operator()(transaction_t * xact) const {
    xact->flags &= ~(TRANSACTION_HANDLED | TRANSACTION_DISPLAYED);
  }
};

inline void clear_transaction_display_flags(entries_list::iterator begin,
					    entries_list::iterator end)
{
  walk_entries<clear_flags>(begin, end, clear_flags());
}

template <typename Function>
void walk_transactions(transactions_list::iterator begin,
		       transactions_list::iterator end, Function functor)
{
  for (transactions_list::iterator i = begin; i != end; i++)
    functor(*i);
}

template <typename Function>
void walk_transactions(transactions_deque::iterator begin,
		       transactions_deque::iterator end, Function functor)
{
  for (transactions_deque::iterator i = begin; i != end; i++)
    functor(*i);
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

template <typename Function>
void walk__accounts(const account_t *  account,
		    Function	       functor,
		    const unsigned int max_depth,
		    item_predicate<account_t>& disp_pred_functor)
{
  if (disp_pred_functor(account))
    functor(account, max_depth);

  for (accounts_map::const_iterator i = account->accounts.begin();
       i != account->accounts.end();
       i++)
    walk__accounts((*i).second, functor, max_depth, disp_pred_functor);
}

template <typename Function>
void walk__accounts_sorted(const account_t *  account,
			   Function	      functor,
			   const unsigned int max_depth,
			   const node_t *     sort_order,
			   item_predicate<account_t>& disp_pred_functor)
{
  if (disp_pred_functor(account))
    functor(account, max_depth);

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
    walk__accounts_sorted(*i, functor, max_depth, sort_order,
			  disp_pred_functor);
}

template <typename Function>
void for_each_account(account_t * account, Function functor)
{
  functor(account);

  for (accounts_map::iterator i = account->accounts.begin();
       i != account->accounts.end();
       i++)
    walk__accounts((*i).second, functor);
}

void calc__accounts(account_t * account,
		    item_predicate<transaction_t>& pred_functor,
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

template <typename Function>
void walk_accounts(account_t *	      account,
		   Function	      functor,
		   const node_t *     predicate,
		   unsigned int	      flags,
		   const bool	      calc_subtotals,
		   const unsigned int max_depth,
		   const node_t *     display_predicate = NULL,
		   const node_t *     sort_order	= NULL)
{
  item_predicate<transaction_t> pred_functor(predicate);
  item_predicate<account_t>     disp_pred_functor(display_predicate);

  calc__accounts(account, pred_functor, flags);
  if (calc_subtotals)
    sum__accounts(account);

  if (sort_order)
    walk__accounts_sorted<Function>(account, functor, max_depth, sort_order,
				    disp_pred_functor);
  else
    walk__accounts<Function>(account, functor, max_depth, disp_pred_functor);
}

} // namespace ledger

#endif // _WALK_H
