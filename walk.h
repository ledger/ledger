#ifndef _WALK_H
#define _WALK_H

#include "ledger.h"
#include "balance.h"
#include "format.h"
#include "valexpr.h"

#include <iostream>
#include <deque>

namespace ledger {

class item_predicate
{
  const node_t *   predicate;
  balance_pair_t * balance;
  unsigned int *   index;

 public:
  item_predicate(const node_t *   _predicate,
		 balance_pair_t * _balance = NULL,
		 unsigned int *   _index   = NULL)
    : predicate(_predicate), balance(_balance), index(_index) {}

  bool operator()(const entry_t * entry) const {
    if (predicate) {
      balance_t result;
      predicate->compute(result, details_t(entry, balance, index));
      return result;
    } else {
      return true;
    }
  }

  bool operator()(const transaction_t * xact) const {
    if (predicate) {
      balance_t result;
      predicate->compute(result, details_t(xact, balance, index));
      return result;
    } else {
      return true;
    }
  }

  bool operator()(const account_t * account) const {
    if (predicate) {
      balance_t result;
      predicate->compute(result, details_t(account));
      return result;
    } else {
      return true;
    }
  }
};

inline void add_to_balance_pair(balance_pair_t& balance,
				transaction_t * xact,
				const bool      inverted = false)
{
  if (inverted) {
    balance.quantity += - xact->amount;
    balance.cost     += - xact->cost;
  } else {
    balance += *xact;
  }
}

class format_transaction
{
  std::ostream&     output_stream;
  const format_t&   first_line_format;
  const format_t&   next_lines_format;
  mutable entry_t * last_entry;

 public:
  format_transaction(std::ostream&   _output_stream,
		     const format_t& _first_line_format,
		     const format_t& _next_lines_format)
    : output_stream(_output_stream),
      first_line_format(_first_line_format),
      next_lines_format(_next_lines_format),
      last_entry(NULL) {}

  void operator()(transaction_t *  xact,
		  balance_pair_t * balance,
		  unsigned int *   index,
		  const bool	   inverted) const;
};

struct compare_transactions {
  const node_t * sort_order;

  compare_transactions(const node_t * _sort_order)
    : sort_order(_sort_order) {
    assert(sort_order);
  }

  bool operator()(const transaction_t * left,
		  const transaction_t * right) const {
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

  void operator()(transaction_t *  xact,
		  balance_pair_t * balance,
		  unsigned int *   index,
		  const bool	   inverted) {
    transactions.push_back(xact);
  }
};

class ignore_transaction
{
 public:
  void operator()(transaction_t *  xact,
		  balance_pair_t * balance,
		  unsigned int *   index,
		  const bool	   inverted) const {}
};

template <typename Function>
void handle_transaction(transaction_t *  xact,
			Function         functor,
			item_predicate&  pred_functor,
			const bool	 related,
			const bool	 inverted,
			balance_pair_t * balance = NULL,
			unsigned int *	 index   = NULL)
{
  // If inverted is true, it implies related.
  if (! inverted && ! (xact->flags & TRANSACTION_HANDLED)) {
    xact->flags |= TRANSACTION_HANDLED;
    if (pred_functor(xact)) {
      xact->flags |= TRANSACTION_DISPLAYED;
      functor(xact, balance, index, inverted);
    }
  }

  if (related)
    for (transactions_list::iterator i = xact->entry->transactions.begin();
	 i != xact->entry->transactions.end();
	 i++) {
      if (*i == xact || ((*i)->flags & (TRANSACTION_AUTO |
					TRANSACTION_HANDLED)))
	continue;

      (*i)->flags |= TRANSACTION_HANDLED;
      if (pred_functor(xact)) {
	xact->flags |= TRANSACTION_DISPLAYED;
	functor(*i, balance, index, inverted);
      }
    }
}

template <typename Function>
void walk_entries(entries_list::iterator begin,
		  entries_list::iterator end,
		  Function		 functor,
		  const node_t *	 predicate,
		  const bool		 related,
		  const bool		 inverted,
		  const node_t *         display_predicate = NULL)
{
  balance_pair_t balance;
  unsigned int   index;
  item_predicate pred_functor(predicate, &balance, &index);
  item_predicate disp_pred_functor(display_predicate, &balance, &index);

  for (entries_list::iterator i = begin; i != end; i++)
    for (transactions_list::iterator j = (*i)->transactions.begin();
	 j != (*i)->transactions.end();
	 j++)
      if (pred_functor(*j))
	handle_transaction(*j, functor, disp_pred_functor,
			   related, inverted, &balance, &index);
}

template <typename Function>
void walk_transactions(transactions_list::iterator begin,
		       transactions_list::iterator end,
		       Function	      functor,
		       const node_t * predicate,
		       const bool     related,
		       const bool     inverted,
		       const node_t * display_predicate = NULL)
{
  balance_pair_t balance;
  unsigned int   index;

  for (transactions_list::iterator i = begin; i != end; i++)
    functor(*i, &balance, &index, inverted);
}

template <typename Function>
void walk_transactions(transactions_deque::iterator begin,
		       transactions_deque::iterator end,
		       Function	      functor,
		       const node_t * predicate,
		       const bool     related,
		       const bool     inverted,
		       const node_t * display_predicate = NULL)
{
  balance_pair_t balance;
  unsigned int   index;

  for (transactions_deque::iterator i = begin; i != end; i++)
    functor(*i, &balance, &index, inverted);
}

class format_account
{
  std::ostream&   output_stream;
  const format_t& format;

  mutable const account_t * last_account;

 public:
  format_account(std::ostream& _output_stream, const format_t& _format)
    : output_stream(_output_stream), format(_format) {}

  void operator()(const account_t * account, bool report_top = false);
};

void calc__accounts(account_t *    account,
		    const node_t * predicate,
		    const bool	   related,
		    const bool	   inverted,
		    const bool     calc_subtotals);

template <typename Function>
void walk__accounts(const account_t * account,
		    Function	      functor,
		    const node_t *    display_predicate)
{
  if (! display_predicate || item_predicate(display_predicate)(account))
    functor(account);

  for (accounts_map::const_iterator i = account->accounts.begin();
       i != account->accounts.end();
       i++)
    walk__accounts((*i).second, functor, display_predicate);
}

template <typename Function>
void walk_accounts(account_t *	  account,
		   Function	  functor,
		   const node_t * predicate,
		   const bool	  related,
		   const bool	  inverted,
		   const bool	  calc_subtotals,
		   const node_t * display_predicate = NULL)
{
  calc__accounts(account, predicate, related, inverted, calc_subtotals);
  walk__accounts<Function>(account, functor, display_predicate);
}

#if 0

void sum_entries(entries_list& entries,
		 account_t *   account,
		 const bool    show_subtotals)
{
  for (entries_list::const_iterator i = entries.begin();
       i != entries.end();
       i++)
    for (transactions_list::const_iterator j = (*i)->transactions.begin();
	 j != (*i)->transactions.end();
	 j++)
      if ((*j)->account == account) {
	account->value += *(*j);
	if (show_subtotals)
	  for (account_t * a = account;
	       a;
	       a = a->parent)
	    a->total += *(*j);
      }

  for (accounts_map::iterator i = account->accounts.begin();
       i != account->accounts.end();
       i++)
    sum_items(entries, *i, show_subtotals);
}

#endif

} // namespace ledger

#endif // _WALK_H
