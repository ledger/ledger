#include "reconcile.h"
#include "walk.h"

namespace ledger {

#define xact_next(x)     ((transaction_t *)(x)->data)
#define xact_next_ptr(x) ((transaction_t **)&(x)->data)

bool search_for_balance(amount_t& amount,
			transaction_t ** prev, transaction_t * next)
{
  for (; next; next = xact_next(next)) {
    transaction_t * temp = *prev;
    *prev = next;

    amount -= next->amount;
    if (! amount ||
	search_for_balance(amount, xact_next_ptr(next), xact_next(next)))
      return true;
    amount += next->amount;

    *prev = temp;
  }
  return false;
}

static void push_chain_to_list(transaction_t * first,
			       transactions_list& xact_list)
{
  while (first) {
    transaction_t * curr = first;
    xact_list.push_back(curr);
    first = xact_next(first);
    curr->data = 0;
  }
}

void reconcile_transactions(transactions_list& xact_list,
			    value_t&	       balance,
			    const time_t       cutoff,
			    const bool         all_pending)
{
  value_t cleared_balance;
  value_t pending_balance;

  transaction_t *  first    = NULL;
  transaction_t ** last_ptr = &first;

  clear_transactions_xdata();

  bool found_pending = false;
  for (transactions_list::iterator x = xact_list.begin();
       x != xact_list.end();
       x++)
    if (! cutoff || std::difftime((*x)->entry->date, cutoff) < 0)
      switch ((*x)->entry->state) {
      case entry_t::CLEARED:
	cleared_balance += (*x)->amount;
	if (! found_pending)
	  break;
	// fall through...
      case entry_t::UNCLEARED:
      case entry_t::PENDING:
	pending_balance += (*x)->amount;
	if (all_pending)
	  found_pending = true;
	*last_ptr = *x;
	last_ptr = (transaction_t **) &(*x)->data;
	break;
      }

  if (all_pending) {
    xact_list.clear();
    push_chain_to_list(first, xact_list);
    return;
  }

  if (cleared_balance.type >= value_t::BALANCE)
    throw error("Cannot reconcile accounts with multiple commodities");

  cleared_balance.cast(value_t::AMOUNT);
  balance.cast(value_t::AMOUNT);

  commodity_t& cb_comm = ((amount_t *) cleared_balance.data)->commodity();
  commodity_t& b_comm  = ((amount_t *) balance.data)->commodity();

  balance -= cleared_balance;
  if (balance.type >= value_t::BALANCE)
    throw error(std::string("Reconcile balance is not of the same commodity ('") +
		b_comm.symbol + "' != '" + cb_comm.symbol + "')");

  // If the amount to reconcile is the same as the pending balance,
  // then assume an exact match and return the results right away.
  amount_t to_reconcile = *((amount_t *) balance.data);
  pending_balance.cast(value_t::AMOUNT);
  if (to_reconcile == *((amount_t *) pending_balance.data) ||
      search_for_balance(to_reconcile, &first, first)) {
    xact_list.clear();
    push_chain_to_list(first, xact_list);
  } else {
    throw error("Could not reconcile account!");
  }
}

} // namespace ledger
