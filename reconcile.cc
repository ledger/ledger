#include "reconcile.h"
#include "walk.h"

namespace ledger {

#if 0
#define xact_next(x)     ((transaction_t *)transaction_xdata(*x).ptr)
#define xact_next_ptr(x) ((transaction_t **)&transaction_xdata(*x).ptr)

static bool search_for_balance(amount_t& amount,
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

void reconcile_transactions::push_to_handler(transaction_t * first)
{
  for (; first; first = xact_next(first))
    item_handler<transaction_t>::operator()(*first);

  item_handler<transaction_t>::flush();
}

void reconcile_transactions::flush()
{
  value_t cleared_balance;
  value_t pending_balance;

  transaction_t *  first    = NULL;
  transaction_t ** last_ptr = &first;

  bool found_pending = false;
  for (transactions_list::iterator x = xacts.begin();
       x != xacts.end();
       x++) {
    if (! cutoff || (*x)->date() < cutoff) {
      switch ((*x)->state) {
      case transaction_t::CLEARED:
	cleared_balance += (*x)->amount;
	break;
      case transaction_t::UNCLEARED:
      case transaction_t::PENDING:
	pending_balance += (*x)->amount;
	*last_ptr = *x;
	last_ptr = xact_next_ptr(*x);
	break;
      }
    }
  }

  if (cleared_balance.type >= value_t::BALANCE)
    throw new error("Cannot reconcile accounts with multiple commodities");

  cleared_balance.cast(value_t::AMOUNT);
  balance.cast(value_t::AMOUNT);

  commodity_t& cb_comm = ((amount_t *) cleared_balance.data)->commodity();
  commodity_t& b_comm  = ((amount_t *) balance.data)->commodity();

  balance -= cleared_balance;
  if (balance.type >= value_t::BALANCE)
    throw new error(std::string("Reconcile balance is not of the same commodity ('") +
		    b_comm.symbol() + "' != '" + cb_comm.symbol() + "')");

  // If the amount to reconcile is the same as the pending balance,
  // then assume an exact match and return the results right away.
  amount_t to_reconcile = *((amount_t *) balance.data);
  pending_balance.cast(value_t::AMOUNT);
  if (to_reconcile == *((amount_t *) pending_balance.data) ||
      search_for_balance(to_reconcile, &first, first)) {
    push_to_handler(first);
  } else {
    throw new error("Could not reconcile account!");
  }
}
#endif

} // namespace ledger
