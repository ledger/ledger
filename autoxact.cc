#include "autoxact.h"

namespace ledger {

void automated_transaction_t::extend_entry(entry_t * entry)
{
  transactions_deque initial_xacts(entry->transactions.begin(),
				   entry->transactions.end());

  for (transactions_deque::iterator i = initial_xacts.begin();
       i != initial_xacts.end();
       i++)
    if (predicate(**i))
      for (transactions_deque::iterator t = transactions.begin();
	   t != transactions.end();
	   t++) {
	amount_t amt;
	if ((*t)->amount.commodity().symbol.empty())
	  amt = (*i)->amount * (*t)->amount;
	else
	  amt = (*t)->amount;

	transaction_t * xact
	  = new transaction_t((*t)->account, amt,
			      (*t)->flags | TRANSACTION_AUTO);
	entry->add_transaction(xact);
      }
}

automated_transactions_t * current_auto_xacts = NULL;

bool handle_auto_xacts(entry_t * entry)
{
  if (current_auto_xacts &&
      ! current_auto_xacts->automated_transactions.empty())
    current_auto_xacts->extend_entry(entry);

  return true;
}

} // namespace ledger
