#include "autoxact.h"

namespace ledger {

void automated_transaction_t::extend_entry(entry_t * entry)
{
  for (transactions_list::iterator i = entry->transactions.begin();
       i != entry->transactions.end();
       i++)
    if (matches(masks, *((*i)->account))) {
      for (transactions_list::iterator t = transactions.begin();
	   t != transactions.end();
	   t++) {
	amount_t amt;
	if ((*t)->amount.commodity->symbol.empty())
	  amt = (*i)->amount * (*t)->amount;
	else
	  amt = (*t)->amount;

	transaction_t * xact
	  = new transaction_t(entry, (*t)->account, amt, amt,
			      (*t)->flags | TRANSACTION_AUTO);
	entry->add_transaction(xact);
      }
    }
}

} // namespace ledger
