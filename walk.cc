#include "walk.h"

namespace ledger {

struct sum_in_account : public item_handler<transaction_t> {
  virtual void operator()(transaction_t * xact) const {
    xact->account->value += *xact;
  }
};

void calc__accounts(account_t * account,
		    const item_predicate<transaction_t>& pred,
		    unsigned int flags)
{
  sum_in_account handler;

  for (transactions_list::iterator i = account->transactions.begin();
       i != account->transactions.end();
       i++)
    if (pred(*i))
      handle_transaction(*i, handler, flags);

  for (accounts_map::iterator i = account->accounts.begin();
       i != account->accounts.end();
       i++)
    calc__accounts((*i).second, pred, flags);
}

} // namespace ledger
