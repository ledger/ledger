#include "walk.h"

namespace ledger {

class sum_in_account
{
 public:
  void operator()(transaction_t * xact) const {
    xact->account->value += *xact;
  }
};

void calc__accounts(account_t * account,
		    item_predicate<transaction_t>& pred_functor,
		    unsigned int flags)
{
  sum_in_account functor;

  for (transactions_list::iterator i = account->transactions.begin();
       i != account->transactions.end();
       i++)
    if (pred_functor(*i))
      handle_transaction(*i, functor, flags);

  for (accounts_map::iterator i = account->accounts.begin();
       i != account->accounts.end();
       i++)
    calc__accounts((*i).second, pred_functor, flags);
}

} // namespace ledger
