#ifndef _RECONCILE_H
#define _RECONCILE_H

#include "journal.h"
#include "walk.h"

namespace ledger {

struct reconcile_results_t
{
  value_t previous_balance;
  value_t remaining_balance;

  transactions_list pending_xacts;
};

reconcile_results_t reconcile_account(journal_t&     journal,
				      account_t&     account,
				      const value_t& balance);

} // namespace ledger

#endif // _RECONCILE_H
