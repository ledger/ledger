#ifndef _RECONCILE_H
#define _RECONCILE_H

#include "journal.h"

namespace ledger {

void reconcile_transactions(transactions_list& xacts,
			    value_t&	       balance,
			    const time_t       cutoff,
			    const bool         all_pending = false);

} // namespace ledger

#endif // _RECONCILE_H
