#ifndef _DERIVE_H
#define _DERIVE_H

#include "journal.h"

namespace ledger {

entry_t * derive_new_entry(journal_t& journal,
			   strings_list::iterator begin,
			   strings_list::iterator end);

} // namespace ledger

#endif // _DERIVE_H
