#ifndef _DERIVE_H
#define _DERIVE_H

#include "report.h"

namespace ledger {

entry_t * derive_new_entry(report_t& report,
			   strings_list::iterator begin,
			   strings_list::iterator end);

} // namespace ledger

#endif // _DERIVE_H
