#ifndef _TEXTUAL_H
#define _TEXTUAL_H

#include "ledger.h"

namespace ledger {

extern unsigned int parse_textual_ledger(std::istream& in, ledger_t * ledger,
					 account_t * master = NULL);

} // namespace ledger

#endif // _TEXTUAL_H
