#ifndef _TEXTUAL_H
#define _TEXTUAL_H

#include "ledger.h"

namespace ledger {

extern unsigned int parse_textual_ledger(std::istream& in, ledger_t *& ledger,
					 account_t * master = NULL);

extern bool parse_date_mask(const char * date_str, struct std::tm * result);

extern bool parse_date(const char * date_str, std::time_t * result,
		       const int year = -1);

extern void print_textual_ledger(std::ostream& out, ledger_t * ledger,
				 bool shortcut = true);

extern void print_textual_entry(std::ostream& out, entry_t * entry,
				bool shortcut = true);

} // namespace ledger

#endif // _TEXTUAL_H
