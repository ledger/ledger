#ifndef _DATETIME_H
#define _DATETIME_H

#include "ledger.h"

namespace ledger {

extern bool parse_date_mask(const char * date_str, struct std::tm * result);

extern bool parse_date(const char * date_str, std::time_t * result,
		       const int year = -1);

extern bool quick_parse_date(char * date_str, std::time_t * result);

extern struct std::tm * now_tm;

} // namespace ledger

#endif // _DATETIME_H
