#ifndef _DATETIME_H
#define _DATETIME_H

#include "ledger.h"

namespace ledger {

struct interval_t
{
  unsigned long years;
  unsigned long months;
  unsigned long seconds;

  interval_t(unsigned long _seconds, unsigned long _months,
	     unsigned long _years)
    : years(_years), months(_months), seconds(_seconds) {}

  std::time_t increment(const std::time_t);
};

extern bool parse_date_mask(const char * date_str, struct std::tm * result);

extern bool parse_date(const char * date_str, std::time_t * result,
		       const int year = -1);

extern bool quick_parse_date(char * date_str, std::time_t * result);

extern struct std::tm * now_tm;

} // namespace ledger

#endif // _DATETIME_H
