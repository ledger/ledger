#ifndef _DATETIME_H
#define _DATETIME_H

#include "ledger.h"

#include <ctime>

namespace ledger {

struct interval_t
{
  int years;
  int months;
  int seconds;

  interval_t(int _seconds, int _months = 0, int _years = 0)
    : years(_years), months(_months), seconds(_seconds) {}

  std::time_t increment(const std::time_t);

  static interval_t * parse(std::istream& in, std::time_t * begin,
			    std::time_t * end);
};

extern std::time_t now;
extern int	   now_year;

bool parse_date_mask(const char * date_str, struct std::tm * result);

bool parse_date(const char * date_str, std::time_t * result,
		const int year = -1);

bool quick_parse_date(char * date_str, std::time_t * result);

} // namespace ledger

#endif // _DATETIME_H
