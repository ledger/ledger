#ifndef _DATETIME_H
#define _DATETIME_H

#include "journal.h"

#include <ctime>
#include <sstream>

namespace ledger {

struct interval_t
{
  unsigned int years;
  unsigned int months;
  unsigned int seconds;
  std::time_t  begin;
  std::time_t  end;

  interval_t(int _seconds = 0, int _months = 0, int _years = 0,
	     std::time_t _begin = 0, std::time_t _end = 0)
    : years(_years), months(_months), seconds(_seconds),
      begin(_begin), end(_end) {
    DEBUG_PRINT("ledger.memory.ctors", "ctor interval_t");
  }
  interval_t(const std::string& desc)
    : years(0), months(0), seconds(0), begin(0), end(0){
    DEBUG_PRINT("ledger.memory.ctors", "ctor interval_t");
    std::istringstream stream(desc);
    parse(stream);
  }
#ifdef DEBUG_ENABLED
  ~interval_t() {
    DEBUG_PRINT("ledger.memory.dtors", "dtor interval_t");
  }
#endif

  operator bool() const {
    return seconds > 0 || months > 0 || years > 0;
  }

  std::time_t first(const std::time_t moment = 0);
  std::time_t increment(const std::time_t);

  void parse(std::istream& in);
};

extern std::time_t now;
extern int	   now_year;

bool parse_date_mask(const char * date_str, struct std::tm * result);

bool parse_date(const char * date_str, std::time_t * result,
		const int year = -1);

bool quick_parse_date(char * date_str, std::time_t * result);

} // namespace ledger

#endif // _DATETIME_H
