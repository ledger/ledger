#include "datetime.h"

#include <ctime>

namespace ledger {

static std::time_t	now	  = std::time(NULL);
       struct std::tm * now_tm	  = std::localtime(&now);

static std::time_t	base      = -1;
static int		base_year = -1;

static const int	month_days[12] = {
  31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31
};

static const char *	formats[] = {
  "%Y/%m/%d",
  "%m/%d",
  "%Y.%m.%d",
  "%m.%d",
  "%Y-%m-%d",
  "%m-%d",
  "%a",
  "%A",
  "%b",
  "%B",
  "%Y",
  NULL
};

std::time_t interval_t::increment(const std::time_t moment)
{
  // jww (2004-08-11): NYI
  return moment + seconds;
}

bool parse_date_mask(const char * date_str, struct std::tm * result)
{
  for (const char ** f = formats; *f; f++) {
    memset(result, INT_MAX, sizeof(struct std::tm));
    if (strptime(date_str, *f, result))
      return true;
  }
  return false;
}

bool parse_date(const char * date_str, std::time_t * result, const int year)
{
  struct std::tm when;

  if (! parse_date_mask(date_str, &when))
    return false;

  when.tm_hour = 0;
  when.tm_min  = 0;
  when.tm_sec  = 0;

  if (when.tm_year == -1)
    when.tm_year = ((year == -1) ? now_tm->tm_year : (year - 1900));

  if (when.tm_mon == -1)
    when.tm_mon = 0;

  if (when.tm_mday == -1)
    when.tm_mday = 1;

  *result = std::mktime(&when);

  return true;
}

bool quick_parse_date(char * date_str, std::time_t * result)
{
  int year = -1, month = -1, day, num = 0;

  for (char * p = date_str; *p; p++) {
    if (*p == '/' || *p == '-' || *p == '.') {
      if (year == -1)
	year = num;
      else
	month = num;
      num = 0;
    }
    else if (*p < '0' || *p > '9') {
      return false;
    }
    else {
      num *= 10;
      num += *p - '0';
    }
  }

  day = num;

  if (month == -1) {
    month = year;
    year  = -1;
  }

  if (base == -1 || year != base_year) {
    struct std::tm when;

    when.tm_hour = 0;
    when.tm_min  = 0;
    when.tm_sec  = 0;

    base_year    = year == -1 ? now_tm->tm_year + 1900 : year;
    when.tm_year = year == -1 ? now_tm->tm_year : year - 1900;
    when.tm_mon  = 0;
    when.tm_mday = 1;

    base = std::mktime(&when);
  }

  *result = base;

  --month;
  while (--month >= 0) {
    *result += month_days[month] * 24 * 60 * 60;
    if (month == 1 && year % 4 == 0 && year != 2000) // february in leap years
      *result += 24 * 60 * 60;
  }

  if (--day)
    *result += day * 24 * 60 * 60;

  return true;
}

} // namespace ledger
