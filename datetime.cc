#include "datetime.h"
#include "error.h"

#include <ctime>

namespace ledger {

std::time_t now	     = std::time(NULL);
int         now_year = std::localtime(&now)->tm_year;

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
  std::time_t then = moment;

  if (years || months) {
    struct std::tm * desc = std::localtime(&then);

    if (years)
      desc->tm_year += years;

    if (months) {
      desc->tm_mon += months;

      if (desc->tm_mon > 11) {
	desc->tm_year++;
	desc->tm_mon -= 12;
      }
      else if (desc->tm_mon < 0) {
	desc->tm_year--;
	desc->tm_mon += 12;
      }
    }

    then = std::mktime(desc);
  }

  return then + seconds;
}

static void parse_inclusion_specifier(const std::string& word,
				      std::time_t *	 begin,
				      std::time_t *	 end)
{
  struct std::tm when;

  if (! parse_date_mask(word.c_str(), &when))
    throw interval_expr_error("Could not parse 'in' date mask");

  when.tm_hour = 0;
  when.tm_min  = 0;
  when.tm_sec  = 0;

  bool saw_year = true;
  bool saw_mon  = true;

  if (when.tm_year == -1) {
    when.tm_year = now_year;
    saw_year = false;
  }
  if (when.tm_mon == -1) {
    when.tm_mon = 0;
    saw_mon = false;
  }
  if (when.tm_mday == -1)
    when.tm_mday = 1;

  *begin = std::mktime(&when);
  *end   = interval_t(0, saw_mon ? 1 : 0, saw_year ? 1 : 0).increment(*begin);
}

interval_t interval_t::parse(std::istream& in,
			     std::time_t * begin,
			     std::time_t * end)
{
  unsigned long years   = 0;
  unsigned long months  = 0;
  unsigned long seconds = 0;

  std::string word;
  while (! in.eof()) {
    in >> word;
    if (word == "every") {
      in >> word;
      if (std::isdigit(word[0])) {
	int quantity = std::atol(word.c_str());
	in >> word;
	if (word == "days")
	  seconds = 86400 * quantity;
	else if (word == "weeks")
	  seconds = 7 * 86400 * quantity;
	else if (word == "months")
	  months = quantity;
	else if (word == "quarters")
	  months = 3 * quantity;
	else if (word == "years")
	  years = quantity;
      }
      else if (word == "day")
	seconds = 86400;
      else if (word == "week")
	seconds = 7 * 86400;
      else if (word == "monthly")
	months = 1;
      else if (word == "quarter")
	months = 3;
      else if (word == "year")
	years = 1;
    }
    else if (word == "daily")
      seconds = 86400;
    else if (word == "weekly")
      seconds = 7 * 86400;
    else if (word == "biweekly")
      seconds = 14 * 86400;
    else if (word == "monthly")
      months = 1;
    else if (word == "bimonthly")
      months = 2;
    else if (word == "quarterly")
      months = 3;
    else if (word == "yearly")
      years = 1;
    else if (word == "this" || word == "last" || word == "next") {
      std::string type	   = word;
      bool	  mon_spec = false;
      char	  buf[32];

      if (! in.eof())
	in >> word;
      else
	word = "month";

      if (word == "month") {
	std::strftime(buf, 31, "%B", std::localtime(&now));
	word = buf;
	mon_spec = true;
      }
      else if (word == "year") {
	std::strftime(buf, 31, "%Y", std::localtime(&now));
	word = buf;
      }

      parse_inclusion_specifier(word, begin, end);

      if (type == "last") {
	if (mon_spec) {
	  *begin = interval_t(0, -1, 0).increment(*begin);
	  *end   = interval_t(0, -1, 0).increment(*end);
	} else {
	  *begin = interval_t(0, 0, -1).increment(*begin);
	  *end   = interval_t(0, 0, -1).increment(*end);
	}
      }
      else if (type == "next") {
	if (mon_spec) {
	  *begin = interval_t(0, 1, 0).increment(*begin);
	  *end   = interval_t(0, 1, 0).increment(*end);
	} else {
	  *begin = interval_t(0, 0, 1).increment(*begin);
	  *end   = interval_t(0, 0, 1).increment(*end);
	}
      }
    }
    else if (word == "in") {
      in >> word;
      parse_inclusion_specifier(word, begin, end);
    }
    else if (word == "from") {
      in >> word;
      if (! parse_date(word.c_str(), begin))
	throw interval_expr_error("Could not parse 'from' date");
      if (! in.eof())
	in >> word;
    }
    else if (word == "to") {
      in >> word;
      if (! parse_date(word.c_str(), end))
	throw interval_expr_error("Could not parse 'to' date");
    }
    else {
      parse_inclusion_specifier(word, begin, end);
    }
  }

  return interval_t(seconds, months, years);
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
    when.tm_year = ((year == -1) ? now_year : (year - 1900));

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
    std::memset(&when, 0, sizeof(when));

    base_year    = year == -1 ? now_year + 1900 : year;
    when.tm_year = year == -1 ? now_year : year - 1900;
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
