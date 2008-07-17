#if defined(__GNUG__) && __GNUG__ < 3
#define _XOPEN_SOURCE
#endif

#include "debug.h"
#include "datetime.h"

#include <ctime>
#include <cctype>
#include <climits>
#include <cstdlib>

date_t       date_t::now(std::time(NULL));
int	     date_t::current_year = date_t::now.year();
std::string  date_t::input_format;
std::string  date_t::output_format = "%Y/%m/%d";

const char * date_t::formats[] = {
  "%y/%m/%d",
  "%Y/%m/%d",
  "%m/%d",
  "%y.%m.%d",
  "%Y.%m.%d",
  "%m.%d",
  "%y-%m-%d",
  "%Y-%m-%d",
  "%m-%d",
  "%a",
  "%A",
  "%b",
  "%B",
  "%Y",
  NULL
};

datetime_t datetime_t::now(std::time(NULL));

namespace {
  static std::time_t base      = -1;
  static int	     base_year = -1;

  static const int month_days[12] = {
    31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31
  };

  bool parse_date_mask(const char * date_str, struct std::tm * result);
  bool parse_date(const char * date_str, std::time_t * result,
		  const int year = -1);
  bool quick_parse_date(const char * date_str, std::time_t * result);
}

date_t::date_t(const std::string& _when)
{
  if (! quick_parse_date(_when.c_str(), &when))
    throw new date_error
      (std::string("Invalid date string: ") + _when);
}

datetime_t::datetime_t(const std::string& _when)
{
  if (const char * p = std::strchr(_when.c_str(), ' ')) {
    date_t date(std::string(_when, 0, p - _when.c_str()));

    struct std::tm moment = *std::localtime(&date.when);
    if (! strptime(++p, "%H:%M:%S", &moment))
      throw new datetime_error
	(std::string("Invalid date/time string: ") + _when);

    when = std::mktime(&moment);
  } else {
    when = date_t(_when).when;
  }
}

datetime_t interval_t::first(const datetime_t& moment) const
{
  datetime_t quant(begin);

  if (moment && moment > quant) {
    // Find an efficient starting point for the upcoming while loop.
    // We want a date early enough that the range will be correct, but
    // late enough that we don't spend hundreds of thousands of loops
    // skipping through time.

    struct std::tm * desc = std::localtime(&moment.when);

    if (years)
      desc->tm_mon = 0;
    desc->tm_mday  = 1;

    desc->tm_hour  = 0;
    desc->tm_min   = 0;
    desc->tm_sec   = 0;
    desc->tm_isdst = -1;

    quant = std::mktime(desc);

    datetime_t temp;
    while (moment >= (temp = increment(quant))) {
      if (quant == temp)
	break;
      quant = temp;
    }
  }

  return quant;
}

datetime_t interval_t::increment(const datetime_t& moment) const
{
  struct std::tm * desc = std::localtime(&moment.when);

  if (years)
    desc->tm_year += years;
  if (months)
    desc->tm_mon += months;
  if (days)
    desc->tm_mday += days;

  desc->tm_hour += hours;
  desc->tm_min  += minutes;
  desc->tm_sec  += seconds;

  desc->tm_isdst = -1;

  return std::mktime(desc);
}

namespace {
  void parse_inclusion_specifier(const std::string& word,
				 datetime_t * begin, datetime_t * end)
  {
    struct std::tm when;

    if (! parse_date_mask(word.c_str(), &when))
      throw new datetime_error(std::string("Could not parse date mask: ") + word);

    when.tm_hour   = 0;
    when.tm_min	   = 0;
    when.tm_sec	   = 0;
    when.tm_isdst  = -1;

    bool saw_year = true;
    bool saw_mon  = true;
    bool saw_day  = true;

    if (when.tm_year == -1) {
      when.tm_year = date_t::current_year - 1900;
      saw_year = false;
    }
    if (when.tm_mon == -1) {
      when.tm_mon = 0;
      saw_mon = false;
    } else {
      saw_year = false;		// don't increment by year if month used
    }
    if (when.tm_mday == -1) {
      when.tm_mday = 1;
      saw_day = false;
    } else {
      saw_mon  = false;		// don't increment by month if day used
      saw_year = false;		// don't increment by year if day used
    }

    if (begin) {
      *begin = std::mktime(&when);
      assert(int(*begin) != -1);
      if (end) {
	*end = interval_t(saw_day ? 1 : 0, saw_mon ? 1 : 0,
			  saw_year ? 1 : 0).increment(*begin);
	assert(int(*end) != -1);
      }
    }
    else if (end) {
      *end = std::mktime(&when);
      assert(int(*end) != -1);
    }
  }

  inline void read_lower_word(std::istream& in, std::string& word) {
    in >> word;
    for (int i = 0, l = word.length(); i < l; i++)
      word[i] = std::tolower(word[i]);
  }

  void parse_date_words(std::istream& in, std::string& word,
			datetime_t * begin, datetime_t * end)
  {
    std::string type;

    bool mon_spec = false;
    char buf[32];

    if (word == "this" || word == "last" || word == "next") {
      type = word;
      if (! in.eof())
	read_lower_word(in, word);
      else
	word = "month";
    } else {
      type = "this";
    }

    if (word == "month") {
      std::strftime(buf, 31, "%B", datetime_t::now.localtime());
      word = buf;
      mon_spec = true;
    }
    else if (word == "year") {
      std::strftime(buf, 31, "%Y", datetime_t::now.localtime());
      word = buf;
    }

    parse_inclusion_specifier(word, begin, end);

    if (type == "last") {
      if (mon_spec) {
	if (begin)
	  *begin = interval_t(0, -1, 0).increment(*begin);
	if (end)
	  *end   = interval_t(0, -1, 0).increment(*end);
      } else {
	if (begin)
	  *begin = interval_t(0, 0, -1).increment(*begin);
	if (end)
	  *end   = interval_t(0, 0, -1).increment(*end);
      }
    }
    else if (type == "next") {
      if (mon_spec) {
	if (begin)
	  *begin = interval_t(0, 1, 0).increment(*begin);
	if (end)
	  *end   = interval_t(0, 1, 0).increment(*end);
      } else {
	if (begin)
	  *begin = interval_t(0, 0, 1).increment(*begin);
	if (end)
	  *end   = interval_t(0, 0, 1).increment(*end);
      }
    }
  }
}

void interval_t::parse(std::istream& in)
{
  std::string word;

  while (! in.eof()) {
    read_lower_word(in, word);
    if (word == "every") {
      read_lower_word(in, word);
      if (std::isdigit(word[0])) {
	int quantity = std::atol(word.c_str());
	read_lower_word(in, word);
	if (word == "days")
	  days = quantity;
	else if (word == "weeks")
	  days = 7 * quantity;
	else if (word == "months")
	  months = quantity;
	else if (word == "quarters")
	  months = 3 * quantity;
	else if (word == "years")
	  years = quantity;
	else if (word == "hours")
	  hours = quantity;
	else if (word == "minutes")
	  minutes = quantity;
	else if (word == "seconds")
	  seconds = quantity;
      }
      else if (word == "day")
	days = 1;
      else if (word == "week")
	days = 7;
      else if (word == "month")
	months = 1;
      else if (word == "quarter")
	months = 3;
      else if (word == "year")
	years = 1;
      else if (word == "hour")
	hours = 1;
      else if (word == "minute")
	minutes = 1;
      else if (word == "second")
	seconds = 1;
    }
    else if (word == "daily")
      days = 1;
    else if (word == "weekly")
      days = 7;
    else if (word == "biweekly")
      days = 14;
    else if (word == "monthly")
      months = 1;
    else if (word == "bimonthly")
      months = 2;
    else if (word == "quarterly")
      months = 3;
    else if (word == "yearly")
      years = 1;
    else if (word == "hourly")
      hours = 1;
    else if (word == "this" || word == "last" || word == "next") {
      parse_date_words(in, word, &begin, &end);
    }
    else if (word == "in") {
      read_lower_word(in, word);
      parse_date_words(in, word, &begin, &end);
    }
    else if (word == "from" || word == "since") {
      read_lower_word(in, word);
      parse_date_words(in, word, &begin, NULL);
    }
    else if (word == "to" || word == "until") {
      read_lower_word(in, word);
      parse_date_words(in, word, NULL, &end);
    }
    else {
      parse_inclusion_specifier(word, &begin, &end);
    }
  }
}

namespace {
  bool parse_date_mask(const char * date_str, struct std::tm * result)
  {
    if (! date_t::input_format.empty()) {
      std::memset(result, -1, sizeof(struct std::tm));
      if (strptime(date_str, date_t::input_format.c_str(), result))
	return true;
    }
    for (const char ** f = date_t::formats; *f; f++) {
      std::memset(result, -1, sizeof(struct std::tm));
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
      when.tm_year = ((year == -1) ? date_t::current_year : year) - 1900;

    if (when.tm_mon == -1)
      when.tm_mon = 0;

    if (when.tm_mday == -1)
      when.tm_mday = 1;

    *result = std::mktime(&when);

    return true;
  }

  bool quick_parse_date(const char * date_str, std::time_t * result)
  {
    return parse_date(date_str, result, date_t::current_year);
  }
}
