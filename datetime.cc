#if defined(__GNUG__) && __GNUG__ < 3
#define _XOPEN_SOURCE
#endif

#include "datetime.h"

#include <ctime>
#include <cctype>

std::time_t         now	      = std::time(NULL);
int                 now_year  = std::localtime(&now)->tm_year;

static std::time_t  base      = -1;
static int	    base_year = -1;

static const int    month_days[12] = {
  31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31
};

char input_format[128];

const char * formats[] = {
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

std::time_t interval_t::first(const std::time_t moment) const
{
  std::time_t quant = begin;

  if (moment && std::difftime(moment, quant) > 0) {
    // Find an efficient starting point for the upcoming while loop.
    // We want a date early enough that the range will be correct, but
    // late enough that we don't spend hundreds of thousands of loops
    // skipping through time.

    struct std::tm * desc = std::localtime(&moment);
    if (years)
      desc->tm_mon = 0;
    desc->tm_mday = 1;
    desc->tm_hour = 0;
    desc->tm_min  = 0;
    desc->tm_sec  = 0;
    quant = std::mktime(desc);

    std::time_t temp;
    while (std::difftime(moment, temp = increment(quant)) >= 0) {
      if (quant == temp)
	break;
      quant = temp;
    }
  }

  return quant;
}

std::time_t interval_t::increment(const std::time_t moment) const
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

    desc->tm_hour  = 0;
    desc->tm_min   = 0;
    desc->tm_sec   = 0;
    desc->tm_isdst = 0;

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
    throw datetime_error(std::string("Could not parse date mask: ") + word);

  when.tm_hour = 0;
  when.tm_min  = 0;
  when.tm_sec  = 0;

  bool saw_year = true;
  bool saw_mon  = true;
  bool saw_day  = true;

  if (when.tm_year == -1) {
    when.tm_year = now_year;
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
    saw_mon = false;		// don't increment by month if day used
    saw_year = false;		// don't increment by year if day used
  }

  if (begin) {
    *begin = std::mktime(&when);
    if (end)
      *end = interval_t(saw_day ? 86400 : 0, saw_mon ? 1 : 0,
			saw_year ? 1 : 0).increment(*begin);
  }
  else if (end) {
    *end = std::mktime(&when);
  }
}

static inline void read_lower_word(std::istream& in, std::string& word) {
  in >> word;
  for (int i = 0, l = word.length(); i < l; i++)
    word[i] = std::tolower(word[i]);
}

static void parse_date_words(std::istream& in, std::string& word,
			     std::time_t * begin, std::time_t * end)
{
  std::string type;
  bool	      mon_spec = false;
  char	      buf[32];

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
      else if (word == "month")
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

bool quick_parse_date(const char * date_str, std::time_t * result)
{
  return parse_date(date_str, result, now_year + 1900);
}
