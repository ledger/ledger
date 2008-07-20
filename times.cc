/*
 * Copyright (c) 2003-2007, John Wiegley.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * - Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 *
 * - Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in the
 *   documentation and/or other materials provided with the distribution.
 *
 * - Neither the name of New Artisans LLC nor the names of its
 *   contributors may be used to endorse or promote products derived from
 *   this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include "utils.h"

namespace ledger {

#ifdef BOOST_DATE_TIME_HAS_HIGH_PRECISION_CLOCK
const ptime time_now = boost::posix_time::microsec_clock::universal_time();
#else
const ptime time_now = boost::posix_time::second_clock::universal_time();
#endif
const date  date_now = boost::gregorian::day_clock::universal_day();

#ifdef SUPPORT_DATE_AND_TIME
const datetime_t& current_moment(time_now);
#else
const datetime_t& current_moment(date_now);
#endif

int current_year(current_moment.date().year());

string input_time_format;
string output_time_format = "%Y/%m/%d";

#if 0
static const char * formats[] = {
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
#endif

bool	    day_before_month		 = false;
static bool day_before_month_initialized = false;

#if 0
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
#endif

datetime_t parse_datetime(const char * str)
{
  if (! day_before_month_initialized) {
#ifdef HAVE_NL_LANGINFO
    const char * d_fmt = nl_langinfo(D_FMT);
    if (d_fmt && std::strlen(d_fmt) > 1 && d_fmt[1] == 'd')
      day_before_month = true;
    day_before_month_initialized = true;
#endif
  }
#if 0
  return parse_abs_datetime(in);
#else
  int year = ((str[0] - '0') * 1000 +
	      (str[1] - '0') * 100 +
	      (str[2] - '0') * 10 +
	      (str[3] - '0'));

  int mon = ((str[5] - '0') * 10 +
	     (str[6] - '0'));

  int day = ((str[8] - '0') * 10 +
	     (str[9] - '0'));

  return datetime_t(boost::gregorian::date(year, mon, day));
#endif
}

datetime_t interval_t::first(const datetime_t& moment) const
{
  datetime_t quant(begin);

#if 0
  // jww (2008-05-08): Implement
  if (is_valid(moment) && moment > quant) {
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
#endif

  return quant;
}

datetime_t interval_t::increment(const datetime_t& moment) const
{
#if 0
  struct std::tm * desc = std::localtime(&moment.when);

  // jww (2008-05-08): Implement
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
#else
  return datetime_t();
#endif
}

namespace {
  void parse_inclusion_specifier(const string& word,
				 datetime_t * begin, datetime_t * end)
  {
#if 0
    // jww (2008-05-08): Implement!
    struct std::tm when;

    if (! parse_date_mask(word.c_str(), &when))
      throw new datetime_error(string("Could not parse date mask: ") + word);

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
#endif
  }

  inline void read_lower_word(std::istream& in, string& word) {
    in >> word;
    for (int i = 0, l = word.length(); i < l; i++)
      word[i] = std::tolower(word[i]);
  }

  void parse_date_words(std::istream& in, string& word,
			datetime_t * begin, datetime_t * end)
  {
    string type;

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
#if 0
      // jww (2008-05-08): 
      std::strftime(buf, 31, "%B", datetime_t::now.localtime());
#endif
      word = buf;
      mon_spec = true;
    }
    else if (word == "year") {
#if 0
      // jww (2008-05-08): 
      std::strftime(buf, 31, "%Y", datetime_t::now.localtime());
#endif
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
  string word;

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
#if 0
    // jww (2008-05-08): 
    if (! date_t::input_format.empty()) {
      std::memset(result, -1, sizeof(struct std::tm));
      if (strptime(date_str, date_t::input_format.c_str(), result))
	return true;
    }
    for (const char ** f = formats; *f; f++) {
      std::memset(result, INT_MAX, sizeof(struct std::tm));
      if (strptime(date_str, *f, result))
	return true;
    }
#endif
    return false;
  }

  bool parse_date(const char * date_str, std::time_t * result, const int year)
  {
#if 0
    // jww (2008-05-08): 
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
#endif

    return true;
  }

  bool quick_parse_date(const char * date_str, std::time_t * result)
  {
#if 0
    // jww (2008-05-08): 
    return parse_date(date_str, result, date_t::current_year);
#else
    return false;
#endif
  }
}

} // namespace ledger
