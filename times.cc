/*
 * Copyright (c) 2003-2008, John Wiegley.  All rights reserved.
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

#include "utils.h"		// this brings in times.h

namespace ledger {

namespace {
#ifdef BOOST_DATE_TIME_HAS_HIGH_PRECISION_CLOCK
  const ptime time_now = boost::posix_time::microsec_clock::universal_time();
#else
  const ptime time_now = boost::posix_time::second_clock::universal_time();
#endif
  const date  date_now = boost::gregorian::day_clock::universal_day();
}

const datetime_t& current_time(time_now);
const date_t&	  current_date(date_now);
      int         current_year(current_date.year());

namespace {
  const char * formats[] = {
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
}

optional<string> input_date_format;
string		 output_date_format = "%Y/%m/%d";

namespace {
  bool parse_date_mask(const char * date_str, std::tm& result)
  {
    if (input_date_format) {
      std::memset(&result, -1, sizeof(std::tm));
      if (strptime(date_str, input_date_format->c_str(), &result))
	return true;
    }
    for (const char ** f = formats; *f; f++) {
      std::memset(&result, -1, sizeof(std::tm));
      if (strptime(date_str, *f, &result))
	return true;
    }
    return false;
  }

  bool parse_date(const char * date_str, std::tm& result, const int year)
  {
    if (! parse_date_mask(date_str, result))
      return false;

    result.tm_hour = 0;
    result.tm_min  = 0;
    result.tm_sec  = 0;

    if (result.tm_year == -1)
      result.tm_year = ((year == -1) ? current_year : year) - 1900;

    if (result.tm_mon == -1)
      result.tm_mon = 0;

    if (result.tm_mday == -1)
      result.tm_mday = 1;

    return true;
  }

  bool quick_parse_date(const char * date_str, std::tm& result)
  {
    return parse_date(date_str, result, current_year);
  }
}

datetime_t parse_datetime(const char * str)
{
  std::tm when;
  // jww (2008-08-01): This needs to look for HH:MM:SS as well.
  quick_parse_date(str, when);
  return posix_time::ptime_from_tm(when);
}

date_t parse_date(const char * str)
{
  std::tm when;
  quick_parse_date(str, when);
  return gregorian::date_from_tm(when);
}

date_t interval_t::first(const optional<date_t>& moment) const
{
  if (! is_valid(begin))
    throw_(date_error,
	   "Use of interval_t::first() with specifying a range start");

  date_t quant(begin);

  if (! advanced)
    advanced = true;

  if (moment && *moment > quant) {
    // Find an efficient starting point for the upcoming while loop.
    // We want a date early enough that the range will be correct, but
    // late enough that we don't spend hundreds of thousands of loops
    // skipping through time.

    date_t quant(moment->year(), gregorian::Jan, 1);
    date_t temp;
    while (*moment >= (temp = increment(quant))) {
      if (quant == temp)
	break;
      quant = temp;
    }
  }
  return quant;
}

date_t interval_t::increment(const date_t& moment) const
{
  date_t future(moment);

  if (years)  future += gregorian::years(years);
  if (months) future += gregorian::months(months);
  if (days)   future += gregorian::days(days);

  return future;
}

namespace {
  void parse_inclusion_specifier(const string& word,
				 date_t * begin, date_t * end)
  {
    struct std::tm when;

    if (! parse_date_mask(word.c_str(), when))
      throw_(date_error, "Could not parse date mask: " << word);

    when.tm_hour   = 0;
    when.tm_min	   = 0;
    when.tm_sec	   = 0;
    when.tm_isdst  = -1;

    bool saw_year = true;
    bool saw_mon  = true;
    bool saw_day  = true;

    if (when.tm_year == -1) {
      when.tm_year = current_year - 1900;
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
      *begin = gregorian::date_from_tm(when);
      if (end)
	*end = interval_t(saw_day ? 1 : 0, saw_mon ? 1 : 0,
			  saw_year ? 1 : 0).increment(*begin);
    }
    else if (end) {
      *end = gregorian::date_from_tm(when);
    }
  }

  inline void read_lower_word(std::istream& in, string& word) {
    in >> word;
    for (int i = 0, l = word.length(); i < l; i++)
      word[i] = std::tolower(word[i]);
  }

  void parse_date_words(std::istream& in, string& word,
			date_t * begin, date_t * end)
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
      time_t now = to_time_t(current_time);
      std::strftime(buf, 31, "%B", localtime(&now));
      word = buf;
      mon_spec = true;
    }
    else if (word == "year") {
      std::sprintf(buf, "%04d", current_year);
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

} // namespace ledger
