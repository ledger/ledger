/*
 * Copyright (c) 2003-2009, John Wiegley.  All rights reserved.
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

int                   start_of_week          = 0;
optional<std::string> input_date_format;
std::string	      output_datetime_format = "%Y-%m-%d %H:%M:%S";
std::string	      output_date_format     = "%Y-%m-%d";

namespace {
  const char * formats[] = {
    "%y/%m/%d",
    "%Y/%m/%d",
    "%m/%d",
    "%Y/%m",
    "%y.%m.%d",
    "%Y.%m.%d",
    "%m.%d",
    "%Y.%m",
    "%y-%m-%d",
    "%Y-%m-%d",
    "%m-%d",
    "%Y-%m",
    "%a",
    "%A",
    "%b",
    "%B",
    "%Y",
    NULL
  };

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

  bool quick_parse_date(const char * date_str, std::tm& result, const int year)
  {
    if (! parse_date_mask(date_str, result))
      return false;

    result.tm_hour = 0;
    result.tm_min  = 0;
    result.tm_sec  = 0;

    if (result.tm_mday == -1)
      result.tm_mday = 1;

    if (result.tm_mon == -1) {
      result.tm_mon = 0;

      if (result.tm_mday > (CURRENT_DATE().day() - 1))
	result.tm_mon = 11;
    }

    if (result.tm_year == -1) {
      result.tm_year = (year == -1 ? int(CURRENT_DATE().year()) : year) - 1900;

      if (result.tm_mon > (CURRENT_DATE().month() - 1))
	result.tm_year--;
    }

    return true;
  }
}

int string_to_day_of_week(const std::string& str)
{
  if (str == _("sun") || str == _("sunday") || str == "0")
    return 0;
  else if (str == _("mon") || str == _("monday") || str == "1")
    return 1;
  else if (str == _("tue") || str == _("tuesday") || str == "2")
    return 2;
  else if (str == _("wed") || str == _("wednesday") || str == "3")
    return 3;
  else if (str == _("thu") || str == _("thursday") || str == "4")
    return 4;
  else if (str == _("fri") || str == _("friday") || str == "5")
    return 5;
  else if (str == _("sat") || str == _("saturday") || str == "6")
    return 6;

  assert(false);
  return -1;
}
  
datetime_t parse_datetime(const char * str, int)
{
  std::tm when;
  std::memset(&when, -1, sizeof(std::tm));
  if (strptime(str, "%Y/%m/%d %H:%M:%S", &when))
    return posix_time::ptime_from_tm(when);
  else
    return datetime_t();
}

date_t parse_date(const char * str, int current_year)
{
  std::tm when;
  quick_parse_date(str, when, current_year);
  return gregorian::date_from_tm(when);
}

date_t interval_t::first(const optional<date_t>& moment)
{
  if (! is_valid(begin)) {
    // Find an efficient starting point for the upcoming while loop.  We want
    // a date early enough that the range will be correct, but late enough
    // that we don't spend hundreds of thousands of loops skipping through
    // time.
    assert(moment);

    if (months > 0 || years > 0) {
      begin = date_t(moment->year(), gregorian::Jan, 1);
    } else {
      begin = date_t(*moment - gregorian::days(400));

      // jww (2009-02-21): Add support for starting a week on any day
      if (weekly) {		// move it to a Sunday
	while (begin.day_of_week() != start_of_week)
	  begin += gregorian::days(1);
      }
    }
  }

  date_t quant(begin);

  if (moment && *moment >= quant) {
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
				 date_t *      begin,
				 date_t *      end)
  {
    struct std::tm when;

    if (! parse_date_mask(word.c_str(), when))
      throw_(date_error, _("Could not parse date mask: %1") << word);

    when.tm_hour   = 0;
    when.tm_min	   = 0;
    when.tm_sec	   = 0;
    when.tm_isdst  = -1;

    bool saw_year = true;
    bool saw_mon  = true;
    bool saw_day  = true;

    if (when.tm_year == -1) {
      when.tm_year = CURRENT_DATE().year() - 1900;
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

    if (word == _("this") || word == _("last") || word == _("next")) {
      type = word;
      if (! in.eof())
	read_lower_word(in, word);
      else
	word = _("month");
    } else {
      type = _("this");
    }

    if (word == _("month")) {
      time_t now = to_time_t(CURRENT_TIME());
      std::strftime(buf, 31, "%B", localtime(&now));
      word = buf;
      mon_spec = true;
    }
    else if (word == _("year")) {
      int year = CURRENT_DATE().year();
      std::sprintf(buf, "%04d", year);
      word = buf;
    }

    parse_inclusion_specifier(word, begin, end);

    if (type == _("last")) {
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
    else if (type == _("next")) {
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
    if (word == _("every")) {
      read_lower_word(in, word);
      if (std::isdigit(word[0])) {
	int quantity = lexical_cast<int>(word);
	read_lower_word(in, word);
	if (word == _("days"))
	  days = quantity;
	else if (word == _("weeks")) {
	  days = 7 * quantity;
	  weekly = true;
	}
	else if (word == _("months"))
	  months = quantity;
	else if (word == _("quarters"))
	  months = 3 * quantity;
	else if (word == _("years"))
	  years = quantity;
      }
      else if (word == _("day"))
	days = 1;
      else if (word == _("week")) {
	days = 7;
	weekly = true;
      }
      else if (word == _("month"))
	months = 1;
      else if (word == _("quarter"))
	months = 3;
      else if (word == _("year"))
	years = 1;
    }
    else if (word == _("daily"))
      days = 1;
    else if (word == _("weekly")) {
      days = 7;
      weekly = true;
    }
    else if (word == _("biweekly"))
      days = 14;
    else if (word == _("monthly"))
      months = 1;
    else if (word == _("bimonthly"))
      months = 2;
    else if (word == _("quarterly"))
      months = 3;
    else if (word == _("yearly"))
      years = 1;
    else if (word == _("this") || word == _("last") || word == _("next")) {
      parse_date_words(in, word, &begin, &end);
    }
    else if (word == _("in")) {
      read_lower_word(in, word);
      parse_date_words(in, word, &begin, &end);
    }
    else if (word == _("from") || word == _("since")) {
      read_lower_word(in, word);
      parse_date_words(in, word, &begin, NULL);
    }
    else if (word == _("to") || word == _("until")) {
      read_lower_word(in, word);
      parse_date_words(in, word, NULL, &end);
    }
    else {
      parse_inclusion_specifier(word, &begin, &end);
    }
  }
}

} // namespace ledger
