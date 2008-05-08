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

#ifndef _TIMES_H
#define _TIMES_H

namespace ledger {

#define SUPPORT_DATE_AND_TIME 1
#ifdef SUPPORT_DATE_AND_TIME

typedef boost::posix_time::ptime	datetime_t;
typedef datetime_t::time_duration_type	duration_t;

inline bool is_valid(const datetime_t& moment) {
  return ! moment.is_not_a_date_time();
}

#else // SUPPORT_DATE_AND_TIME

typedef boost::gregorian::date          datetime_t;
typedef boost::gregorian::date_duration duration_t;

inline bool is_valid(const datetime_t& moment) {
  return ! moment.is_not_a_date();
}

#endif // SUPPORT_DATE_AND_TIME

extern const datetime_t& current_moment;

extern int    current_year;
extern string input_time_format;
extern string output_time_format;

DECLARE_EXCEPTION(error, datetime_error);

struct interval_t
{
  unsigned short years;
  unsigned short months;
  unsigned short days;
  unsigned short hours;
  unsigned short minutes;
  unsigned short seconds;

  datetime_t begin;
  datetime_t end;

  interval_t(int _days = 0, int _months = 0, int _years = 0,
	     const datetime_t& _begin = datetime_t(),
	     const datetime_t& _end   = datetime_t())
    : years(_years), months(_months), days(_days),
      hours(0), minutes(0), seconds(0),
      begin(_begin), end(_end) {}

  interval_t(const string& desc)
    : years(0), months(0), days(0),
      hours(0), minutes(0), seconds(0) {
    std::istringstream stream(desc);
    parse(stream);
  }

  operator bool() const {
    return (years > 0 || months > 0  || days > 0 ||
	    hours > 0 || minutes > 0 || seconds > 0);
  }

  void start(const datetime_t& moment) {
    begin = first(moment);
  }
  datetime_t first(const datetime_t& moment = datetime_t()) const;
  datetime_t increment(const datetime_t&) const;

  void parse(std::istream& in);
};

#if 0
inline datetime_t ptime_local_to_utc(const datetime_t& when) {
  struct std::tm tm_gmt = to_tm(when);
  return boost::posix_time::from_time_t(std::mktime(&tm_gmt));
}

// jww (2007-04-18): I need to make a general parsing function
// instead, and then make these into private methods.
inline datetime_t ptime_from_local_date_string(const string& date_string) {
  return ptime_local_to_utc(datetime_t(boost::gregorian::from_string(date_string),
				       time_duration()));
}

inline datetime_t ptime_from_local_time_string(const string& time_string) {
  return ptime_local_to_utc(boost::posix_time::time_from_string(time_string));
}
#endif

datetime_t parse_datetime(const char * str);

inline datetime_t parse_datetime(const string& str) {
  return parse_datetime(str.c_str());
}

extern const ptime time_now;
extern const date  date_now;
extern bool        day_before_month;

#if 0
struct intorchar
{
  int	      ival;
  string sval;

  intorchar() : ival(-1) {}
  intorchar(int val) : ival(val) {}
  intorchar(const string& val) : ival(-1), sval(val) {}
  intorchar(const intorchar& o) : ival(o.ival), sval(o.sval) {}
};

ledger::datetime_t parse_abs_datetime(std::istream& input);
#endif

} // namespace ledger

#endif // _TIMES_H
