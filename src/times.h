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

/**
 * @addtogroup util
 */

/**
 * @file   times.h
 * @author John Wiegley
 *
 * @ingroup util
 *
 * @brief datetime_t and date_t objects
 */
#ifndef _TIMES_H
#define _TIMES_H

#include "utils.h"

namespace ledger {

DECLARE_EXCEPTION(datetime_error, std::runtime_error);
DECLARE_EXCEPTION(date_error, std::runtime_error);

typedef boost::posix_time::ptime	datetime_t;
typedef datetime_t::time_duration_type	time_duration_t;

inline bool is_valid(const datetime_t& moment) {
  return ! moment.is_not_a_date_time();
}

typedef boost::gregorian::date          date_t;
typedef boost::gregorian::date_duration date_duration_t;
typedef boost::gregorian::date_iterator date_iterator_t;

inline bool is_valid(const date_t& moment) {
  return ! moment.is_not_a_date();
}

#ifdef BOOST_DATE_TIME_HAS_HIGH_PRECISION_CLOCK
#define CURRENT_TIME() boost::posix_time::microsec_clock::universal_time()
#else
#define CURRENT_TIME() boost::posix_time::second_clock::universal_time()
#endif
#define CURRENT_DATE() boost::gregorian::day_clock::universal_day()

extern date_time::weekdays   start_of_week;

optional<date_time::weekdays>
string_to_day_of_week(const std::string& str);
optional<date_time::months_of_year>
string_to_month_of_year(const std::string& str);

datetime_t parse_datetime(const char * str,
			  optional<date_t::year_type> current_year = none);

inline datetime_t parse_datetime(const std::string& str,
				 optional<date_t::year_type> current_year = none) {
  return parse_datetime(str.c_str(), current_year);
}

date_t parse_date(const char * str,
		  optional<date_t::year_type> current_year = none);

inline date_t parse_date(const std::string& str,
			 optional<date_t::year_type> current_year = none) {
  return parse_date(str.c_str(), current_year);
}

enum format_type_t {
  FMT_WRITTEN, FMT_PRINTED, FMT_CUSTOM
};

std::string format_datetime(const datetime_t& when,
			    const format_type_t format_type = FMT_PRINTED,
			    const optional<const char *>& format = none);
void set_datetime_format(const char * format);

std::string format_date(const date_t& when,
			const format_type_t format_type = FMT_PRINTED,
			const optional<const char *>& format = none);
void set_date_format(const char * format);
void set_input_date_format(const char * format);

class date_interval_t : public equality_comparable<date_interval_t>
{
public:
  typedef variant<gregorian::days,
		  gregorian::weeks,
		  gregorian::months,
		  gregorian::years> duration_t;

  static date_t add_duration(const date_t&     date,
			     const duration_t& duration);
  static date_t subtract_duration(const date_t&     date,
				  const duration_t& duration);

  optional<date_t>     start;
  bool                 aligned;
  optional<duration_t> skip_duration;
  std::size_t	       factor;
  optional<date_t>     next;
  optional<duration_t> duration;
  optional<date_t>     end_of_duration;
  optional<date_t>     end;

  explicit date_interval_t() : aligned(false), factor(1) {
    TRACE_CTOR(date_interval_t, "");
  }
  date_interval_t(const string& str) : aligned(false), factor(1) {
    TRACE_CTOR(date_interval_t, "const string&");
    parse(str);
  }
  date_interval_t(const date_interval_t& other)
    : start(other.start),
      aligned(other.aligned),
      skip_duration(other.skip_duration),
      factor(other.factor),
      next(other.next),
      duration(other.duration),
      end_of_duration(other.end_of_duration),
      end(other.end) {
    TRACE_CTOR(date_interval_t, "copy");
  }
  ~date_interval_t() throw() {
    TRACE_DTOR(date_interval_t);
  }

  bool operator==(const date_interval_t& other) const {
    return (start == other.start &&
	    (! start || *start == *other.start));
  }

  operator bool() const {
    return is_valid();
  }

  void parse(std::istream& in);

  void parse(const string& str) {
    std::istringstream in(str);
    parse(in);
  }

  void resolve_end();
  void stabilize(const optional<date_t>& date = none);

  bool is_valid() const {
    return start;
  }

  /** Find the current or next period containing date.  Returns true if the
      date_interval_t object has been altered to reflect the interval
      containing date, or false if no such period can be found. */
  bool find_period(const date_t& date);

  optional<date_t> inclusive_end() const {
    if (end_of_duration)
      return *end_of_duration - gregorian::days(1);
    else
      return none;
  }

  date_interval_t& operator++();
};

void times_initialize();
void times_shutdown();

std::ostream& operator<<(std::ostream& out,
			 const date_interval_t::duration_t& duration);

} // namespace ledger

#endif // _TIMES_H
