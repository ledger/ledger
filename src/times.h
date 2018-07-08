/*
 * Copyright (c) 2003-2018, John Wiegley.  All rights reserved.
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

typedef boost::posix_time::ptime        datetime_t;
typedef datetime_t::time_duration_type  time_duration_t;

inline bool is_valid(const datetime_t& moment) {
  return ! moment.is_not_a_date_time();
}

typedef boost::gregorian::date          date_t;
typedef boost::gregorian::date_iterator date_iterator_t;

inline bool is_valid(const date_t& moment) {
  return ! moment.is_not_a_date();
}

extern optional<datetime_t> epoch;

#ifdef BOOST_DATE_TIME_HAS_HIGH_PRECISION_CLOCK
#define TRUE_CURRENT_TIME() (boost::posix_time::microsec_clock::local_time())
#define CURRENT_TIME()      (epoch ? *epoch : TRUE_CURRENT_TIME())
#else
#define TRUE_CURRENT_TIME() (boost::posix_time::second_clock::local_time())
#define CURRENT_TIME()      (epoch ? *epoch : TRUE_CURRENT_TIME())
#endif
#define CURRENT_DATE() \
  (epoch ? epoch->date() : boost::gregorian::day_clock::local_day())

extern date_time::weekdays start_of_week;

optional<date_time::weekdays>
string_to_day_of_week(const std::string& str);
optional<date_time::months_of_year>
string_to_month_of_year(const std::string& str);

datetime_t parse_datetime(const char * str);

inline datetime_t parse_datetime(const std::string& str) {
  return parse_datetime(str.c_str());
}

date_t parse_date(const char * str);

inline date_t parse_date(const std::string& str) {
  return parse_date(str.c_str());
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

inline void put_datetime(property_tree::ptree& pt, const datetime_t& when) {
  pt.put_value(format_datetime(when, FMT_WRITTEN));
}

inline void put_date(property_tree::ptree& pt, const date_t& when) {
  pt.put_value(format_date(when, FMT_WRITTEN));
}

struct date_traits_t
{
  bool has_year;
  bool has_month;
  bool has_day;

  date_traits_t(bool _has_year  = false,
                bool _has_month = false,
                bool _has_day   = false)
    : has_year(_has_year), has_month(_has_month), has_day(_has_day) {
    TRACE_CTOR(date_traits_t, "bool, bool, bool");
  }
  date_traits_t(const date_traits_t& traits)
    : has_year(traits.has_year),
      has_month(traits.has_month),
      has_day(traits.has_day) {
    TRACE_CTOR(date_traits_t, "copy");
  }
  ~date_traits_t() throw() {
    TRACE_DTOR(date_traits_t);
  }

  date_traits_t& operator=(const date_traits_t& traits) {
    has_year    = traits.has_year;
    has_month = traits.has_month;
    has_day     = traits.has_day;
    return *this;
  }

  bool operator==(const date_traits_t& traits) const {
    return (has_year    == traits.has_year &&
            has_month == traits.has_month &&
            has_day     == traits.has_day);
  }
};

struct date_duration_t
{
  enum skip_quantum_t {
    DAYS, WEEKS, MONTHS, QUARTERS, YEARS
  } quantum;
  int length;

  date_duration_t() : quantum(DAYS), length(0) {
    TRACE_CTOR(date_duration_t, "");
  }
  date_duration_t(skip_quantum_t _quantum, int _length)
    : quantum(_quantum), length(_length) {
    TRACE_CTOR(date_duration_t, "skip_quantum_t, int");
  }
  date_duration_t(const date_duration_t& dur)
    : quantum(dur.quantum), length(dur.length) {
    TRACE_CTOR(date_duration_t, "copy");
  }
  ~date_duration_t() throw() {
    TRACE_DTOR(date_duration_t);
  }

  date_t add(const date_t& date) const {
    switch (quantum) {
    case DAYS:
      return date + gregorian::days(length);
    case WEEKS:
      return date + gregorian::weeks(length);
    case MONTHS:
      return date + gregorian::months(length);
    case QUARTERS:
      return date + gregorian::months(length * 3);
    case YEARS:
      return date + gregorian::years(length);
    }
#if !defined(__clang__)
    return date_t();
#endif
  }

  date_t subtract(const date_t& date) const {
    switch (quantum) {
    case DAYS:
      return date - gregorian::days(length);
    case WEEKS:
      return date - gregorian::weeks(length);
    case MONTHS:
      return date - gregorian::months(length);
    case QUARTERS:
      return date - gregorian::months(length * 3);
    case YEARS:
      return date - gregorian::years(length);
    }
#if !defined(__clang__)
    return date_t();
#endif
  }

  string to_string() const {
    std::ostringstream out;

    out << length << ' ';

    switch (quantum) {
    case DAYS:     out << "day"; break;
    case WEEKS:    out << "week"; break;
    case MONTHS:   out << "month"; break;
    case QUARTERS: out << "quarter"; break;
    case YEARS:    out << "year"; break;
    }

    if (length > 1)
      out << 's';

    return out.str();
  }

  static date_t find_nearest(const date_t& date, skip_quantum_t skip);
};

class date_specifier_t
{
  friend class date_parser_t;

public:
#if 0
  typedef date_t::year_type        year_type;
#else
  typedef unsigned short           year_type;
#endif
  typedef date_t::month_type       month_type;
  typedef date_t::day_type         day_type;
  typedef date_t::day_of_week_type day_of_week_type;

protected:
  optional<year_type>        year;
  optional<month_type>       month;
  optional<day_type>         day;
  optional<day_of_week_type> wday;

public:
  date_specifier_t(const optional<year_type>&        _year  = none,
                   const optional<month_type>&       _month = none,
                   const optional<day_type>&         _day   = none,
                   const optional<day_of_week_type>& _wday  = none)
    : year(_year), month(_month), day(_day), wday(_wday) {
    TRACE_CTOR(date_specifier_t,
               "year_type, month_type, day_type, day_of_week_type");
  }
  date_specifier_t(const date_t& date,
                   const optional<date_traits_t>& traits = none) {
    if (! traits || traits->has_year)
      year = date.year();
    if (! traits || traits->has_month)
      month = date.month();
    if (! traits || traits->has_day)
      day = date.day();

    TRACE_CTOR(date_specifier_t, "date_t, date_traits_t");
  }
  date_specifier_t(const date_specifier_t& other)
    : year(other.year), month(other.month),
      day(other.day), wday(other.wday) {
    TRACE_CTOR(date_specifier_t, "copy");
  }
  ~date_specifier_t() throw() {
    TRACE_DTOR(date_specifier_t);
  }

  date_t begin() const;
  date_t end() const;

  bool is_within(const date_t& date) const {
    return date >= begin() && date < end();
  }

  optional<date_duration_t> implied_duration() const {
    if (day || wday)
      return date_duration_t(date_duration_t::DAYS, 1);
    else if (month)
      return date_duration_t(date_duration_t::MONTHS, 1);
    else if (year)
      return date_duration_t(date_duration_t::YEARS, 1);
    else
      return none;
  }

  string to_string() const {
    std::ostringstream out;

    if (year)
      out << " year " << *year;
    if (month)
      out << " month " << *month;
    if (day)
      out << " day " << *day;
    if (wday)
      out << " wday " << *wday;

    return out.str();
  }
};

class date_range_t
{
  friend class date_parser_t;

  optional<date_specifier_t> range_begin;
  optional<date_specifier_t> range_end;

  bool end_inclusive;

public:
  date_range_t(const optional<date_specifier_t>& _range_begin = none,
               const optional<date_specifier_t>& _range_end   = none)
    : range_begin(_range_begin), range_end(_range_end),
      end_inclusive(false) {
    TRACE_CTOR(date_range_t, "date_specifier_t, date_specifier_t");
  }
  date_range_t(const date_range_t& other)
    : range_begin(other.range_begin), range_end(other.range_end),
      end_inclusive(other.end_inclusive) {
    TRACE_CTOR(date_range_t, "date_range_t");
  }
  ~date_range_t() throw() {
    TRACE_DTOR(date_range_t);
  }

  optional<date_t> begin() const {
    if (range_begin)
      return range_begin->begin();
    else
      return none;
  }
  optional<date_t> end() const {
    if (range_end) {
      if (end_inclusive)
        return range_end->end();
      else
        return range_end->begin();
    } else {
      return none;
    }
  }

  bool is_within(const date_t& date) const {
    optional<date_t> b = begin();
    optional<date_t> e = end();
    bool after_begin = b ? date >= *b : true;
    bool before_end  = e ? date <  *e : true;
    return after_begin && before_end;
  }

  string to_string() const {
    std::ostringstream out;

    if (range_begin)
      out << "from" << range_begin->to_string();
    if (range_end)
      out << " to" << range_end->to_string();

    return out.str();
  }
};

class date_specifier_or_range_t
{
  typedef variant<int, date_specifier_t, date_range_t> value_type;

  value_type specifier_or_range;

public:
  date_specifier_or_range_t() {
    TRACE_CTOR(date_specifier_or_range_t, "");
  }
  date_specifier_or_range_t(const date_specifier_or_range_t& other)
    : specifier_or_range(other.specifier_or_range) {
    TRACE_CTOR(date_specifier_or_range_t, "copy");
  }
  date_specifier_or_range_t(const date_specifier_t& specifier)
    : specifier_or_range(specifier) {
    TRACE_CTOR(date_specifier_or_range_t, "date_specifier_t");
  }
  date_specifier_or_range_t(const date_range_t& range)
    : specifier_or_range(range) {
    TRACE_CTOR(date_specifier_or_range_t, "date_range_t");
  }
  ~date_specifier_or_range_t() throw() {
    TRACE_DTOR(date_specifier_or_range_t);
  }

  optional<date_t> begin() const {
    if (specifier_or_range.type() == typeid(date_specifier_t))
      return boost::get<date_specifier_t>(specifier_or_range).begin();
    else if (specifier_or_range.type() == typeid(date_range_t))
      return boost::get<date_range_t>(specifier_or_range).begin();
    else
      return none;
  }
  optional<date_t> end() const {
    if (specifier_or_range.type() == typeid(date_specifier_t))
      return boost::get<date_specifier_t>(specifier_or_range).end();
    else if (specifier_or_range.type() == typeid(date_range_t))
      return boost::get<date_range_t>(specifier_or_range).end();
    else
      return none;
  }


  string to_string() const {
    std::ostringstream out;

    if (specifier_or_range.type() == typeid(date_specifier_t))
      out << "in" << boost::get<date_specifier_t>(specifier_or_range).to_string();
    else if (specifier_or_range.type() == typeid(date_range_t))
      out << boost::get<date_range_t>(specifier_or_range).to_string();

    return out.str();
  }
};

class date_interval_t : public equality_comparable<date_interval_t>
{
public:
  static date_t add_duration(const date_t&          date,
                             const date_duration_t& duration);
  static date_t subtract_duration(const date_t&          date,
                                  const date_duration_t& duration);

  optional<date_specifier_or_range_t> range;

  optional<date_t>          start;  // the real start, after adjustment
  optional<date_t>          finish; // the real end, likewise
  bool                      aligned;
  optional<date_t>          next;
  optional<date_duration_t> duration;
  optional<date_t>          end_of_duration;

  explicit date_interval_t() : aligned(false) {
    TRACE_CTOR(date_interval_t, "");
  }
  date_interval_t(const string& str) : aligned(false) {
    parse(str);
    TRACE_CTOR(date_interval_t, "const string&");
  }
  date_interval_t(const date_interval_t& other)
    : range(other.range),
      start(other.start),
      finish(other.finish),
      aligned(other.aligned),
      next(other.next),
      duration(other.duration),
      end_of_duration(other.end_of_duration) {
    TRACE_CTOR(date_interval_t, "copy");
  }
  ~date_interval_t() throw() {
    TRACE_DTOR(date_interval_t);
  }

  bool operator==(const date_interval_t& other) const {
    return (start == other.start &&
            (! start || *start == *other.start));
  }
  bool operator<(const date_interval_t& other) const {
    return (start == other.start &&
            (! start || *start < *other.start));
  }

  operator bool() const {
    return is_valid();
  }

  optional<date_t> begin() const {
    return start ? start : (range ? range->begin() : none);
  }
  optional<date_t> end() const {
    return finish ? finish : (range ? range->end() : none);
  }

  void   parse(const string& str);

  void   resolve_end();
  void   stabilize(const optional<date_t>& date = none);

  bool   is_valid() const {
    return static_cast<bool>(start);
  }

  /** Find the current or next period containing date.  Returns false if
      no such period can be found.  If allow_shift is true, the default,
      then the interval may be shifted in time to find the period. */
  bool find_period(const date_t& date        = CURRENT_DATE(),
                   const bool    allow_shift = true);
  bool within_period(const date_t& date = CURRENT_DATE()) {
    return find_period(date, false);
  }

  optional<date_t> inclusive_end() const {
    if (end_of_duration)
      return *end_of_duration - gregorian::days(1);
    else
      return none;
  }

  date_interval_t& operator++();

  void dump(std::ostream& out);
};

void times_initialize();
void times_shutdown();

void show_period_tokens(std::ostream& out, const string& arg);

std::ostream& operator<<(std::ostream& out, const date_duration_t& duration);

} // namespace ledger

#endif // _TIMES_H
