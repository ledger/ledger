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
typedef boost::gregorian::date_iterator date_iterator_t;

inline bool is_valid(const date_t& moment) {
  return ! moment.is_not_a_date();
}

extern optional<datetime_t> epoch;

#ifdef BOOST_DATE_TIME_HAS_HIGH_PRECISION_CLOCK
#define TRUE_CURRENT_TIME() (boost::posix_time::microsec_clock::universal_time())
#define CURRENT_TIME()      (epoch ? *epoch : TRUE_CURRENT_TIME())
#else
#define TRUE_CURRENT_TIME() (boost::posix_time::second_clock::universal_time())
#define CURRENT_TIME()      (epoch ? *epoch : TRUE_CURRENT_TIME())
#endif
#define CURRENT_DATE() \
  (epoch ? epoch->date() : boost::gregorian::day_clock::universal_day())

extern date_time::weekdays   start_of_week;

optional<date_time::weekdays>
string_to_day_of_week(const std::string& str);
optional<date_time::months_of_year>
string_to_month_of_year(const std::string& str);

typedef optional<date_t::year_type> optional_year;

datetime_t parse_datetime(const char * str, optional_year current_year = none);

inline datetime_t parse_datetime(const std::string& str,
				 optional_year current_year = none) {
  return parse_datetime(str.c_str(), current_year);
}

date_t parse_date(const char * str, optional_year current_year = none);

inline date_t parse_date(const std::string& str,
			 optional_year current_year = none) {
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

inline void to_xml(std::ostream& out, const datetime_t& when,
		   bool wrap = true)
{
  if (wrap) {
    push_xml x(out, "datetime");
    out << format_datetime(when, FMT_WRITTEN);
  } else {
    out << format_datetime(when, FMT_WRITTEN);
  }
}

inline void to_xml(std::ostream& out, const date_t& when,
		   bool wrap = true)
{
  if (wrap) {
    push_xml x(out, "date");
    out << format_date(when, FMT_WRITTEN);
  } else {
    out << format_date(when, FMT_WRITTEN);
  }
}

struct date_traits_t
{
  bool has_year;
  bool has_month;
  bool has_day;

  date_traits_t(bool _has_year  = false,
		bool _has_month = false,
		bool _has_day   = false)
    : has_year(_has_year), has_month(_has_month), has_day(_has_day) {}

  date_traits_t(const date_traits_t& traits)
    : has_year(traits.has_year),
      has_month(traits.has_month),
      has_day(traits.has_day) {}

  date_traits_t& operator=(const date_traits_t& traits) {
    has_year	= traits.has_year;
    has_month = traits.has_month;
    has_day	= traits.has_day;
    return *this;
  }

  bool operator==(const date_traits_t& traits) const {
    return (has_year	== traits.has_year &&
	    has_month == traits.has_month &&
	    has_day	== traits.has_day);
  }

#if defined(HAVE_BOOST_SERIALIZATION)
private:
  /** Serialization. */

  friend class boost::serialization::access;

  template<class Archive>
  void serialize(Archive& ar, const unsigned int /* version */) {
    ar & has_year;
    ar & has_month;
    ar & has_day;
  }
#endif // HAVE_BOOST_SERIALIZATION
};

class date_specifier_t
{
  friend class date_parser_t;

#if 0
  typedef date_t::year_type	   year_type;
#else
  typedef unsigned short	   year_type;
#endif
  typedef date_t::month_type	   month_type;
  typedef date_t::day_type	   day_type;
  typedef date_t::day_of_week_type day_of_week_type;

  optional<year_type>	     year;
  optional<month_type>	     month;
  optional<day_type>	     day;
  optional<day_of_week_type> wday;

public:
  date_specifier_t() {}
  date_specifier_t(const date_t& date, const date_traits_t& traits) {
    if (traits.has_year)
      year = date.year();
    if (traits.has_month)
      month = date.month();
    if (traits.has_day)
      day = date.day();
  }

  date_t begin(const optional_year& current_year = none) const;
  date_t end(const optional_year& current_year = none) const;

  bool is_within(const date_t& date,
		 const optional_year& current_year = none) const {
    return date >= begin(current_year) && date < end(current_year);
  }

#if defined(HAVE_BOOST_SERIALIZATION)
private:
  /** Serialization. */

  friend class boost::serialization::access;

  template<class Archive>
  void serialize(Archive& ar, const unsigned int /* version */) {
    ar & year;
    ar & month;
    ar & day;
    ar & wday;
  }
#endif // HAVE_BOOST_SERIALIZATION
};

class date_range_t
{
  friend class date_parser_t;

  optional<date_specifier_t> range_begin;
  optional<date_specifier_t> range_end;

public:
  optional<date_t> begin(const optional_year& current_year = none) const {
    if (range_begin)
      return range_begin->begin(current_year);
    else
      return none;
  }
  optional<date_t> end(const optional_year& current_year = none) const {
    if (range_end)
      return range_end->end(current_year);
    else
      return none;
  }

  bool is_within(const date_t& date,
		 const optional_year& current_year = none) const {
    optional<date_t> b = begin(current_year);
    optional<date_t> e = end(current_year);
    bool after_begin = b ? date >= *b : true;
    bool before_end  = e ? date <  *e : true;
    return after_begin && before_end;
  }

#if defined(HAVE_BOOST_SERIALIZATION)
private:
  /** Serialization. */

  friend class boost::serialization::access;

  template<class Archive>
  void serialize(Archive& ar, const unsigned int /* version */) {
    ar & range_begin;
    ar & range_end;
  }
#endif // HAVE_BOOST_SERIALIZATION
};

struct date_duration_t
{
  enum skip_quantum_t {
    DAYS, WEEKS, MONTHS, YEARS
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
    case YEARS:
      return date + gregorian::years(length);
    default:
      assert(false); return date_t();
    }
  }

  date_t subtract(const date_t& date) const {
    switch (quantum) {
    case DAYS:
      return date - gregorian::days(length);
    case WEEKS:
      return date - gregorian::weeks(length);
    case MONTHS:
      return date - gregorian::months(length);
    case YEARS:
      return date - gregorian::years(length);
    default:
      assert(false); return date_t();
    }
  }

#if defined(HAVE_BOOST_SERIALIZATION)
private:
  /** Serialization. */

  friend class boost::serialization::access;

  template<class Archive>
  void serialize(Archive& ar, const unsigned int /* version */) {
    ar & quantum;
    ar & length;
  }
#endif // HAVE_BOOST_SERIALIZATION
};

class date_specifier_or_range_t
{
  typedef variant<int, date_specifier_t, date_range_t> value_type;

  value_type specifier_or_range;

public:
  optional<date_t> begin(const optional_year& current_year = none) const {
    if (specifier_or_range.type() == typeid(date_specifier_t))
      return boost::get<date_specifier_t>(specifier_or_range).begin(current_year);
    else if (specifier_or_range.type() == typeid(date_range_t))
      return boost::get<date_range_t>(specifier_or_range).begin(current_year);
    else
      return none;
  }
  optional<date_t> end(const optional_year& current_year = none) const {
    if (specifier_or_range.type() == typeid(date_specifier_t))
      return boost::get<date_specifier_t>(specifier_or_range).end(current_year);
    else if (specifier_or_range.type() == typeid(date_range_t))
      return boost::get<date_range_t>(specifier_or_range).end(current_year);
    else
      return none;
  }

#if defined(HAVE_BOOST_SERIALIZATION)
private:
  /** Serialization. */

  friend class boost::serialization::access;

  template<class Archive>
  void serialize(Archive& ar, const unsigned int /* version */) {
    ar & specifier_or_range;
  }
#endif // HAVE_BOOST_SERIALIZATION
};

class date_interval_t : public equality_comparable<date_interval_t>
{
public:
  static date_t add_duration(const date_t&	    date,
			     const date_duration_t& duration);
  static date_t subtract_duration(const date_t&		 date,
				  const date_duration_t& duration);

  optional<date_specifier_or_range_t> range;

  optional<date_t>	    start;  // the real start, after adjustment
  optional<date_t>	    finish; // the real end, likewise
  bool			    aligned;
  optional<date_duration_t> skip_duration;
  std::size_t		    factor;
  optional<date_t>	    next;
  optional<date_duration_t> duration;
  optional<date_t>	    end_of_duration;

  explicit date_interval_t() : aligned(false), factor(1) {
    TRACE_CTOR(date_interval_t, "");
  }
  date_interval_t(const string& str) : aligned(false), factor(1) {
    TRACE_CTOR(date_interval_t, "const string&");
    parse(str);
  }
  date_interval_t(const date_interval_t& other)
    : range(other.range),
      start(other.start),
      finish(other.finish),
      aligned(other.aligned),
      skip_duration(other.skip_duration),
      factor(other.factor),
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

  operator bool() const {
    return is_valid();
  }

  optional<date_t> begin(const optional_year& current_year = none) const {
    return start ? start : (range ? range->begin(current_year) : none);
  }
  optional<date_t> end(const optional_year& current_year = none) const {
    return finish ? finish : (range ? range->end(current_year) : none);
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

#if defined(HAVE_BOOST_SERIALIZATION)
private:
  /** Serialization. */

  friend class boost::serialization::access;

  template<class Archive>
  void serialize(Archive& ar, const unsigned int /* version */) {
    ar & range;
    ar & start;
    ar & finish;
    ar & aligned;
    ar & skip_duration;
    ar & factor;
    ar & next;
    ar & duration;
    ar & end_of_duration;
  }
#endif // HAVE_BOOST_SERIALIZATION
};

class date_parser_t
{
  friend void show_period_tokens(std::ostream& out, const string& arg);
  
  class lexer_t
  {
    friend class date_parser_t;

    string::const_iterator begin;
    string::const_iterator end;

  public:
    struct token_t
    {
      enum kind_t {
	UNKNOWN,

	TOK_DATE,
	TOK_INT,
	TOK_SLASH,
	TOK_DASH,
	TOK_DOT,

	TOK_A_YEAR,
	TOK_A_MONTH,
	TOK_A_DAY,
	TOK_A_WDAY,

	TOK_SINCE,
	TOK_UNTIL,
	TOK_IN,
	TOK_THIS,
	TOK_NEXT,
	TOK_LAST,

	TOK_YEAR,
	TOK_QUARTER,
	TOK_MONTH,
	TOK_WEEK,
	TOK_DAY,

	TOK_YEARLY,
	TOK_QUARTERLY,
	TOK_BIMONTHLY,
	TOK_MONTHLY,
	TOK_BIWEEKLY,
	TOK_WEEKLY,
	TOK_DAILY,

	TOK_YEARS,
	TOK_QUARTERS,
	TOK_MONTHS,
	TOK_WEEKS,
	TOK_DAYS,

	END_REACHED

      } kind;

      typedef variant<string, int, date_specifier_t> content_t;

      optional<content_t> value;

      explicit token_t(kind_t _kind = UNKNOWN,
		       const optional<content_t>& _value = none)
	: kind(_kind), value(_value) {
	TRACE_CTOR(date_parser_t::lexer_t::token_t, "");
      }
      token_t(const token_t& tok)
	: kind(tok.kind), value(tok.value) {
	TRACE_CTOR(date_parser_t::lexer_t::token_t, "copy");
      }
      ~token_t() throw() {
	TRACE_DTOR(date_parser_t::lexer_t::token_t);
      }

      token_t& operator=(const token_t& tok) {
	if (this != &tok) {
	  kind  = tok.kind;
	  value = tok.value;
	}
	return *this;
      }

      operator bool() const {
	return kind != END_REACHED;
      }

      string to_string() const {
	switch (kind) {
	case UNKNOWN:	    return "UNKNOWN";
	case TOK_DATE:	    return "TOK_DATE";
	case TOK_INT:	    return "TOK_INT";
	case TOK_SLASH:	    return "TOK_SLASH";
	case TOK_DASH:	    return "TOK_DASH";
	case TOK_DOT:	    return "TOK_DOT";
	case TOK_A_YEAR:    return "TOK_A_YEAR";
	case TOK_A_MONTH:   return "TOK_A_MONTH";
	case TOK_A_DAY:	    return "TOK_A_DAY";
	case TOK_A_WDAY:    return "TOK_A_WDAY";
	case TOK_SINCE:	    return "TOK_SINCE";
	case TOK_UNTIL:	    return "TOK_UNTIL";
	case TOK_IN:	    return "TOK_IN";
	case TOK_THIS:	    return "TOK_THIS";
	case TOK_NEXT:	    return "TOK_NEXT";
	case TOK_LAST:	    return "TOK_LAST";
	case TOK_YEAR:	    return "TOK_YEAR";
	case TOK_QUARTER:   return "TOK_QUARTER";
	case TOK_MONTH:	    return "TOK_MONTH";
	case TOK_WEEK:	    return "TOK_WEEK";
	case TOK_DAY:	    return "TOK_DAY";
	case TOK_YEARLY:    return "TOK_YEARLY";
	case TOK_QUARTERLY: return "TOK_QUARTERLY";
	case TOK_BIMONTHLY: return "TOK_BIMONTHLY";
	case TOK_MONTHLY:   return "TOK_MONTHLY";
	case TOK_BIWEEKLY:  return "TOK_BIWEEKLY";
	case TOK_WEEKLY:    return "TOK_WEEKLY";
	case TOK_DAILY:	    return "TOK_DAILY";
	case TOK_YEARS:	    return "TOK_YEARS";
	case TOK_QUARTERS:  return "TOK_QUARTERS";
	case TOK_MONTHS:    return "TOK_MONTHS";
	case TOK_WEEKS:	    return "TOK_WEEKS";
	case TOK_DAYS:	    return "TOK_DAYS";
	case END_REACHED:   return "END_REACHED";
	}
	assert(false);
	return empty_string;
      }

      void unexpected();
      static void expected(char wanted, char c = '\0');
    };

    token_t token_cache;

    lexer_t(string::const_iterator _begin,
	    string::const_iterator _end)
      : begin(_begin), end(_end)
    {
      TRACE_CTOR(date_parser_t::lexer_t, "");
    }
    lexer_t(const lexer_t& lexer)
      : begin(lexer.begin), end(lexer.end),
	token_cache(lexer.token_cache)
    {
      TRACE_CTOR(date_parser_t::lexer_t, "copy");
    }
    ~lexer_t() throw() {
      TRACE_DTOR(date_parser_t::lexer_t);
    }

    token_t next_token();
    void    push_token(token_t tok) {
      assert(token_cache.kind == token_t::UNKNOWN);
      token_cache = tok;
    }
    token_t peek_token() {
      if (token_cache.kind == token_t::UNKNOWN)
	token_cache = next_token();
      return token_cache;
    }
  };

  string  arg;
  lexer_t lexer;

  date_interval_t parse_date_expr();

public:
  date_parser_t(const string& _arg)
    : arg(_arg), lexer(arg.begin(), arg.end()) {
    TRACE_CTOR(date_parser_t, "");
  }
  date_parser_t(const date_parser_t& parser)
    : arg(parser.arg), lexer(parser.lexer) {
    TRACE_CTOR(date_parser_t, "copy");
  }
  ~date_parser_t() throw() {
    TRACE_DTOR(date_parser_t);
  }

  date_interval_t parse() {
    return date_interval_t();
  }
};

void times_initialize();
void times_shutdown();

void show_period_tokens(std::ostream& out, const string& arg);
void analyze_period(std::ostream& out, const string& arg);

std::ostream& operator<<(std::ostream& out, const date_duration_t& duration);

} // namespace ledger

#endif // _TIMES_H
