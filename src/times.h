/*
 * Copyright (c) 2003-2025, John Wiegley.  All rights reserved.
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
 * @brief  Date/time parsing, formatting, and period expressions.
 *
 * This header defines the temporal foundation of Ledger.  Every
 * transaction carries a date, and many reports constrain or group
 * results by time.  The types declared here serve three roles:
 *
 *   1. **Parsing and formatting** -- parse_date(), parse_datetime(),
 *      format_date(), and format_datetime() convert between textual
 *      representations and Boost.Date_Time objects.  Multiple input
 *      formats are tried in order, and custom formats can be
 *      registered via set_input_date_format() (the `--input-date-format`
 *      option) or set_date_format() (the `--date-format` option).
 *
 *   2. **Date specifiers and ranges** -- date_specifier_t captures a
 *      possibly-partial date (year, month, day, or weekday, each
 *      optional), while date_range_t combines two specifiers into a
 *      half-open [begin, end) range.  These model the date portion of
 *      period expressions such as `in 2024`, `since jan`, or
 *      `from 2024/01 to 2024/06`.
 *
 *   3. **Period intervals** -- date_interval_t ties together a date
 *      range, a duration (step size), and the iteration state needed
 *      to walk through successive periods.  This is the runtime
 *      representation of `--period monthly`, `every 2 weeks`, and
 *      similar expressions.  The class supports stabilization (aligning
 *      the first period to a reference date), forward iteration via
 *      operator++, and membership testing via find_period().
 *
 * Collectively, these types power the `--begin`, `--end`, `--period`,
 * `--daily`, `--weekly`, `--monthly`, `--quarterly`, and `--yearly`
 * command-line options, as well as the `period` and `since`/`until`
 * clauses in query expressions.
 */
#pragma once

#include "utils.h"

namespace ledger {

/**
 * @brief Exception thrown when a datetime string cannot be parsed.
 */
class datetime_error : public std::runtime_error {
public:
  explicit datetime_error(const string& why) noexcept : std::runtime_error(why) {}
  ~datetime_error() noexcept override {}
};

/**
 * @brief Exception thrown when a date string cannot be parsed.
 */
class date_error : public std::runtime_error {
public:
  explicit date_error(const string& why) noexcept : std::runtime_error(why) {}
  ~date_error() noexcept override {}
};

using datetime_t = boost::posix_time::ptime;            ///< Timestamp with date and time-of-day.
using time_duration_t = datetime_t::time_duration_type; ///< Time-of-day duration component.

/// @brief Test whether a datetime value has been assigned (is not "not_a_date_time").
inline bool is_valid(const datetime_t& moment) {
  return !moment.is_not_a_date_time();
}

using date_t = boost::gregorian::date;                   ///< Calendar date without time-of-day.
using date_iterator_t = boost::gregorian::date_iterator; ///< Day-by-day date iterator.

/// @brief Test whether a date value has been assigned (is not "not_a_date").
inline bool is_valid(const date_t& moment) {
  return !moment.is_not_a_date();
}

/// If set (via --now), overrides the system clock for CURRENT_TIME/CURRENT_DATE.
/// This allows reproducible reports and testing with a fixed "now".
extern optional<datetime_t> epoch;

/// Tracks the year established by a `year` or `apply year` directive.
/// When set, date parsing uses this year instead of the current calendar
/// year for dates whose format lacks a year component (e.g., `01/15`).
extern optional<int> year_directive_year;

#ifdef BOOST_DATE_TIME_HAS_HIGH_PRECISION_CLOCK
#define TRUE_CURRENT_TIME()                                                                        \
  (boost::posix_time::microsec_clock::local_time()) ///< Wall-clock time (microsecond precision).
#define CURRENT_TIME()                                                                             \
  (epoch ? *epoch : TRUE_CURRENT_TIME()) ///< Effective "now", respecting --now override.
#else
#define TRUE_CURRENT_TIME()                                                                        \
  (boost::posix_time::second_clock::local_time()) ///< Wall-clock time (second precision).
#define CURRENT_TIME()                                                                             \
  (epoch ? *epoch : TRUE_CURRENT_TIME()) ///< Effective "now", respecting --now override.
#endif
#define CURRENT_DATE()                                                                             \
  (epoch ? epoch->date()                                                                           \
         : boost::gregorian::day_clock::local_day()) ///< Effective "today", respecting --now
                                                     ///< override.

/// The day that starts each week for period alignment (default: Sunday).
/// Configurable so that `weekly` reports can begin on Monday, etc.
extern date_time::weekdays start_of_week;

/// Number of days to shift the start of monthly/quarterly/yearly periods.
/// When non-zero, period boundaries are moved forward by this many days.
/// For example, `--period-shift 15` makes monthly periods run from the
/// 16th of one month to the 15th of the next.  Does not affect weekly or
/// daily periods.
extern int day_of_period;

/// @brief Convert a locale-aware weekday name or number ("mon", "monday", "1") to a weekdays enum.
/// @return The weekday, or none if the string is not recognized.
optional<date_time::weekdays> string_to_day_of_week(const std::string& str);

/// @brief Convert a locale-aware month name or number ("jan", "january", "0") to a months_of_year
/// enum.
/// @return The month, or none if the string is not recognized.
optional<date_time::months_of_year> string_to_month_of_year(const std::string& str);

/**
 * @brief Parse a datetime string in "YYYY/MM/DD HH:MM:SS" or "MM/DD/YYYY HH:MM:SS" format.
 *
 * Separators `.` and `-` are normalized to `/` before parsing.  The two
 * built-in formats are tried in order; if neither matches, a date_error
 * is thrown.
 *
 * @param str  Null-terminated datetime string (max 127 characters).
 * @return     The parsed datetime value.
 * @throws date_error if the string cannot be parsed.
 */
datetime_t parse_datetime(const char* str);

/// @brief Convenience overload accepting std::string.
inline datetime_t parse_datetime(const std::string& str) {
  return parse_datetime(str.c_str());
}

/**
 * @brief Parse a date string using the registered reader formats.
 *
 * The function tries each registered date format (including any custom
 * format from `--input-date-format`) in priority order.  Separators
 * `.` and `-` are normalized to `/`.  When the format lacks a year
 * component, the year is inferred from the `year` directive, the
 * epoch (--now), or the current date -- with month rollback logic to
 * avoid dates in the future.
 *
 * @param str  Null-terminated date string.
 * @return     The parsed date value.
 * @throws date_error if no registered format matches.
 */
date_t parse_date(const char* str);

/// @brief Convenience overload accepting std::string.
inline date_t parse_date(const std::string& str) {
  return parse_date(str.c_str());
}

/**
 * @brief Selects which format to use when rendering dates or datetimes.
 *
 * - FMT_WRITTEN:  The canonical machine-readable format ("2024/01/15"),
 *                 used for XML output and data serialization.
 * - FMT_PRINTED:  The human-friendly display format ("24-Jan-15"),
 *                 used in register/balance reports.
 * - FMT_CUSTOM:   A caller-supplied strftime format string, used by
 *                 the `format_date()` / `format_datetime()` expression
 *                 functions in report format strings.
 */
enum format_type_t : uint8_t { FMT_WRITTEN, FMT_PRINTED, FMT_CUSTOM };

/**
 * @brief Format a datetime value as a string.
 * @param when         The datetime to format.
 * @param format_type  Which format to use (default: FMT_PRINTED).
 * @param format       Custom strftime format string (only with FMT_CUSTOM).
 */
std::string format_datetime(const datetime_t& when, const format_type_t format_type = FMT_PRINTED,
                            const optional<const char*>& format = none);

/// @brief Override the default printed and written datetime format strings.
void set_datetime_format(const char* format);

/**
 * @brief Format a date value as a string.
 * @param when         The date to format.
 * @param format_type  Which format to use (default: FMT_PRINTED).
 * @param format       Custom strftime format string (only with FMT_CUSTOM).
 */
std::string format_date(const date_t& when, const format_type_t format_type = FMT_PRINTED,
                        const optional<const char*>& format = none);

/// @brief Override the default printed and written date format strings (--date-format).
void set_date_format(const char* format);

/// @brief Register a custom input date format that takes priority over built-in formats.
/// This implements the `--input-date-format` option.  When set, separator
/// normalization (`.`/`-` to `/`) is disabled so the custom format can use
/// its own separators.
void set_input_date_format(const char* format);

/// @brief Returns true if set_date_format() has been called.
bool date_format_is_set();

/// @brief Returns true if set_datetime_format() has been called.
bool datetime_format_is_set();

/// @brief Serialize a datetime into a property_tree node using the FMT_WRITTEN format.
inline void put_datetime(property_tree::ptree& pt, const datetime_t& when) {
  pt.put_value(format_datetime(when, FMT_WRITTEN));
}

/// @brief Serialize a date into a property_tree node using the FMT_WRITTEN format.
inline void put_date(property_tree::ptree& pt, const date_t& when) {
  pt.put_value(format_date(when, FMT_WRITTEN));
}

/**
 * @brief Records which date components a parsed format string can express.
 *
 * When a date is parsed from a format like "%m/%d" (no year), the
 * has_year flag is false, signalling the parser to infer the year from
 * context (year directive, epoch, or current date).  The traits travel
 * alongside parsed dates so that date_specifier_t knows which of its
 * optional fields were genuinely specified by the user.
 */
struct date_traits_t {
  bool has_year;  ///< Format contains %Y, %y, or %F (which expands to include a year).
  bool has_month; ///< Format contains %m, %b, or %F.
  bool has_day;   ///< Format contains %d or %F.

  date_traits_t(bool _has_year = false, bool _has_month = false, bool _has_day = false)
      : has_year(_has_year), has_month(_has_month), has_day(_has_day) {
    TRACE_CTOR(date_traits_t, "bool, bool, bool");
  }
  date_traits_t(const date_traits_t& traits)
      : has_year(traits.has_year), has_month(traits.has_month), has_day(traits.has_day) {
    TRACE_CTOR(date_traits_t, "copy");
  }
  ~date_traits_t() noexcept { TRACE_DTOR(date_traits_t); }

  date_traits_t& operator=(const date_traits_t& traits) {
    if (this == &traits)
      return *this;
    has_year = traits.has_year;
    has_month = traits.has_month;
    has_day = traits.has_day;
    return *this;
  }

  bool operator==(const date_traits_t& traits) const {
    return (has_year == traits.has_year && has_month == traits.has_month &&
            has_day == traits.has_day);
  }
};

/**
 * @brief A duration expressed as a count of calendar units.
 *
 * Models phrases like "2 weeks", "1 month", or "3 years".  The quantum
 * selects the calendar unit and length gives the multiplier.  These
 * durations are used both as the step size for periodic reports
 * (`--period "every 2 weeks"`) and as offsets in relative date
 * expressions (`"3 months ago"`).
 *
 * Note that QUARTERS is stored as a quantum but internally converted to
 * months * 3 for arithmetic, since Boost.Date_Time has no native
 * quarter type.
 */
struct date_duration_t {
  /// The calendar unit for this duration.
  enum skip_quantum_t : uint8_t {
    DAYS,     ///< Duration measured in days.
    WEEKS,    ///< Duration measured in 7-day weeks.
    MONTHS,   ///< Duration measured in calendar months.
    QUARTERS, ///< Duration measured in calendar quarters (3-month groups).
    YEARS     ///< Duration measured in calendar years.
  } quantum;

  int length; ///< Number of quanta (e.g. 2 for "every 2 weeks").

  date_duration_t() : quantum(DAYS), length(0) { TRACE_CTOR(date_duration_t, ""); }
  date_duration_t(skip_quantum_t _quantum, int _length) : quantum(_quantum), length(_length) {
    TRACE_CTOR(date_duration_t, "skip_quantum_t, int");
  }
  date_duration_t(const date_duration_t& dur) : quantum(dur.quantum), length(dur.length) {
    TRACE_CTOR(date_duration_t, "copy");
  }
  date_duration_t& operator=(const date_duration_t&) = default;
  ~date_duration_t() noexcept { TRACE_DTOR(date_duration_t); }

  /// @brief Advance a date forward by this duration.
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

  /// @brief Move a date backward by this duration.
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

  /// @brief Produce a human-readable representation, e.g. "2 weeks".
  string to_string() const {
    std::ostringstream out;

    out << length << ' ';

    switch (quantum) {
    case DAYS:
      out << "day";
      break;
    case WEEKS:
      out << "week";
      break;
    case MONTHS:
      out << "month";
      break;
    case QUARTERS:
      out << "quarter";
      break;
    case YEARS:
      out << "year";
      break;
    }

    if (length > 1)
      out << 's';

    return out.str();
  }

  /**
   * @brief Find the nearest period boundary on or before a date.
   *
   * For YEARS this returns January 1 of the same year; for QUARTERS
   * the first day of the containing quarter; for MONTHS the first of
   * the month; for WEEKS the most recent start_of_week day; and for
   * DAYS the date itself.  This is used during interval stabilization
   * to align period boundaries to natural calendar breaks.
   *
   * @param date  The reference date.
   * @param skip  The quantum whose boundary to find.
   */
  static date_t find_nearest(const date_t& date, skip_quantum_t skip);
};

/**
 * @brief A possibly-partial date specification used in period expressions.
 *
 * A date_specifier_t can hold any combination of year, month, day, and
 * weekday -- all optional.  This models the various levels of precision
 * users write in period expressions:
 *
 *   - `in 2024`      -> year only
 *   - `in jan`       -> month only (year inferred)
 *   - `2024/03/15`   -> year + month + day
 *   - `monday`       -> weekday only
 *
 * The begin() method fills in missing components with defaults (year =
 * current year, month = January, day = 1) to produce a concrete date.
 * The end() method returns one unit past the specified granularity,
 * making the specifier a half-open range: a year specifier spans the
 * whole year, a month specifier spans that month, etc.
 */
class date_specifier_t {
  friend class date_parser_t;

public:
#if 0
  typedef date_t::year_type        year_type;
#else
  using year_type = unsigned short;
#endif
  using month_type = date_t::month_type;
  using day_type = date_t::day_type;
  using day_of_week_type = date_t::day_of_week_type;

protected:
  optional<year_type> year;        ///< Calendar year (e.g. 2024), if specified.
  optional<month_type> month;      ///< Month of year, if specified.
  optional<day_type> day;          ///< Day of month, if specified.
  optional<day_of_week_type> wday; ///< Day of week, if specified.

public:
  date_specifier_t(const optional<year_type>& _year = none,
                   const optional<month_type>& _month = none, const optional<day_type>& _day = none,
                   const optional<day_of_week_type>& _wday = none)
      : year(_year), month(_month), day(_day), wday(_wday) {
    TRACE_CTOR(date_specifier_t, "year_type, month_type, day_type, day_of_week_type");
  }

  /**
   * @brief Construct a specifier from a concrete date, optionally
   *        filtering which components to keep based on format traits.
   *
   * When traits is provided, only the components present in the
   * original format string are stored.  This preserves the user's
   * intent: a date parsed from "%m/%d" will have has_year=false, so
   * the specifier carries no year and matches any year.
   */
  date_specifier_t(const date_t& date, const optional<date_traits_t>& traits = none) {
    if (!traits || traits->has_year)
      year = date.year();
    if (!traits || traits->has_month)
      month = date.month();
    if (!traits || traits->has_day)
      day = date.day();

    TRACE_CTOR(date_specifier_t, "date_t, date_traits_t");
  }
  date_specifier_t(const date_specifier_t& other)
      : year(other.year), month(other.month), day(other.day), wday(other.wday) {
    TRACE_CTOR(date_specifier_t, "copy");
  }
  date_specifier_t& operator=(const date_specifier_t&) = default;
  ~date_specifier_t() noexcept { TRACE_DTOR(date_specifier_t); }

  /// @brief Return the earliest date matching this specifier.
  /// Missing components default to the earliest valid value
  /// (year = current year, month = January, day = 1).
  date_t begin() const;

  /// @brief Return one unit past the specifier's granularity.
  /// For a day specifier: begin() + 1 day.  For month: + 1 month.
  /// For year: + 1 year.  This makes [begin(), end()) a half-open range.
  date_t end() const;

  /// @brief Returns true if this specifier includes an explicit year.
  bool has_year() const { return static_cast<bool>(year); }

  /// @brief Test whether a date falls within [begin(), end()).
  bool is_within(const date_t& date) const { return date >= begin() && date < end(); }

  /**
   * @brief Infer the natural duration for this specifier's granularity.
   *
   * A day specifier implies a 1-day duration; a month specifier
   * implies 1 month; a year specifier implies 1 year.  Returns none
   * if the specifier has no components at all.
   */
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

  /// @brief Produce a debug representation of the specifier's components.
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

/**
 * @brief A date range defined by begin and/or end specifiers.
 *
 * Models expressions like `from 2024/01 to 2024/06` or `since jan`.
 * Either endpoint may be absent, creating an open-ended range.
 *
 * The end_inclusive flag distinguishes dash-separated ranges
 * (`2024/01/01 - 2024/01/31`, which includes the end date) from
 * `since`/`until` ranges (where the end is exclusive by default).
 * When end_inclusive is true, end() returns the specifier's end()
 * (i.e. one day past the last specified day); when false, it returns
 * the specifier's begin() (treating the end specifier as "up to but
 * not including").
 */
class date_range_t {
  friend class date_parser_t;

  optional<date_specifier_t> range_begin; ///< Start of the range (inclusive).
  optional<date_specifier_t> range_end;   ///< End of the range.

  bool end_inclusive; ///< If true, range_end is included (dash syntax).

public:
  date_range_t(const optional<date_specifier_t>& _range_begin = none,
               const optional<date_specifier_t>& _range_end = none)
      : range_begin(_range_begin), range_end(_range_end), end_inclusive(false) {
    TRACE_CTOR(date_range_t, "date_specifier_t, date_specifier_t");
  }
  date_range_t(const date_range_t& other)
      : range_begin(other.range_begin), range_end(other.range_end),
        end_inclusive(other.end_inclusive) {
    TRACE_CTOR(date_range_t, "date_range_t");
  }
  date_range_t& operator=(const date_range_t&) = default;
  ~date_range_t() noexcept { TRACE_DTOR(date_range_t); }

  /// @brief Resolve the start of the range to a concrete date, or none if unbounded.
  optional<date_t> begin() const {
    if (range_begin)
      return range_begin->begin();
    else
      return none;
  }

  /// @brief Resolve the end of the range to a concrete date, or none if unbounded.
  /// When end_inclusive is true (dash syntax), returns the end specifier's
  /// end() so the final day is included.  When false (since/until syntax),
  /// returns the end specifier's begin() for exclusive behavior.
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

  /// @brief Returns true if the range's begin specifier has an explicit year.
  bool begin_has_year() const { return range_begin && range_begin->has_year(); }

  /// @brief Test whether a date falls within [begin(), end()).
  bool is_within(const date_t& date) const {
    optional<date_t> b = begin();
    optional<date_t> e = end();
    bool after_begin = b ? date >= *b : true;
    bool before_end = e ? date < *e : true;
    return after_begin && before_end;
  }

  /// @brief Produce a debug representation of the range.
  string to_string() const {
    std::ostringstream out;

    if (range_begin)
      out << "from" << range_begin->to_string();
    if (range_end)
      out << " to" << range_end->to_string();

    return out.str();
  }
};

/**
 * @brief A type-safe union of a single date specifier or a date range.
 *
 * The period expression parser produces either a point-in-time
 * specifier (e.g. `in 2024`) or a range (e.g. `from jan to mar`).
 * This variant wraps both so that date_interval_t can store the
 * parsed range information uniformly.
 */
class date_specifier_or_range_t {
  using value_type = std::variant<int, date_specifier_t, date_range_t>;

  value_type specifier_or_range; ///< The underlying specifier or range.

public:
  date_specifier_or_range_t() { TRACE_CTOR(date_specifier_or_range_t, ""); }
  date_specifier_or_range_t(const date_specifier_or_range_t& other)
      : specifier_or_range(other.specifier_or_range) {
    TRACE_CTOR(date_specifier_or_range_t, "copy");
  }
  date_specifier_or_range_t& operator=(const date_specifier_or_range_t&) = default;
  date_specifier_or_range_t(const date_specifier_t& specifier) : specifier_or_range(specifier) {
    TRACE_CTOR(date_specifier_or_range_t, "date_specifier_t");
  }
  date_specifier_or_range_t(const date_range_t& range) : specifier_or_range(range) {
    TRACE_CTOR(date_specifier_or_range_t, "date_range_t");
  }
  ~date_specifier_or_range_t() noexcept { TRACE_DTOR(date_specifier_or_range_t); }

  /// @brief The begin date, delegated to whichever alternative is active.
  optional<date_t> begin() const {
    if (const auto* s = std::get_if<date_specifier_t>(&specifier_or_range))
      return s->begin();
    if (const auto* r = std::get_if<date_range_t>(&specifier_or_range))
      return r->begin();
    return none;
  }

  /// @brief The end date, delegated to whichever alternative is active.
  optional<date_t> end() const {
    if (const auto* s = std::get_if<date_specifier_t>(&specifier_or_range))
      return s->end();
    if (const auto* r = std::get_if<date_range_t>(&specifier_or_range))
      return r->end();
    return none;
  }

  /// @brief Whether the begin specifier carries an explicit year.
  bool begin_has_year() const {
    if (const auto* s = std::get_if<date_specifier_t>(&specifier_or_range))
      return s->has_year();
    if (const auto* r = std::get_if<date_range_t>(&specifier_or_range))
      return r->begin_has_year();
    return false;
  }

  /// @brief Debug representation showing the active alternative.
  string to_string() const {
    std::ostringstream out;

    if (std::holds_alternative<date_specifier_t>(specifier_or_range))
      out << "in" << std::get<date_specifier_t>(specifier_or_range).to_string();
    else if (std::holds_alternative<date_range_t>(specifier_or_range))
      out << std::get<date_range_t>(specifier_or_range).to_string();

    return out.str();
  }
};

/**
 * @brief The primary period expression type: range + duration + iteration state.
 *
 * date_interval_t is the runtime representation of a fully-parsed period
 * expression.  It combines three concerns:
 *
 *   - **Range**: an optional date_specifier_or_range_t that constrains
 *     the overall time window (e.g. `from 2024/01 to 2024/12`).
 *
 *   - **Duration**: an optional date_duration_t that gives the step
 *     size for iteration (e.g. `monthly` = 1 month, `every 2 weeks`).
 *
 *   - **Iteration state**: the mutable fields start, finish, next, and
 *     end_of_duration that track where the interval currently sits as
 *     operator++ advances through successive periods.
 *
 * The lifecycle of a date_interval_t is:
 *
 *   1. **Parsing**: constructed from a string (`"monthly from 2024/01"`),
 *      which populates range and duration but leaves iteration state empty.
 *
 *   2. **Stabilization**: stabilize() is called with a reference date,
 *      aligning the first period to the appropriate calendar boundary
 *      (e.g. the start of the week or month containing the date).
 *
 *   3. **Iteration**: operator++ advances start to the next period,
 *      updating end_of_duration.  Iteration halts when start passes
 *      finish (the end of the constraining range).
 *
 * This type directly implements `--period`, `--begin`, `--end`,
 * `--daily`, `--weekly`, `--monthly`, `--quarterly`, and `--yearly`.
 */
class date_interval_t : public equality_comparable<date_interval_t> {
public:
  /// @brief Add a duration to a date (convenience wrapper around date_duration_t::add).
  static date_t add_duration(const date_t& date, const date_duration_t& duration);
  /// @brief Subtract a duration from a date (convenience wrapper around date_duration_t::subtract).
  static date_t subtract_duration(const date_t& date, const date_duration_t& duration);

  optional<date_specifier_or_range_t> range; ///< The parsed date range or specifier.

  optional<date_t> start;  ///< Effective start date, after stabilization and alignment.
  optional<date_t> finish; ///< Effective end date, after stabilization and alignment.
  bool aligned;            ///< True once stabilize() has aligned the interval to a reference date.
  optional<date_t> next;   ///< The start date of the next period (set by resolve_end).

  optional<date_duration_t> duration; ///< The step size for periodic iteration.
  optional<date_t> end_of_duration;   ///< One past the last date of the current period.
  bool since_specified = false;       ///< True if the user gave an explicit `since`/`from` date.

  explicit date_interval_t() : aligned(false) { TRACE_CTOR(date_interval_t, ""); }

  /// @brief Construct by parsing a period expression string.
  date_interval_t(const string& str) : aligned(false) {
    parse(str);
    TRACE_CTOR(date_interval_t, "const string&");
  }
  date_interval_t(const date_interval_t& other)
      : range(other.range), start(other.start), finish(other.finish), aligned(other.aligned),
        next(other.next), duration(other.duration), end_of_duration(other.end_of_duration),
        since_specified(other.since_specified) {
    TRACE_CTOR(date_interval_t, "copy");
  }
  date_interval_t& operator=(const date_interval_t&) = default;
  ~date_interval_t() noexcept { TRACE_DTOR(date_interval_t); }

  bool operator==(const date_interval_t& other) const {
    return (start == other.start && (!start || *start == *other.start));
  }
  bool operator<(const date_interval_t& other) const {
    return (start == other.start && (!start || *start < *other.start));
  }

  /// @brief True if the interval has been stabilized (i.e. has a start date).
  operator bool() const { return is_valid(); }

  /// @brief The effective begin date: start if set, otherwise the range's begin.
  optional<date_t> begin() const { return start ? start : (range ? range->begin() : none); }
  /// @brief The effective end date: finish if set, otherwise the range's end.
  optional<date_t> end() const { return finish ? finish : (range ? range->end() : none); }

  // Returns true if the begin date was specified with an explicit year in the
  // original expression (as opposed to having the year inferred from context).
  bool begin_has_year() const { return range && range->begin_has_year(); }

  /// @brief Parse a period expression string and populate this interval.
  void parse(const string& str);

  /**
   * @brief Compute end_of_duration and next from the current start and duration.
   *
   * Called after start is set to establish the end of the current period
   * and the beginning of the next.  If finish is set and the computed
   * end would exceed it, end_of_duration is clamped to finish.
   */
  void resolve_end();

  /**
   * @brief Align the interval to a reference date, initializing iteration state.
   *
   * Stabilization finds the period boundary that contains or precedes
   * the given date, sets start accordingly, and computes end_of_duration.
   * The align_intervals parameter controls whether explicit `since` dates
   * are used directly (true) or snapped to the nearest quantum boundary.
   *
   * If no duration is set, the range's begin/end are used directly.
   * This method is idempotent once `aligned` is true.
   *
   * @param date             Reference date to align to (default: today).
   * @param align_intervals  If true and since_specified, use the since date directly.
   */
  void stabilize(const optional<date_t>& date = none, bool align_intervals = false);

  /// @brief Returns true if start has been set.
  bool is_valid() const { return static_cast<bool>(start); }

  /** Find the current or next period containing date.  Returns false if
      no such period can be found.  If allow_shift is true, the default,
      then the interval may be shifted in time to find the period. */
  bool find_period(const date_t& date = CURRENT_DATE(), const bool align_intervals = false,
                   const bool allow_shift = true);

  /// @brief Shorthand for find_period with shifting disabled.
  /// Returns true only if date is within the current period boundaries.
  bool within_period(const date_t& date = CURRENT_DATE()) {
    return find_period(date, false, false);
  }

  /// @brief The last date included in the current period (end_of_duration - 1 day).
  /// Returns none if end_of_duration is not set.
  optional<date_t> inclusive_end() const {
    if (end_of_duration)
      return *end_of_duration - gregorian::days(1);
    else
      return none;
  }

  /**
   * @brief Advance to the next period.
   *
   * Moves start to next (the beginning of the following period),
   * recomputes end_of_duration, and clears next so resolve_end()
   * can set it again.  If next would pass finish, start is set to
   * none, signalling that iteration is complete.
   *
   * @throws date_error if the interval has no start or no duration.
   */
  date_interval_t& operator++();

  /// @brief Write a diagnostic dump of the interval's state to a stream.
  /// Shows the interval before and after stabilization, plus up to 20
  /// sample dates within the range.
  void dump(std::ostream& out);
};

/// @brief Initialize the date/time subsystem: register built-in date formats,
/// create the input/output IO objects, and set defaults.  Must be called
/// before any date parsing or formatting.
void times_initialize();

/// @brief Tear down the date/time subsystem: release all IO objects and
/// cached format strings.
void times_shutdown();

/// @brief Tokenize a period expression string and dump each token with its
/// type to the stream.  Used for debugging period expression parsing
/// (invoked by the `period` command with `--debug`).
void show_period_tokens(std::ostream& out, const string& arg);

/// @brief Stream insertion operator for date_duration_t.
std::ostream& operator<<(std::ostream& out, const date_duration_t& duration);

} // namespace ledger
