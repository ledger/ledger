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
 * @file   times.cc
 * @author John Wiegley
 *
 * @ingroup util
 *
 * @brief  Implementation of date/time parsing, formatting, and period expressions.
 *
 * This file implements the temporal machinery declared in times.h.  The
 * code is organized into several logical sections:
 *
 *   - **Date I/O infrastructure**: the `temporal_io_t` template and its
 *     specializations handle strptime-based parsing and strftime-based
 *     formatting for both date_t and datetime_t.  A deque of readers
 *     implements the format-cascade: custom formats (--input-date-format)
 *     are prepended and tried first.
 *
 *   - **String-to-enum conversions**: translate locale-aware month and
 *     weekday names into Boost.Date_Time enums.
 *
 *   - **Date specifier and range resolution**: the begin()/end() methods
 *     of date_specifier_t turn partial specifications into concrete dates.
 *
 *   - **Period expression parser**: a hand-written recursive-descent
 *     parser (date_parser_t) with its own lexer that tokenizes period
 *     expression strings like "monthly from 2024/01 to 2024/12" into
 *     a date_interval_t.  The lexer recognizes dates, integers, month
 *     and weekday names, and keywords like `since`, `until`, `every`,
 *     `this`, `next`, `last`, `ago`, `hence`, etc.
 *
 *   - **Interval arithmetic**: stabilize(), find_period(), operator++(),
 *     and resolve_end() manage the iteration state that lets Ledger walk
 *     through successive periods for grouped reports.
 *
 *   - **Formatting**: format_date() and format_datetime() render dates
 *     using written (canonical), printed (display), or custom formats.
 *
 *   - **Initialization and shutdown**: times_initialize() registers the
 *     built-in date formats; times_shutdown() releases them.
 */

#include <cstddef>
#include <system.hh>

#include "times.h"

#if defined(_WIN32) || defined(__CYGWIN__)
#include "strptime.h"
#endif

namespace ledger {

/*--- Global State ---*/

optional<datetime_t> epoch;
optional<int> year_directive_year; // Track year from year directives separately

date_time::weekdays start_of_week = gregorian::Sunday;

/*--- Date I/O Infrastructure ---*/

/**
 * The temporal_io_t template wraps strptime/strftime for a single format
 * string, paired with date_traits_t that record which date components
 * the format can express.  Two specializations of parse() exist: one for
 * datetime_t and one for date_t, because date_t needs default-year logic
 * (using the year directive or current date when the format lacks %Y).
 *
 * Instances are kept in shared_ptrs and organized into:
 *   - input_datetime_io / timelog_datetime_io: the two built-in datetime readers
 *   - written_*_io / printed_*_io: output formatters for FMT_WRITTEN / FMT_PRINTED
 *   - readers: a deque of date_io_t tried in order by parse_date_mask()
 */
namespace {
template <typename T, typename InputFacetType, typename OutputFacetType>
class temporal_io_t : public noncopyable {
  string fmt_str; ///< The strptime/strftime format string.

public:
  date_traits_t traits; ///< Which date components this format can express.
  bool input;           ///< True if this is an input (parsing) format.

  temporal_io_t(const char* _fmt_str, bool _input)
      : fmt_str(_fmt_str),
        traits(icontains(fmt_str, "%F") || icontains(fmt_str, "%y"),
               icontains(fmt_str, "%F") || icontains(fmt_str, "%m") || icontains(fmt_str, "%b"),
               icontains(fmt_str, "%F") || icontains(fmt_str, "%d")),
        input(_input) {}

  /// @brief Replace the format string and recompute traits.
  void set_format(const char* fmt) {
    fmt_str = fmt;
    traits = date_traits_t(icontains(fmt_str, "%F") || icontains(fmt_str, "%y"),
                           icontains(fmt_str, "%F") || icontains(fmt_str, "%m") ||
                               icontains(fmt_str, "%b"),
                           icontains(fmt_str, "%F") || icontains(fmt_str, "%d"));
  }

  T parse(const char*) {}

  /// @brief Format a temporal value using strftime.
  std::string format(const T& when) {
    std::tm data(to_tm(when));
    char buf[128];
    std::strftime(buf, 127, fmt_str.c_str(), &data);
    return buf;
  }
};

/**
 * @brief datetime_t parsing specialization.
 *
 * Uses strptime to parse the string into a std::tm, then converts to
 * a Boost ptime.  Returns a not-a-date-time sentinel on failure.
 */
template <>
datetime_t temporal_io_t<datetime_t, posix_time::time_input_facet, posix_time::time_facet>::parse(
    const char* str) {
  std::tm data;
  std::memset(&data, 0, sizeof(std::tm));
  if (strptime(str, fmt_str.c_str(), &data))
    return posix_time::ptime_from_tm(data);
  else
    return datetime_t();
}

/**
 * @brief date_t parsing specialization.
 *
 * Before calling strptime, pre-fills tm_year with a sensible default:
 * the year directive year if active, otherwise the current year.  This
 * ensures formats without %Y (like "%m/%d") still produce a valid date.
 * Also pre-fills tm_mday=1 for formats without %d (like "%Y/%m").
 */
template <>
date_t
temporal_io_t<date_t, gregorian::date_input_facet, gregorian::date_facet>::parse(const char* str) {
  std::tm data;
  std::memset(&data, 0, sizeof(std::tm));
  // When year_directive_year is set (from "apply year" or "year" directive),
  // use that instead of CURRENT_DATE() to avoid issues with leap year dates
  if (year_directive_year) {
    data.tm_year = *year_directive_year - 1900;
  } else {
    data.tm_year = CURRENT_DATE().year() - 1900;
  }
  data.tm_mday = 1; // some formats have no day
  if (strptime(str, fmt_str.c_str(), &data))
    return gregorian::date_from_tm(data);
  else
    return date_t();
}

using datetime_io_t =
    temporal_io_t<datetime_t, posix_time::time_input_facet, posix_time::time_facet>;
using date_io_t = temporal_io_t<date_t, gregorian::date_input_facet, gregorian::date_facet>;

std::shared_ptr<datetime_io_t> input_datetime_io; ///< Reader for "YYYY/MM/DD HH:MM:SS".
std::shared_ptr<datetime_io_t>
    timelog_datetime_io; ///< Reader for "MM/DD/YYYY HH:MM:SS" (timeclock format).
std::shared_ptr<datetime_io_t> written_datetime_io; ///< Writer for FMT_WRITTEN datetimes.
std::shared_ptr<date_io_t> written_date_io;         ///< Writer for FMT_WRITTEN dates.
std::shared_ptr<datetime_io_t> printed_datetime_io; ///< Writer for FMT_PRINTED datetimes.
std::shared_ptr<date_io_t> printed_date_io;         ///< Writer for FMT_PRINTED dates.

/// The ordered list of date input formats.  Custom formats from
/// --input-date-format are pushed to the front; built-in formats
/// are appended during times_initialize().
std::deque<std::shared_ptr<date_io_t>> readers;

/// When true (the default), '.' and '-' in date strings are converted to '/'
/// before parsing.  Disabled when a custom --input-date-format is set, since
/// the custom format may use those characters literally.
bool convert_separators_to_slashes = true;

/*--- Low-Level Date Parsing ---*/

/**
 * @brief Try to parse a date string against a single format.
 *
 * This is the workhorse behind parse_date_mask().  It:
 *   1. Copies the date string into a buffer, normalizing separators.
 *   2. Parses via the given io object.
 *   3. Round-trip validates: formats the parsed date back to a string
 *      and verifies it matches the input (catching invalid dates like
 *      "02/30").
 *   4. When the format lacks a year component, infers the year from
 *      the year directive, epoch, or current date -- with month-rollback
 *      logic to avoid dates in the future.
 *
 * @param date_str  The date string to parse.
 * @param io        The format-specific IO object.
 * @param traits    If non-null, receives the format's date_traits_t.
 * @return          The parsed date, or not-a-date on failure.
 */
date_t parse_date_mask_routine(const char* date_str, date_io_t& io,
                               date_traits_t* traits = nullptr) {
  char buf[128];
  if (std::strlen(date_str) >= sizeof(buf)) {
    throw_(date_error, _f("Invalid date: %1%") % date_str);
  }

  std::strcpy(buf, date_str);

  // Normalize separators so users can write 2024.01.15 or 2024-01-15
  if (convert_separators_to_slashes) {
    for (char* p = buf; *p; p++)
      if (*p == '.' || *p == '-')
        *p = '/';
  }

  date_t when = io.parse(buf);

  if (!when.is_not_a_date()) {
    DEBUG("times.parse", "Passed date string:  " << date_str);
    DEBUG("times.parse", "Parsed date string:  " << buf);
    DEBUG("times.parse", "Parsed result is:    " << when);
    DEBUG("times.parse", "Formatted result is: " << io.format(when));

    // Round-trip validation: format the parsed date back and compare
    // character by character to catch invalid dates that strptime
    // silently accepted (e.g. month 13, day 32).  Leading zeros in
    // the formatted output are skipped during comparison.
    string when_str = io.format(when); // NOLINT(bugprone-unused-local-non-trivial-variable)

    const char* p = when_str.c_str();
    const char* q = buf;
    for (; *p && *q; p++, q++) {
      if (*p != *q && *p == '0')
        p++;
      if (!*p || *p != *q)
        break;
    }
    if (*p != '\0' || *q != '\0')
      throw_(date_error, _f("Invalid date: %1%") % date_str);

    if (traits)
      *traits = io.traits;

    // Year inference: when the format lacks a year component, we must
    // choose a year.  Priority: year directive > epoch (--now) > current date.
    // Month rollback prevents inferring a future date when the parsed
    // month is later than the reference month.
    if (!io.traits.has_year) {
      // First check if we have a year directive year to use
      if (year_directive_year) {
        // Use the year from the year directive
        when = date_t(*year_directive_year, when.month(), when.day());
        DEBUG("times.parse", "Using year directive year: " << *year_directive_year);
      } else if (epoch) {
        // When using the epoch (e.g., from --now), use it for the year
        date_t reference_date = epoch->date();
        when = date_t(reference_date.year(), when.month(), when.day());

        // Apply month rollback if the parsed month is after the epoch's month
        if (when.month() > reference_date.month())
          when -= gregorian::years(1);

        DEBUG("times.parse", "Using epoch year: " << reference_date.year()
                                                  << ", epoch month: " << reference_date.month()
                                                  << ", parsed month: " << when.month()
                                                  << ", final year: " << when.year());
      } else {
        // When no epoch, use current date and handle month rollback
        when = date_t(CURRENT_DATE().year(), when.month(), when.day());
        if (when.month() > CURRENT_DATE().month())
          when -= gregorian::years(1);
      }
    }
  }
  return when;
}

/**
 * @brief Try all registered readers in order until one parses the date.
 *
 * This implements the format cascade: custom formats (pushed to the front
 * by set_input_date_format) are tried first, followed by the built-in
 * formats registered during times_initialize().
 *
 * @param date_str  The date string to parse.
 * @param traits    If non-null, receives the successful format's traits.
 * @return          The parsed date.
 * @throws date_error if no format matches.
 */
date_t parse_date_mask(const char* date_str, date_traits_t* traits = nullptr) {
  for (std::shared_ptr<date_io_t>& reader : readers) {
    date_t when = parse_date_mask_routine(date_str, *reader.get(), traits);
    if (!when.is_not_a_date())
      return when;
  }

  throw_(date_error, _f("Invalid date: %1%") % date_str);
  return date_t();
}
} // namespace

/*--- String-to-Enum Conversions ---*/

/**
 * @brief Map a weekday name (locale-aware, case-insensitive) or digit to a Boost weekday.
 *
 * Accepts full names ("monday"), abbreviations ("mon"), or digit strings
 * ("0" for Sunday through "6" for Saturday).
 */
optional<date_time::weekdays> string_to_day_of_week(const std::string& str) {
  if (str == _("sun") || str == _("sunday") || str == "0")
    return gregorian::Sunday;
  else if (str == _("mon") || str == _("monday") || str == "1")
    return gregorian::Monday;
  else if (str == _("tue") || str == _("tuesday") || str == "2")
    return gregorian::Tuesday;
  else if (str == _("wed") || str == _("wednesday") || str == "3")
    return gregorian::Wednesday;
  else if (str == _("thu") || str == _("thursday") || str == "4")
    return gregorian::Thursday;
  else if (str == _("fri") || str == _("friday") || str == "5")
    return gregorian::Friday;
  else if (str == _("sat") || str == _("saturday") || str == "6")
    return gregorian::Saturday;
  else
    return none;
}

/**
 * @brief Map a month name (locale-aware, case-insensitive) or digit to a Boost month.
 *
 * Accepts full names ("january"), abbreviations ("jan"), or digit
 * strings ("0" for January through "11" for December).
 */
optional<date_time::months_of_year> string_to_month_of_year(const std::string& str) {
  if (str == _("jan") || str == _("january") || str == "0")
    return gregorian::Jan;
  else if (str == _("feb") || str == _("february") || str == "1")
    return gregorian::Feb;
  else if (str == _("mar") || str == _("march") || str == "2")
    return gregorian::Mar;
  else if (str == _("apr") || str == _("april") || str == "3")
    return gregorian::Apr;
  else if (str == _("may") || str == _("may") || str == "4")
    return gregorian::May;
  else if (str == _("jun") || str == _("june") || str == "5")
    return gregorian::Jun;
  else if (str == _("jul") || str == _("july") || str == "6")
    return gregorian::Jul;
  else if (str == _("aug") || str == _("august") || str == "7")
    return gregorian::Aug;
  else if (str == _("sep") || str == _("september") || str == "8")
    return gregorian::Sep;
  else if (str == _("oct") || str == _("october") || str == "9")
    return gregorian::Oct;
  else if (str == _("nov") || str == _("november") || str == "10")
    return gregorian::Nov;
  else if (str == _("dec") || str == _("december") || str == "11")
    return gregorian::Dec;
  else
    return none;
}

/*--- Top-Level Parse Functions ---*/

/**
 * @brief Parse a datetime string using the two built-in datetime formats.
 *
 * Tries "YYYY/MM/DD HH:MM:SS" first, then "MM/DD/YYYY HH:MM:SS"
 * (the timeclock format).  Separators are normalized before parsing.
 */
datetime_t parse_datetime(const char* str) {
  if (std::strlen(str) > 127) {
    throw_(date_error, _f("Invalid date: %1%") % str);
  }

  char buf[128];
  std::strcpy(buf, str);

  for (char* p = buf; *p; p++)
    if (*p == '.' || *p == '-')
      *p = '/';

  datetime_t when = input_datetime_io->parse(buf);
  if (when.is_not_a_date_time()) {
    when = timelog_datetime_io->parse(buf);
    if (when.is_not_a_date_time()) {
      throw_(date_error, _f("Invalid date/time: %1%") % str);
    }
  }
  return when;
}

/// @brief Parse a date string using the registered format cascade.
date_t parse_date(const char* str) {
  return parse_date_mask(str);
}

/*--- Date Specifier Resolution ---*/

/**
 * @brief Resolve a possibly-partial specifier to its earliest concrete date.
 *
 * Fills in missing components with the earliest valid value:
 *   - year defaults to the current year
 *   - month defaults to January
 *   - day defaults to the 1st
 *
 * Day and weekday are mutually exclusive (asserted).
 */
date_t date_specifier_t::begin() const {
  year_type the_year = year ? *year : year_type(CURRENT_DATE().year());
  month_type the_month = month ? *month : date_t::month_type(1);
  day_type the_day = day ? *day : date_t::day_type(1);

  if (day)
    assert(!wday);
  else if (wday)
    assert(!day);

  // jww (2009-11-16): Handle wday.  If a month is set, find the most recent
  // wday in that month; if the year is set, then in that year.

  return gregorian::date(static_cast<date_t::year_type>(the_year),
                         static_cast<date_t::month_type>(the_month),
                         static_cast<date_t::day_type>(the_day));
}

/**
 * @brief Compute the exclusive end date implied by this specifier's granularity.
 *
 * Returns begin() + 1 day (for day/weekday specifiers), + 1 month
 * (for month specifiers), or + 1 year (for year-only specifiers).
 * This makes [begin(), end()) a half-open range spanning exactly the
 * specified calendar unit.
 */
date_t date_specifier_t::end() const {
  if (day || wday)
    return begin() + gregorian::days(1);
  else if (month)
    return begin() + gregorian::months(1); // NOLINT(bugprone-branch-clone)
  else if (year)
    return begin() + gregorian::years(1);
  else {
    assert(false);
    return date_t();
  }
}

/*--- Duration Output ---*/

/// @brief Stream insertion operator for date_duration_t.
std::ostream& operator<<(std::ostream& out, const date_duration_t& duration) {
  if (duration.quantum == date_duration_t::DAYS)
    out << duration.length << " day(s)";
  else if (duration.quantum == date_duration_t::WEEKS)
    out << duration.length << " week(s)";
  else if (duration.quantum == date_duration_t::MONTHS)
    out << duration.length << " month(s)";
  else if (duration.quantum == date_duration_t::QUARTERS)
    out << duration.length << " quarter(s)";
  else {
    assert(duration.quantum == date_duration_t::YEARS);
    out << duration.length << " year(s)";
  }
  return out;
}

/*--- Period Expression Parser ---*/

/**
 * @brief Recursive-descent parser for period expressions.
 *
 * Parses strings like:
 *   - "monthly"
 *   - "every 2 weeks"
 *   - "from 2024/01 to 2024/06"
 *   - "monthly since jan until jun"
 *   - "this month"
 *   - "last 3 months"
 *   - "2024/01/01 - 2024/12/31"
 *
 * The grammar is token-driven: the lexer breaks the input into tokens
 * (dates, integers, keywords, month/weekday names), and the parser
 * dispatches on token type.  The result is a date_interval_t with its
 * range and duration fields populated.
 *
 * The parser collects three intermediate specifiers:
 *   - since_specifier: from the `since`/`from` keyword or left side of `-`
 *   - until_specifier: from the `until`/`to` keyword or right side of `-`
 *   - inclusion_specifier: from `in`, bare dates, or relative expressions
 *
 * These are finalized into the interval's range at the end of parsing.
 */
class date_parser_t {
  friend void show_period_tokens(std::ostream& out, const string& arg);

  /**
   * @brief Lexer for period expression tokens.
   *
   * Scans the input string character-by-character, producing tokens for:
   *   - Full dates (tried via parse_date_mask when the text starts with a digit)
   *   - Integers (bare numbers that did not parse as dates)
   *   - Punctuation: slash, dash, dot
   *   - Month names (jan, february, etc.)
   *   - Weekday names (mon, tuesday, etc.)
   *   - Keywords: ago, hence, since, until, in, this, next, last, every,
   *     today, tomorrow, yesterday
   *   - Period units: year(s), quarter(s), month(s), week(s), day(s)
   *   - Frequency shortcuts: yearly, quarterly, bimonthly, monthly,
   *     biweekly, weekly, daily
   *
   * A single-token lookahead (token_cache) supports peek_token() and
   * push_token() for the parser's benefit.
   */
  class lexer_t {
    friend class date_parser_t;

    string::const_iterator begin; ///< Current scan position.
    string::const_iterator end;   ///< End of the input string.

  public:
    /// @brief A single token produced by the lexer.
    struct token_t {
      /// @brief Enumerates all recognized token types.
      enum kind_t : uint8_t {
        UNKNOWN,

        TOK_DATE,  ///< A full date parsed from the input (e.g. "2024/01/15").
        TOK_INT,   ///< A bare integer (e.g. "3" in "every 3 months").
        TOK_SLASH, ///< The '/' character.
        TOK_DASH,  ///< The '-' character (range separator).
        TOK_DOT,   ///< The '.' character.

        TOK_A_MONTH, ///< A month name (e.g. "jan", "february").
        TOK_A_WDAY,  ///< A weekday name (e.g. "mon", "tuesday").

        TOK_AGO,   ///< The keyword "ago" (relative past).
        TOK_HENCE, ///< The keyword "hence" (relative future).
        TOK_SINCE, ///< The keyword "since" or "from" (range start).
        TOK_UNTIL, ///< The keyword "until" or "to" (range end).
        TOK_IN,    ///< The keyword "in" (inclusion specifier).
        TOK_THIS,  ///< The keyword "this" (current period).
        TOK_NEXT,  ///< The keyword "next" (following period).
        TOK_LAST,  ///< The keyword "last" (preceding period).
        TOK_EVERY, ///< The keyword "every" (recurrence prefix).

        TOK_TODAY,     ///< The keyword "today".
        TOK_TOMORROW,  ///< The keyword "tomorrow".
        TOK_YESTERDAY, ///< The keyword "yesterday" or "yday".

        TOK_YEAR,    ///< The singular unit "year".
        TOK_QUARTER, ///< The singular unit "quarter".
        TOK_MONTH,   ///< The singular unit "month".
        TOK_WEEK,    ///< The singular unit "week".
        TOK_DAY,     ///< The singular unit "day".

        TOK_YEARLY,    ///< The frequency shortcut "yearly".
        TOK_QUARTERLY, ///< The frequency shortcut "quarterly".
        TOK_BIMONTHLY, ///< The frequency shortcut "bimonthly".
        TOK_MONTHLY,   ///< The frequency shortcut "monthly".
        TOK_BIWEEKLY,  ///< The frequency shortcut "biweekly".
        TOK_WEEKLY,    ///< The frequency shortcut "weekly".
        TOK_DAILY,     ///< The frequency shortcut "daily".

        TOK_YEARS,    ///< The plural unit "years".
        TOK_QUARTERS, ///< The plural unit "quarters".
        TOK_MONTHS,   ///< The plural unit "months".
        TOK_WEEKS,    ///< The plural unit "weeks".
        TOK_DAYS,     ///< The plural unit "days".

        END_REACHED ///< Sentinel: no more input.

      } kind;

      using content_t = std::variant<unsigned short, string, date_time::months_of_year,
                                     date_time::weekdays, date_specifier_t>;

      optional<content_t> value; ///< The token's payload (type depends on kind).

      explicit token_t(kind_t _kind = UNKNOWN,
                       const optional<content_t>& _value = content_t(empty_string))
          : kind(_kind), value(_value) {
        TRACE_CTOR(date_parser_t::lexer_t::token_t, "");
      }
      token_t(const token_t& tok) : kind(tok.kind), value(tok.value) {
        TRACE_CTOR(date_parser_t::lexer_t::token_t, "copy");
      }
      ~token_t() noexcept { TRACE_DTOR(date_parser_t::lexer_t::token_t); }

      token_t& operator=(const token_t& tok) {
        if (this != &tok) {
          kind = tok.kind;
          value = tok.value;
        }
        return *this;
      }

      /// @brief True if the token is not END_REACHED.
      operator bool() const { return kind != END_REACHED; }

      /// @brief Render the token as a human-readable string for diagnostics.
      string to_string() const {
        std::ostringstream out;

        switch (kind) {
        case UNKNOWN:
          out << std::get<string>(*value);
          break;
        case TOK_DATE:
          return std::get<date_specifier_t>(*value).to_string();
        case TOK_INT:
          out << std::get<unsigned short>(*value);
          break;
        case TOK_SLASH:
          return "/";
        case TOK_DASH:
          return "-";
        case TOK_DOT:
          return ".";
        case TOK_A_MONTH: // NOLINT(bugprone-branch-clone)
          out << date_specifier_t::month_type(std::get<date_time::months_of_year>(*value));
          break;
        case TOK_A_WDAY:
          out << date_specifier_t::day_of_week_type(std::get<date_time::weekdays>(*value));
          break;
        case TOK_AGO:
          return "ago";
        case TOK_HENCE:
          return "hence";
        case TOK_SINCE:
          return "since";
        case TOK_UNTIL:
          return "until";
        case TOK_IN:
          return "in";
        case TOK_THIS:
          return "this";
        case TOK_NEXT:
          return "next";
        case TOK_LAST:
          return "last";
        case TOK_EVERY:
          return "every";
        case TOK_TODAY:
          return "today";
        case TOK_TOMORROW:
          return "tomorrow";
        case TOK_YESTERDAY:
          return "yesterday";
        case TOK_YEAR:
          return "year";
        case TOK_QUARTER:
          return "quarter";
        case TOK_MONTH:
          return "month";
        case TOK_WEEK:
          return "week";
        case TOK_DAY:
          return "day";
        case TOK_YEARLY:
          return "yearly";
        case TOK_QUARTERLY:
          return "quarterly";
        case TOK_BIMONTHLY:
          return "bimonthly";
        case TOK_MONTHLY:
          return "monthly";
        case TOK_BIWEEKLY:
          return "biweekly";
        case TOK_WEEKLY:
          return "weekly";
        case TOK_DAILY:
          return "daily";
        case TOK_YEARS:
          return "years";
        case TOK_QUARTERS:
          return "quarters";
        case TOK_MONTHS:
          return "months";
        case TOK_WEEKS:
          return "weeks";
        case TOK_DAYS:
          return "days";
        case END_REACHED:
          return "<EOF>";
        }

        return out.str();
      }

      /// @brief Write the token's kind_t name for debug output.
      void dump(std::ostream& out) const {
        switch (kind) {
        case UNKNOWN:
          out << "UNKNOWN";
          break;
        case TOK_DATE:
          out << "TOK_DATE";
          break;
        case TOK_INT:
          out << "TOK_INT";
          break;
        case TOK_SLASH:
          out << "TOK_SLASH";
          break;
        case TOK_DASH:
          out << "TOK_DASH";
          break;
        case TOK_DOT:
          out << "TOK_DOT";
          break;
        case TOK_A_MONTH:
          out << "TOK_A_MONTH";
          break;
        case TOK_A_WDAY:
          out << "TOK_A_WDAY";
          break;
        case TOK_AGO:
          out << "TOK_AGO";
          break;
        case TOK_HENCE:
          out << "TOK_HENCE";
          break;
        case TOK_SINCE:
          out << "TOK_SINCE";
          break;
        case TOK_UNTIL:
          out << "TOK_UNTIL";
          break;
        case TOK_IN:
          out << "TOK_IN";
          break;
        case TOK_THIS:
          out << "TOK_THIS";
          break;
        case TOK_NEXT:
          out << "TOK_NEXT";
          break;
        case TOK_LAST:
          out << "TOK_LAST";
          break;
        case TOK_EVERY:
          out << "TOK_EVERY";
          break;
        case TOK_TODAY:
          out << "TOK_TODAY";
          break;
        case TOK_TOMORROW:
          out << "TOK_TOMORROW";
          break;
        case TOK_YESTERDAY:
          out << "TOK_YESTERDAY";
          break;
        case TOK_YEAR:
          out << "TOK_YEAR";
          break;
        case TOK_QUARTER:
          out << "TOK_QUARTER";
          break;
        case TOK_MONTH:
          out << "TOK_MONTH";
          break;
        case TOK_WEEK:
          out << "TOK_WEEK";
          break;
        case TOK_DAY:
          out << "TOK_DAY";
          break;
        case TOK_YEARLY:
          out << "TOK_YEARLY";
          break;
        case TOK_QUARTERLY:
          out << "TOK_QUARTERLY";
          break;
        case TOK_BIMONTHLY:
          out << "TOK_BIMONTHLY";
          break;
        case TOK_MONTHLY:
          out << "TOK_MONTHLY";
          break;
        case TOK_BIWEEKLY:
          out << "TOK_BIWEEKLY";
          break;
        case TOK_WEEKLY:
          out << "TOK_WEEKLY";
          break;
        case TOK_DAILY:
          out << "TOK_DAILY";
          break;
        case TOK_YEARS:
          out << "TOK_YEARS";
          break;
        case TOK_QUARTERS:
          out << "TOK_QUARTERS";
          break;
        case TOK_MONTHS:
          out << "TOK_MONTHS";
          break;
        case TOK_WEEKS:
          out << "TOK_WEEKS";
          break;
        case TOK_DAYS:
          out << "TOK_DAYS";
          break;
        case END_REACHED:
          out << "END_REACHED";
          break;
        }
      }

      /// @brief Throw a date_error for an unexpected token.
      void unexpected();
      /// @brief Throw a date_error for an unexpected character.
      static void expected(char wanted, char c = '\0');
    };

    token_t token_cache; ///< One-token lookahead buffer.

    lexer_t(string::const_iterator _begin, string::const_iterator _end) : begin(_begin), end(_end) {
      TRACE_CTOR(date_parser_t::lexer_t, "");
    }
    lexer_t(const lexer_t& other)
        : begin(other.begin), end(other.end), token_cache(other.token_cache) {
      TRACE_CTOR(date_parser_t::lexer_t, "copy");
    }
    ~lexer_t() noexcept { TRACE_DTOR(date_parser_t::lexer_t); }

    /// @brief Consume and return the next token from the input.
    token_t next_token();

    /// @brief Push a token back into the lookahead buffer.
    /// Only one token can be pushed back at a time.
    void push_token(const token_t& tok) {
      assert(token_cache.kind == token_t::UNKNOWN);
      token_cache = tok;
    }

    /// @brief Return the next token without consuming it.
    token_t peek_token() {
      if (token_cache.kind == token_t::UNKNOWN)
        token_cache = next_token();
      return token_cache;
    }
  };

  string arg;    ///< The original period expression string.
  lexer_t lexer; ///< The lexer scanning over arg.

public:
  date_parser_t(const string& _arg) : arg(_arg), lexer(arg.begin(), arg.end()) {
    TRACE_CTOR(date_parser_t, "");
  }
  date_parser_t(const date_parser_t& parser) : arg(parser.arg), lexer(parser.lexer) {
    TRACE_CTOR(date_parser_t, "copy");
  }
  ~date_parser_t() noexcept { TRACE_DTOR(date_parser_t); }

  /// @brief Parse the expression and return a populated date_interval_t.
  date_interval_t parse();

private:
  /// @name Parser helper methods
  /// Each handles a specific class of token encountered in the main loop.
  /// @{

  /// @brief Handle a date, integer, month, or weekday token as an inclusion specifier.
  void handle_date_token(lexer_t::token_t& tok, optional<date_specifier_t>& inclusion_specifier);

  /// @brief Handle the dash operator: converts an inclusion specifier into a
  /// since/until range with inclusive end semantics.
  void handle_dash_operator(lexer_t::token_t& tok, optional<date_specifier_t>& inclusion_specifier,
                            optional<date_specifier_t>& since_specifier,
                            optional<date_specifier_t>& until_specifier, bool& end_inclusive);

  /// @brief Handle the "since"/"from" keyword: parse the following date as the range start.
  void handle_since_token(lexer_t::token_t& tok, optional<date_specifier_t>& since_specifier);

  /// @brief Handle the "until"/"to" keyword: parse the following date as the range end.
  void handle_until_token(lexer_t::token_t& tok, optional<date_specifier_t>& until_specifier);

  /// @brief Handle the "in" keyword: parse the following date as an inclusion specifier.
  void handle_in_token(lexer_t::token_t& tok, optional<date_specifier_t>& inclusion_specifier);

  /// @brief Handle "this"/"next"/"last" followed by a unit or quantity.
  /// Produces since/until specifiers (for quarters, weeks) or an
  /// inclusion specifier (for months, years, days, named months/weekdays).
  void handle_relative_token(lexer_t::token_t& tok, optional<date_specifier_t>& since_specifier,
                             optional<date_specifier_t>& until_specifier,
                             optional<date_specifier_t>& inclusion_specifier, const date_t& today);

  /// @brief Handle "today", "tomorrow", "yesterday" as inclusion specifiers.
  void handle_simple_period_token(lexer_t::token_t::kind_t kind,
                                  optional<date_specifier_t>& inclusion_specifier,
                                  const date_t& today);

  /// @brief Handle the "every" keyword followed by a quantity and unit.
  /// Sets the interval's duration (e.g. "every 2 weeks" -> WEEKS/2).
  void handle_every_token(lexer_t::token_t& tok, optional<date_duration_t>& duration);

  /// @brief Handle frequency shortcut tokens (yearly, monthly, weekly, etc.).
  void handle_period_token(lexer_t::token_t::kind_t kind, optional<date_duration_t>& duration);

  /// @brief Assemble the final date_interval_t from the collected specifiers.
  /// Builds a date_range_t from since/until, or stores the inclusion
  /// specifier directly.
  void finalize_period(date_interval_t& period, const optional<date_specifier_t>& since_specifier,
                       const optional<date_specifier_t>& until_specifier,
                       const optional<date_specifier_t>& inclusion_specifier, bool end_inclusive);
  /// @}

private:
  /**
   * @brief Resolve a token into a concrete date_specifier_t.
   *
   * This is the core date-resolution method.  Depending on the token:
   *   - TOK_DATE: extracts the already-parsed specifier
   *   - TOK_INT: interprets as a relative offset ("3 months ago") or a
   *     bare year/day number
   *   - TOK_THIS/NEXT/LAST: computes a relative period
   *   - TOK_A_MONTH/TOK_A_WDAY: constructs from the named month/weekday
   *   - TOK_TODAY/TOMORROW/YESTERDAY: resolves to today +/- days
   */
  void determine_when(lexer_t::token_t& tok, date_specifier_t& specifier);
};

/*--- determine_when: Token-to-Specifier Resolution ---*/

void date_parser_t::determine_when(date_parser_t::lexer_t::token_t& tok,
                                   date_specifier_t& specifier) {
  date_t today = epoch ? epoch->date() : CURRENT_DATE();

  switch (tok.kind) {
  case lexer_t::token_t::TOK_DATE:
    specifier = std::get<date_specifier_t>(*tok.value);
    break;

  case lexer_t::token_t::TOK_INT: {
    // An integer can be:
    //   - A relative offset: "3 months ago", "2 years hence"
    //   - A bare year number (> 31): "2024"
    //   - A bare day number (<= 31): "15"
    unsigned short amount = std::get<unsigned short>(*tok.value);
    int8_t adjust = 0;

    tok = lexer.peek_token();
    lexer_t::token_t::kind_t kind = tok.kind;
    switch (kind) {
    case lexer_t::token_t::TOK_YEAR:
    case lexer_t::token_t::TOK_YEARS:
    case lexer_t::token_t::TOK_QUARTER:
    case lexer_t::token_t::TOK_QUARTERS:
    case lexer_t::token_t::TOK_MONTH:
    case lexer_t::token_t::TOK_MONTHS:
    case lexer_t::token_t::TOK_WEEK:
    case lexer_t::token_t::TOK_WEEKS:
    case lexer_t::token_t::TOK_DAY:
    case lexer_t::token_t::TOK_DAYS:
      lexer.next_token();
      tok = lexer.next_token();
      switch (tok.kind) {
      case lexer_t::token_t::TOK_AGO:
        adjust = -1;
        break;
      case lexer_t::token_t::TOK_HENCE:
        adjust = 1;
        break;
      default:
        tok.unexpected();
        break;
      }
      break;
    default:
      break;
    }

    // Apply the offset (if any) to today's date
    date_t when(today);

    switch (kind) {
    case lexer_t::token_t::TOK_YEAR:
    case lexer_t::token_t::TOK_YEARS:
      when += gregorian::years(amount * adjust);
      break;
    case lexer_t::token_t::TOK_QUARTER:
    case lexer_t::token_t::TOK_QUARTERS:
      when += gregorian::months(amount * 3 * adjust);
      break;
    case lexer_t::token_t::TOK_MONTH:
    case lexer_t::token_t::TOK_MONTHS:
      when += gregorian::months(amount * adjust);
      break;
    case lexer_t::token_t::TOK_WEEK:
    case lexer_t::token_t::TOK_WEEKS:
      when += gregorian::weeks(static_cast<int>(amount * adjust));
      break;
    case lexer_t::token_t::TOK_DAY:
    case lexer_t::token_t::TOK_DAYS:
      when += gregorian::days(static_cast<int>(amount * adjust));
      break;
    default:
      // No unit token followed the integer: interpret as year or day
      if (amount > 31) {
        specifier.year = date_specifier_t::year_type(amount);
      } else {
        specifier.day = date_specifier_t::day_type(amount);
      }
      break;
    }

    if (adjust)
      specifier = date_specifier_t(when);
    break;
  }

  case lexer_t::token_t::TOK_THIS:
  case lexer_t::token_t::TOK_NEXT:
  case lexer_t::token_t::TOK_LAST: {
    // Relative period: "this month", "next year", "last quarter", etc.
    int8_t adjust = 0;
    if (tok.kind == lexer_t::token_t::TOK_NEXT)
      adjust = 1;
    else if (tok.kind == lexer_t::token_t::TOK_LAST)
      adjust = -1;

    tok = lexer.next_token();
    switch (tok.kind) {
    case lexer_t::token_t::TOK_A_MONTH: {
      // "this jan" / "next february" / "last march"
      date_t temp(today.year(), std::get<date_time::months_of_year>(*tok.value), 1);
      temp += gregorian::years(adjust);
      specifier =
          date_specifier_t(static_cast<date_specifier_t::year_type>(temp.year()), temp.month());
      break;
    }

    case lexer_t::token_t::TOK_A_WDAY: {
      // "this monday" / "next friday" / "last wednesday"
      date_t temp = date_duration_t::find_nearest(today, date_duration_t::WEEKS);
      while (temp.day_of_week() != std::get<date_time::months_of_year>(*tok.value))
        temp += gregorian::days(1);
      temp += gregorian::days(static_cast<int>(7 * adjust));
      specifier = date_specifier_t(temp);
      break;
    }

    case lexer_t::token_t::TOK_YEAR: {
      // "this year" / "next year" / "last year"
      date_t temp(today);
      temp += gregorian::years(adjust);
      specifier = date_specifier_t(static_cast<date_specifier_t::year_type>(temp.year()));
      break;
    }

    case lexer_t::token_t::TOK_QUARTER: {
      // "this quarter" / "next quarter" / "last quarter"
      date_t base = date_duration_t::find_nearest(today, date_duration_t::QUARTERS);
      date_t temp;
      if (adjust < 0) {
        temp = base + gregorian::months(3 * adjust);
      } else if (adjust == 0) {
        temp = base + gregorian::months(3);
      } else if (adjust > 0) {
        base += gregorian::months(3 * adjust);
        temp = base + gregorian::months(3 * adjust);
      }
      specifier = date_specifier_t(adjust < 0 ? temp : base);
      break;
    }

    case lexer_t::token_t::TOK_WEEK: {
      // "this week" / "next week" / "last week"
      date_t base = date_duration_t::find_nearest(today, date_duration_t::WEEKS);
      date_t temp;
      if (adjust < 0) {
        temp = base + gregorian::days(static_cast<int>(7 * adjust));
      } else if (adjust == 0) {
        temp = base + gregorian::days(7);
      } else if (adjust > 0) {
        base += gregorian::days(static_cast<int>(7 * adjust));
        temp = base + gregorian::days(static_cast<int>(7 * adjust));
      }
      specifier = date_specifier_t(adjust < 0 ? temp : base);
      break;
    }

    case lexer_t::token_t::TOK_DAY: {
      // "this day" / "next day" / "last day"
      date_t temp(today);
      temp += gregorian::days(adjust);
      specifier = date_specifier_t(temp);
      break;
    }

    default:
    case lexer_t::token_t::TOK_MONTH: {
      // "this month" / "next month" / "last month"
      // Also the default case for unrecognized unit tokens.
      date_t temp(today);
      temp += gregorian::months(adjust);
      specifier =
          date_specifier_t(static_cast<date_specifier_t::year_type>(temp.year()), temp.month());
      break;
    }
    }
    break;
  }

  case lexer_t::token_t::TOK_A_MONTH:
    // Bare month name: "jan" optionally followed by a year integer
    specifier.month = date_specifier_t::month_type(std::get<date_time::months_of_year>(*tok.value));
    tok = lexer.peek_token();
    switch (tok.kind) {
    case lexer_t::token_t::TOK_INT:
      specifier.year = std::get<unsigned short>(*tok.value);
      break;
    case lexer_t::token_t::END_REACHED:
    default:
      break;
    }
    break;
  case lexer_t::token_t::TOK_A_WDAY:
    // Bare weekday name: "monday"
    specifier.wday = date_specifier_t::day_of_week_type(std::get<date_time::weekdays>(*tok.value));
    break;

  case lexer_t::token_t::TOK_TODAY:
    specifier = date_specifier_t(today);
    break;
  case lexer_t::token_t::TOK_TOMORROW:
    specifier = date_specifier_t(today + gregorian::days(1));
    break;
  case lexer_t::token_t::TOK_YESTERDAY:
    specifier = date_specifier_t(today - gregorian::days(1));
    break;

  default:
    tok.unexpected();
    break;
  }
}

/*--- Parser Helper Methods ---*/

/// @brief Handle date/integer/month/weekday tokens as inclusion specifiers.
void date_parser_t::handle_date_token(lexer_t::token_t& tok,
                                      optional<date_specifier_t>& inclusion_specifier) {
  if (!inclusion_specifier)
    inclusion_specifier = date_specifier_t();
  determine_when(tok, *inclusion_specifier);
}

/**
 * @brief Handle the dash range operator (e.g. "2024/01/01 - 2024/12/31").
 *
 * The dash operator converts the current inclusion specifier into a
 * since_specifier, parses the token after the dash as the until_specifier,
 * and sets end_inclusive=true so that the end date is included in the range.
 * This differs from `since`/`until` syntax, which is exclusive on the end.
 */
void date_parser_t::handle_dash_operator(lexer_t::token_t& tok,
                                         optional<date_specifier_t>& inclusion_specifier,
                                         optional<date_specifier_t>& since_specifier,
                                         optional<date_specifier_t>& until_specifier,
                                         bool& end_inclusive) {
  if (inclusion_specifier) {
    since_specifier = inclusion_specifier;
    until_specifier = date_specifier_t();
    inclusion_specifier = none;

    tok = lexer.next_token();
    determine_when(tok, *until_specifier);

    // The dash operator is special: it has an _inclusive_ end.
    end_inclusive = true;
  } else {
    tok.unexpected();
  }
}

/// @brief Parse the date following "since"/"from" as the range start.
void date_parser_t::handle_since_token(lexer_t::token_t& tok,
                                       optional<date_specifier_t>& since_specifier) {
  if (since_specifier) {
    tok.unexpected();
  } else {
    since_specifier = date_specifier_t();
    tok = lexer.next_token();
    determine_when(tok, *since_specifier);
  }
}

/// @brief Parse the date following "until"/"to" as the range end.
void date_parser_t::handle_until_token(lexer_t::token_t& tok,
                                       optional<date_specifier_t>& until_specifier) {
  if (until_specifier) {
    tok.unexpected();
  } else {
    until_specifier = date_specifier_t();
    tok = lexer.next_token();
    determine_when(tok, *until_specifier);
  }
}

/// @brief Parse the date following "in" as an inclusion specifier.
void date_parser_t::handle_in_token(lexer_t::token_t& tok,
                                    optional<date_specifier_t>& inclusion_specifier) {
  if (inclusion_specifier) {
    tok.unexpected();
  } else {
    inclusion_specifier = date_specifier_t();
    tok = lexer.next_token();
    determine_when(tok, *inclusion_specifier);
  }
}

/**
 * @brief Handle "this"/"next"/"last" followed by a quantity and unit.
 *
 * This method handles the more complex relative expressions that appear
 * in the main parsing loop (as opposed to determine_when, which handles
 * them in the context of since/until/in).  Examples:
 *
 *   - "last 3 months" -> since = today - 3 months, until = today
 *   - "next 2 weeks"  -> since = today, until = today + 2 weeks
 *   - "this quarter"  -> since = start of quarter, until = end of quarter
 *   - "next january"  -> inclusion specifier for jan of next year
 *   - "last monday"   -> inclusion specifier for the previous monday
 */
void date_parser_t::handle_relative_token(lexer_t::token_t& tok,
                                          optional<date_specifier_t>& since_specifier,
                                          optional<date_specifier_t>& until_specifier,
                                          optional<date_specifier_t>& inclusion_specifier,
                                          const date_t& today) {
  int8_t adjust = 0;
  if (tok.kind == lexer_t::token_t::TOK_NEXT)
    adjust = 1;
  else if (tok.kind == lexer_t::token_t::TOK_LAST)
    adjust = -1;

  tok = lexer.next_token();
  switch (tok.kind) {
  case lexer_t::token_t::TOK_INT: {
    // "last 3 months" / "next 2 weeks" / "this 5 days"
    unsigned short amount = std::get<unsigned short>(*tok.value);
    date_t base(today);
    date_t end(today);

    if (!adjust)
      adjust = 1;

    tok = lexer.next_token();
    switch (tok.kind) {
    case lexer_t::token_t::TOK_YEARS:
      base += gregorian::years(amount * adjust);
      break;
    case lexer_t::token_t::TOK_QUARTERS:
      base += gregorian::months(amount * adjust * 3);
      break;
    case lexer_t::token_t::TOK_MONTHS:
      base += gregorian::months(amount * adjust);
      break;
    case lexer_t::token_t::TOK_WEEKS:
      base += gregorian::weeks(static_cast<int>(amount * adjust));
      break;
    case lexer_t::token_t::TOK_DAYS:
      base += gregorian::days(static_cast<int>(amount * adjust));
      break;
    default:
      tok.unexpected();
      break;
    }

    // Ensure base < end (swap if looking forward)
    if (adjust >= 0) {
      date_t temp = base;
      base = end;
      end = temp;
    }

    since_specifier = date_specifier_t(base);
    until_specifier = date_specifier_t(end);
    break;
  }

  case lexer_t::token_t::TOK_A_MONTH: {
    // "next january" / "last march"
    inclusion_specifier = date_specifier_t();
    determine_when(tok, *inclusion_specifier);

    date_t temp(today.year(), *inclusion_specifier->month, 1);
    temp += gregorian::years(adjust);
    inclusion_specifier =
        date_specifier_t(static_cast<date_specifier_t::year_type>(temp.year()), temp.month());
    break;
  }

  case lexer_t::token_t::TOK_A_WDAY: {
    // "next monday" / "last friday"
    inclusion_specifier = date_specifier_t();
    determine_when(tok, *inclusion_specifier);

    date_t temp = date_duration_t::find_nearest(today, date_duration_t::WEEKS);
    while (temp.day_of_week() != inclusion_specifier->wday)
      temp += gregorian::days(1);
    temp += gregorian::days(static_cast<int>(7 * adjust));
    inclusion_specifier = date_specifier_t(temp);
    break;
  }

  case lexer_t::token_t::TOK_YEAR: {
    // "next year" / "last year"
    date_t temp(today);
    temp += gregorian::years(adjust);
    inclusion_specifier = date_specifier_t(static_cast<date_specifier_t::year_type>(temp.year()));
    break;
  }

  case lexer_t::token_t::TOK_QUARTER: {
    // "this quarter" / "next quarter" / "last quarter"
    // Produces since/until specifiers bounding the quarter.
    date_t base = date_duration_t::find_nearest(today, date_duration_t::QUARTERS);
    date_t temp;
    if (adjust < 0) {
      temp = base + gregorian::months(3 * adjust);
    } else if (adjust == 0) {
      temp = base + gregorian::months(3);
    } else if (adjust > 0) {
      base += gregorian::months(3 * adjust);
      temp = base + gregorian::months(3 * adjust);
    }
    since_specifier = date_specifier_t(adjust < 0 ? temp : base);
    until_specifier = date_specifier_t(adjust < 0 ? base : temp);
    break;
  }

  case lexer_t::token_t::TOK_WEEK: {
    // "this week" / "next week" / "last week"
    // Produces since/until specifiers bounding the week.
    date_t base = date_duration_t::find_nearest(today, date_duration_t::WEEKS);
    date_t temp;
    if (adjust < 0) {
      temp = base + gregorian::days(static_cast<int>(7 * adjust));
    } else if (adjust == 0) {
      temp = base + gregorian::days(7);
    } else if (adjust > 0) {
      base += gregorian::days(static_cast<int>(7 * adjust));
      temp = base + gregorian::days(static_cast<int>(7 * adjust));
    }
    since_specifier = date_specifier_t(adjust < 0 ? temp : base);
    until_specifier = date_specifier_t(adjust < 0 ? base : temp);
    break;
  }

  case lexer_t::token_t::TOK_DAY: {
    // "next day" / "last day"
    date_t temp(today);
    temp += gregorian::days(adjust);
    inclusion_specifier = date_specifier_t(temp);
    break;
  }

  default:
  case lexer_t::token_t::TOK_MONTH: {
    // "this month" / "next month" / "last month"
    // Also the default case for unrecognized unit tokens.
    date_t temp(today);
    temp += gregorian::months(adjust);
    inclusion_specifier =
        date_specifier_t(static_cast<date_specifier_t::year_type>(temp.year()), temp.month());
    break;
  }
  }
}

/// @brief Map "today"/"tomorrow"/"yesterday" to an inclusion specifier.
void date_parser_t::handle_simple_period_token(lexer_t::token_t::kind_t kind,
                                               optional<date_specifier_t>& inclusion_specifier,
                                               const date_t& today) {
  switch (kind) {
  case lexer_t::token_t::TOK_TODAY:
    inclusion_specifier = date_specifier_t(today);
    break;
  case lexer_t::token_t::TOK_TOMORROW:
    inclusion_specifier = date_specifier_t(today + gregorian::days(1));
    break;
  case lexer_t::token_t::TOK_YESTERDAY:
    inclusion_specifier = date_specifier_t(today - gregorian::days(1));
    break;
  default:
    break;
  }
}

/**
 * @brief Parse "every N units" or "every unit" into a duration.
 *
 * Handles both "every 3 months" (with an explicit count) and
 * "every month" (count defaults to 1).
 */
void date_parser_t::handle_every_token(lexer_t::token_t& tok, optional<date_duration_t>& duration) {
  tok = lexer.next_token();
  if (tok.kind == lexer_t::token_t::TOK_INT) {
    // "every N units" -- explicit count
    int quantity = std::get<unsigned short>(*tok.value);
    tok = lexer.next_token();
    switch (tok.kind) {
    case lexer_t::token_t::TOK_YEARS:
      duration = date_duration_t(date_duration_t::YEARS, quantity);
      break;
    case lexer_t::token_t::TOK_QUARTERS:
      duration = date_duration_t(date_duration_t::QUARTERS, quantity);
      break;
    case lexer_t::token_t::TOK_MONTHS:
      duration = date_duration_t(date_duration_t::MONTHS, quantity);
      break;
    case lexer_t::token_t::TOK_WEEKS:
      duration = date_duration_t(date_duration_t::WEEKS, quantity);
      break;
    case lexer_t::token_t::TOK_DAYS:
      duration = date_duration_t(date_duration_t::DAYS, quantity);
      break;
    default:
      tok.unexpected();
      break;
    }
  } else {
    // "every unit" -- count defaults to 1
    switch (tok.kind) {
    case lexer_t::token_t::TOK_YEAR:
      duration = date_duration_t(date_duration_t::YEARS, 1);
      break;
    case lexer_t::token_t::TOK_QUARTER:
      duration = date_duration_t(date_duration_t::QUARTERS, 1);
      break;
    case lexer_t::token_t::TOK_MONTH:
      duration = date_duration_t(date_duration_t::MONTHS, 1);
      break;
    case lexer_t::token_t::TOK_WEEK:
      duration = date_duration_t(date_duration_t::WEEKS, 1);
      break;
    case lexer_t::token_t::TOK_DAY:
      duration = date_duration_t(date_duration_t::DAYS, 1);
      break;
    default:
      tok.unexpected();
      break;
    }
  }
}

/// @brief Map frequency shortcut tokens to a duration.
/// "yearly" -> YEARS/1, "quarterly" -> QUARTERS/1, "bimonthly" -> MONTHS/2, etc.
void date_parser_t::handle_period_token(lexer_t::token_t::kind_t kind,
                                        optional<date_duration_t>& duration) {
  switch (kind) {
  case lexer_t::token_t::TOK_YEARLY:
    duration = date_duration_t(date_duration_t::YEARS, 1);
    break;
  case lexer_t::token_t::TOK_QUARTERLY:
    duration = date_duration_t(date_duration_t::QUARTERS, 1);
    break;
  case lexer_t::token_t::TOK_BIMONTHLY:
    duration = date_duration_t(date_duration_t::MONTHS, 2);
    break;
  case lexer_t::token_t::TOK_MONTHLY:
    duration = date_duration_t(date_duration_t::MONTHS, 1);
    break;
  case lexer_t::token_t::TOK_BIWEEKLY:
    duration = date_duration_t(date_duration_t::WEEKS, 2);
    break;
  case lexer_t::token_t::TOK_WEEKLY:
    duration = date_duration_t(date_duration_t::WEEKS, 1);
    break;
  case lexer_t::token_t::TOK_DAILY:
    duration = date_duration_t(date_duration_t::DAYS, 1);
    break;
  default:
    break;
  }
}

/**
 * @brief Assemble the final date_interval_t from parsed specifiers.
 *
 * If since or until specifiers were found, constructs a date_range_t.
 * Otherwise, if an inclusion specifier was found, wraps it directly.
 * The end_inclusive flag is passed through for dash-separated ranges.
 */
void date_parser_t::finalize_period(date_interval_t& period,
                                    const optional<date_specifier_t>& since_specifier,
                                    const optional<date_specifier_t>& until_specifier,
                                    const optional<date_specifier_t>& inclusion_specifier,
                                    bool end_inclusive) {
  if (since_specifier || until_specifier) {
    date_range_t range(since_specifier, until_specifier);
    range.end_inclusive = end_inclusive;

    period.range = date_specifier_or_range_t(range);
    period.since_specified = static_cast<bool>(since_specifier);
  } else if (inclusion_specifier) {
    period.range = date_specifier_or_range_t(*inclusion_specifier);
  }
  // else: it's something like "monthly", with no date reference
}

/*--- Main Parser Entry Point ---*/

/**
 * @brief Parse the period expression string into a date_interval_t.
 *
 * The main loop consumes tokens sequentially.  Each token dispatches
 * to a handler that updates one or more of:
 *   - since_specifier / until_specifier (range endpoints)
 *   - inclusion_specifier (point-in-time or named period)
 *   - period.duration (step size for iteration)
 *
 * After all tokens are consumed, finalize_period() assembles these
 * into the interval's range field.
 */
date_interval_t date_parser_t::parse() {
  optional<date_specifier_t> since_specifier;
  optional<date_specifier_t> until_specifier;
  optional<date_specifier_t> inclusion_specifier;

  date_interval_t period;
  date_t today = epoch ? epoch->date() : CURRENT_DATE();
  bool end_inclusive = false;

  // Main parsing loop - process tokens sequentially
  for (lexer_t::token_t tok = lexer.next_token(); tok.kind != lexer_t::token_t::END_REACHED;
       tok = lexer.next_token()) {

    switch (tok.kind) {
    // Simple date/time specifiers
    case lexer_t::token_t::TOK_DATE:
    case lexer_t::token_t::TOK_INT:
    case lexer_t::token_t::TOK_A_MONTH:
    case lexer_t::token_t::TOK_A_WDAY:
      handle_date_token(tok, inclusion_specifier);
      break;

    // Range operators
    case lexer_t::token_t::TOK_DASH:
      handle_dash_operator(tok, inclusion_specifier, since_specifier, until_specifier,
                           end_inclusive);
      break;

    case lexer_t::token_t::TOK_SINCE:
      handle_since_token(tok, since_specifier);
      break;

    case lexer_t::token_t::TOK_UNTIL:
      handle_until_token(tok, until_specifier);
      break;

    case lexer_t::token_t::TOK_IN:
      handle_in_token(tok, inclusion_specifier);
      break;

    // Relative time expressions (this/next/last)
    case lexer_t::token_t::TOK_THIS:
    case lexer_t::token_t::TOK_NEXT:
    case lexer_t::token_t::TOK_LAST:
      handle_relative_token(tok, since_specifier, until_specifier, inclusion_specifier, today);
      break;

    // Simple period tokens (today/tomorrow/yesterday)
    case lexer_t::token_t::TOK_TODAY:
    case lexer_t::token_t::TOK_TOMORROW:
    case lexer_t::token_t::TOK_YESTERDAY:
      handle_simple_period_token(tok.kind, inclusion_specifier, today);
      break;

    // Recurring period specifications
    case lexer_t::token_t::TOK_EVERY:
      handle_every_token(tok, period.duration);
      break;

    // Period shortcuts
    case lexer_t::token_t::TOK_YEARLY:
    case lexer_t::token_t::TOK_QUARTERLY:
    case lexer_t::token_t::TOK_BIMONTHLY:
    case lexer_t::token_t::TOK_MONTHLY:
    case lexer_t::token_t::TOK_BIWEEKLY:
    case lexer_t::token_t::TOK_WEEKLY:
    case lexer_t::token_t::TOK_DAILY:
      handle_period_token(tok.kind, period.duration);
      break;

    default:
      tok.unexpected();
      break;
    }
  }

#if 0
  if (!period.duration && inclusion_specifier)
    period.duration = inclusion_specifier->implied_duration();
#endif

  // Finalize the period structure with collected specifiers
  finalize_period(period, since_specifier, until_specifier, inclusion_specifier, end_inclusive);

  return period;
}

/*--- Interval Lifecycle ---*/

/// @brief Parse a period expression string, delegating to date_parser_t.
void date_interval_t::parse(const string& str) {
  date_parser_t parser(str);
  *this = parser.parse();
}

/**
 * @brief Compute end_of_duration and next from the current start and duration.
 *
 * This establishes the boundaries of the current period:
 *   - end_of_duration = start + duration (if not already set)
 *   - Clamped to finish if finish is set and closer
 *   - next = end_of_duration (if next is not already set)
 */
void date_interval_t::resolve_end() {
  if (start && !end_of_duration) {
    end_of_duration = duration->add(*start);
    DEBUG("times.interval", "stabilize: end_of_duration = " << *end_of_duration);
  }

  if (finish && *end_of_duration > *finish) {
    end_of_duration = finish;
    DEBUG("times.interval", "stabilize: end_of_duration reset to end: " << *end_of_duration);
  }

  if (start && !next) {
    next = end_of_duration;
    DEBUG("times.interval", "stabilize: next set to: " << *next);
  }
}

/*--- Period Boundary Alignment ---*/

/**
 * @brief Find the nearest period boundary on or before a date.
 *
 * Used during stabilization to snap the start date to a natural
 * calendar boundary:
 *   - YEARS: January 1 of the same year
 *   - QUARTERS: first day of the containing quarter (Jan/Apr/Jul/Oct)
 *   - MONTHS: first day of the same month
 *   - WEEKS: most recent start_of_week day
 *   - DAYS: the date itself
 */
date_t date_duration_t::find_nearest(const date_t& date, skip_quantum_t skip) {
  date_t result;

  switch (skip) {
  case date_duration_t::YEARS:
    result = date_t(date.year(), gregorian::Jan, 1);
    break;
  case date_duration_t::QUARTERS:
    result = date_t(date.year(), date.month(), 1);
    while (result.month() != gregorian::Jan && result.month() != gregorian::Apr &&
           result.month() != gregorian::Jul && result.month() != gregorian::Oct)
      result -= gregorian::months(1);
    break;
  case date_duration_t::MONTHS:
    result = date_t(date.year(), date.month(), 1);
    break;
  case date_duration_t::WEEKS:
    result = date;
    while (result.day_of_week() != start_of_week)
      result -= gregorian::days(1);
    break;
  case date_duration_t::DAYS:
    result = date;
    break;
  }
  return result;
}

/*--- Interval Stabilization ---*/

/**
 * @brief Align the interval to a reference date, initializing iteration state.
 *
 * Stabilization is the process of turning a parsed period expression
 * (which may have relative references like "monthly") into a concrete
 * start/finish/end_of_duration for a specific reference date.  It:
 *
 *   1. Finds the natural period boundary (via find_nearest) for the
 *      duration's quantum that contains or precedes the reference date.
 *
 *   2. For months/quarters/years with an explicit since date, either
 *      preserves the user's day-of-period (advancing by whole intervals)
 *      or snaps to the period boundary.
 *
 *   3. For weeks, starts from a point sufficiently far back that the
 *      subsequent scan will find the correct period.
 *
 *   4. Iterates forward through periods until the one containing the
 *      reference date is found.
 *
 *   5. Clamps start and finish to the original range boundaries.
 *
 * This method is idempotent: once `aligned` is true, subsequent calls
 * are no-ops.
 */
void date_interval_t::stabilize(const optional<date_t>& date, bool align_intervals) {
#if DEBUG_ON
  if (date)
    DEBUG("times.interval", "stabilize: with date = " << *date);
#endif

  if (date && !aligned) {
    DEBUG("times.interval", "stabilize: date passed, but not aligned");
    if (duration) {
      DEBUG("times.interval", "stabilize: aligning with a duration: " << *duration);

      // The interval object has not been seeded with a start date yet, so
      // find the nearest period before or on date which fits, if possible.
      //
      // Find an efficient starting point for the upcoming while loop.  We
      // want a date early enough that the range will be correct, but late
      // enough that we don't spend hundreds of thousands of loops skipping
      // through time.
      optional<date_t> initial_start = start ? start : begin();
      optional<date_t> initial_finish = finish ? finish : end();

#if DEBUG_ON
      if (initial_start)
        DEBUG("times.interval", "stabilize: initial_start  = " << *initial_start);
      if (initial_finish)
        DEBUG("times.interval", "stabilize: initial_finish = " << *initial_finish);
#endif

      date_t when = start ? *start : *date;

      // Phase 1: Choose a starting point based on the duration quantum
      switch (duration->quantum) {
      case date_duration_t::MONTHS:
      case date_duration_t::QUARTERS:
      case date_duration_t::YEARS:
        // These start on most recent period start quantum before when.
        DEBUG("times.interval", "stabilize: monthly, quarterly or yearly duration");
        if (align_intervals && since_specified) { // NOLINT(bugprone-branch-clone)
          start = when;
        } else if (since_specified && initial_start) {
          // When the user specified an explicit start date (e.g.
          // "every 1 months from 2023-01-15"), preserve the day-of-period
          // rather than snapping to the first of the month.  Advance from
          // the range begin by whole intervals until we reach the interval
          // that contains or immediately precedes 'when'.
          start = *initial_start;
          {
            date_t next = duration->add(*start);
            while (next <= when) {
              start = next;
              next = duration->add(*start);
            }
          }
        } else {
          start = date_duration_t::find_nearest(when, duration->quantum);
        }
        break;
      case date_duration_t::WEEKS:
        // Weeks start on the beginning of week prior to 400 remainder period length
        // Either the first quanta of the period or the last quanta of the period seems more
        // sensible implies period is never less than 400 days not too unreasonable
        DEBUG("times.interval", "stabilize: weekly duration");
        {
          if (align_intervals && since_specified) {
            start = when;
          } else {
            int period = duration->length * 7;
            start = date_duration_t::find_nearest(when - gregorian::days(period + 400 % period),
                                                  duration->quantum);
          }
        }
        break;
      default:
        // multiples of days have a quanta of 1 day so should not have the start date adjusted to a
        // quanta
        DEBUG("times.interval", "stabilize: daily duration - stable by definition");
        start = when;
        break;
      }

      DEBUG("times.interval", "stabilize: beginning start date = " << *start);

      // Phase 2: Scan forward through periods to find the one containing
      // or just preceding the reference date.
      while (*start < *date) {
        date_interval_t next_interval(*this);
        ++next_interval;

        if (next_interval.start && *next_interval.start <= *date) {
          *this = next_interval;
        } else {
          end_of_duration = none;
          next = none;
          break;
        }
      }

      DEBUG("times.interval", "stabilize: proposed start date = " << *start);

      // Phase 3: Clamp to the original range boundaries
      if (initial_start && (!start || *start < *initial_start)) {
        // Using the discovered start, find the end of the period
        resolve_end();

        start = initial_start;
        DEBUG("times.interval", "stabilize: start reset to initial start");
      }
      if (initial_finish && (!finish || *finish > *initial_finish)) {
        finish = initial_finish;
        DEBUG("times.interval", "stabilize: finish reset to initial finish");
      }

#if DEBUG_ON
      if (start)
        DEBUG("times.interval", "stabilize: final start  = " << *start);
      if (finish)
        DEBUG("times.interval", "stabilize: final finish = " << *finish);
#endif
    } else if (range) {
      // No duration -- just use the range boundaries directly
      start = range->begin();
      finish = range->end();
    }
    aligned = true;
  }

  // If there is no duration, then if we've reached here the date falls
  // between start and finish.
  if (!duration) {
    DEBUG("times.interval", "stabilize: there was no duration given");

    if (!start && !finish)
      throw_(date_error, _("Invalid date interval: neither start, nor finish, nor duration"));
  } else {
    resolve_end();
  }
}

/*--- Period Search ---*/

/**
 * @brief Find the current or next period containing a date.
 *
 * After stabilization, this method checks whether the date falls
 * within the current period [start, end_of_duration).  If not, and
 * allow_shift is true, it scans forward through successive periods
 * until it finds one that contains the date, or determines that none
 * can.  Returns false if the date is outside the overall [start, finish)
 * range or no containing period exists.
 *
 * @param date             The date to search for.
 * @param align_intervals  Passed through to stabilize().
 * @param allow_shift      If false, only checks the current period (no scanning).
 * @return True if a containing period was found.
 */
bool date_interval_t::find_period(const date_t& date, const bool align_intervals,
                                  const bool allow_shift) {
  stabilize(date, align_intervals);

  if (finish && date > *finish) {
    DEBUG("times.interval", "false: date [" << date << "] > finish [" << *finish << "]");
    return false;
  }

  if (!start) {
    throw_(std::runtime_error, _("Date interval is improperly initialized"));
  } else if (date < *start) {
    DEBUG("times.interval", "false: date [" << date << "] < start [" << *start << "]");
    return false;
  }

  if (end_of_duration) {
    if (date < *end_of_duration) {
      DEBUG("times.interval",
            "true: date [" << date << "] < end_of_duration [" << *end_of_duration << "]");
      return true;
    }
  } else {
    DEBUG("times.interval", "false: there is no end_of_duration");
    return false;
  }

  // If we've reached here, it means the date does not fall into the current
  // interval, so we must seek another interval that does match -- unless we
  // pass by date in so doing, which means we shouldn't alter the current
  // period of the interval at all.

  date_t scan = *start;
  date_t end_of_scan = *end_of_duration;

  DEBUG("times.interval", "date        = " << date);
  DEBUG("times.interval", "scan        = " << scan);
  DEBUG("times.interval", "end_of_scan = " << end_of_scan);
#if DEBUG_ON
  if (finish)
    DEBUG("times.interval", "finish      = " << *finish);
  else
    DEBUG("times.interval", "finish is not set");
#endif

  // Scan forward through periods until we find one containing the date,
  // or pass beyond it (or beyond finish).
  while (date >= scan && (!finish || scan < *finish)) {
    if (date < end_of_scan) {
      start = scan;
      end_of_duration = end_of_scan;
      next = none;

      DEBUG("times.interval", "true: start           = " << *start);
      DEBUG("times.interval", "true: end_of_duration = " << *end_of_duration);

      resolve_end();

      return true;
    } else if (!allow_shift) {
      break;
    }

    scan = duration->add(scan);
    end_of_scan = duration->add(scan);

    DEBUG("times.interval", "scan        = " << scan);
    DEBUG("times.interval", "end_of_scan = " << end_of_scan);
  }

  DEBUG("times.interval", "false: failed scan");

  return false;
}

/*--- Interval Iteration ---*/

/**
 * @brief Advance the interval to the next period.
 *
 * Moves start forward to the next period boundary (previously
 * computed by resolve_end and stored in `next`).  Recomputes
 * end_of_duration for the new period.  If the new start would
 * pass finish, sets start to none to signal iteration is complete.
 */
date_interval_t& date_interval_t::operator++() {
  if (!start)
    throw_(date_error, _("Cannot increment an unstarted date interval"));

  stabilize();

  if (!duration)
    throw_(date_error, _("Cannot increment a date interval without a duration"));

  assert(next);

  // NOLINTNEXTLINE(bugprone-branch-clone)
  if (finish && *next >= *finish) {
    start = none;
  } else {
    start = *next;
    end_of_duration = duration->add(*start);
  }
  next = none;

  resolve_end();

  return *this;
}

/*--- Diagnostic Output ---*/

/**
 * @brief Dump the interval's state for debugging.
 *
 * Shows the interval before and after stabilization, then iterates
 * through up to 20 sample periods, printing each period's start and
 * inclusive end date.  This is used by the `period` debugging command.
 */
void date_interval_t::dump(std::ostream& out) {
  out << _("--- Before stabilization ---") << '\n';

  if (range)
    out << _("   range: ") << range->to_string() << '\n';
  if (start)
    out << _("   start: ") << format_date(*start) << '\n';
  if (finish)
    out << _("  finish: ") << format_date(*finish) << '\n';

  if (duration)
    out << _("duration: ") << duration->to_string() << '\n';

  optional<date_t> when(begin());
  if (!when)
    when = CURRENT_DATE();

  stabilize(when);

  out << '\n' << _("--- After stabilization ---") << '\n';

  if (range)
    out << _("   range: ") << range->to_string() << '\n';
  if (start)
    out << _("   start: ") << format_date(*start) << '\n';
  if (finish)
    out << _("  finish: ") << format_date(*finish) << '\n';

  if (duration)
    out << _("duration: ") << duration->to_string() << '\n';

  out << '\n' << _("--- Sample dates in range (max. 20) ---") << '\n';

  date_t last_date;

  for (int i = 0; i < 20 && *this; ++i, ++*this) {
    out << std::right;
    out.width(2);

    if (!last_date.is_not_a_date() && last_date == *start)
      break;

    out << (i + 1) << ": " << format_date(*start);
    if (duration)
      out << " -- " << format_date(*inclusive_end());
    out << '\n';

    if (!duration)
      break;

    last_date = *start;
  }
}

/*--- Lexer Implementation ---*/

/**
 * @brief Scan and return the next token from the input.
 *
 * The lexer operates in three phases:
 *
 *   1. **Punctuation**: single-character tokens for '/', '-', '.'.
 *
 *   2. **Date attempt**: if the input starts with a digit, extract
 *      the entire whitespace-delimited word and try to parse it as
 *      a full date via parse_date_mask().  This allows formats like
 *      "2024/01/15" or custom --input-date-format patterns.
 *
 *   3. **Word/number**: collect an alphanumeric run and match against
 *      keywords (since, until, every, monthly, etc.), month names,
 *      weekday names, or bare integers.
 */
date_parser_t::lexer_t::token_t date_parser_t::lexer_t::next_token() {
  // Return cached token if available (from peek_token or push_token)
  if (token_cache.kind != token_t::UNKNOWN) {
    token_t tok = token_cache;
    token_cache = token_t();
    return tok;
  }

  // Skip whitespace
  while (begin != end && std::isspace(static_cast<unsigned char>(*begin)))
    begin++;

  if (begin == end)
    return token_t(token_t::END_REACHED);

  // Phase 1: Single-character punctuation tokens
  switch (*begin) {
  case '/':
    ++begin;
    return token_t(token_t::TOK_SLASH);
  case '-':
    ++begin;
    return token_t(token_t::TOK_DASH);
  case '.':
    ++begin;
    return token_t(token_t::TOK_DOT);
  default:
    break;
  }

  string::const_iterator start = begin;

  // Phase 2: If the first character is a digit, try parsing the whole
  // argument as a date using the typical date formats.  This allows not
  // only dates like "2009/08/01", but also dates that fit the user's
  // --input-date-format, assuming their format fits in one argument and
  // begins with a digit.
  if (std::isdigit(static_cast<unsigned char>(*begin))) {
    string::const_iterator i = begin;
    for (i = begin; i != end && !std::isspace(static_cast<unsigned char>(*i)); i++) {}
    assert(i != begin);

    string possible_date(start, i); // NOLINT(bugprone-unused-local-non-trivial-variable)

    try {
      date_traits_t traits;
      date_t when = parse_date_mask(possible_date.c_str(), &traits);
      if (!when.is_not_a_date()) {
        begin = i;
        return token_t(token_t::TOK_DATE, token_t::content_t(date_specifier_t(when, traits)));
      }
    } catch (date_error&) {
      // If the string contains date separators, re-throw: the user
      // clearly intended a date.  Otherwise, fall through to try
      // parsing as a bare integer.
      if (contains(possible_date, "/") || contains(possible_date, "-") ||
          contains(possible_date, "."))
        throw;
    }
  }

  start = begin;

  // Phase 3: Collect an alphanumeric (or non-alphanumeric) word and
  // match against known tokens.
  string term;
  bool alnum = std::isalnum(static_cast<unsigned char>(*begin));
  for (; (begin != end && !std::isspace(static_cast<unsigned char>(*begin)) &&
          ((alnum && static_cast<bool>(std::isalnum(static_cast<unsigned char>(*begin)))) ||
           (!alnum && !static_cast<bool>(std::isalnum(static_cast<unsigned char>(*begin))))));
       begin++)
    term.push_back(*begin);

  if (!term.empty()) {
    if (std::isdigit(static_cast<unsigned char>(term[0]))) {
      // Bare integer (not a full date)
      return token_t(token_t::TOK_INT, token_t::content_t(lexical_cast<unsigned short>(term)));
    } else if (std::isalpha(static_cast<unsigned char>(term[0]))) {
      to_lower(term);

      // Try month and weekday names first, then keywords
      if (optional<date_time::months_of_year> month = string_to_month_of_year(term)) {
        return token_t(token_t::TOK_A_MONTH, token_t::content_t(*month));
      } else if (optional<date_time::weekdays> wday = string_to_day_of_week(term)) {
        return token_t(token_t::TOK_A_WDAY, token_t::content_t(*wday));
      } else if (term == _("ago"))
        return token_t(token_t::TOK_AGO);
      else if (term == _("hence"))
        return token_t(token_t::TOK_HENCE);
      else if (term == _("from") || term == _("since"))
        return token_t(token_t::TOK_SINCE);
      else if (term == _("to") || term == _("until"))
        return token_t(token_t::TOK_UNTIL);
      else if (term == _("in"))
        return token_t(token_t::TOK_IN);
      else if (term == _("this"))
        return token_t(token_t::TOK_THIS);
      else if (term == _("next"))
        return token_t(token_t::TOK_NEXT);
      else if (term == _("last"))
        return token_t(token_t::TOK_LAST);
      else if (term == _("every"))
        return token_t(token_t::TOK_EVERY);
      else if (term == _("today"))
        return token_t(token_t::TOK_TODAY);
      else if (term == _("tomorrow"))
        return token_t(token_t::TOK_TOMORROW);
      else if (term == _("yesterday") or term == _("yday"))
        return token_t(token_t::TOK_YESTERDAY);
      else if (term == _("year"))
        return token_t(token_t::TOK_YEAR);
      else if (term == _("quarter"))
        return token_t(token_t::TOK_QUARTER);
      else if (term == _("month"))
        return token_t(token_t::TOK_MONTH);
      else if (term == _("week"))
        return token_t(token_t::TOK_WEEK);
      else if (term == _("day"))
        return token_t(token_t::TOK_DAY);
      else if (term == _("yearly"))
        return token_t(token_t::TOK_YEARLY);
      else if (term == _("quarterly"))
        return token_t(token_t::TOK_QUARTERLY);
      else if (term == _("bimonthly"))
        return token_t(token_t::TOK_BIMONTHLY);
      else if (term == _("monthly"))
        return token_t(token_t::TOK_MONTHLY);
      else if (term == _("biweekly"))
        return token_t(token_t::TOK_BIWEEKLY);
      else if (term == _("weekly"))
        return token_t(token_t::TOK_WEEKLY);
      else if (term == _("daily"))
        return token_t(token_t::TOK_DAILY);
      else if (term == _("years"))
        return token_t(token_t::TOK_YEARS);
      else if (term == _("quarters"))
        return token_t(token_t::TOK_QUARTERS);
      else if (term == _("months"))
        return token_t(token_t::TOK_MONTHS);
      else if (term == _("weeks"))
        return token_t(token_t::TOK_WEEKS);
      else if (term == _("days"))
        return token_t(token_t::TOK_DAYS);
    } else {
      token_t::expected('\0', term[0]);
      begin = ++start;
    }
  } else {
    token_t::expected('\0', *begin);
  }

  return token_t(token_t::UNKNOWN, token_t::content_t(term));
}

/*--- Lexer Error Reporting ---*/

/// @brief Throw a date_error for an unexpected token in a period expression.
void date_parser_t::lexer_t::token_t::unexpected() {
  switch (kind) {
  case END_REACHED:
    kind = UNKNOWN;
    throw_(date_error, _("Unexpected end of expression"));
  default: {
    string desc = to_string(); // NOLINT(bugprone-unused-local-non-trivial-variable)
    kind = UNKNOWN;
    throw_(date_error, _f("Unexpected date period token '%1%'") % desc);
  }
  }
}

/// @brief Throw a date_error for an unexpected character in a period expression.
void date_parser_t::lexer_t::token_t::expected(char wanted, char c) {
  if (wanted == '\0')
    throw_(date_error, _f("Invalid char '%1%'") % c);
  else
    throw_(date_error, _f("Invalid char '%1%' (wanted '%2%')") % c % wanted);
}

/*--- Date/Time Formatting ---*/

namespace {
using datetime_io_map = std::map<std::string, std::unique_ptr<datetime_io_t>>;
using date_io_map = std::map<std::string, std::unique_ptr<date_io_t>>;

/// Cache of custom datetime format objects, keyed by format string.
/// Avoids re-creating IO objects for repeated format_datetime() calls
/// with the same FMT_CUSTOM format.
datetime_io_map temp_datetime_io;

/// Cache of custom date format objects, keyed by format string.
date_io_map temp_date_io;
} // namespace

/**
 * @brief Format a datetime value as a string.
 *
 * Dispatches to the appropriate IO object based on format_type:
 *   - FMT_WRITTEN: canonical "YYYY/MM/DD HH:MM:SS" for serialization
 *   - FMT_PRINTED: human-friendly "YY-Mon-DD HH:MM:SS" for display
 *   - FMT_CUSTOM: a caller-supplied strftime format, with caching
 */
std::string format_datetime(const datetime_t& when, const format_type_t format_type,
                            const optional<const char*>& format) {
  // NOLINTNEXTLINE(bugprone-branch-clone)
  if (format_type == FMT_WRITTEN) {
    return written_datetime_io->format(when);
  } else if (format_type == FMT_CUSTOM && format) {
    if (auto i = temp_datetime_io.find(*format); i != temp_datetime_io.end()) {
      return (*i).second->format(when);
    } else {
      auto& stored = (temp_datetime_io[*format] = std::make_unique<datetime_io_t>(*format, false));
      return stored->format(when);
    }
  } else if (format_type == FMT_PRINTED) {
    return printed_datetime_io->format(when);
  } else {
    assert(false);
    return empty_string;
  }
}

/**
 * @brief Format a date value as a string.
 *
 * Dispatches to the appropriate IO object based on format_type:
 *   - FMT_WRITTEN: canonical "YYYY/MM/DD" for serialization
 *   - FMT_PRINTED: human-friendly "YY-Mon-DD" for display
 *   - FMT_CUSTOM: a caller-supplied strftime format, with caching
 */
std::string format_date(const date_t& when, const format_type_t format_type,
                        const optional<const char*>& format) {
  // NOLINTNEXTLINE(bugprone-branch-clone)
  if (format_type == FMT_WRITTEN) {
    return written_date_io->format(when);
  } else if (format_type == FMT_CUSTOM && format) {
    if (auto i = temp_date_io.find(*format); i != temp_date_io.end()) {
      return (*i).second->format(when);
    } else {
      auto& stored = (temp_date_io[*format] = std::make_unique<date_io_t>(*format, false));
      return stored->format(when);
    }
  } else if (format_type == FMT_PRINTED) {
    return printed_date_io->format(when);
  } else {
    assert(false);
    return empty_string;
  }
}

/*--- Format Configuration ---*/

namespace {
bool is_initialized = false;           ///< Guard for times_initialize/times_shutdown.
bool explicit_date_format = false;     ///< True after set_date_format() is called.
bool explicit_datetime_format = false; ///< True after set_datetime_format() is called.
} // namespace

/// @brief Override both the written and printed datetime format.
void set_datetime_format(const char* format) {
  explicit_datetime_format = true;
  written_datetime_io->set_format(format);
  printed_datetime_io->set_format(format);
}

/// @brief Override both the written and printed date format.
void set_date_format(const char* format) {
  explicit_date_format = true;
  written_date_io->set_format(format);
  printed_date_io->set_format(format);
}

bool date_format_is_set() {
  return explicit_date_format;
}
bool datetime_format_is_set() {
  return explicit_datetime_format;
}

/**
 * @brief Register a custom input date format at the highest priority.
 *
 * Pushes a new reader to the front of the format cascade, so it is
 * tried before all built-in formats.  Also disables separator
 * normalization, since custom formats may use '.' or '-' literally.
 */
void set_input_date_format(const char* format) {
  readers.push_front(std::shared_ptr<date_io_t>(new date_io_t(format, true)));
  convert_separators_to_slashes = false;
}

/*--- Initialization and Shutdown ---*/

/**
 * @brief Initialize the date/time subsystem.
 *
 * Creates all built-in IO objects and registers the default date
 * input formats in priority order:
 *
 *   1. "%m/%d"        (month/day, no year -- year inferred)
 *   2. "%Y/%m/%d"     (four-digit year)
 *   3. "%Y/%m"        (year/month, no day)
 *   4. "%y/%m/%d"     (two-digit year)
 *   5. "%Y-%m-%d"     (ISO 8601 with dashes)
 *   6. "%m-%d"        (month-day with dashes)
 *
 * Custom formats from --input-date-format are prepended later via
 * set_input_date_format() and take priority over these.
 *
 * This function is idempotent and must be called before any date
 * parsing or formatting.
 */
void times_initialize() {
  if (!is_initialized) {
    explicit_date_format = false;
    explicit_datetime_format = false;

    input_datetime_io.reset(new datetime_io_t("%Y/%m/%d %H:%M:%S", true));
    timelog_datetime_io.reset(new datetime_io_t("%m/%d/%Y %H:%M:%S", true));

    written_datetime_io.reset(new datetime_io_t("%Y/%m/%d %H:%M:%S", false));
    written_date_io.reset(new date_io_t("%Y/%m/%d", false));

    printed_datetime_io.reset(new datetime_io_t("%y-%b-%d %H:%M:%S", false));
    printed_date_io.reset(new date_io_t("%y-%b-%d", false));

    readers.push_back(std::shared_ptr<date_io_t>(new date_io_t("%m/%d", true)));
    readers.push_back(std::shared_ptr<date_io_t>(new date_io_t("%Y/%m/%d", true)));
    readers.push_back(std::shared_ptr<date_io_t>(new date_io_t("%Y/%m", true)));
    readers.push_back(std::shared_ptr<date_io_t>(new date_io_t("%y/%m/%d", true)));
    readers.push_back(std::shared_ptr<date_io_t>(new date_io_t("%Y-%m-%d", true)));
    readers.push_back(std::shared_ptr<date_io_t>(new date_io_t("%m-%d", true)));

    is_initialized = true;
  }
}

/// @brief Release all IO objects, clear caches, and reset the initialized flag.
void times_shutdown() {
  if (is_initialized) {
    input_datetime_io.reset();
    timelog_datetime_io.reset();
    written_datetime_io.reset();
    written_date_io.reset();
    printed_datetime_io.reset();
    printed_date_io.reset();

    readers.clear();

    temp_datetime_io.clear();

    temp_date_io.clear();

    is_initialized = false;
  }
}

/*--- Debug Utilities ---*/

/// @brief Tokenize a period expression and dump each token's type and value.
/// Used by the `period` debug command to inspect how a period expression
/// string is lexed.
void show_period_tokens(std::ostream& out, const string& arg) {
  date_parser_t::lexer_t lexer(arg.begin(), arg.end());

  out << _("--- Period expression tokens ---") << '\n';

  date_parser_t::lexer_t::token_t token;
  do {
    token = lexer.next_token();
    token.dump(out);
    out << ": " << token.to_string() << '\n';
  } while (token.kind != date_parser_t::lexer_t::token_t::END_REACHED);
}

} // namespace ledger
