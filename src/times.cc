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

int day_of_period = 0;

/*--- Date I/O Infrastructure ---*/

/**
 * The temporal_io_t template wraps strptime/strftime for a single format
 * string, paired with date_traits_t that record which date components
 * the format can express.  parse() has two specializations, for datetime_t
 * and for date_t; the date_t one picks the year of a format without one.
 *
 * Instances are kept in shared_ptrs and organized into:
 *   - input_datetime_io / timelog_datetime_io: the two built-in datetime readers
 *   - written_*_io / printed_*_io: output formatters for FMT_WRITTEN / FMT_PRINTED
 *   - readers: a deque of date_io_t tried in order by parse_date_mask()
 */
namespace {
/// @brief The year of a date written without one: the year directive's year
/// when one is active, otherwise today's year, or the year before when the
/// month is still ahead of today's.
int yearless_year(int month) {
  if (year_directive_year)
    return *year_directive_year;
  const date_t today = CURRENT_DATE();
  const int year = today.year();
  return month > today.month() ? year - 1 : year;
}

/// @brief The date parts a strftime/strptime format carries, read
/// case-sensitively from its conversion specifiers.
date_traits_t traits_of(const string& fmt) {
  date_traits_t traits;
  for (std::size_t i = 0; i + 1 < fmt.size(); ++i) {
    if (fmt[i] != '%')
      continue;
    char c = fmt[++i];
    if ((c == 'E' || c == 'O') && i + 1 < fmt.size()) // POSIX modifiers: %Ey, %Om
      c = fmt[++i];
    switch (c) {
    case 'D': // %m/%d/%y
    case 'F': // %Y-%m-%d
    case 'x': // the locale's date (%m/%d/%y in the C locale)
    case 'c': // the locale's date and time
      traits.has_year = traits.has_month = traits.has_day = true;
      break;
    case 'Y':
    case 'y':
      traits.has_year = true;
      break;
    case 'm':
    case 'b':
    case 'B':
    case 'h':
      traits.has_month = true;
      break;
    case 'd':
    case 'e':
      traits.has_day = true;
      break;
    default: // includes "%%", a literal percent sign
      break;
    }
  }
  return traits;
}

template <typename T>
class temporal_io_t : public noncopyable {
  string fmt_str; ///< The strptime/strftime format string.

public:
  date_traits_t traits; ///< Which date components this format can express.

  explicit temporal_io_t(const char* fmt) { set_format(fmt); }

  /// @brief Replace the format string and recompute traits.
  void set_format(const char* fmt) {
    fmt_str = fmt;
    traits = traits_of(fmt_str);
  }

  T parse(const char* str);

  /// @brief Format a temporal value using strftime.
  std::string format(const T& when) {
    std::tm data(to_tm(when));
    char buf[128];
    std::size_t len = std::strftime(buf, sizeof(buf), fmt_str.c_str(), &data);
    if (len != 0 || fmt_str.empty())
      return std::string(buf, len);
    // strftime returns 0 when the output does not fit (or is empty, e.g. a
    // lone %p in a locale without AM/PM): retry with larger buffers, bounded
    // well above any output this format can produce.
    for (std::size_t size = 256; size <= 128 * fmt_str.size() + 128; size *= 2) {
      std::string out(size, '\0');
      len = std::strftime(out.data(), size, fmt_str.c_str(), &data);
      if (len != 0) {
        out.resize(len);
        return out;
      }
    }
    return std::string();
  }
};

/**
 * @brief datetime_t parsing specialization.
 *
 * Uses strptime to parse the string into a std::tm, then converts to
 * a Boost ptime.  Returns a not-a-date-time sentinel on failure.
 */
template <>
datetime_t temporal_io_t<datetime_t>::parse(const char* str) {
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
 * Pre-fills tm_mday=1 for formats without %d (like "%Y/%m").  After
 * parsing, picks the year of a format without one (like "%m/%d"): the
 * year directive's year if active, otherwise today's year, or the
 * previous year when the month is still ahead.  The date is built once,
 * in that year, so a Feb 29 in a year without one is an error.
 */
template <>
date_t temporal_io_t<date_t>::parse(const char* str) {
  std::tm data;
  std::memset(&data, 0, sizeof(std::tm));
  data.tm_mday = 1; // some formats have no day
  if (!strptime(str, fmt_str.c_str(), &data))
    return date_t();
  if (!traits.has_year)
    data.tm_year = yearless_year(data.tm_mon + 1) - 1900;
  return gregorian::date_from_tm(data);
}

using datetime_io_t = temporal_io_t<datetime_t>;
using date_io_t = temporal_io_t<date_t>;

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
/// the custom format may use those characters literally.  It governs dates
/// only: parse_datetime() always rewrites the separators.
bool convert_separators_to_slashes = true;

/*--- Low-Level Date Parsing ---*/

/**
 * @brief Copy date or datetime input into a fixed buffer, normalizing
 *        its separators.
 *
 * Every date and datetime parser works on this copy, made once per call.
 * The buffer holds 127 bytes and a terminator: longer input is refused
 * here, before strptime sees it.  Error messages name the original
 * date_str, not buf, so the user sees the text as written.
 *
 * @param date_str    The text to parse, as the user wrote it.
 * @param buf         Receives a copy of date_str.
 * @param to_slashes  When true, every '.' and '-' in buf becomes '/', so
 *                    users can write 2024.01.15 or 2024-01-15.
 * @throws date_error if date_str does not fit in buf.
 */
void normalize_date_input(const char* date_str, char (&buf)[128], bool to_slashes) {
  if (std::strlen(date_str) >= sizeof(buf))
    throw_(date_error, _f("Invalid date: %1%") % date_str);

  std::strcpy(buf, date_str);

  if (to_slashes) {
    for (char* p = buf; *p; p++)
      if (*p == '.' || *p == '-')
        *p = '/';
  }
}

/**
 * @brief Try to parse a date string against a single format.
 *
 * This is the workhorse behind parse_date_mask().  It:
 *   1. Parses buf via the given io object.
 *   2. Round-trip validates: formats the parsed date back to a string
 *      and verifies it matches buf (catching invalid dates like
 *      "02/30").
 *   3. Reports the format's traits; the parse itself has already chosen
 *      the year of a yearless format (see temporal_io_t<date_t>::parse).
 *
 * @param date_str  The date string as written, named in error messages.
 * @param buf       date_str after normalize_date_input().
 * @param io        The format-specific IO object.
 * @param traits    If non-null, receives the format's date_traits_t.
 * @return          The parsed date, or not-a-date on failure.
 */
date_t parse_date_mask_routine(const char* date_str, const char* buf, date_io_t& io,
                               date_traits_t* traits = nullptr) {
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
      if (std::tolower(static_cast<unsigned char>(*p)) !=
              std::tolower(static_cast<unsigned char>(*q)) &&
          *p == '0')
        p++;
      if (!*p || std::tolower(static_cast<unsigned char>(*p)) !=
                     std::tolower(static_cast<unsigned char>(*q)))
        break;
    }
    if (*p != '\0' || *q != '\0')
      throw_(date_error, _f("Invalid date: %1%") % date_str);

    if (traits)
      *traits = io.traits;
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
  char buf[128];
  normalize_date_input(date_str, buf, convert_separators_to_slashes);

  for (std::shared_ptr<date_io_t>& reader : readers) {
    date_t when = parse_date_mask_routine(date_str, buf, *reader.get(), traits);
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
 * ("0" for Sunday through "6" for Saturday).  Names are matched without
 * regard to case, so "Monday", "MON" and "monday" all name the same day;
 * callers such as --start-of-week pass the user's text through verbatim.
 */
optional<date_time::weekdays> string_to_day_of_week(const std::string& str) {
  const string s = lowered(str);
  if (s == _("sun") || s == _("sunday") || s == "0")
    return gregorian::Sunday;
  else if (s == _("mon") || s == _("monday") || s == "1")
    return gregorian::Monday;
  else if (s == _("tue") || s == _("tuesday") || s == "2")
    return gregorian::Tuesday;
  else if (s == _("wed") || s == _("wednesday") || s == "3")
    return gregorian::Wednesday;
  else if (s == _("thu") || s == _("thursday") || s == "4")
    return gregorian::Thursday;
  else if (s == _("fri") || s == _("friday") || s == "5")
    return gregorian::Friday;
  else if (s == _("sat") || s == _("saturday") || s == "6")
    return gregorian::Saturday;
  else
    return none;
}

/**
 * @brief Map a month name (locale-aware, case-insensitive) to a Boost month.
 *
 * Accepts full names ("january") or abbreviations ("jan"), matched
 * without regard to case.
 */
optional<date_time::months_of_year> string_to_month_of_year(const std::string& str) {
  const string s = lowered(str);
  if (s == _("jan") || s == _("january"))
    return gregorian::Jan;
  else if (s == _("feb") || s == _("february"))
    return gregorian::Feb;
  else if (s == _("mar") || s == _("march"))
    return gregorian::Mar;
  else if (s == _("apr") || s == _("april"))
    return gregorian::Apr;
  else if (s == _("may"))
    return gregorian::May;
  else if (s == _("jun") || s == _("june"))
    return gregorian::Jun;
  else if (s == _("jul") || s == _("july"))
    return gregorian::Jul;
  else if (s == _("aug") || s == _("august"))
    return gregorian::Aug;
  else if (s == _("sep") || s == _("september"))
    return gregorian::Sep;
  else if (s == _("oct") || s == _("october"))
    return gregorian::Oct;
  else if (s == _("nov") || s == _("november"))
    return gregorian::Nov;
  else if (s == _("dec") || s == _("december"))
    return gregorian::Dec;
  else
    return none;
}

/*--- Top-Level Parse Functions ---*/

/**
 * @brief Parse a datetime string using the two built-in datetime formats.
 *
 * Tries "YYYY/MM/DD HH:MM:SS" first, then "MM/DD/YYYY HH:MM:SS"
 * (the timeclock format).  Separators `.` and `-` are normalized to `/`
 * before parsing.  Command-line options (--begin, --end, --now) use
 * try_parse_datetime() instead, which also accepts HH:MM and an '@'
 * separator.
 */
datetime_t parse_datetime(const char* str) {
  // Always rewrite separators, even under --input-date-format.
  char buf[128];
  normalize_date_input(str, buf, true);

  datetime_t when = input_datetime_io->parse(buf);
  if (when.is_not_a_date_time()) {
    when = timelog_datetime_io->parse(buf);
    if (when.is_not_a_date_time()) {
      throw_(date_error, _f("Invalid date/time: %1%") % str);
    }
  }
  return when;
}

/**
 * @brief Attempt to parse a datetime string, returning none on failure.
 *
 * Non-throwing parser for inputs that may carry a time component.  A datetime
 * is recognized structurally as `<date><sep><time>`, where `sep` is a space
 * or `@` (the latter avoids shell quoting in forms like "2018/12/16@08:12").
 * Strings without such a separator -- plain dates, one-word period
 * expressions and relative-date keywords -- return none at once; a
 * multi-word period expression such as "last month" has a space, fails the
 * date parse and returns none through the catch.  The time of day must lie
 * in [00:00, 24:00): duration_from_string accepts any duration, and a
 * larger or negative one would shift the date.  When a separator is
 * present, the date and time portions are parsed independently and
 * combined.
 *
 * This grammar serves command-line options only; journal and expression
 * datetimes (the P directive, timeclock entries, to_datetime()) go through
 * parse_datetime(), which accepts just its two fixed formats.
 */
optional<datetime_t> try_parse_datetime(const std::string& str) {
  if (str.size() > 127)
    return none;

  std::string::size_type sep = str.find_first_of(" @");
  if (sep == std::string::npos)
    return none;

  try {
    date_t day = parse_date(str.substr(0, sep).c_str());
    time_duration_t tod = posix_time::duration_from_string(str.substr(sep + 1));
    if (tod.is_negative() || tod >= posix_time::hours(24))
      return none;
    return datetime_t(day, tod);
  } catch (const std::exception&) { // NOLINT(bugprone-empty-catch)
    return none;
  }
}

/// @brief Parse a date string using the registered format cascade.
date_t parse_date(const char* str) {
  return parse_date_mask(str);
}

date_t clamped_date(int year, int month, int day) {
  const int last = gregorian::gregorian_calendar::end_of_month_day(
      static_cast<unsigned short>(year), static_cast<unsigned short>(month));
  return date_t(static_cast<unsigned short>(year), static_cast<unsigned short>(month),
                static_cast<unsigned short>(std::min(day, last)));
}

/**
 * @brief Parse a date string that may be a formatted date or a relative
 *        date expression.
 *
 * First delegates to parse_date() (which tries each registered date format).
 * If that fails, the string is handed to date_interval_t, which supports the
 * full period-expression grammar: `today`/`yesterday`/`tomorrow`, `this`/
 * `next`/`last <unit>`, `<N> <unit> ago/hence`, and so on.  Only the interval's
 * begin date is returned; callers interested in ranges should use
 * date_interval_t directly.
 */
date_t parse_date_expr(const std::string& str) {
  try {
    return parse_date(str);
  } catch (const date_error&) { // NOLINT(bugprone-empty-catch)
    // Fall through to the richer period-expression grammar.
  }

  date_interval_t interval(str);
  if (optional<date_t> when = interval.begin())
    return *when;

  throw_(date_error, _f("Invalid date: %1%") % str);
  return date_t();
}

/*--- Date Specifier Resolution ---*/

namespace {

/// Whether month/day exists in year (false for Feb 29 outside a leap year
/// and for a day past the month's end such as 04/31).
bool day_exists(int year, date_t::month_type month, date_t::day_type day) {
  return day <= gregorian::gregorian_calendar::end_of_month_day(
                    static_cast<date_t::year_type>(year), month);
}

/// The nearest occurrence of month/day on or before `reference`, skipping
/// years in which the day does not exist (Feb 29).
date_t nearest_occurrence(date_t::month_type month, date_t::day_type day, const date_t& reference) {
  for (int year = reference.year(), tries = 0; tries < 9; ++tries, --year) {
    if (day_exists(year, month, day)) {
      date_t when(static_cast<date_t::year_type>(year), month, day);
      if (when <= reference)
        return when;
    }
  }
  // No such day in any year (e.g. 04/31): let date_t report it.
  return date_t(reference.year(), month, day);
}

} // namespace

/**
 * @brief Resolve a possibly-partial specifier to its earliest concrete date.
 *
 * Fills in missing components with the earliest valid value:
 *   - year: when absent and a month is given, the year of the most recent
 *     occurrence on or before CURRENT_DATE() (which honours --now); when
 *     month is absent too, the current year
 *   - month defaults to January
 *   - day defaults to the 1st
 *
 * Day and weekday are mutually exclusive (asserted).  A weekday alone (no
 * year, month or day) names its most recent occurrence on or before
 * CURRENT_DATE(), which honours --now; a weekday with a year or a month is
 * not consulted.
 */
date_t date_specifier_t::begin() const {
  year_type the_year = year ? *year : year_type(CURRENT_DATE().year());
  month_type the_month = month ? *month : date_t::month_type(1);
  day_type the_day = day ? *day : date_t::day_type(1);

  if (day)
    assert(!wday);
  else if (wday)
    assert(!day);

  if (month && !year)
    return nearest_occurrence(*month, the_day, CURRENT_DATE());

  if (wday && !year && !month && !day) {
    date_t when = CURRENT_DATE();
    while (when.day_of_week() != *wday)
      when -= gregorian::days(1);
    return when;
  }

  return gregorian::date(static_cast<date_t::year_type>(the_year),
                         static_cast<date_t::month_type>(the_month),
                         static_cast<date_t::day_type>(the_day));
}

date_specifier_t date_specifier_t::with_year_on_or_after(const date_t& anchor,
                                                         bool inclusive) const {
  if (year || !month)
    return *this;
  date_specifier_t spec(*this);
  const day_type first_day = day ? *day : day_type(1);
  for (int y = anchor.year(), tries = 0; tries < 9; ++tries, ++y) {
    if (!day_exists(y, *month, first_day))
      continue;
    spec.year = year_type(y);
    if (inclusive ? spec.end() > anchor : spec.begin() >= anchor)
      return spec;
  }
  // No such day in any year (e.g. 04/31): let begin() report it.
  spec.year = year_type(anchor.year());
  return spec;
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
  return out << duration.to_string();
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
  friend optional<date_t> string_to_relative_date(const std::string& str);

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
   *     today/tday, tomorrow/tmrw, yesterday/yday
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

        TOK_TODAY,     ///< The keyword "today" or "tday".
        TOK_TOMORROW,  ///< The keyword "tomorrow" or "tmrw".
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

      /// @brief The name and canonical text of one token kind.
      struct kind_info_t {
        kind_t kind;      ///< The kind this row describes (its index in kinds[]).
        const char* name; ///< The enumerator's spelling, printed by dump().
        /// The canonical keyword or punctuation printed by to_string(), or
        /// nullptr when the token carries a payload that to_string() prints.
        const char* text;
      };

      /// @brief One row per kind_t, in enum order, indexed by kind.
      static constexpr kind_info_t kinds[] = {
          {UNKNOWN, "UNKNOWN", nullptr},
          {TOK_DATE, "TOK_DATE", nullptr},
          {TOK_INT, "TOK_INT", nullptr},
          {TOK_SLASH, "TOK_SLASH", "/"},
          {TOK_DASH, "TOK_DASH", "-"},
          {TOK_DOT, "TOK_DOT", "."},
          {TOK_A_MONTH, "TOK_A_MONTH", nullptr},
          {TOK_A_WDAY, "TOK_A_WDAY", nullptr},
          {TOK_AGO, "TOK_AGO", "ago"},
          {TOK_HENCE, "TOK_HENCE", "hence"},
          {TOK_SINCE, "TOK_SINCE", "since"},
          {TOK_UNTIL, "TOK_UNTIL", "until"},
          {TOK_IN, "TOK_IN", "in"},
          {TOK_THIS, "TOK_THIS", "this"},
          {TOK_NEXT, "TOK_NEXT", "next"},
          {TOK_LAST, "TOK_LAST", "last"},
          {TOK_EVERY, "TOK_EVERY", "every"},
          {TOK_TODAY, "TOK_TODAY", "today"},
          {TOK_TOMORROW, "TOK_TOMORROW", "tomorrow"},
          {TOK_YESTERDAY, "TOK_YESTERDAY", "yesterday"},
          {TOK_YEAR, "TOK_YEAR", "year"},
          {TOK_QUARTER, "TOK_QUARTER", "quarter"},
          {TOK_MONTH, "TOK_MONTH", "month"},
          {TOK_WEEK, "TOK_WEEK", "week"},
          {TOK_DAY, "TOK_DAY", "day"},
          {TOK_YEARLY, "TOK_YEARLY", "yearly"},
          {TOK_QUARTERLY, "TOK_QUARTERLY", "quarterly"},
          {TOK_BIMONTHLY, "TOK_BIMONTHLY", "bimonthly"},
          {TOK_MONTHLY, "TOK_MONTHLY", "monthly"},
          {TOK_BIWEEKLY, "TOK_BIWEEKLY", "biweekly"},
          {TOK_WEEKLY, "TOK_WEEKLY", "weekly"},
          {TOK_DAILY, "TOK_DAILY", "daily"},
          {TOK_YEARS, "TOK_YEARS", "years"},
          {TOK_QUARTERS, "TOK_QUARTERS", "quarters"},
          {TOK_MONTHS, "TOK_MONTHS", "months"},
          {TOK_WEEKS, "TOK_WEEKS", "weeks"},
          {TOK_DAYS, "TOK_DAYS", "days"},
          {END_REACHED, "END_REACHED", "<EOF>"},
      };
      static_assert(std::size(kinds) == END_REACHED + 1, "kinds[] needs one row per kind_t");

      /// @brief True iff every row of kinds[] sits at the index of its kind.
      static constexpr bool kinds_in_order() {
        for (std::size_t i = 0; i < std::size(kinds); ++i)
          if (static_cast<std::size_t>(kinds[i].kind) != i)
            return false;
        return true;
      }

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
      ///
      /// Kinds that carry a payload print it; every other kind prints its
      /// canonical text from kinds[].
      string to_string() const {
        std::ostringstream out;

        switch (kind) {
        case UNKNOWN:
          return std::get<string>(*value);
        case TOK_DATE:
          return std::get<date_specifier_t>(*value).to_string();
        case TOK_INT:
          out << std::get<unsigned short>(*value);
          break;
        case TOK_A_MONTH: // NOLINT(bugprone-branch-clone)
          out << date_specifier_t::month_type(std::get<date_time::months_of_year>(*value));
          break;
        case TOK_A_WDAY:
          out << date_specifier_t::day_of_week_type(std::get<date_time::weekdays>(*value));
          break;
        default:
          return kinds[kind].text;
        }

        return out.str();
      }

      /// @brief Write the token's kind_t name for debug output.
      void dump(std::ostream& out) const {
        // Checked here, not beside the size assert: a member function body
        // is a complete-class context, where kinds_in_order() is defined.
        static_assert(kinds_in_order(), "kinds[] rows must follow kind_t order");
        out << kinds[kind].name;
      }

      /// @brief Throw a date_error for an unexpected token.
      void unexpected();
      /// @brief Throw a date_error for an unexpected character.
      [[noreturn]] static void expected(char wanted, char c = '\0');
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

  /// @brief Handle a date, integer, month, weekday or day-keyword token as an
  /// inclusion specifier.
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

namespace {

/**
 * @brief The first day of the quarter or week after the one that begins
 * at boundary, on the grid date_duration_t::find_nearest() defines.
 *
 * A quarter of that grid is 90-92 days long and a week 7, so the date 92
 * (or 7) days after a boundary lies in the following period.
 */
date_t boundary_after(const date_t& boundary, date_duration_t::skip_quantum_t quantum) {
  const int longest = quantum == date_duration_t::QUARTERS ? 92 : 7;
  return date_duration_t::find_nearest(boundary + gregorian::days(longest), quantum);
}

/**
 * @brief The [begin, end) of "last", "this" or "next" quarter or week.
 *
 * adjust is -1, 0 or 1.  "this" is the grid period that contains today;
 * both ends of every span are grid boundaries, so adjacent spans meet.
 */
std::pair<date_t, date_t> relative_span(int adjust, date_duration_t::skip_quantum_t quantum,
                                        const date_t& today) {
  const date_t base = date_duration_t::find_nearest(today, quantum);
  if (adjust < 0)
    return {date_duration_t::find_nearest(base - gregorian::days(1), quantum), base};
  const date_t begin = adjust > 0 ? boundary_after(base, quantum) : base;
  return {begin, boundary_after(begin, quantum)};
}

} // namespace

/*--- determine_when: Token-to-Specifier Resolution ---*/

void date_parser_t::determine_when(date_parser_t::lexer_t::token_t& tok,
                                   date_specifier_t& specifier) {
  date_t today = CURRENT_DATE();

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
      while (temp.day_of_week() != std::get<date_time::weekdays>(*tok.value))
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

    case lexer_t::token_t::TOK_QUARTER:
      // "this quarter" / "next quarter" / "last quarter"
      specifier = date_specifier_t(relative_span(adjust, date_duration_t::QUARTERS, today).first);
      break;

    case lexer_t::token_t::TOK_WEEK:
      // "this week" / "next week" / "last week"
      specifier = date_specifier_t(relative_span(adjust, date_duration_t::WEEKS, today).first);
      break;

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

/// @brief Handle date/integer/month/weekday/day-keyword tokens as inclusion specifiers.
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
 * @brief Handle "this"/"next"/"last" in the main parsing loop.
 *
 * A count gives a range from today ("last 3 months" -> since = today - 3
 * months, until = today; "next 2 weeks" -> since = today, until = today +
 * 2 weeks).  A quarter or week gives since/until bounding that whole
 * period (relative_span).  Any other unit ("next january", "last monday",
 * "this year", "next day", "last month") resolves through determine_when
 * to the inclusion specifier, the same date that since/until/in use.
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

  switch (lexer.peek_token().kind) {
  case lexer_t::token_t::TOK_INT: {
    tok = lexer.next_token();
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

  case lexer_t::token_t::TOK_QUARTER:
  case lexer_t::token_t::TOK_WEEK: {
    // "this quarter" / "last week": since/until bound the whole period.
    tok = lexer.next_token();
    const auto [begin, end] =
        relative_span(adjust,
                      tok.kind == lexer_t::token_t::TOK_QUARTER ? date_duration_t::QUARTERS
                                                                : date_duration_t::WEEKS,
                      today);
    since_specifier = date_specifier_t(begin);
    until_specifier = date_specifier_t(end);
    break;
  }

  default:
    // A month or weekday name, year, month or day ("next january", "last
    // monday", "this year"): the same date or year-month that since, until
    // and in resolve to.
    inclusion_specifier = date_specifier_t();
    determine_when(tok, *inclusion_specifier);
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
    if (quantity == 0)
      throw_(date_error, _f("Invalid period length: %1%") % quantity);
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
  date_t today = CURRENT_DATE();
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
    case lexer_t::token_t::TOK_TODAY:
    case lexer_t::token_t::TOK_TOMORROW:
    case lexer_t::token_t::TOK_YESTERDAY:
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
 * @brief Resolve a whole string that is one day keyword to its date.
 *
 * The keywords are the period lexer's today/tday, tomorrow/tmrw and
 * yesterday/yday, matched without regard to case, so the xact command's
 * positional date accepts the same words as --begin, --period and `on`.
 */
optional<date_t> string_to_relative_date(const std::string& str) {
  try {
    date_parser_t::lexer_t lexer(str.begin(), str.end());
    switch (lexer.next_token().kind) {
    case date_parser_t::lexer_t::token_t::TOK_TODAY:
    case date_parser_t::lexer_t::token_t::TOK_TOMORROW:
    case date_parser_t::lexer_t::token_t::TOK_YESTERDAY:
      if (lexer.next_token().kind == date_parser_t::lexer_t::token_t::END_REACHED)
        return date_interval_t(str).begin();
      break;
    default:
      break;
    }
  } catch (const std::exception&) { // NOLINT(bugprone-empty-catch)
    // The lexer rejects the text (e.g. "today," or a number too large for a
    // day or year), so it is not a day keyword.
  }
  return none;
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
    end_of_duration = next_boundary(*start);
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

/**
 * @brief The start of the period after the one starting at boundary.
 *
 * Days and weeks step by the duration.  Months, quarters and years step
 * from the first day of the calendar month the boundary belongs to (a
 * first of the month never snaps), then keep anchor_day, clamped to the
 * month's length, and add anchor_shift days.
 */
date_t date_interval_t::next_boundary(const date_t& boundary) const {
  int months;
  switch (duration->quantum) {
  case date_duration_t::MONTHS:
    months = duration->length;
    break;
  case date_duration_t::QUARTERS:
    months = 3 * duration->length;
    break;
  case date_duration_t::YEARS:
    months = 12 * duration->length;
    break;
  default:
    return duration->add(boundary);
  }
  const date_t base = boundary - gregorian::days(anchor_shift);
  const date_t first = date_t(base.year(), base.month(), 1) + gregorian::months(months);
  return clamped_date(first.year(), first.month(), anchor_day) + gregorian::days(anchor_shift);
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
 *
 * With --period-shift N the boundary is the calendar boundary plus N days.
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

  if (day_of_period != 0 && skip != date_duration_t::WEEKS && skip != date_duration_t::DAYS) {
    // Each period starts day_of_period days after its calendar start.
    // Back up whole calendar periods (whose first day never snaps) until
    // that boundary is on or before date; for a shift of 28 days or more
    // one step may not be enough.
    const int months = skip == date_duration_t::YEARS      ? 12
                       : skip == date_duration_t::QUARTERS ? 3
                                                           : 1;
    date_t base = result;
    result = base + gregorian::days(day_of_period);
    while (result > date) {
      base -= gregorian::months(months);
      result = base + gregorian::days(day_of_period);
    }
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
 *   1. Without an explicit since date, finds the natural period boundary
 *      (via find_nearest) for the duration's quantum that contains or
 *      precedes the reference date.
 *
 *   2. With an explicit since date (weeks, months, quarters and years),
 *      starts at the reference date when align_intervals is set, and
 *      otherwise at the since date advanced by whole intervals, keeping
 *      the user's day-of-period.
 *
 *   3. For weeks without a since date, starts from a point sufficiently
 *      far back that the subsequent scan will find the correct period.
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

      // Phase 1: Choose the first period start and the anchor later
      // boundaries keep.
      DEBUG("times.interval", "stabilize: quantum = " << *duration);
      if (duration->quantum == date_duration_t::DAYS) {
        // Days are their own quantum: the reference date is a boundary.
        start = when;
      } else if (align_intervals && since_specified) {
        start = when;
        anchor_day = when.day();
      } else if (since_specified && initial_start) {
        // An explicit since/from date (e.g. "every 1 months from
        // 2023-01-15") anchors the periods: advance from it by whole
        // intervals to the one containing or immediately preceding when.
        start = *initial_start;
        anchor_day = initial_start->day();
        for (date_t next = next_boundary(*start); next <= when; next = next_boundary(*start))
          start = next;
      } else if (duration->quantum == date_duration_t::WEEKS) {
        // Back up one period plus (400 mod period) days and snap to the
        // week start; the forward scan below then finds the period that
        // contains the date.
        int period = duration->length * 7;
        start = date_duration_t::find_nearest(when - gregorian::days(period + 400 % period),
                                              duration->quantum);
      } else {
        // Months, quarters and years start on their calendar boundary,
        // shifted by --period-shift.
        start = date_duration_t::find_nearest(when, duration->quantum);
        anchor_day = 1;
        anchor_shift = day_of_period;
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
        // Compute end_of_duration from the quantum-aligned start before
        // clamping, so that weekly/monthly periods keep their natural
        // period boundary.
        resolve_end();

        start = initial_start;

        // If end_of_duration (computed from the old start) is no longer
        // past the clamped start, it is stale and must be recomputed.
        if (end_of_duration && *end_of_duration <= *start) {
          end_of_duration = none;
          next = none;
          // The clamped start is off the period grid: later periods step from it.
          anchor_day = start->day();
          anchor_shift = 0;
          resolve_end();
        }

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

    scan = next_boundary(scan);
    end_of_scan = next_boundary(scan);

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
    end_of_duration = next_boundary(*start);
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

  // Phase 2: If the first character is a digit, try parsing the whole
  // argument as a date using the typical date formats.  This allows not
  // only dates like "2009/08/01", but also dates that fit the user's
  // --input-date-format, assuming their format fits in one argument and
  // begins with a digit.
  if (std::isdigit(static_cast<unsigned char>(*begin))) {
    string::const_iterator i = begin;
    for (i = begin; i != end && !std::isspace(static_cast<unsigned char>(*i)); i++) {}
    assert(i != begin);

    string possible_date(begin, i); // NOLINT(bugprone-unused-local-non-trivial-variable)

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

  // Phase 3: Collect an alphanumeric (or non-alphanumeric) word and
  // match against known tokens.
  string term;
  bool alnum = std::isalnum(static_cast<unsigned char>(*begin));
  for (; (begin != end && !std::isspace(static_cast<unsigned char>(*begin)) &&
          ((alnum && static_cast<bool>(std::isalnum(static_cast<unsigned char>(*begin)))) ||
           (!alnum && !static_cast<bool>(std::isalnum(static_cast<unsigned char>(*begin))))));
       begin++)
    term.push_back(*begin);

  assert(!term.empty());
  if (std::isdigit(static_cast<unsigned char>(term[0]))) {
    // Bare integer (not a full date)
    try {
      return token_t(token_t::TOK_INT, token_t::content_t(lexical_cast<unsigned short>(term)));
    } catch (const bad_lexical_cast&) {
      throw_(date_error, _f("Invalid number in period: %1%") % term);
    }
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
    else if (term == _("today") || term == _("tday"))
      return token_t(token_t::TOK_TODAY);
    else if (term == _("tomorrow") || term == _("tmrw"))
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

/**
 * @brief Format a date or datetime value as a string.
 *
 * Dispatches to the appropriate IO object based on format_type:
 *   - FMT_WRITTEN: the written IO, for serialization
 *   - FMT_PRINTED: the printed IO, for display
 *   - FMT_CUSTOM: a caller-supplied strftime format, with its IO object
 *     created on first use and kept in cache, keyed by the format string
 *
 * written and printed are taken as pointers and dereferenced only on
 * their own branch, so a FMT_CUSTOM call never touches them.
 */
template <typename T, typename IO>
std::string format_temporal(const T& when, const format_type_t format_type,
                            const optional<const char*>& format, const std::shared_ptr<IO>& written,
                            const std::shared_ptr<IO>& printed,
                            std::map<std::string, std::unique_ptr<IO>>& cache) {
  // NOLINTNEXTLINE(bugprone-branch-clone)
  if (format_type == FMT_WRITTEN) {
    return written->format(when);
  } else if (format_type == FMT_CUSTOM && format) {
    if (auto i = cache.find(*format); i != cache.end()) {
      return (*i).second->format(when);
    } else {
      auto& stored = (cache[*format] = std::make_unique<IO>(*format));
      return stored->format(when);
    }
  } else if (format_type == FMT_PRINTED) {
    return printed->format(when);
  } else {
    assert(false);
    return empty_string;
  }
}
} // namespace

/// @brief Format a datetime; FMT_WRITTEN is "YYYY/MM/DD HH:MM:SS",
/// FMT_PRINTED "YY-Mon-DD HH:MM:SS" (see format_temporal()).
std::string format_datetime(const datetime_t& when, const format_type_t format_type,
                            const optional<const char*>& format) {
  return format_temporal(when, format_type, format, written_datetime_io, printed_datetime_io,
                         temp_datetime_io);
}

/// @brief Format a date; FMT_WRITTEN is "YYYY/MM/DD", FMT_PRINTED
/// "YY-Mon-DD" (see format_temporal()).
std::string format_date(const date_t& when, const format_type_t format_type,
                        const optional<const char*>& format) {
  return format_temporal(when, format_type, format, written_date_io, printed_date_io, temp_date_io);
}

/*--- Format Configuration ---*/

namespace {
bool is_initialized = false; ///< Guard for times_initialize/times_shutdown.
} // namespace

/// @brief Override both the written and printed datetime format.
void set_datetime_format(const char* format) {
  written_datetime_io->set_format(format);
  printed_datetime_io->set_format(format);
}

/// @brief Override both the written and printed date format.
void set_date_format(const char* format) {
  written_date_io->set_format(format);
  printed_date_io->set_format(format);
}

/**
 * @brief Register a custom input date format at the highest priority.
 *
 * Pushes a new reader to the front of the format cascade, so it is
 * tried before all built-in formats.  Also disables separator
 * normalization, since custom formats may use '.' or '-' literally.
 */
void set_input_date_format(const char* format) {
  readers.push_front(std::shared_ptr<date_io_t>(new date_io_t(format)));
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
    input_datetime_io.reset(new datetime_io_t("%Y/%m/%d %H:%M:%S"));
    timelog_datetime_io.reset(new datetime_io_t("%m/%d/%Y %H:%M:%S"));

    written_datetime_io.reset(new datetime_io_t("%Y/%m/%d %H:%M:%S"));
    written_date_io.reset(new date_io_t("%Y/%m/%d"));

    printed_datetime_io.reset(new datetime_io_t("%y-%b-%d %H:%M:%S"));
    printed_date_io.reset(new date_io_t("%y-%b-%d"));

    readers.push_back(std::shared_ptr<date_io_t>(new date_io_t("%m/%d")));
    readers.push_back(std::shared_ptr<date_io_t>(new date_io_t("%Y/%m/%d")));
    readers.push_back(std::shared_ptr<date_io_t>(new date_io_t("%Y/%m")));
    readers.push_back(std::shared_ptr<date_io_t>(new date_io_t("%y/%m/%d")));
    readers.push_back(std::shared_ptr<date_io_t>(new date_io_t("%Y-%m-%d")));
    readers.push_back(std::shared_ptr<date_io_t>(new date_io_t("%m-%d")));

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
