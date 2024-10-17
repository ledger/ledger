/*
 * Copyright (c) 2003-2023, John Wiegley.  All rights reserved.
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

#include <system.hh>

#include "times.h"

#if defined(_WIN32) || defined(__CYGWIN__)
#include "strptime.h"
#endif

namespace ledger {

optional<datetime_t> epoch;

date_time::weekdays start_of_week = gregorian::Sunday;

namespace {
  template <typename T, typename InputFacetType, typename OutputFacetType>
  class temporal_io_t : public noncopyable
  {
    string fmt_str;

  public:
    date_traits_t traits;
    bool input;

    temporal_io_t(const char * _fmt_str, bool _input)
      : fmt_str(_fmt_str),
        traits(icontains(fmt_str, "%F") || icontains(fmt_str, "%y"),
               icontains(fmt_str, "%F") || icontains(fmt_str, "%m") || icontains(fmt_str, "%b"),
               icontains(fmt_str, "%F") || icontains(fmt_str, "%d")),
        input(_input) {
    }

    void set_format(const char * fmt) {
      fmt_str  = fmt;
      traits   = date_traits_t(icontains(fmt_str, "%F") || icontains(fmt_str, "%y"),
                               icontains(fmt_str, "%F") ||
                               icontains(fmt_str, "%m") || icontains(fmt_str, "%b"),
                               icontains(fmt_str, "%F") || icontains(fmt_str, "%d"));
    }

    T parse(const char *) {}

    std::string format(const T& when) {
      std::tm data(to_tm(when));
      char buf[128];
      std::strftime(buf, 127, fmt_str.c_str(), &data);
      return buf;
    }
  };

  template <>
  datetime_t temporal_io_t<datetime_t, posix_time::time_input_facet,
                           posix_time::time_facet>
    ::parse(const char * str)
  {
    std::tm data;
    std::memset(&data, 0, sizeof(std::tm));
    if (strptime(str, fmt_str.c_str(), &data))
      return posix_time::ptime_from_tm(data);
    else
      return datetime_t();
  }

  template <>
  date_t temporal_io_t<date_t, gregorian::date_input_facet,
                       gregorian::date_facet>
    ::parse(const char * str)
  {
    std::tm data;
    std::memset(&data, 0, sizeof(std::tm));
    data.tm_year = CURRENT_DATE().year() - 1900;
    data.tm_mday = 1;           // some formats have no day
    if (strptime(str, fmt_str.c_str(), &data))
      return gregorian::date_from_tm(data);
    else
      return date_t();
  }

  typedef temporal_io_t<datetime_t, posix_time::time_input_facet,
                        posix_time::time_facet> datetime_io_t;
  typedef temporal_io_t<date_t, gregorian::date_input_facet,
                        gregorian::date_facet> date_io_t;

  shared_ptr<datetime_io_t> input_datetime_io;
  shared_ptr<datetime_io_t> timelog_datetime_io;
  shared_ptr<datetime_io_t> written_datetime_io;
  shared_ptr<date_io_t>     written_date_io;
  shared_ptr<datetime_io_t> printed_datetime_io;
  shared_ptr<date_io_t>     printed_date_io;

  std::deque<shared_ptr<date_io_t> > readers;

  bool convert_separators_to_slashes = true;

  date_t parse_date_mask_routine(const char * date_str, date_io_t& io,
                                 date_traits_t * traits = NULL)
  {
    if (std::strlen(date_str) > 127) {
        throw_(date_error, _f("Invalid date: %1%") % date_str);
    }

    char buf[128];
    std::strcpy(buf, date_str);

    if (convert_separators_to_slashes) {
      for (char * p = buf; *p; p++)
        if (*p == '.' || *p == '-')
          *p = '/';
    }

    date_t when = io.parse(buf);

    if (! when.is_not_a_date()) {
      DEBUG("times.parse", "Passed date string:  " << date_str);
      DEBUG("times.parse", "Parsed date string:  " << buf);
      DEBUG("times.parse", "Parsed result is:    " << when);
      DEBUG("times.parse", "Formatted result is: " << io.format(when));

      string when_str = io.format(when);

      const char * p = when_str.c_str();
      const char * q = buf;
      for (; *p && *q; p++, q++) {
        if (*p != *q && *p == '0') p++;
        if (! *p || *p != *q) break;
      }
      if (*p != '\0' || *q != '\0')
        throw_(date_error, _f("Invalid date: %1%") % date_str);

      if (traits)
        *traits = io.traits;

      if (! io.traits.has_year) {
        when = date_t(CURRENT_DATE().year(), when.month(), when.day());

        if (when.month() > CURRENT_DATE().month())
          when -= gregorian::years(1);
      }
    }
    return when;
  }

  date_t parse_date_mask(const char * date_str, date_traits_t * traits = NULL)
  {
    foreach (shared_ptr<date_io_t>& reader, readers) {
      date_t when = parse_date_mask_routine(date_str, *reader.get(), traits);
      if (! when.is_not_a_date())
        return when;
    }

    throw_(date_error, _f("Invalid date: %1%") % date_str);
    return date_t();
  }
}

optional<date_time::weekdays> string_to_day_of_week(const std::string& str)
{
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

optional<date_time::months_of_year>
string_to_month_of_year(const std::string& str)
{
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

datetime_t parse_datetime(const char * str)
{
  if (std::strlen(str) > 127) {
    throw_(date_error, _f("Invalid date: %1%") % str);
  }

  char buf[128];
  std::strcpy(buf, str);

  for (char * p = buf; *p; p++)
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

date_t parse_date(const char * str)
{
  return parse_date_mask(str);
}

date_t date_specifier_t::begin() const
{
  year_type  the_year  = year  ? *year  : year_type(CURRENT_DATE().year());
  month_type the_month = month ? *month : date_t::month_type(1);
  day_type   the_day   = day   ? *day   : date_t::day_type(1);

  if (day)
    assert(! wday);
  else if (wday)
    assert(! day);

  // jww (2009-11-16): Handle wday.  If a month is set, find the most recent
  // wday in that month; if the year is set, then in that year.

  return gregorian::date(static_cast<date_t::year_type>(the_year),
                         static_cast<date_t::month_type>(the_month),
                         static_cast<date_t::day_type>(the_day));
}

date_t date_specifier_t::end() const
{
  if (day || wday)
    return begin() + gregorian::days(1);
  else if (month)
    return begin() + gregorian::months(1);
  else if (year)
    return begin() + gregorian::years(1);
  else {
    assert(false);
    return date_t();
  }
}

std::ostream& operator<<(std::ostream& out,
                         const date_duration_t& duration)
{
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

        TOK_A_MONTH,
        TOK_A_WDAY,

        TOK_AGO,
        TOK_HENCE,
        TOK_SINCE,
        TOK_UNTIL,
        TOK_IN,
        TOK_THIS,
        TOK_NEXT,
        TOK_LAST,
        TOK_EVERY,

        TOK_TODAY,
        TOK_TOMORROW,
        TOK_YESTERDAY,

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

      typedef variant<unsigned short,
                      string,
                      date_specifier_t::year_type,
                      date_time::months_of_year,
                      date_time::weekdays,
                      date_specifier_t> content_t;

      optional<content_t> value;

      explicit token_t(kind_t _kind = UNKNOWN,
                       const optional<content_t>& _value =
                       content_t(empty_string))
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
        std::ostringstream out;

        switch (kind) {
        case UNKNOWN:
          out << boost::get<string>(*value);
          break;
        case TOK_DATE:
          return boost::get<date_specifier_t>(*value).to_string();
        case TOK_INT:
          out << boost::get<unsigned short>(*value);
          break;
        case TOK_SLASH:     return "/";
        case TOK_DASH:      return "-";
        case TOK_DOT:       return ".";
        case TOK_A_MONTH:
          out << date_specifier_t::month_type
            (boost::get<date_time::months_of_year>(*value));
          break;
        case TOK_A_WDAY:
          out << date_specifier_t::day_of_week_type
            (boost::get<date_time::weekdays>(*value));
          break;
        case TOK_AGO:       return "ago";
        case TOK_HENCE:     return "hence";
        case TOK_SINCE:     return "since";
        case TOK_UNTIL:     return "until";
        case TOK_IN:        return "in";
        case TOK_THIS:      return "this";
        case TOK_NEXT:      return "next";
        case TOK_LAST:      return "last";
        case TOK_EVERY:     return "every";
        case TOK_TODAY:     return "today";
        case TOK_TOMORROW:  return "tomorrow";
        case TOK_YESTERDAY: return "yesterday";
        case TOK_YEAR:      return "year";
        case TOK_QUARTER:   return "quarter";
        case TOK_MONTH:     return "month";
        case TOK_WEEK:      return "week";
        case TOK_DAY:       return "day";
        case TOK_YEARLY:    return "yearly";
        case TOK_QUARTERLY: return "quarterly";
        case TOK_BIMONTHLY: return "bimonthly";
        case TOK_MONTHLY:   return "monthly";
        case TOK_BIWEEKLY:  return "biweekly";
        case TOK_WEEKLY:    return "weekly";
        case TOK_DAILY:     return "daily";
        case TOK_YEARS:     return "years";
        case TOK_QUARTERS:  return "quarters";
        case TOK_MONTHS:    return "months";
        case TOK_WEEKS:     return "weeks";
        case TOK_DAYS:      return "days";
        case END_REACHED:   return "<EOF>";
        }

        return out.str();
      }

      void dump(std::ostream& out) const {
        switch (kind) {
        case UNKNOWN:       out << "UNKNOWN"; break;
        case TOK_DATE:      out << "TOK_DATE"; break;
        case TOK_INT:       out << "TOK_INT"; break;
        case TOK_SLASH:     out << "TOK_SLASH"; break;
        case TOK_DASH:      out << "TOK_DASH"; break;
        case TOK_DOT:       out << "TOK_DOT"; break;
        case TOK_A_MONTH:   out << "TOK_A_MONTH"; break;
        case TOK_A_WDAY:    out << "TOK_A_WDAY"; break;
        case TOK_AGO:       out << "TOK_AGO"; break;
        case TOK_HENCE:     out << "TOK_HENCE"; break;
        case TOK_SINCE:     out << "TOK_SINCE"; break;
        case TOK_UNTIL:     out << "TOK_UNTIL"; break;
        case TOK_IN:        out << "TOK_IN"; break;
        case TOK_THIS:      out << "TOK_THIS"; break;
        case TOK_NEXT:      out << "TOK_NEXT"; break;
        case TOK_LAST:      out << "TOK_LAST"; break;
        case TOK_EVERY:     out << "TOK_EVERY"; break;
        case TOK_TODAY:     out << "TOK_TODAY"; break;
        case TOK_TOMORROW:  out << "TOK_TOMORROW"; break;
        case TOK_YESTERDAY: out << "TOK_YESTERDAY"; break;
        case TOK_YEAR:      out << "TOK_YEAR"; break;
        case TOK_QUARTER:   out << "TOK_QUARTER"; break;
        case TOK_MONTH:     out << "TOK_MONTH"; break;
        case TOK_WEEK:      out << "TOK_WEEK"; break;
        case TOK_DAY:       out << "TOK_DAY"; break;
        case TOK_YEARLY:    out << "TOK_YEARLY"; break;
        case TOK_QUARTERLY: out << "TOK_QUARTERLY"; break;
        case TOK_BIMONTHLY: out << "TOK_BIMONTHLY"; break;
        case TOK_MONTHLY:   out << "TOK_MONTHLY"; break;
        case TOK_BIWEEKLY:  out << "TOK_BIWEEKLY"; break;
        case TOK_WEEKLY:    out << "TOK_WEEKLY"; break;
        case TOK_DAILY:     out << "TOK_DAILY"; break;
        case TOK_YEARS:     out << "TOK_YEARS"; break;
        case TOK_QUARTERS:  out << "TOK_QUARTERS"; break;
        case TOK_MONTHS:    out << "TOK_MONTHS"; break;
        case TOK_WEEKS:     out << "TOK_WEEKS"; break;
        case TOK_DAYS:      out << "TOK_DAYS"; break;
        case END_REACHED:   out << "END_REACHED"; break;
        }
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
    lexer_t(const lexer_t& other)
      : begin(other.begin), end(other.end),
        token_cache(other.token_cache)
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

  date_interval_t parse();

private:
  void determine_when(lexer_t::token_t& tok, date_specifier_t& specifier);
};

void date_parser_t::determine_when(date_parser_t::lexer_t::token_t& tok,
                                   date_specifier_t& specifier)
{
  date_t today = CURRENT_DATE();

  switch (tok.kind) {
  case lexer_t::token_t::TOK_DATE:
    specifier = boost::get<date_specifier_t>(*tok.value);
    break;

  case lexer_t::token_t::TOK_INT: {
    unsigned short amount = boost::get<unsigned short>(*tok.value);
    int8_t         adjust = 0;

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
      when += gregorian::weeks(amount * adjust);
      break;
    case lexer_t::token_t::TOK_DAY:
    case lexer_t::token_t::TOK_DAYS:
      when += gregorian::days(amount * adjust);
      break;
    default:
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
    int8_t adjust = 0;
    if (tok.kind == lexer_t::token_t::TOK_NEXT)
      adjust = 1;
    else if (tok.kind == lexer_t::token_t::TOK_LAST)
      adjust = -1;

    tok = lexer.next_token();
    switch (tok.kind) {
    case lexer_t::token_t::TOK_A_MONTH: {
      date_t temp(today.year(),
                  boost::get<date_time::months_of_year>(*tok.value), 1);
      temp += gregorian::years(adjust);
      specifier =
        date_specifier_t(static_cast<date_specifier_t::year_type>(temp.year()),
                         temp.month());
      break;
    }

    case lexer_t::token_t::TOK_A_WDAY: {
      date_t temp =
        date_duration_t::find_nearest(today, date_duration_t::WEEKS);
      while (temp.day_of_week() !=
             boost::get<date_time::months_of_year>(*tok.value))
        temp += gregorian::days(1);
      temp += gregorian::days(7 * adjust);
      specifier = date_specifier_t(temp);
      break;
    }

    case lexer_t::token_t::TOK_YEAR: {
      date_t temp(today);
      temp += gregorian::years(adjust);
      specifier =
        date_specifier_t(static_cast<date_specifier_t::year_type>(temp.year()));
      break;
    }

    case lexer_t::token_t::TOK_QUARTER: {
      date_t base =
        date_duration_t::find_nearest(today, date_duration_t::QUARTERS);
      date_t temp;
      if (adjust < 0) {
        temp = base + gregorian::months(3 * adjust);
      }
      else if (adjust == 0) {
        temp = base + gregorian::months(3);
      }
      else if (adjust > 0) {
        base += gregorian::months(3 * adjust);
        temp = base + gregorian::months(3 * adjust);
      }
      specifier = date_specifier_t(adjust < 0 ? temp : base);
      break;
    }

    case lexer_t::token_t::TOK_WEEK: {
      date_t base =
        date_duration_t::find_nearest(today, date_duration_t::WEEKS);
      date_t temp;
      if (adjust < 0) {
        temp = base + gregorian::days(7 * adjust);
      }
      else if (adjust == 0) {
        temp = base + gregorian::days(7);
      }
      else if (adjust > 0) {
        base += gregorian::days(7 * adjust);
        temp = base + gregorian::days(7 * adjust);
      }
      specifier = date_specifier_t(adjust < 0 ? temp : base);
      break;
    }

    case lexer_t::token_t::TOK_DAY: {
      date_t temp(today);
      temp += gregorian::days(adjust);
      specifier = date_specifier_t(temp);
      break;
    }

    default:
    case lexer_t::token_t::TOK_MONTH: {
      date_t temp(today);
      temp += gregorian::months(adjust);
      specifier =
        date_specifier_t(static_cast<date_specifier_t::year_type>(temp.year()),
                         temp.month());
      break;
    }
    }
    break;
  }

  case lexer_t::token_t::TOK_A_MONTH:
    specifier.month =
      date_specifier_t::month_type
        (boost::get<date_time::months_of_year>(*tok.value));
    tok = lexer.peek_token();
    switch (tok.kind) {
    case lexer_t::token_t::TOK_INT:
      specifier.year = boost::get<date_specifier_t::year_type>(*tok.value);
      break;
    case lexer_t::token_t::END_REACHED:
      break;
    default:
      break;
    }
    break;
  case lexer_t::token_t::TOK_A_WDAY:
    specifier.wday  =
      date_specifier_t::day_of_week_type
        (boost::get<date_time::weekdays>(*tok.value));
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

date_interval_t date_parser_t::parse()
{
  optional<date_specifier_t> since_specifier;
  optional<date_specifier_t> until_specifier;
  optional<date_specifier_t> inclusion_specifier;

  date_interval_t period;
  date_t          today = CURRENT_DATE();
  bool            end_inclusive = false;

  for (lexer_t::token_t tok = lexer.next_token();
       tok.kind != lexer_t::token_t::END_REACHED;
       tok = lexer.next_token()) {
    switch (tok.kind) {
    case lexer_t::token_t::TOK_DATE:
      if (! inclusion_specifier)
        inclusion_specifier = date_specifier_t();
      determine_when(tok, *inclusion_specifier);
      break;

    case lexer_t::token_t::TOK_INT:
      if (! inclusion_specifier)
        inclusion_specifier = date_specifier_t();
      determine_when(tok, *inclusion_specifier);
      break;

    case lexer_t::token_t::TOK_A_MONTH:
      if (! inclusion_specifier)
        inclusion_specifier = date_specifier_t();
      determine_when(tok, *inclusion_specifier);
      break;

    case lexer_t::token_t::TOK_A_WDAY:
      if (! inclusion_specifier)
        inclusion_specifier = date_specifier_t();
      determine_when(tok, *inclusion_specifier);
      break;

    case lexer_t::token_t::TOK_DASH:
      if (inclusion_specifier) {
        since_specifier     = inclusion_specifier;
        until_specifier     = date_specifier_t();
        inclusion_specifier = none;

        tok = lexer.next_token();
        determine_when(tok, *until_specifier);

        // The dash operator is special: it has an _inclusive_ end.
        end_inclusive = true;
      } else {
        tok.unexpected();
      }
      break;

    case lexer_t::token_t::TOK_SINCE:
      if (since_specifier) {
        tok.unexpected();
      } else {
        since_specifier = date_specifier_t();
        tok = lexer.next_token();
        determine_when(tok, *since_specifier);
      }
      break;

    case lexer_t::token_t::TOK_UNTIL:
      if (until_specifier) {
        tok.unexpected();
      } else {
        until_specifier = date_specifier_t();
        tok = lexer.next_token();
        determine_when(tok, *until_specifier);
      }
      break;

    case lexer_t::token_t::TOK_IN:
      if (inclusion_specifier) {
        tok.unexpected();
      } else {
        inclusion_specifier = date_specifier_t();
        tok = lexer.next_token();
        determine_when(tok, *inclusion_specifier);
      }
      break;

    case lexer_t::token_t::TOK_THIS:
    case lexer_t::token_t::TOK_NEXT:
    case lexer_t::token_t::TOK_LAST: {
      int8_t adjust = 0;
      if (tok.kind == lexer_t::token_t::TOK_NEXT)
        adjust = 1;
      else if (tok.kind == lexer_t::token_t::TOK_LAST)
        adjust = -1;

      tok = lexer.next_token();
      switch (tok.kind) {
      case lexer_t::token_t::TOK_INT: {
        unsigned short amount = boost::get<unsigned short>(*tok.value);

        date_t base(today);
        date_t end(today);

        if (! adjust)
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
          base += gregorian::weeks(amount * adjust);
          break;
        case lexer_t::token_t::TOK_DAYS:
          base += gregorian::days(amount * adjust);
          break;
        default:
          tok.unexpected();
          break;
        }

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
        inclusion_specifier = date_specifier_t();
        determine_when(tok, *inclusion_specifier);

        date_t temp(today.year(), *inclusion_specifier->month, 1);
        temp += gregorian::years(adjust);
        inclusion_specifier =
          date_specifier_t(static_cast<date_specifier_t::year_type>(temp.year()),
                           temp.month());
        break;
      }

      case lexer_t::token_t::TOK_A_WDAY: {
        inclusion_specifier = date_specifier_t();
        determine_when(tok, *inclusion_specifier);

        date_t temp =
          date_duration_t::find_nearest(today, date_duration_t::WEEKS);
        while (temp.day_of_week() != inclusion_specifier->wday)
          temp += gregorian::days(1);
        temp += gregorian::days(7 * adjust);
        inclusion_specifier = date_specifier_t(temp);
        break;
      }

      case lexer_t::token_t::TOK_YEAR: {
        date_t temp(today);
        temp += gregorian::years(adjust);
        inclusion_specifier =
          date_specifier_t(static_cast<date_specifier_t::year_type>(temp.year()));
        break;
      }

      case lexer_t::token_t::TOK_QUARTER: {
        date_t base =
          date_duration_t::find_nearest(today, date_duration_t::QUARTERS);
        date_t temp;
        if (adjust < 0) {
          temp = base + gregorian::months(3 * adjust);
        }
        else if (adjust == 0) {
          temp = base + gregorian::months(3);
        }
        else if (adjust > 0) {
          base += gregorian::months(3 * adjust);
          temp = base + gregorian::months(3 * adjust);
        }
        since_specifier = date_specifier_t(adjust < 0 ? temp : base);
        until_specifier = date_specifier_t(adjust < 0 ? base : temp);
        break;
      }

      case lexer_t::token_t::TOK_WEEK: {
        date_t base =
          date_duration_t::find_nearest(today, date_duration_t::WEEKS);
        date_t temp;
        if (adjust < 0) {
          temp = base + gregorian::days(7 * adjust);
        }
        else if (adjust == 0) {
          temp = base + gregorian::days(7);
        }
        else if (adjust > 0) {
          base += gregorian::days(7 * adjust);
          temp = base + gregorian::days(7 * adjust);
        }
        since_specifier = date_specifier_t(adjust < 0 ? temp : base);
        until_specifier = date_specifier_t(adjust < 0 ? base : temp);
        break;
      }

      case lexer_t::token_t::TOK_DAY: {
        date_t temp(today);
        temp += gregorian::days(adjust);
        inclusion_specifier = date_specifier_t(temp);
        break;
      }

      default:
      case lexer_t::token_t::TOK_MONTH: {
        date_t temp(today);
        temp += gregorian::months(adjust);
        inclusion_specifier =
          date_specifier_t(static_cast<date_specifier_t::year_type>(temp.year()),
                           temp.month());
        break;
      }
      }
      break;
    }

    case lexer_t::token_t::TOK_TODAY:
      inclusion_specifier = date_specifier_t(today);
      break;
    case lexer_t::token_t::TOK_TOMORROW:
      inclusion_specifier = date_specifier_t(today + gregorian::days(1));
      break;
    case lexer_t::token_t::TOK_YESTERDAY:
      inclusion_specifier = date_specifier_t(today - gregorian::days(1));
      break;

    case lexer_t::token_t::TOK_EVERY:
      tok = lexer.next_token();
      if (tok.kind == lexer_t::token_t::TOK_INT) {
        int quantity = boost::get<unsigned short>(*tok.value);
        tok = lexer.next_token();
        switch (tok.kind) {
        case lexer_t::token_t::TOK_YEARS:
          period.duration = date_duration_t(date_duration_t::YEARS, quantity);
          break;
        case lexer_t::token_t::TOK_QUARTERS:
          period.duration = date_duration_t(date_duration_t::QUARTERS, quantity);
          break;
        case lexer_t::token_t::TOK_MONTHS:
          period.duration = date_duration_t(date_duration_t::MONTHS, quantity);
          break;
        case lexer_t::token_t::TOK_WEEKS:
          period.duration = date_duration_t(date_duration_t::WEEKS, quantity);
          break;
        case lexer_t::token_t::TOK_DAYS:
          period.duration = date_duration_t(date_duration_t::DAYS, quantity);
          break;
        default:
          tok.unexpected();
          break;
        }
      } else {
        switch (tok.kind) {
        case lexer_t::token_t::TOK_YEAR:
          period.duration = date_duration_t(date_duration_t::YEARS, 1);
          break;
        case lexer_t::token_t::TOK_QUARTER:
          period.duration = date_duration_t(date_duration_t::QUARTERS, 1);
          break;
        case lexer_t::token_t::TOK_MONTH:
          period.duration = date_duration_t(date_duration_t::MONTHS, 1);
          break;
        case lexer_t::token_t::TOK_WEEK:
          period.duration = date_duration_t(date_duration_t::WEEKS, 1);
          break;
        case lexer_t::token_t::TOK_DAY:
          period.duration = date_duration_t(date_duration_t::DAYS, 1);
          break;
        default:
          tok.unexpected();
          break;
        }
      }
      break;

    case lexer_t::token_t::TOK_YEARLY:
      period.duration = date_duration_t(date_duration_t::YEARS, 1);
      break;
    case lexer_t::token_t::TOK_QUARTERLY:
      period.duration = date_duration_t(date_duration_t::QUARTERS, 1);
      break;
    case lexer_t::token_t::TOK_BIMONTHLY:
      period.duration = date_duration_t(date_duration_t::MONTHS, 2);
      break;
    case lexer_t::token_t::TOK_MONTHLY:
      period.duration = date_duration_t(date_duration_t::MONTHS, 1);
      break;
    case lexer_t::token_t::TOK_BIWEEKLY:
      period.duration = date_duration_t(date_duration_t::WEEKS, 2);
      break;
    case lexer_t::token_t::TOK_WEEKLY:
      period.duration = date_duration_t(date_duration_t::WEEKS, 1);
      break;
    case lexer_t::token_t::TOK_DAILY:
      period.duration = date_duration_t(date_duration_t::DAYS, 1);
      break;

    default:
      tok.unexpected();
      break;
    }
  }

#if 0
  if (! period.duration && inclusion_specifier)
    period.duration = inclusion_specifier->implied_duration();
#endif

  if (since_specifier || until_specifier) {
    date_range_t range(since_specifier, until_specifier);
    range.end_inclusive = end_inclusive;

    period.range = date_specifier_or_range_t(range);
    period.since_specified = static_cast<bool>(since_specifier);
  }
  else if (inclusion_specifier) {
    period.range = date_specifier_or_range_t(*inclusion_specifier);
  }
  else {
    /* otherwise, it's something like "monthly", with no date reference */
  }

  return period;
}

void date_interval_t::parse(const string& str)
{
  date_parser_t parser(str);
  *this = parser.parse();
}

void date_interval_t::resolve_end()
{
  if (start && ! end_of_duration) {
    end_of_duration = duration->add(*start);
    DEBUG("times.interval",
          "stabilize: end_of_duration = " << *end_of_duration);
  }

  if (finish && *end_of_duration > *finish) {
    end_of_duration = finish;
    DEBUG("times.interval",
          "stabilize: end_of_duration reset to end: " << *end_of_duration);
  }

  if (start && ! next) {
    next = end_of_duration;
    DEBUG("times.interval", "stabilize: next set to: " << *next);
  }
}

date_t date_duration_t::find_nearest(const date_t& date, skip_quantum_t skip)
{
  date_t result;

  switch (skip) {
  case date_duration_t::YEARS:
    result = date_t(date.year(), gregorian::Jan, 1);
    break;
  case date_duration_t::QUARTERS:
    result = date_t(date.year(), date.month(), 1);
    while (result.month() != gregorian::Jan &&
           result.month() != gregorian::Apr &&
           result.month() != gregorian::Jul &&
           result.month() != gregorian::Oct)
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

void date_interval_t::stabilize(const optional<date_t>& date, bool align_intervals)
{
#if DEBUG_ON
  if (date)
    DEBUG("times.interval", "stabilize: with date = " << *date);
#endif

  if (date && ! aligned) {
    DEBUG("times.interval", "stabilize: date passed, but not aligned");
    if (duration) {
      DEBUG("times.interval",
            "stabilize: aligning with a duration: " << *duration);

      // The interval object has not been seeded with a start date yet, so
      // find the nearest period before or on date which fits, if possible.
      //
      // Find an efficient starting point for the upcoming while loop.  We
      // want a date early enough that the range will be correct, but late
      // enough that we don't spend hundreds of thousands of loops skipping
      // through time.
      optional<date_t> initial_start  = start  ? start  : begin();
      optional<date_t> initial_finish = finish ? finish : end();

#if DEBUG_ON
      if (initial_start)
        DEBUG("times.interval",
              "stabilize: initial_start  = " << *initial_start);
      if (initial_finish)
        DEBUG("times.interval",
              "stabilize: initial_finish = " << *initial_finish);
#endif

      date_t when = start ? *start : *date;
      switch (duration->quantum) {
      case date_duration_t::MONTHS:
      case date_duration_t::QUARTERS:
      case date_duration_t::YEARS:
        // These start on most recent period start quantum before when
        DEBUG("times.interval",
            "stabilize: monthly, quarterly or yearly duration");
        if (align_intervals && since_specified) {
          start = when;
        } else {
          start = date_duration_t::find_nearest(when, duration->quantum);
        }
        break;
      case date_duration_t::WEEKS:
        // Weeks start on the beginning of week prior to 400 remainder period length
        // Either the first quanta of the period or the last quanta of the period seems more sensible
        // implies period is never less than 400 days not too unreasonable
        DEBUG("times.interval", "stabilize: weekly duration");
        {
          if (align_intervals && since_specified) {
            start = when;
          } else {
            int period = duration->length * 7;
            start = date_duration_t::find_nearest(
                when - gregorian::days(period + 400 % period), duration->quantum);
          }
        }
        break;
      default:
        // multiples of days have a quanta of 1 day so should not have the start date adjusted to a quanta
        DEBUG("times.interval",
            "stabilize: daily duration - stable by definition");
        start = when;
        break;
      }

      DEBUG("times.interval", "stabilize: beginning start date = " << *start);

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

      if (initial_start && (! start || *start < *initial_start)) {
        // Using the discovered start, find the end of the period
        resolve_end();

        start = initial_start;
        DEBUG("times.interval", "stabilize: start reset to initial start");
      }
      if (initial_finish && (! finish || *finish > *initial_finish)) {
        finish = initial_finish;
        DEBUG("times.interval", "stabilize: finish reset to initial finish");
      }

#if DEBUG_ON
      if (start)
        DEBUG("times.interval", "stabilize: final start  = " << *start);
      if (finish)
        DEBUG("times.interval", "stabilize: final finish = " << *finish);
#endif
    }
    else if (range) {
      start  = range->begin();
      finish = range->end();
    }
    aligned = true;
  }

  // If there is no duration, then if we've reached here the date falls
  // between start and finish.
  if (! duration) {
    DEBUG("times.interval", "stabilize: there was no duration given");

    if (! start && ! finish)
      throw_(date_error,
             _("Invalid date interval: neither start, nor finish, nor duration"));
  } else {
    resolve_end();
  }
}

bool date_interval_t::find_period(const date_t& date,
                                  const bool    align_intervals,
                                  const bool    allow_shift)
{
  stabilize(date, align_intervals);

  if (finish && date > *finish) {
    DEBUG("times.interval",
          "false: date [" << date << "] > finish [" << *finish << "]");
    return false;
  }

  if (! start) {
    throw_(std::runtime_error, _("Date interval is improperly initialized"));
  }
  else if (date < *start) {
    DEBUG("times.interval",
          "false: date [" << date << "] < start [" << *start << "]");
    return false;
  }

  if (end_of_duration) {
    if (date < *end_of_duration) {
      DEBUG("times.interval",
            "true: date [" << date << "] < end_of_duration ["
            << *end_of_duration << "]");
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

  date_t scan        = *start;
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

  while (date >= scan && (! finish || scan < *finish)) {
    if (date < end_of_scan) {
      start           = scan;
      end_of_duration = end_of_scan;
      next            = none;

      DEBUG("times.interval", "true: start           = " << *start);
      DEBUG("times.interval", "true: end_of_duration = " << *end_of_duration);

      resolve_end();

      return true;
    }
    else if (! allow_shift) {
      break;
    }

    scan        = duration->add(scan);
    end_of_scan = duration->add(scan);

    DEBUG("times.interval", "scan        = " << scan);
    DEBUG("times.interval", "end_of_scan = " << end_of_scan);
  }

  DEBUG("times.interval", "false: failed scan");

  return false;
}

date_interval_t& date_interval_t::operator++()
{
  if (! start)
    throw_(date_error, _("Cannot increment an unstarted date interval"));

  stabilize();

  if (! duration)
    throw_(date_error,
           _("Cannot increment a date interval without a duration"));

  assert(next);

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

void date_interval_t::dump(std::ostream& out)
{
  out << _("--- Before stabilization ---") << std::endl;

  if (range)
    out << _("   range: ") << range->to_string() << std::endl;
  if (start)
    out << _("   start: ") << format_date(*start) << std::endl;
  if (finish)
    out << _("  finish: ") << format_date(*finish) << std::endl;

  if (duration)
    out << _("duration: ") << duration->to_string() << std::endl;

  optional<date_t> when(begin());
  if (! when)
    when = CURRENT_DATE();

  stabilize(when);

  out << std::endl
      << _("--- After stabilization ---") << std::endl;

  if (range)
    out << _("   range: ") << range->to_string() << std::endl;
  if (start)
    out << _("   start: ") << format_date(*start) << std::endl;
  if (finish)
    out << _("  finish: ") << format_date(*finish) << std::endl;

  if (duration)
    out << _("duration: ") << duration->to_string() << std::endl;

  out << std::endl
      << _("--- Sample dates in range (max. 20) ---") << std::endl;

  date_t last_date;

  for (int i = 0; i < 20 && *this; ++i, ++*this) {
    out << std::right;
    out.width(2);

    if (! last_date.is_not_a_date() && last_date == *start)
      break;

    out << (i + 1) << ": " << format_date(*start);
    if (duration)
      out << " -- " << format_date(*inclusive_end());
    out << std::endl;

    if (! duration)
      break;

    last_date = *start;
  }
}

date_parser_t::lexer_t::token_t date_parser_t::lexer_t::next_token()
{
  if (token_cache.kind != token_t::UNKNOWN) {
    token_t tok = token_cache;
    token_cache = token_t();
    return tok;
  }

  while (begin != end && std::isspace(static_cast<unsigned char>(*begin)))
    begin++;

  if (begin == end)
    return token_t(token_t::END_REACHED);

  switch (*begin) {
  case '/': ++begin; return token_t(token_t::TOK_SLASH);
  case '-': ++begin; return token_t(token_t::TOK_DASH);
  case '.': ++begin; return token_t(token_t::TOK_DOT);
  default: break;
  }

  string::const_iterator start = begin;

  // If the first character is a digit, try parsing the whole argument as a
  // date using the typical date formats.  This allows not only dates like
  // "2009/08/01", but also dates that fit the user's --input-date-format,
  // assuming their format fits in one argument and begins with a digit.
  if (std::isdigit(static_cast<unsigned char>(*begin))) {
    string::const_iterator i = begin;
    for (i = begin;
         i != end && ! std::isspace(static_cast<unsigned char>(*i));
         i++) {}
    assert(i != begin);

    string possible_date(start, i);

    try {
      date_traits_t traits;
      date_t when = parse_date_mask(possible_date.c_str(), &traits);
      if (! when.is_not_a_date()) {
        begin = i;
        return token_t(token_t::TOK_DATE,
                       token_t::content_t(date_specifier_t(when, traits)));
      }
    }
    catch (date_error&) {
      if (contains(possible_date, "/") ||
          contains(possible_date, "-") ||
          contains(possible_date, "."))
        throw;
    }
  }

  start = begin;

  string term;
  bool alnum = std::isalnum(static_cast<unsigned char>(*begin));
  for (; (begin != end && ! std::isspace(static_cast<unsigned char>(*begin)) &&
          ((alnum && static_cast<bool>(std::isalnum(
             static_cast<unsigned char>(*begin)))) ||
           (! alnum && ! static_cast<bool>(std::isalnum(
             static_cast<unsigned char>(*begin)))))); begin++)
    term.push_back(*begin);

  if (! term.empty()) {
    if (std::isdigit(static_cast<unsigned char>(term[0]))) {
      return token_t(token_t::TOK_INT,
                     token_t::content_t(lexical_cast<unsigned short>(term)));
    }
    else if (std::isalpha(static_cast<unsigned char>(term[0]))) {
      to_lower(term);

      if (optional<date_time::months_of_year> month =
          string_to_month_of_year(term)) {
        return token_t(token_t::TOK_A_MONTH, token_t::content_t(*month));
      }
      else if (optional<date_time::weekdays> wday =
               string_to_day_of_week(term)) {
        return token_t(token_t::TOK_A_WDAY, token_t::content_t(*wday));
      }
      else if (term == _("ago"))
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
      else if (term == _("yesterday"))
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
    }
    else {
      token_t::expected('\0', term[0]);
      begin = ++start;
    }
  } else {
    token_t::expected('\0', *begin);
  }

  return token_t(token_t::UNKNOWN, token_t::content_t(term));
}

void date_parser_t::lexer_t::token_t::unexpected()
{
  switch (kind) {
  case END_REACHED:
    kind = UNKNOWN;
    throw_(date_error, _("Unexpected end of expression"));
  default: {
    string desc = to_string();
    kind = UNKNOWN;
    throw_(date_error, _f("Unexpected date period token '%1%'") % desc);
  }
  }
}

void date_parser_t::lexer_t::token_t::expected(char wanted, char c)
{
  if (wanted == '\0')
    throw_(date_error, _f("Invalid char '%1%'") % c);
  else
    throw_(date_error, _f("Invalid char '%1%' (wanted '%2%')") % c % wanted);
}

namespace {
  typedef std::map<std::string, datetime_io_t *> datetime_io_map;
  typedef std::map<std::string, date_io_t *>     date_io_map;

  datetime_io_map temp_datetime_io;
  date_io_map     temp_date_io;
}

std::string format_datetime(const datetime_t&             when,
                            const format_type_t           format_type,
                            const optional<const char *>& format)
{
  if (format_type == FMT_WRITTEN) {
    return written_datetime_io->format(when);
  }
  else if (format_type == FMT_CUSTOM && format) {
    datetime_io_map::iterator i = temp_datetime_io.find(*format);
    if (i != temp_datetime_io.end()) {
      return (*i).second->format(when);
    } else {
      datetime_io_t * formatter = new datetime_io_t(*format, false);
      temp_datetime_io.insert(datetime_io_map::value_type(*format, formatter));
      return formatter->format(when);
    }
  }
  else if (format_type == FMT_PRINTED) {
    return printed_datetime_io->format(when);
  }
  else {
    assert(false);
    return empty_string;
  }
}

std::string format_date(const date_t&                 when,
                        const format_type_t           format_type,
                        const optional<const char *>& format)
{
  if (format_type == FMT_WRITTEN) {
    return written_date_io->format(when);
  }
  else if (format_type == FMT_CUSTOM && format) {
    date_io_map::iterator i = temp_date_io.find(*format);
    if (i != temp_date_io.end()) {
      return (*i).second->format(when);
    } else {
      date_io_t * formatter = new date_io_t(*format, false);
      temp_date_io.insert(date_io_map::value_type(*format, formatter));
      return formatter->format(when);
    }
  }
  else if (format_type == FMT_PRINTED) {
    return printed_date_io->format(when);
  }
  else {
    assert(false);
    return empty_string;
  }
}

namespace {
  bool is_initialized = false;
}

void set_datetime_format(const char * format)
{
  written_datetime_io->set_format(format);
  printed_datetime_io->set_format(format);
}

void set_date_format(const char * format)
{
  written_date_io->set_format(format);
  printed_date_io->set_format(format);
}

void set_input_date_format(const char * format)
{
  readers.push_front(shared_ptr<date_io_t>(new date_io_t(format, true)));
  convert_separators_to_slashes = false;
}

void times_initialize()
{
  if (! is_initialized) {
    input_datetime_io.reset(new datetime_io_t("%Y/%m/%d %H:%M:%S", true));
    timelog_datetime_io.reset(new datetime_io_t("%m/%d/%Y %H:%M:%S", true));

    written_datetime_io.reset(new datetime_io_t("%Y/%m/%d %H:%M:%S", false));
    written_date_io.reset(new date_io_t("%Y/%m/%d", false));

    printed_datetime_io.reset(new datetime_io_t("%y-%b-%d %H:%M:%S", false));
    printed_date_io.reset(new date_io_t("%y-%b-%d", false));

    readers.push_back(shared_ptr<date_io_t>(new date_io_t("%m/%d", true)));
    readers.push_back(shared_ptr<date_io_t>(new date_io_t("%Y/%m/%d", true)));
    readers.push_back(shared_ptr<date_io_t>(new date_io_t("%Y/%m", true)));
    readers.push_back(shared_ptr<date_io_t>(new date_io_t("%y/%m/%d", true)));
    readers.push_back(shared_ptr<date_io_t>(new date_io_t("%Y-%m-%d", true)));

    is_initialized = true;
  }
}

void times_shutdown()
{
  if (is_initialized) {
    input_datetime_io.reset();
    timelog_datetime_io.reset();
    written_datetime_io.reset();
    written_date_io.reset();
    printed_datetime_io.reset();
    printed_date_io.reset();

    readers.clear();

    foreach (datetime_io_map::value_type& pair, temp_datetime_io)
      checked_delete(pair.second);
    temp_datetime_io.clear();

    foreach (date_io_map::value_type& pair, temp_date_io)
      checked_delete(pair.second);
    temp_date_io.clear();

    is_initialized = false;
  }
}

void show_period_tokens(std::ostream& out, const string& arg)
{
  date_parser_t::lexer_t lexer(arg.begin(), arg.end());

  out << _("--- Period expression tokens ---") << std::endl;

  date_parser_t::lexer_t::token_t token;
  do {
    token = lexer.next_token();
    token.dump(out);
    out << ": " << token.to_string() << std::endl;
  }
  while (token.kind != date_parser_t::lexer_t::token_t::END_REACHED);
}

} // namespace ledger
