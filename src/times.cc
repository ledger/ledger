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

#include <system.hh>

#include "times.h"

namespace ledger {

optional<datetime_t> epoch;

date_time::weekdays start_of_week = gregorian::Sunday;

//#define USE_BOOST_FACETS 1

namespace {
  template <typename T, typename InputFacetType, typename OutputFacetType>
  class temporal_io_t : public noncopyable
  {
    const char *       fmt_str;
#if defined(USE_BOOST_FACETS)
    std::istringstream input_stream;
    std::ostringstream output_stream;
    InputFacetType *   input_facet;
    OutputFacetType *  output_facet;
    std::string        temp_string;
#endif // USE_BOOST_FACETS

  public:
    date_traits_t traits;
    bool input;

    temporal_io_t(const char * _fmt_str, bool _input)
      : fmt_str(_fmt_str),
	traits(icontains(fmt_str, "%y"),
	       icontains(fmt_str, "%m") || icontains(fmt_str, "%b"),
	       icontains(fmt_str, "%d")),
	input(_input) {
#if defined(USE_BOOST_FACETS)
      if (input) {
	input_facet  = new InputFacetType(fmt_str);
	input_stream.imbue(std::locale(std::locale::classic(), input_facet));
      } else {
	output_facet = new OutputFacetType(fmt_str);
	output_stream.imbue(std::locale(std::locale::classic(), output_facet));
      }
#endif // USE_BOOST_FACETS
    }

    void set_format(const char * fmt) {
      fmt_str  = fmt;
      traits   = date_traits_t(icontains(fmt_str, "%y"),
			       icontains(fmt_str, "%m") ||
			       icontains(fmt_str, "%b"),
			       icontains(fmt_str, "%d"));
#if defined(USE_BOOST_FACETS)
      if (input)
	input_facet->format(fmt_str);
      else
	output_facet->format(fmt_str);
#endif // USE_BOOST_FACETS
    }

    T parse(const char * str) {
    }

    std::string format(const T& when) {
#if defined(USE_BOOST_FACETS)
      output_stream.str(temp_string);
      output_stream.seekp(std::ios_base::beg);
      output_stream.clear();
      output_stream << when;
      return output_stream.str();
#else // USE_BOOST_FACETS
      std::tm data(to_tm(when));
      char buf[128];
      std::strftime(buf, 127, fmt_str, &data);
      return buf;
#endif // USE_BOOST_FACETS
    }
  };

  template <>
  datetime_t temporal_io_t<datetime_t, posix_time::time_input_facet,
			   posix_time::time_facet>
    ::parse(const char * str)
  {
#if defined(USE_BOOST_FACETS)
      input_stream.seekg(std::ios_base::beg);
      input_stream.clear();
      input_stream.str(str);

      datetime_t when;
      input_stream >> when;
#if defined(DEBUG_ON)
      if (when.is_not_a_date_time())
	DEBUG("times.parse", "Failed to parse date/time '" << str
	      << "' using pattern '" << fmt_str << "'");
#endif

      if (! when.is_not_a_date_time() &&
	  input_stream.good() && ! input_stream.eof() &&
	  input_stream.peek() != EOF) {
	DEBUG("times.parse", "This string has leftovers: '" << str << "'");
	return datetime_t();
      }
      return when;
#else // USE_BOOST_FACETS
    std::tm data;
    std::memset(&data, 0, sizeof(std::tm));
    if (strptime(str, fmt_str, &data))
      return posix_time::ptime_from_tm(data);
    else
      return datetime_t();
#endif // USE_BOOST_FACETS
  }

  template <>
  date_t temporal_io_t<date_t, gregorian::date_input_facet,
		       gregorian::date_facet>
    ::parse(const char * str)
  {
#if defined(USE_BOOST_FACETS)
      input_stream.seekg(std::ios_base::beg);
      input_stream.clear();
      input_stream.str(str);

      date_t when;
      input_stream >> when;
#if defined(DEBUG_ON)
      if (when.is_not_a_date())
	DEBUG("times.parse", "Failed to parse date '" << str
	      << "' using pattern '" << fmt_str << "'");
#endif

      if (! when.is_not_a_date() &&
	  input_stream.good() && ! input_stream.eof() &&
	  input_stream.peek() != EOF) {
	DEBUG("times.parse", "This string has leftovers: '" << str << "'");
	return date_t();
      }
      return when;
#else // USE_BOOST_FACETS
    std::tm data;
    std::memset(&data, 0, sizeof(std::tm));
    data.tm_mday = 1;		// some formats have no day
    if (strptime(str, fmt_str, &data))
      return gregorian::date_from_tm(data);
    else
      return date_t();
#endif // USE_BOOST_FACETS
  }

  typedef temporal_io_t<datetime_t, posix_time::time_input_facet,
			posix_time::time_facet> datetime_io_t;
  typedef temporal_io_t<date_t, gregorian::date_input_facet,
			gregorian::date_facet> date_io_t;

  shared_ptr<datetime_io_t> input_datetime_io;
  shared_ptr<date_io_t>     input_date_io;
  shared_ptr<datetime_io_t> written_datetime_io;
  shared_ptr<date_io_t>     written_date_io;
  shared_ptr<datetime_io_t> printed_datetime_io;
  shared_ptr<date_io_t>     printed_date_io;

  std::vector<shared_ptr<date_io_t> > readers;

  date_t parse_date_mask_routine(const char * date_str, date_io_t& io,
				 optional_year year,
				 date_traits_t * traits = NULL)
  {
    date_t when;

    if (std::strchr(date_str, '/')) {
      when = io.parse(date_str);
    } else {
      char buf[128];
      VERIFY(std::strlen(date_str) < 127);
      std::strcpy(buf, date_str);

      for (char * p = buf; *p; p++)
	if (*p == '.' || *p == '-')
	  *p = '/';

      when = io.parse(buf);
    }

    if (! when.is_not_a_date()) {
      DEBUG("times.parse", "Parsed date string: " << date_str);
      DEBUG("times.parse", "Parsed result is:   " << when);

      if (traits)
	*traits = io.traits;

      if (! io.traits.has_year) {
	when = date_t(year ? *year : CURRENT_DATE().year(),
		      when.month(), when.day());

	if (when.month() > CURRENT_DATE().month())
	  when -= gregorian::years(1);
      }
    }
    return when;
  }

  date_t parse_date_mask(const char * date_str, optional_year year,
			 date_traits_t * traits = NULL)
  {
    if (input_date_io.get()) {
      date_t when = parse_date_mask_routine(date_str, *input_date_io.get(),
					    year, traits);
      if (! when.is_not_a_date())
	return when;
    }

    foreach (shared_ptr<date_io_t>& reader, readers) {
      date_t when = parse_date_mask_routine(date_str, *reader.get(),
					    year, traits);
      if (! when.is_not_a_date())
	return when;
    }

    throw_(date_error, _("Invalid date: %1") << date_str);
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

datetime_t parse_datetime(const char * str, optional_year)
{
  datetime_t when = input_datetime_io->parse(str);
  if (when.is_not_a_date_time())
    throw_(date_error, _("Invalid date/time: %1") << str);
  return when;
}

date_t parse_date(const char * str, optional_year current_year)
{
  return parse_date_mask(str, current_year);
}

date_t date_specifier_t::begin(const optional_year& current_year) const
{
  assert(year || current_year);

  year_type  the_year  = year ? *year : static_cast<year_type>(*current_year);
  month_type the_month = month ? *month : date_t::month_type(1);
  day_type   the_day   = day ? *day : date_t::day_type(1);

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

date_t date_specifier_t::end(const optional_year& current_year) const
{
  if (day || wday)
    return begin(current_year) + gregorian::days(1);
  else if (month)
    return begin(current_year) + gregorian::months(1);
  else if (year)
    return begin(current_year) + gregorian::years(1);
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

	TOK_A_YEAR,
	TOK_A_MONTH,
	TOK_A_WDAY,

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
		      date_specifier_t::year_type,
		      date_time::months_of_year,
		      date_time::weekdays,
		      date_specifier_t> content_t;

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
	case TOK_A_WDAY:    return "TOK_A_WDAY";
	case TOK_SINCE:	    return "TOK_SINCE";
	case TOK_UNTIL:	    return "TOK_UNTIL";
	case TOK_IN:	    return "TOK_IN";
	case TOK_THIS:	    return "TOK_THIS";
	case TOK_NEXT:	    return "TOK_NEXT";
	case TOK_LAST:	    return "TOK_LAST";
	case TOK_EVERY:	    return "TOK_EVERY";
	case TOK_TODAY:	    return "TOK_EVERY";
	case TOK_TOMORROW:  return "TOK_TOMORROW";
	case TOK_YESTERDAY: return "TOK_YESTERDAY";
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
  switch (tok.kind) {
  case lexer_t::token_t::TOK_DATE:
    specifier = boost::get<date_specifier_t>(*tok.value);
    break;

  case lexer_t::token_t::TOK_INT:
    specifier.day   =
      date_specifier_t::day_type(boost::get<unsigned short>(*tok.value));
    break;
  case lexer_t::token_t::TOK_A_YEAR:
    specifier.year  = boost::get<date_specifier_t::year_type>(*tok.value);
    break;
  case lexer_t::token_t::TOK_A_MONTH:
    specifier.month =
      date_specifier_t::month_type
        (boost::get<date_time::months_of_year>(*tok.value));
    break;
  case lexer_t::token_t::TOK_A_WDAY:
    specifier.wday  =
      date_specifier_t::day_of_week_type
        (boost::get<date_time::weekdays>(*tok.value));
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
  date_t	  today = CURRENT_DATE();
  bool		  end_inclusive = false;

  for (lexer_t::token_t tok = lexer.next_token();
       tok.kind != lexer_t::token_t::END_REACHED;
       tok = lexer.next_token()) {
    switch (tok.kind) {
#if 0
    case lexer_t::token_t::TOK_INT:
      // jww (2009-11-18): NYI
      assert(! "Need to allow for expressions like \"4 months ago\"");
      tok.unexpected();
      break;
#endif

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

    case lexer_t::token_t::TOK_A_YEAR:
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
	since_specifier	    = inclusion_specifier;
	until_specifier	    = date_specifier_t();
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
      case lexer_t::token_t::TOK_INT:
	// jww (2009-11-18): Allow things like "last 5 weeks"
	assert(! "Need to allow for expressions like \"last 5 weeks\"");
	tok.unexpected();
	break;

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
	date_t temp =
	  date_duration_t::find_nearest(today, date_duration_t::QUARTERS);
	temp += gregorian::months(3 * adjust);
	inclusion_specifier =
	  date_specifier_t(static_cast<date_specifier_t::year_type>(temp.year()),
			   temp.month());
	period.duration = date_duration_t(date_duration_t::QUARTERS, 1);
	break;
      }

      case lexer_t::token_t::TOK_MONTH: {
	date_t temp(today);
	temp += gregorian::months(adjust);
	inclusion_specifier =
	  date_specifier_t(static_cast<date_specifier_t::year_type>(temp.year()),
			   temp.month());
	break;
      }

      case lexer_t::token_t::TOK_WEEK: {
	date_t temp =
	  date_duration_t::find_nearest(today, date_duration_t::WEEKS);
	temp += gregorian::days(7 * adjust);
	inclusion_specifier = date_specifier_t(today);
	period.duration = date_duration_t(date_duration_t::WEEKS, 1);
	break;
      }

      case lexer_t::token_t::TOK_DAY: {
	date_t temp(today);
	temp += gregorian::days(adjust);
	inclusion_specifier = date_specifier_t(temp);
	break;
      }

      default:
	tok.unexpected();
	break;
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
      if (tok == lexer_t::token_t::TOK_INT) {
	int quantity = boost::get<unsigned short>(*tok.value);
	tok = lexer.next_token();
	switch (tok.kind) {
	case lexer_t::token_t::TOK_YEARS:
	  period.skip_duration = date_duration_t(date_duration_t::YEARS, quantity);
	  break;
	case lexer_t::token_t::TOK_QUARTERS:
	  period.skip_duration = date_duration_t(date_duration_t::QUARTERS, quantity);
	  break;
	case lexer_t::token_t::TOK_MONTHS:
	  period.skip_duration = date_duration_t(date_duration_t::MONTHS, quantity);
	  break;
	case lexer_t::token_t::TOK_WEEKS:
	  period.skip_duration = date_duration_t(date_duration_t::WEEKS, quantity);
	  break;
	case lexer_t::token_t::TOK_DAYS:
	  period.skip_duration = date_duration_t(date_duration_t::DAYS, quantity);
	  break;
	default:
	  tok.unexpected();
	  break;
	}
      } else {
	switch (tok.kind) {
	case lexer_t::token_t::TOK_YEAR:
	  period.skip_duration = date_duration_t(date_duration_t::YEARS, 1);
	  break;
	case lexer_t::token_t::TOK_QUARTER:
	  period.skip_duration = date_duration_t(date_duration_t::QUARTERS, 1);
	  break;
	case lexer_t::token_t::TOK_MONTH:
	  period.skip_duration = date_duration_t(date_duration_t::MONTHS, 1);
	  break;
	case lexer_t::token_t::TOK_WEEK:
	  period.skip_duration = date_duration_t(date_duration_t::WEEKS, 1);
	  break;
	case lexer_t::token_t::TOK_DAY:
	  period.skip_duration = date_duration_t(date_duration_t::DAYS, 1);
	  break;
	default:
	  tok.unexpected();
	  break;
	}
      }
      break;

    case lexer_t::token_t::TOK_YEARLY:
      period.skip_duration = date_duration_t(date_duration_t::YEARS, 1);
      break;
    case lexer_t::token_t::TOK_QUARTERLY:
      period.skip_duration = date_duration_t(date_duration_t::QUARTERS, 1);
      break;
    case lexer_t::token_t::TOK_BIMONTHLY:
      period.skip_duration = date_duration_t(date_duration_t::MONTHS, 2);
      break;
    case lexer_t::token_t::TOK_MONTHLY:
      period.skip_duration = date_duration_t(date_duration_t::MONTHS, 1);
      break;
    case lexer_t::token_t::TOK_BIWEEKLY:
      period.skip_duration = date_duration_t(date_duration_t::WEEKS, 2);
      break;
    case lexer_t::token_t::TOK_WEEKLY:
      period.skip_duration = date_duration_t(date_duration_t::WEEKS, 1);
      break;
    case lexer_t::token_t::TOK_DAILY:
      period.skip_duration = date_duration_t(date_duration_t::DAYS, 1);
      break;

    default:
      tok.unexpected();
      break;
    }
  }

  if (! period.duration && inclusion_specifier)
    period.duration = inclusion_specifier->implied_duration();

  if (since_specifier || until_specifier) {
    date_range_t range(since_specifier, until_specifier);
    range.end_inclusive = end_inclusive;

    period.range = date_specifier_or_range_t(range);
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

  if (! skip_duration) {
    skip_duration = duration;
    DEBUG("times.interval",
	  "stabilize: skip_duration set to: " << *skip_duration);
  }

  if (start && ! next) {
    next = skip_duration->add(*start);
    DEBUG("times.interval",
	  "stabilize: next set to: " << *next);
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
  default:
    assert(false);
    break;
  }
  return result;
}

void date_interval_t::stabilize(const optional<date_t>& date)
{
#if defined(DEBUG_ON)
  if (date)
    DEBUG("times.interval", "stabilize: with date = " << *date);
#endif

  if (date && ! aligned) {
    DEBUG("times.interval", "stabilize: date passed, but not aligned");
    if (duration) {
      DEBUG("times.interval",
	    "stabilize: aligning with a duration: " << *duration);

      // The interval object has not been seeded with a start date yet, so
      // find the nearest period before on on date which fits, if possible.
      //
      // Find an efficient starting point for the upcoming while loop.  We
      // want a date early enough that the range will be correct, but late
      // enough that we don't spend hundreds of thousands of loops skipping
      // through time.
      optional<date_t> initial_start  = start  ? start  : begin(date->year());
      optional<date_t> initial_finish = finish ? finish : end(date->year());

#if defined(DEBUG_ON)
      if (initial_start)
	DEBUG("times.interval",
	      "stabilize: initial_start = " << *initial_start);
      if (initial_finish)
	DEBUG("times.interval",
	      "stabilize: initial_finish   = " << *initial_finish);
#endif

      date_t when = start ? *start : *date;

      if (duration->quantum == date_duration_t::MONTHS ||
	  duration->quantum == date_duration_t::QUARTERS ||
	  duration->quantum == date_duration_t::YEARS) {
	DEBUG("times.interval",
	      "stabilize: monthly, quarterly or yearly duration");
	start = date_duration_t::find_nearest(when, duration->quantum);
      } else {
	DEBUG("times.interval", "stabilize: daily or weekly duration");
	start = date_duration_t::find_nearest(when - gregorian::days(400),
					      duration->quantum);
      }

      DEBUG("times.interval",
	    "stabilize: beginning start date = " << *start);

      while (*start < *date) {
	date_interval_t next_interval(*this);
	++next_interval;

	if (next_interval.start && *next_interval.start < *date) {
	  *this = next_interval;
	} else {
	  end_of_duration = none;
	  next		  = none;
	  break;
	}
      }

      DEBUG("times.interval", "stabilize: final start date = " << *start);

      if (initial_start && (! start || *start < *initial_start)) {
	resolve_end();

	start = initial_start;
	DEBUG("times.interval", "stabilize: start reset to initial start");
      }
      if (initial_finish && (! finish || *finish > *initial_finish)) {
	finish = initial_finish;
	DEBUG("times.interval", "stabilize: finish reset to initial finish");
      }
    }
    else if (range) {
      start    = range->begin();
      finish   = range->end();

      if (start && finish)
	duration = date_duration_t(date_duration_t::DAYS,
				   static_cast<int>((*finish - *start).days()));
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

bool date_interval_t::find_period(const date_t& date)
{
  stabilize(date);

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

  while (date >= scan && (! finish || scan < *finish)) {
    if (date < end_of_scan) {
      start	      = scan;
      end_of_duration = end_of_scan;
      next	      = none;

      DEBUG("times.interval", "true: start	     = " << *start);
      DEBUG("times.interval", "true: end_of_duration = " << *end_of_duration);

      return true;
    }

    scan = skip_duration->add(scan);
    end_of_scan = duration->add(scan);
  }

  return false;
}

date_interval_t& date_interval_t::operator++()
{
  if (! start)
    throw_(date_error, _("Cannot increment an unstarted date interval"));

  stabilize();

  if (! skip_duration || ! duration)
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

  if (skip_duration)
    out << _("    skip: ") << skip_duration->to_string() << std::endl;
  if (factor)
    out << _("  factor: ") << factor << std::endl;
  if (duration)
    out << _("duration: ") << duration->to_string() << std::endl;

  stabilize(begin());

  out << std::endl
      << _("--- After stabilization ---") << std::endl;

  if (range)
    out << _("   range: ") << range->to_string() << std::endl;
  if (start)
    out << _("   start: ") << format_date(*start) << std::endl;
  if (finish)
    out << _("  finish: ") << format_date(*finish) << std::endl;

  if (skip_duration)
    out << _("    skip: ") << skip_duration->to_string() << std::endl;
  if (factor)
    out << _("  factor: ") << factor << std::endl;
  if (duration)
    out << _("duration: ") << duration->to_string() << std::endl;

  out << std::endl
      << _("--- Sample dates in range (max. 20) ---") << std::endl;

  for (int i = 0; i < 20 && *this; i++, ++*this) {
    out << std::right;
    out.width(2);

    out << (i + 1) << ": " << format_date(*start);
    if (skip_duration)
      out << " -- " << format_date(*inclusive_skip_end());
    out << std::endl;

    if (! skip_duration)
      break;
  }
}

date_parser_t::lexer_t::token_t date_parser_t::lexer_t::next_token()
{
  if (token_cache.kind != token_t::UNKNOWN) {
    token_t tok = token_cache;
    token_cache = token_t();
    return tok;
  }

  while (begin != end && std::isspace(*begin))
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
  if (std::isdigit(*begin)) {
    try {
      string::const_iterator i = begin;
      for (i = begin; i != end && ! std::isspace(*i); i++) {}
      assert(i != begin);

      string possible_date(start, i);
      date_traits_t traits;

      date_t when = parse_date_mask(possible_date.c_str(), none, &traits);
      if (! when.is_not_a_date()) {
	begin = i;
	return token_t(token_t::TOK_DATE,
		       token_t::content_t(date_specifier_t(when, traits)));
      }
    }
    catch (...) {}
  }

  string term;
  bool alnum = std::isalnum(*begin);
  for (start = begin; (begin != end && ! std::isspace(*begin) &&
		       alnum == std::isalnum(*begin)); begin++)
    term.push_back(*begin);

  if (! term.empty()) {
    if (std::isdigit(term[0])) {
      if (term.length() == 4)
	return token_t(token_t::TOK_A_YEAR,
		       token_t::content_t
		       (lexical_cast<date_specifier_t::year_type>(term)));
      else
	return token_t(token_t::TOK_INT,
		       token_t::content_t(lexical_cast<unsigned short>(term)));
    }
    else if (std::isalpha(term[0])) {
      if (optional<date_time::months_of_year> month =
	  string_to_month_of_year(term)) {
	return token_t(token_t::TOK_A_MONTH, token_t::content_t(*month));
      }
      else if (optional<date_time::weekdays> wday =
	       string_to_day_of_week(term)) {
	return token_t(token_t::TOK_A_WDAY, token_t::content_t(*wday));
      }
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

  return token_t(token_t::UNKNOWN);
}

void date_parser_t::lexer_t::token_t::unexpected()
{
  kind_t prev_kind = kind;

  kind = UNKNOWN;

  switch (prev_kind) {
  case END_REACHED:
    throw_(date_error, _("Unexpected end of expression"));
  default:
    throw_(date_error, _("Unexpected token '%1'") << to_string());
  }
}

void date_parser_t::lexer_t::token_t::expected(char wanted, char c)
{
  if (c == '\0' || c == -1) {
    if (wanted == '\0' || wanted == -1)
      throw_(date_error, _("Unexpected end"));
    else
      throw_(date_error, _("Missing '%1'") << wanted);
  } else {
    if (wanted == '\0' || wanted == -1)
      throw_(date_error, _("Invalid char '%1'") << c);
    else
      throw_(date_error, _("Invalid char '%1' (wanted '%2')") << c << wanted);
  }
}

namespace {
  typedef std::map<std::string, datetime_io_t *> datetime_io_map;
  typedef std::map<std::string, date_io_t *>     date_io_map;

  datetime_io_map temp_datetime_io;
  date_io_map	  temp_date_io;
}

std::string format_datetime(const datetime_t&		  when,
			    const format_type_t		  format_type,
			    const optional<const char *>& format)
{
  if (format_type == FMT_WRITTEN) {
    return written_datetime_io->format(when);
  }
  else if (format_type == FMT_CUSTOM || format) {
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

std::string format_date(const date_t&		      when,
			const format_type_t	      format_type,
			const optional<const char *>& format)
{
  if (format_type == FMT_WRITTEN) {
    return written_date_io->format(when);
  }
  else if (format_type == FMT_CUSTOM || format) {
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
  printed_datetime_io->set_format(format);
}

void set_date_format(const char * format)
{
  printed_date_io->set_format(format);
}

void set_input_date_format(const char * format)
{
  input_date_io.reset(new date_io_t(format, true));
}

void times_initialize()
{
  if (! is_initialized) {
    input_datetime_io.reset(new datetime_io_t("%Y/%m/%d %H:%M:%S", true));

    written_datetime_io.reset(new datetime_io_t("%Y/%m/%d %H:%M:%S", false));
    written_date_io.reset(new date_io_t("%Y/%m/%d", false));

    printed_datetime_io.reset(new datetime_io_t("%y-%b-%d %H:%M:%S", false));
    printed_date_io.reset(new date_io_t("%y-%b-%d", false));

    readers.push_back(shared_ptr<date_io_t>(new date_io_t("%m/%d", true)));
    readers.push_back(shared_ptr<date_io_t>(new date_io_t("%Y/%m/%d", true)));
    readers.push_back(shared_ptr<date_io_t>(new date_io_t("%Y/%m", true)));
    readers.push_back(shared_ptr<date_io_t>(new date_io_t("%y/%m/%d", true)));

    is_initialized = true;
  }
}

void times_shutdown()
{
  if (is_initialized) {
    input_datetime_io.reset();
    input_date_io.reset();
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
    out << token.to_string() << std::endl;
  }
  while (token.kind != date_parser_t::lexer_t::token_t::END_REACHED);
}

} // namespace ledger
