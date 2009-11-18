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
				 optional<date_t::year_type> year,
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

  date_t parse_date_mask(const char * date_str,
			 optional<date_t::year_type> year,
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

datetime_t parse_datetime(const char * str, optional<date_t::year_type>)
{
  datetime_t when = input_datetime_io->parse(str);
  if (when.is_not_a_date_time())
    throw_(date_error, _("Invalid date/time: %1") << str);
  return when;
}

date_t parse_date(const char * str, optional<date_t::year_type> current_year)
{
  return parse_date_mask(str, current_year);
}

date_t
date_specifier_t::begin(const optional<date_t::year_type>& current_year) const
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

date_t
date_specifier_t::end(const optional<date_t::year_type>& current_year) const
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
  else {
    assert(duration.quantum == date_duration_t::YEARS);
    out << duration.length << " year(s)";
  }
  return out;
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
      optional<date_t> initial_start  = start;
      optional<date_t> initial_finish = finish;

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
	  duration->quantum == date_duration_t::YEARS) {
	DEBUG("times.interval", "stabilize: monthly or yearly duration");

	start = date_t(when.year(), gregorian::Jan, 1);
      } else {
	DEBUG("times.interval", "stabilize: daily or weekly duration");

	start = date_t(when - gregorian::days(400));

	if (duration->quantum == date_duration_t::WEEKS) {
	  // Move it to a Sunday
	  while (start->day_of_week() != start_of_week)
	    *start += gregorian::days(1);
	}
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

    scan	= skip_duration->add(scan);
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

namespace {
  void parse_inclusion_specifier(const string& word,
				 date_t *      begin,
				 date_t *      end)
  {
    date_traits_t traits;
    date_t	  when = parse_date_mask(word.c_str(), none, &traits);

    if (when.is_not_a_date())
      throw_(date_error, _("Could not parse date mask: %1") << word);

    if (begin) {
      *begin = when;

      if (end) {
	if (traits.has_day)
	  *end = *begin + gregorian::days(1);
	else if (traits.has_month)
	  *end = *begin + gregorian::months(1);
	else
	  *end = *begin + gregorian::years(1);
      }
    }
    else if (end) {
      *end = when;
    }
  }

  inline void read_lower_word(std::istream& in, string& word) {
    in >> word;
    for (string::size_type i = 0, l = word.length(); i < l; i++)
      word[i] = static_cast<char>(std::tolower(word[i]));
  }

  void parse_date_words(std::istream&	 in,
			string&		 word,
			date_interval_t& interval,
			bool             look_for_start  = true,
			bool             look_for_finish = true)
  {
    string type;

    if (word == _("this") || word == _("last") || word == _("next")) {
      type = word;
      if (! in.eof())
	read_lower_word(in, word);
      else
	word = _("month");
    } else {
      type = _("this");
    }

    date_t start = CURRENT_DATE();
    date_t finish;
    bool   parse_specifier = false;

    optional<date_duration_t> duration;

    assert(look_for_start || look_for_finish);

    if (word == _("year")) {
      duration = date_duration_t(date_duration_t::YEARS, 1);
      start    = gregorian::date(start.year(), 1, 1);
    }
    else if (word == _("month")) {
      duration = date_duration_t(date_duration_t::MONTHS, 1);
      start    = gregorian::date(start.year(), start.month(), 1);
    }
    else if (word == _("today") || word == _("day")) {
      duration = date_duration_t(date_duration_t::DAYS, 1);
    }
    else {
      parse_specifier = true;
    }

    if (parse_specifier)
      parse_inclusion_specifier(word, &start, &finish);
    else
      finish = duration->add(start);

    if (type == _("last") && duration) {
      start = duration->subtract(start);
      finish   = duration->subtract(finish);
    }
    else if (type == _("next") && duration) {
      start = duration->add(start);
      finish   = duration->add(finish);
    }

    if (look_for_start  && is_valid(start))  interval.start  = start;
    if (look_for_finish && is_valid(finish)) interval.finish = finish;
  }
}

void date_interval_t::parse(std::istream& in)
{
  string word;

  optional<date_time::months_of_year> mon;
  optional<date_time::weekdays>       wday;
  optional<date_t::year_type>         year;

  while (! in.eof()) {
    read_lower_word(in, word);
    if (word == _("every")) {
      read_lower_word(in, word);
      if (std::isdigit(word[0])) {
	int quantity = lexical_cast<int>(word);
	read_lower_word(in, word);
	if (word == _("days"))
	  duration = date_duration_t(date_duration_t::DAYS, quantity);
	else if (word == _("weeks"))
	  duration = date_duration_t(date_duration_t::WEEKS, quantity);
	else if (word == _("months"))
	  duration = date_duration_t(date_duration_t::MONTHS, quantity);
	else if (word == _("quarters"))
	  duration = date_duration_t(date_duration_t::MONTHS, 3 * quantity);
	else if (word == _("years"))
	  duration = date_duration_t(date_duration_t::YEARS, quantity);
      }
      else if (word == _("day"))
	duration = date_duration_t(date_duration_t::DAYS, 1);
      else if (word == _("week"))
	duration = date_duration_t(date_duration_t::WEEKS, 1);
      else if (word == _("month"))
	duration = date_duration_t(date_duration_t::MONTHS, 1);
      else if (word == _("quarter"))
	duration = date_duration_t(date_duration_t::MONTHS, 3);
      else if (word == _("year"))
	duration = date_duration_t(date_duration_t::YEARS, 1);
    }
    else if (word == _("daily"))
      duration = date_duration_t(date_duration_t::DAYS, 1);
    else if (word == _("weekly"))
      duration = date_duration_t(date_duration_t::WEEKS, 1);
    else if (word == _("biweekly"))
      duration = date_duration_t(date_duration_t::WEEKS, 2);
    else if (word == _("monthly"))
      duration = date_duration_t(date_duration_t::MONTHS, 1);
    else if (word == _("bimonthly"))
      duration = date_duration_t(date_duration_t::MONTHS, 2);
    else if (word == _("quarterly"))
      duration = date_duration_t(date_duration_t::MONTHS, 3);
    else if (word == _("yearly"))
      duration = date_duration_t(date_duration_t::YEARS, 1);
    else if (word == _("this") || word == _("last") || word == _("next") ||
	     word == _("today")) {
      parse_date_words(in, word, *this);
    }
    else if (word == _("in")) {
      read_lower_word(in, word);
      parse_date_words(in, word, *this);
    }
    else if (word == _("from") || word == _("since")) {
      read_lower_word(in, word);
      parse_date_words(in, word, *this, true, false);
    }
    else if (word == _("to") || word == _("until")) {
      read_lower_word(in, word);
      parse_date_words(in, word, *this, false, true);
    }
    else if (optional<date_time::months_of_year>
	     m = string_to_month_of_year(word)) {
      mon = m;
    }
    else if (optional<date_time::weekdays>
	     d = string_to_day_of_week(word)) {
      wday = d;
    }
    else if (all(word, is_digit())) {
      year = lexical_cast<unsigned short>(word);
    }
    else {
      // otherwise, it should be an explicit date
      date_t s, f;
      parse_inclusion_specifier(word, &s, &f);
      start  = s;
      finish = f;
    }
  }

  if (year || mon || wday) {
    if (! start)
      start = CURRENT_DATE();

    if (wday) {
      while (start->day_of_week() != *wday)
	*start -= gregorian::days(1);

      if (! finish)
	finish = *start + gregorian::days(1);
    } else {
      bool overwrite_finish = false;

      if (year) {
	start = date_t(*year, 1, 1);
	if (! finish) {
	  finish = *start + gregorian::years(1);
	  overwrite_finish = true;
	}
      }

      if (mon) {
	start = date_t(start->year(), *mon, 1);
	if (! finish || overwrite_finish)
	  finish = *start + gregorian::months(1);
      }
    }
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
      return token_t(term.length() == 4 ?
		     token_t::TOK_A_YEAR : token_t::TOK_INT,
		     token_t::content_t(lexical_cast<int>(term)));
    }
    else if (std::isalpha(term[0])) {
      if (optional<date_time::months_of_year> month =
	  string_to_month_of_year(term)) {
	date_specifier_t specifier;
	specifier.month = static_cast<date_t::month_type>(*month);
	return token_t(token_t::TOK_A_MONTH, token_t::content_t(specifier));
      }
      else if (optional<date_time::weekdays> wday =
	       string_to_day_of_week(term)) {
	date_specifier_t specifier;
	specifier.wday = static_cast<date_t::day_of_week_type>(*wday);
	return token_t(token_t::TOK_A_WDAY, token_t::content_t(specifier));
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

date_interval_t date_parser_t::parse_date_expr()
{
  date_interval_t interval;

  return interval;
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

  date_parser_t::lexer_t::token_t token;
  do {
    token = lexer.next_token();
    out << _("token: ") << token.to_string() << std::endl;
  }
  while (token.kind != date_parser_t::lexer_t::token_t::END_REACHED);
}

void analyze_period(std::ostream& out, const string& arg)
{
  date_parser_t date_parser(arg);

  date_interval_t interval = date_parser.parse();

  out << _("global details => ") << std::endl << std::endl;

  if (interval.start)
    out << _("   start: ") << format_date(*interval.start) << std::endl;
  else
    out << _("   start: TODAY: ") << format_date(CURRENT_DATE()) << std::endl;
  if (interval.finish)
    out << _("  finish: ") << format_date(*interval.finish) << std::endl;

  if (interval.skip_duration)
    out << _("    skip: ") << *interval.skip_duration << std::endl;
  if (interval.factor)
    out << _("  factor: ") << interval.factor << std::endl;
  if (interval.duration)
    out << _("duration: ") << *interval.duration << std::endl;

  if (interval.find_period(interval.start ?
			   *interval.start : CURRENT_DATE())) {
    out << std::endl
	<< _("after finding first period => ") << std::endl
	<< std::endl;

    if (interval.start)
      out << _("   start: ") << format_date(*interval.start) << std::endl;
    if (interval.finish)
      out << _("  finish: ") << format_date(*interval.finish) << std::endl;

    if (interval.skip_duration)
      out << _("    skip: ") << *interval.skip_duration << std::endl;
    if (interval.factor)
      out << _("  factor: ") << interval.factor << std::endl;
    if (interval.duration)
      out << _("duration: ") << *interval.duration << std::endl;

    out << std::endl;

    for (int i = 0; i < 20 && interval; i++, ++interval) {
      out << std::right;
      out.width(2);

      out << i << "): " << format_date(*interval.start);
      if (interval.end_of_duration)
	out << " -- " << format_date(*interval.inclusive_end());
      out << std::endl;

      if (! interval.skip_duration)
	break;
    }
  }
}

} // namespace ledger

#if defined(TIMES_HARNESS)

int main(int argc, char *argv[])
{
  if (argc > 1) {
    ledger::times_initialize();
    ledger::analyze_period(std::cout, argv[1]);
    ledger::times_shutdown();
  } else {
    std::cerr << "Usage: times <PERIOD>" << std::endl;
  }
  return 0;
}

#endif // TIMES_HARNESS
