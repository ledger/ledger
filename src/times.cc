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

date_time::weekdays start_of_week	    = gregorian::Sunday;

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
#endif

  public:
    bool	       has_year;
    bool	       input;

    temporal_io_t(const char * _fmt_str, bool _input)
      : fmt_str(_fmt_str), has_year(icontains(fmt_str, "%y")),
	input(_input) {
#if defined(USE_BOOST_FACETS)
      if (input) {
	input_facet  = new InputFacetType(fmt_str);
	input_stream.imbue(std::locale(std::locale::classic(), input_facet));
      } else {
	output_facet = new OutputFacetType(fmt_str);
	output_stream.imbue(std::locale(std::locale::classic(), output_facet));
      }
#endif
    }

    void set_format(const char * fmt) {
      fmt_str  = fmt;
      has_year = icontains(fmt_str, "%y");

#if defined(USE_BOOST_FACETS)
      if (input)
	input_facet->format(fmt_str);
      else
	output_facet->format(fmt_str);
#endif
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
#else
      std::tm data(to_tm(when));
      char buf[128];
      std::strftime(buf, 127, fmt_str, &data);
      return buf;
#endif
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
	DEBUG("times.parse", "Failed to parse '" << str
	      << "' using pattern '" << fmt_str << "'");
#endif

      if (! when.is_not_a_date_time() &&
	  input_stream.good() && ! input_stream.eof() &&
	  input_stream.peek() != EOF)
	return datetime_t();
      return when;
#else
    std::tm data;
    std::memset(&data, 0, sizeof(std::tm));
    if (strptime(str, fmt_str, &data))
      return posix_time::ptime_from_tm(data);
    else
      return datetime_t();
#endif
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
	DEBUG("times.parse", "Failed to parse '" << str
	      << "' using pattern '" << fmt_str << "'");
#endif

      if (! when.is_not_a_date() &&
	  input_stream.good() && ! input_stream.eof() &&
	  input_stream.peek() != EOF)
	return date_t();
      return when;
#else
    std::tm data;
    std::memset(&data, 0, sizeof(std::tm));
    data.tm_mday = 1;		// some formats have no day
    if (strptime(str, fmt_str, &data))
      return gregorian::date_from_tm(data);
    else
      return date_t();
#endif
  }

  typedef temporal_io_t<datetime_t, posix_time::time_input_facet,
			posix_time::time_facet> datetime_io_t;
  typedef temporal_io_t<date_t, gregorian::date_input_facet,
			gregorian::date_facet> date_io_t;

  std::auto_ptr<datetime_io_t> written_datetime_io;
  std::auto_ptr<datetime_io_t> printed_datetime_io;
  std::auto_ptr<date_io_t>     input_date_io;
  std::auto_ptr<date_io_t>     written_date_io;
  std::auto_ptr<date_io_t>     printed_date_io;

  std::vector<shared_ptr<date_io_t> > readers;

  date_t parse_date_mask_routine(const char * date_str, date_io_t& io,
				 optional<date_t::year_type> year,
				 bool& saw_year)
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

      if (! io.has_year) {
	saw_year = false;

	when = date_t(year ? *year : CURRENT_DATE().year(),
		      when.month(), when.day());

	if (when.month() > CURRENT_DATE().month())
	  when -= gregorian::years(1);
      } else {
	saw_year = true;
      }
    }
    return when;
  }

  date_t parse_date_mask(const char * date_str,
			 optional<date_t::year_type> year, bool& saw_year)
  {
    if (input_date_io.get()) {
      date_t when = parse_date_mask_routine(date_str, *input_date_io.get(),
					    year, saw_year);
      if (! when.is_not_a_date())
	return when;
    }

    foreach (shared_ptr<date_io_t>& reader, readers) {
      date_t when = parse_date_mask_routine(date_str, *reader.get(),
					    year, saw_year);
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
  return written_datetime_io->parse(str);
}

date_t parse_date(const char * str, optional<date_t::year_type> current_year)
{
  bool saw_year;
  return parse_date_mask(str, current_year, saw_year);
}

date_t date_interval_t::add_duration(const date_t&     date,
				     const duration_t& duration)
{
  if (duration.type() == typeid(gregorian::days))
    return date + boost::get<gregorian::days>(duration);
  else if (duration.type() == typeid(gregorian::weeks))
    return date + boost::get<gregorian::weeks>(duration);
  else if (duration.type() == typeid(gregorian::months))
    return date + boost::get<gregorian::months>(duration);
  else
    assert(duration.type() == typeid(gregorian::years));
  return date + boost::get<gregorian::years>(duration);
}

date_t date_interval_t::subtract_duration(const date_t&     date,
					  const duration_t& duration)
{
  if (duration.type() == typeid(gregorian::days))
    return date - boost::get<gregorian::days>(duration);
  else if (duration.type() == typeid(gregorian::weeks))
    return date - boost::get<gregorian::weeks>(duration);
  else if (duration.type() == typeid(gregorian::months))
    return date - boost::get<gregorian::months>(duration);
  else
    assert(duration.type() == typeid(gregorian::years));
  return date - boost::get<gregorian::years>(duration);
}

std::ostream& operator<<(std::ostream& out,
			 const date_interval_t::duration_t& duration)
{
  if (duration.type() == typeid(gregorian::days))
    out << boost::get<gregorian::days>(duration).days()
	<< " day(s)";
  else if (duration.type() == typeid(gregorian::weeks))
    out << (boost::get<gregorian::weeks>(duration).days() / 7)
	<< " week(s)";
  else if (duration.type() == typeid(gregorian::months))
    out << boost::get<gregorian::months>(duration).number_of_months()
	<< " month(s)";
  else {
    assert(duration.type() == typeid(gregorian::years));
    out << boost::get<gregorian::years>(duration).number_of_years()
	<< " year(s)";
  }
  return out;
}

void date_interval_t::resolve_end()
{
  if (start && ! end_of_duration) {
    end_of_duration = add_duration(*start, *duration);
    DEBUG("times.interval",
	  "stabilize: end_of_duration = " << *end_of_duration);
  }

  if (end && *end_of_duration > *end) {
    end_of_duration = end;
    DEBUG("times.interval",
	  "stabilize: end_of_duration reset to end: " << *end_of_duration);
  }

  if (! skip_duration) {
    skip_duration = duration;
    DEBUG("times.interval",
	  "stabilize: skip_duration set to: " << *skip_duration);
  }

  if (start && ! next) {
    next = add_duration(*start, *skip_duration);
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
      optional<date_t> initial_start = start;
      optional<date_t> initial_end   = end;

#if defined(DEBUG_ON)
      if (initial_start)
	DEBUG("times.interval",
	      "stabilize: initial_start = " << *initial_start);
      if (initial_end)
	DEBUG("times.interval",
	      "stabilize: initial_end   = " << *initial_end);
#endif

      date_t when = start ? *start : *date;

      if (duration->type() == typeid(gregorian::months) ||
	  duration->type() == typeid(gregorian::years)) {
	DEBUG("times.interval", "stabilize: monthly or yearly duration");

	start = date_t(when.year(), gregorian::Jan, 1);
      } else {
	DEBUG("times.interval", "stabilize: daily or weekly duration");

	start = date_t(when - gregorian::days(400));

	if (duration->type() == typeid(gregorian::weeks)) {
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
      if (initial_end && (! end || *end > *initial_end)) {
	end = initial_end;
	DEBUG("times.interval", "stabilize: end reset to initial end");
      }
    }
    aligned = true;
  }

  // If there is no duration, then if we've reached here the date falls
  // between begin and end.
  if (! duration) {
    DEBUG("times.interval", "stabilize: there was no duration given");

    if (! start && ! end)
      throw_(date_error,
	     _("Invalid date interval: neither start, nor end, nor duration"));
  } else {
    resolve_end();
  }
}

bool date_interval_t::find_period(const date_t& date)
{
  stabilize(date);

  if (end && date > *end) {
    DEBUG("times.interval",
	  "false: date [" << date << "] > end [" << *end << "]");
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

  while (date >= scan && (! end || scan < *end)) {
    if (date < end_of_scan) {
      start	      = scan;
      end_of_duration = end_of_scan;
      next	      = none;

      DEBUG("times.interval", "true: start	     = " << *start);
      DEBUG("times.interval", "true: end_of_duration = " << *end_of_duration);

      return true;
    }

    scan	= add_duration(scan, *skip_duration);
    end_of_scan = add_duration(scan, *duration);
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

  if (end && *next >= *end) {
    start = none;
  } else {
    start = *next;

    end_of_duration = add_duration(*start, *duration);
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
    bool   saw_year = true;
    date_t when	    = parse_date_mask(word.c_str(), none, saw_year);

    if (when.is_not_a_date())
      throw_(date_error, _("Could not parse date mask: %1") << word);

    if (begin) {
      *begin = when;

      if (end) {
	if (saw_year)
	  *end = *begin + gregorian::years(1);
	else
	  *end = *begin + gregorian::months(1);
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
			bool             look_for_start = true,
			bool             look_for_end   = true)
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
    date_t end;
    bool   parse_specifier = false;

    optional<date_interval_t::duration_t> duration;

    assert(look_for_start || look_for_end);

    if (word == _("year")) {
      duration = gregorian::years(1);
      start    = gregorian::date(start.year(), 1, 1);
    }
    else if (word == _("month")) {
      duration = gregorian::months(1);
      start    = gregorian::date(start.year(), start.month(), 1);
    }
    else if (word == _("today") || word == _("day")) {
      duration = gregorian::days(1);
    }
    else {
      parse_specifier = true;
    }

    if (parse_specifier)
      parse_inclusion_specifier(word, &start, &end);
    else
      end = date_interval_t::add_duration(start, *duration);

    if (type == _("last") && duration) {
      start = date_interval_t::subtract_duration(start, *duration);
      end   = date_interval_t::subtract_duration(end, *duration);
    }
    else if (type == _("next") && duration) {
      start = date_interval_t::add_duration(start, *duration);
      end   = date_interval_t::add_duration(end, *duration);
    }

    if (look_for_start && is_valid(start)) interval.start = start;
    if (look_for_end   && is_valid(end))   interval.end   = end;
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
	  duration = gregorian::days(quantity);
	else if (word == _("weeks"))
	  duration = gregorian::weeks(quantity);
	else if (word == _("months"))
	  duration = gregorian::months(quantity);
	else if (word == _("quarters"))
	  duration = gregorian::months(3 * quantity);
	else if (word == _("years"))
	  duration = gregorian::years(quantity);
      }
      else if (word == _("day"))
	duration = gregorian::days(1);
      else if (word == _("week"))
	duration = gregorian::weeks(1);
      else if (word == _("month"))
	duration = gregorian::months(1);
      else if (word == _("quarter"))
	duration = gregorian::months(3);
      else if (word == _("year"))
	duration = gregorian::years(1);
    }
    else if (word == _("daily"))
      duration = gregorian::days(1);
    else if (word == _("weekly"))
      duration = gregorian::weeks(1);
    else if (word == _("biweekly"))
      duration = gregorian::weeks(2);
    else if (word == _("monthly"))
      duration = gregorian::months(1);
    else if (word == _("bimonthly"))
      duration = gregorian::months(2);
    else if (word == _("quarterly"))
      duration = gregorian::months(3);
    else if (word == _("yearly"))
      duration = gregorian::years(1);
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
      date_t b, e;
      parse_inclusion_specifier(word, &b, &e);
      start = b;
      end   = e;
    }
  }

  if (year || mon || wday) {
    if (! start)
      start = CURRENT_DATE();

    if (wday) {
      while (start->day_of_week() != *wday)
	*start -= gregorian::days(1);

      if (! end)
	end = *start + gregorian::days(1);
    } else {
      bool overwrite_end = false;

      if (year) {
	start = date_t(*year, 1, 1);
	if (! end) {
	  end = *start + gregorian::years(1);
	  overwrite_end = true;
	}
      }

      if (mon) {
	start = date_t(start->year(), *mon, 1);
	if (! end || overwrite_end)
	  end = *start + gregorian::months(1);
      }
    }
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
    assert(0);
    return "";
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
    assert(0);
    return "";
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
    printed_datetime_io.reset();
    written_datetime_io.reset();
    input_date_io.reset();
    printed_date_io.reset();
    written_date_io.reset();

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
} // namespace ledger
