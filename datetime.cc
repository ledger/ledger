#if defined(__GNUG__) && __GNUG__ < 3
#define _XOPEN_SOURCE
#endif

#include "datetime.h"
#include "util.h"

#include <ctime>
#include <cctype>

date_t       date_t::now(std::time(NULL));
int	     date_t::current_year = date_t::now.year();
std::string  date_t::input_format;
std::string  date_t::output_format = "%Y/%m/%d";

const char * date_t::formats[] = {
  "%Y/%m/%d",
  "%m/%d",
  "%Y.%m.%d",
  "%m.%d",
  "%Y-%m-%d",
  "%m-%d",
  "%a",
  "%A",
  "%b",
  "%B",
  "%Y",
  NULL
};

datetime_t datetime_t::now(std::time(NULL));

namespace {
  static std::time_t base = -1;
  static int base_year = -1;

  static const int month_days[12] = {
    31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31
  };

  bool parse_date_mask(const char * date_str, struct std::tm * result)
  {
    if (! date_t::input_format.empty()) {
      std::memset(result, INT_MAX, sizeof(struct std::tm));
      if (strptime(date_str, date_t::input_format.c_str(), result))
	return true;
    }
    for (const char ** f = date_t::formats; *f; f++) {
      std::memset(result, INT_MAX, sizeof(struct std::tm));
      if (strptime(date_str, *f, result))
	return true;
    }
    return false;
  }

  bool parse_date(const char * date_str, std::time_t * result, const int year)
  {
    struct std::tm when;

    if (! parse_date_mask(date_str, &when))
      return false;

    when.tm_hour = 0;
    when.tm_min  = 0;
    when.tm_sec  = 0;

    if (when.tm_year == -1)
      when.tm_year = ((year == -1) ? date_t::current_year : (year - 1900));

    if (when.tm_mon == -1)
      when.tm_mon = 0;

    if (when.tm_mday == -1)
      when.tm_mday = 1;

    *result = std::mktime(&when);

    return true;
  }

  inline bool quick_parse_date(const char * date_str, std::time_t * result)
  {
    return parse_date(date_str, result, date_t::current_year + 1900);
  }
}

date_t::date_t(const std::string& _when)
{
  if (! quick_parse_date(_when.c_str(), &when))
    throw new date_error
      (std::string("Invalid date string: ") + _when);
}

void date_t::parse(std::istream& in)
{
  char buf[256];
  char c = peek_next_nonws(in);
  READ_INTO(in, buf, 255, c,
	    std::isalnum(c) || c == '-' || c == '.' || c == '/');

  if (! quick_parse_date(buf, &when))
    throw new date_error
      (std::string("Invalid date string: ") + buf);
}

datetime_t::datetime_t(const std::string& _when)
{
  std::istringstream datestr(_when);
  parse(datestr);		// parse both the date and optional time
}

void datetime_t::parse(std::istream& in)
{
  date_t::parse(in);		// first grab the date part

  istream_pos_type beg_pos = in.tellg();

  int hour = 0;
  int min  = 0;
  int sec  = 0;

  // Now look for the (optional) time specifier.  If no time is given,
  // we use midnight of the given day.
  char buf[256];
  char c = peek_next_nonws(in);
  if (! std::isdigit(c))
    goto abort;
  READ_INTO(in, buf, 255, c, std::isdigit(c));
  if (buf[0] == '\0')
    goto abort;

  hour = std::atoi(buf);
  if (hour > 23)
    goto abort;

  if (in.peek() == ':') {
    in.get(c);
    READ_INTO(in, buf, 255, c, std::isdigit(c));
    if (buf[0] == '\0')
      goto abort;

    min = std::atoi(buf);
    if (min > 59)
      goto abort;

    if (in.peek() == ':') {
      in.get(c);
      READ_INTO(in, buf, 255, c, std::isdigit(c));
      if (buf[0] == '\0')
	goto abort;

      sec = std::atoi(buf);
      if (sec > 59)
	goto abort;
    }
  }

  c = peek_next_nonws(in);
  if (c == 'a' || c == 'p' || c == 'A' || c == 'P') {
    if (hour > 12)
      goto abort;
    in.get(c);

    if (c == 'p' || c == 'P') {
      if (hour != 12)
	hour += 12;
    } else {
      if (hour == 12)
	hour = 0;
    }

    c = in.peek();
    if (c == 'm' || c == 'M')
      in.get(c);
  }

  struct std::tm * desc = std::localtime(&when);

  desc->tm_hour  = hour;
  desc->tm_min   = min;
  desc->tm_sec   = sec;
  desc->tm_isdst = -1;

  when = std::mktime(desc);

  return;			// the time has been successfully parsed

 abort:				// there was no valid time string to parse
  in.clear();
  in.seekg(beg_pos, std::ios::beg);
}

std::ostream& operator<<(std::ostream& out, const datetime_t& moment)
{
  std::string format = datetime_t::output_format;
  std::tm * when = moment.localtime();
  if (when->tm_hour != 0 || when->tm_min != 0 || when->tm_sec != 0)
    format += " %H:%M:%S";

  char buf[64];
  std::strftime(buf, 63, format.c_str(), when);
  out << buf;
  return out;
}

datetime_t interval_t::first(const datetime_t& moment) const
{
  datetime_t quant(begin);

  if (moment && moment > quant) {
    // Find an efficient starting point for the upcoming while loop.
    // We want a date early enough that the range will be correct, but
    // late enough that we don't spend hundreds of thousands of loops
    // skipping through time.

    struct std::tm * desc = std::localtime(&moment.when);

    if (years)
      desc->tm_mon = 0;
    desc->tm_mday  = 1;

    desc->tm_hour  = 0;
    desc->tm_min   = 0;
    desc->tm_sec   = 0;
    desc->tm_isdst = -1;

    quant = std::mktime(desc);

    datetime_t temp;
    while (moment >= (temp = increment(quant))) {
      if (quant == temp)
	break;
      quant = temp;
    }
  }

  return quant;
}

datetime_t interval_t::increment(const datetime_t& moment) const
{
  struct std::tm * desc = std::localtime(&moment.when);

  if (years)
    desc->tm_year += years;
  if (months)
    desc->tm_mon += months;
  if (days)
    desc->tm_mon += days;

  desc->tm_hour += hours;
  desc->tm_min  += minutes;
  desc->tm_sec  += seconds;

  desc->tm_isdst = -1;

  return std::mktime(desc);
}

namespace {
  void parse_inclusion_specifier(const std::string& word,
				 datetime_t * begin, datetime_t * end)
  {
    struct std::tm when;

    if (! parse_date_mask(word.c_str(), &when))
      throw new datetime_error(std::string("Could not parse date mask: ") + word);

    when.tm_hour	= 0;
    when.tm_min	= 0;
    when.tm_sec	= 0;
    when.tm_isdst = -1;

    bool saw_year = true;
    bool saw_mon  = true;
    bool saw_day  = true;

    if (when.tm_year == -1) {
      when.tm_year = date_t::current_year;
      saw_year = false;
    }
    if (when.tm_mon == -1) {
      when.tm_mon = 0;
      saw_mon = false;
    } else {
      saw_year = false;		// don't increment by year if month used
    }
    if (when.tm_mday == -1) {
      when.tm_mday = 1;
      saw_day = false;
    } else {
      saw_mon = false;		// don't increment by month if day used
      saw_year = false;		// don't increment by year if day used
    }

    if (begin) {
      *begin = std::mktime(&when);
      if (end)
	*end = interval_t(saw_day ? 86400 : 0, saw_mon ? 1 : 0,
			  saw_year ? 1 : 0).increment(*begin);
    }
    else if (end) {
      *end = std::mktime(&when);
    }
  }

  inline void read_lower_word(std::istream& in, std::string& word) {
    in >> word;
    for (int i = 0, l = word.length(); i < l; i++)
      word[i] = std::tolower(word[i]);
  }

  void parse_date_words(std::istream& in, std::string& word,
			datetime_t * begin, datetime_t * end)
  {
    std::string type;

    bool mon_spec = false;
    char buf[32];

    if (word == "this" || word == "last" || word == "next") {
      type = word;
      if (! in.eof())
	read_lower_word(in, word);
      else
	word = "month";
    } else {
      type = "this";
    }

    if (word == "month") {
      std::strftime(buf, 31, "%B", datetime_t::now.localtime());
      word = buf;
      mon_spec = true;
    }
    else if (word == "year") {
      std::strftime(buf, 31, "%Y", datetime_t::now.localtime());
      word = buf;
    }

    parse_inclusion_specifier(word, begin, end);

    if (type == "last") {
      if (mon_spec) {
	if (begin)
	  *begin = interval_t(0, -1, 0).increment(*begin);
	if (end)
	  *end   = interval_t(0, -1, 0).increment(*end);
      } else {
	if (begin)
	  *begin = interval_t(0, 0, -1).increment(*begin);
	if (end)
	  *end   = interval_t(0, 0, -1).increment(*end);
      }
    }
    else if (type == "next") {
      if (mon_spec) {
	if (begin)
	  *begin = interval_t(0, 1, 0).increment(*begin);
	if (end)
	  *end   = interval_t(0, 1, 0).increment(*end);
      } else {
	if (begin)
	  *begin = interval_t(0, 0, 1).increment(*begin);
	if (end)
	  *end   = interval_t(0, 0, 1).increment(*end);
      }
    }
  }
}

void interval_t::parse(std::istream& in)
{
  std::string word;

  while (! in.eof()) {
    read_lower_word(in, word);
    if (word == "every") {
      read_lower_word(in, word);
      if (std::isdigit(word[0])) {
	int quantity = std::atol(word.c_str());
	read_lower_word(in, word);
	if (word == "days")
	  days = quantity;
	else if (word == "weeks")
	  days = 7 * quantity;
	else if (word == "months")
	  months = quantity;
	else if (word == "quarters")
	  months = 3 * quantity;
	else if (word == "years")
	  years = quantity;
	else if (word == "hours")
	  hours = quantity;
	else if (word == "minutes")
	  minutes = quantity;
	else if (word == "seconds")
	  seconds = quantity;
      }
      else if (word == "day")
	days = 1;
      else if (word == "week")
	days = 7;
      else if (word == "month")
	months = 1;
      else if (word == "quarter")
	months = 3;
      else if (word == "year")
	years = 1;
      else if (word == "hour")
	hours = 1;
      else if (word == "minute")
	minutes = 1;
      else if (word == "second")
	seconds = 1;
    }
    else if (word == "daily")
      days = 1;
    else if (word == "weekly")
      days = 7;
    else if (word == "biweekly")
      days = 14;
    else if (word == "monthly")
      months = 1;
    else if (word == "bimonthly")
      months = 2;
    else if (word == "quarterly")
      months = 3;
    else if (word == "yearly")
      years = 1;
    else if (word == "hourly")
      hours = 1;
    else if (word == "this" || word == "last" || word == "next") {
      parse_date_words(in, word, &begin, &end);
    }
    else if (word == "in") {
      read_lower_word(in, word);
      parse_date_words(in, word, &begin, &end);
    }
    else if (word == "from" || word == "since") {
      read_lower_word(in, word);
      parse_date_words(in, word, &begin, NULL);
    }
    else if (word == "to" || word == "until") {
      read_lower_word(in, word);
      parse_date_words(in, word, NULL, &end);
    }
    else {
      parse_inclusion_specifier(word, &begin, &end);
    }
  }
}

#ifdef USE_BOOST_PYTHON

#include <boost/python.hpp>

using namespace boost::python;

unsigned int interval_len(interval_t& interval)
{
  int periods = 1;
  std::time_t when = interval.first();
  while (interval.end && when < interval.end) {
    when = interval.increment(when);
    if (when < interval.end)
      periods++;
  }
  return periods;
}

std::time_t interval_getitem(interval_t& interval, int i)
{
  static std::time_t last_index = 0;
  static std::time_t last_moment = 0;

  if (i == 0) {
    last_index = 0;
    last_moment = interval.first();
  }
  else {
    last_moment = interval.increment(last_moment);
    if (interval.end && last_moment >= interval.end) {
      PyErr_SetString(PyExc_IndexError, "Index out of range");
      throw_error_already_set();
    }
  }
  return last_moment;
}

std::time_t py_parse_date(const char * date_str)
{
  std::time_t temp;
  if (parse_date(date_str, &temp))
    return temp;
  return 0;
}

std::time_t py_parse_date_yr(const char * date_str, const int year)
{
  std::time_t temp;
  if (parse_date(date_str, &temp, year))
    return temp;
  return 0;
}

void export_datetime()
{
  class_< date_t > ("Date")
    .def("now", &date_t::now)
    .def("formats", &date_t::formats)
    .def("current_year", &date_t::current_year)
    .def("input_format", &date_t::input_format)
    .def("output_format", &date_t::output_format)

    .def(init<>())
    .def(init<const date_t&>())
    .def(init<const std::time_t&>())
    .def(init<const interval_t&>())
    .def(init<const std::string&>())

    .def(self += other<const interval_t&>())
    .def(self -= other<const date_t&>())
    .def(self += long())
    .def(self -= long())

    .def(self < other<const date_t&>())
    .def(self <= other<const date_t&>())
    .def(self > other<const date_t&>())
    .def(self >= other<const date_t&>())
    .def(self == other<const date_t&>())
    .def(self != other<const date_t&>())

    .def("year", &date_t::year)
    .def("month", &date_t::month)
    .def("day", &date_t::day)
    .def("wday", &date_t::wday)
    .def("localtime", &date_t::localtime)

    .def("write", &date_t::write)
    .def("parse", &date_t::parse)
    ;

  class_< interval_t >
    ("Interval", init<optional<int, int, int, std::time_t, std::time_t> >())
    .def(init<std::string>())
    .def(! self)

    .def_readwrite("years", &interval_t::years)
    .def_readwrite("months", &interval_t::months)
    .def_readwrite("days", &interval_t::days)
    .def_readwrite("hours", &interval_t::hours)
    .def_readwrite("minutes", &interval_t::minutes)
    .def_readwrite("seconds", &interval_t::seconds)

    .def_readwrite("begin", &interval_t::begin)
    .def_readwrite("end", &interval_t::end)

    .def("__len__", interval_len)
    .def("__getitem__", interval_getitem)

    .def("start", &interval_t::start)
    .def("first", &interval_t::first)
    .def("increment", &interval_t::increment)
    ;

  def("parse_date", py_parse_date);
  def("parse_date", py_parse_date_yr);
}

#endif // USE_BOOST_PYTHON
