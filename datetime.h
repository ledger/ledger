#ifndef _DATETIME_H
#define _DATETIME_H

#include <ctime>
#include <sstream>

#include "error.h"

class date_error : public error {
 public:
  date_error(const std::string& reason) throw() : error(reason) {}
  virtual ~date_error() throw() {}
};

struct interval_t;
class  datetime_t;

class date_t
{
 public:
  static date_t       now;
  static const char * formats[];
  static int	      current_year;
  static std::string  input_format;
  static std::string  output_format;

  std::time_t when;

  date_t() : when(0) {}
  date_t(const date_t& _when) : when(_when.when) {}
  date_t(const datetime_t& _when);
  date_t(const std::time_t _when) : when(_when) {
#if 0
    struct std::tm * moment = std::localtime(&_when);
    moment->tm_hour = 0;
    moment->tm_min  = 0;
    moment->tm_sec  = 0;
    when = std::mktime(moment);
#endif
  }
  date_t(const interval_t& period);
  date_t(const std::string& _when);

  virtual ~date_t() {}

  date_t& operator=(const date_t& _when) {
    when = _when.when;
    return *this;
  }
  date_t& operator=(const std::time_t _when) {
    return *this = date_t(_when);
  }
  date_t& operator=(const datetime_t& _when) {
    return *this = date_t(_when);
  }
  date_t& operator=(const interval_t& period) {
    return *this = date_t(period);
  }
  date_t& operator=(const std::string& _when) {
    return *this = date_t(_when);
  }

  date_t& operator+=(const interval_t& period);

  long operator-=(const date_t& date) {
    return (when - date.when) / 86400;
  }

  virtual date_t& operator+=(const long days) {
    // jww (2006-03-26): This is not accurate enough when DST is in effect!
    assert(0);
    when += days * 86400;
    return *this;
  }
  virtual date_t& operator-=(const long days) {
    assert(0);
    when -= days * 86400;
    return *this;
  }

#define DEF_DATE_OP(OP)				\
  bool operator OP(const date_t& other) const {	\
    return when OP other.when;			\
  }

  DEF_DATE_OP(<)
  DEF_DATE_OP(<=)
  DEF_DATE_OP(>)
  DEF_DATE_OP(>=)
  DEF_DATE_OP(==)
  DEF_DATE_OP(!=)

  operator bool() const {
    return when != 0;
  }
  operator std::string() const {
    return to_string();
  }

  std::string to_string(const std::string& format = output_format) const {
    char buf[64];
    std::strftime(buf, 63, format.c_str(), localtime());
    return buf;
  }

  int year() const {
    return localtime()->tm_year + 1900;
  }
  int month() const {
    return localtime()->tm_mon + 1;
  }
  int day() const {
    return localtime()->tm_mday;
  }
  int wday() const {
    return localtime()->tm_wday;
  }

  std::tm * localtime() const {
    return std::localtime(&when);
  }

  void write(std::ostream& out,
	     const std::string& format = output_format) const {
    out << to_string(format);
  }

  friend class datetime_t;
  friend struct interval_t;
};

inline long operator-(const date_t& left, const date_t& right) {
  date_t temp(left);
  temp -= right;
  return (long)temp;
}

inline date_t operator+(const date_t& left, const long days) {
  date_t temp(left);
  temp += days;
  return temp;
}

inline date_t operator-(const date_t& left, const long days) {
  date_t temp(left);
  temp -= days;
  return temp;
}

inline std::ostream& operator<<(std::ostream& out, const date_t& moment) {
  moment.write(out);
  return out;
}

class datetime_error : public error {
 public:
  datetime_error(const std::string& reason) throw() : error(reason) {}
  virtual ~datetime_error() throw() {}
};

class datetime_t : public date_t
{
 public:
  static datetime_t now;

  datetime_t() : date_t() {}
  datetime_t(const datetime_t& _when) : date_t(_when.when) {}
  datetime_t(const date_t& _when) : date_t(_when) {}

  datetime_t(const std::time_t _when) : date_t(_when) {}
  datetime_t(const std::string& _when);

  datetime_t& operator=(const datetime_t& _when) {
    when = _when.when;
    return *this;
  }
  datetime_t& operator=(const date_t& _when) {
    when = _when.when;
    return *this;
  }
  datetime_t& operator=(const std::time_t _when) {
    return *this = datetime_t(_when);
  }
  datetime_t& operator=(const std::string& _when) {
    return *this = datetime_t(_when);
  }

  long operator-=(const datetime_t& date) {
    return when - date.when;
  }

  virtual datetime_t& operator+=(const long secs) {
    when += secs;
    return *this;
  }
  virtual datetime_t& operator-=(const long secs) {
    when -= secs;
    return *this;
  }

#define DEF_DATETIME_OP(OP)				\
  bool operator OP(const datetime_t& other) const {	\
    return when OP other.when;				\
  }

  DEF_DATETIME_OP(<)
  DEF_DATETIME_OP(<=)
  DEF_DATETIME_OP(>)
  DEF_DATETIME_OP(>=)
  DEF_DATETIME_OP(==)
  DEF_DATETIME_OP(!=)

  int hour() const {
    return localtime()->tm_hour;
  }
  int min() const {
    return localtime()->tm_min;
  }
  int sec() const {
    return localtime()->tm_sec;
  }

  friend inline long operator-(const datetime_t& left, const datetime_t& right) {
    std::time_t left_time  = left.when;
    std::time_t right_time = right.when;
    return long(left_time) - long(right_time);
  }
};

inline datetime_t operator+(const datetime_t& left, const long seconds) {
  datetime_t temp(left);
  temp += seconds;
  return temp;
}

inline datetime_t operator-(const datetime_t& left, const long seconds) {
  datetime_t temp(left);
  temp -= seconds;
  return temp;
}

inline std::ostream& operator<<(std::ostream& out,
				const datetime_t& moment) {
  char buf[64];
  std::strftime(buf, 63, (date_t::output_format + " %H:%M:%S").c_str(),
		moment.localtime());
  out << buf;
  return out;
}

struct interval_t
{
  int years;
  int months;
  int days;
  int hours;
  int minutes;
  int seconds;

  datetime_t begin;
  datetime_t end;

  interval_t(int _days = 0, int _months = 0, int _years = 0,
	     const date_t& _begin = date_t(),
	     const date_t& _end   = date_t())
    : years(_years), months(_months), days(_days),
      hours(0), minutes(0), seconds(0),
      begin(_begin), end(_end) {}

  interval_t(const std::string& desc)
    : years(0), months(0), days(0),
      hours(0), minutes(0), seconds(0) {
    std::istringstream stream(desc);
    parse(stream);
  }

  operator bool() const {
    return (years > 0 || months > 0  || days > 0 ||
	    hours > 0 || minutes > 0 || seconds > 0);
  }

  void start(const datetime_t& moment) {
    begin = first(moment);
  }
  datetime_t first(const datetime_t& moment = datetime_t()) const;
  datetime_t increment(const datetime_t&) const;

  void parse(std::istream& in);
};

inline date_t::date_t(const interval_t& period) {
  when = period.first().when;
}

inline date_t& date_t::operator+=(const interval_t& period) {
  return *this = period.increment(*this);
}

inline date_t::date_t(const datetime_t& _when) {
  struct std::tm * moment = _when.localtime();
  moment->tm_hour = 0;
  moment->tm_min  = 0;
  moment->tm_sec  = 0;
  when = std::mktime(moment);
}

#endif // _DATETIME_H
