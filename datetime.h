#ifndef _DATETIME_H
#define _DATETIME_H

#include <ctime>
#include <sstream>

#include "error.h"

struct interval_t;

struct datetime_t
{
  std::time_t when;

  static std::string date_format;

  datetime_t(const std::time_t _when) : when(_when) {}

  datetime_t& operator+=(const long secs) {
    when += secs;
    return *this;
  }
  datetime_t& operator-=(const long secs) {
    when -= secs;
    return *this;
  }

  datetime_t& operator=(const interval_t& period);
  datetime_t& operator+=(const interval_t& period);

#define DEF_DATETIME_OP(OP)			\
  bool operator OP(const datetime_t& other) {	\
    return when OP other.when;			\
  }

  DEF_DATETIME_OP(<)
  DEF_DATETIME_OP(<=)
  DEF_DATETIME_OP(>)
  DEF_DATETIME_OP(>=)
  DEF_DATETIME_OP(==)
  DEF_DATETIME_OP(!=)

  operator bool() const {
    return when != 0;
  }
  operator long() const {
    return (long)when;
  }
  operator double() const {
    return (double)when;
  }

  int year() const {
    struct std::tm * desc = std::localtime(&when);
    return desc->tm_year + 1900;
  }
  int month() const {
    struct std::tm * desc = std::localtime(&when);
    return desc->tm_mon + 1;
  }
  int day() const {
    struct std::tm * desc = std::localtime(&when);
    return desc->tm_mday;
  }
};

inline std::ostream& operator<<(std::ostream& out, const datetime_t& moment) {
  char buf[32];
  std::strftime(buf, 31, datetime_t::date_format.c_str(),
		std::localtime(&moment.when));
  out << buf;
}

inline long operator-(const datetime_t& left, const datetime_t& right) {
  return (long)left.when - (long)right.when;
}

struct interval_t
{
  unsigned int years;
  unsigned int months;
  unsigned int seconds;
  std::time_t  begin;
  std::time_t  end;

  interval_t(int _seconds = 0, int _months = 0, int _years = 0,
	     std::time_t _begin = 0, std::time_t _end = 0)
    : years(_years), months(_months), seconds(_seconds),
      begin(_begin), end(_end) {
  }
  interval_t(const std::string& desc)
    : years(0), months(0), seconds(0), begin(0), end(0) {
    std::istringstream stream(desc);
    parse(stream);
  }

  operator bool() const {
    return seconds > 0 || months > 0 || years > 0;
  }

  void start(const std::time_t moment) {
    begin = first(moment);
  }
  std::time_t first(const std::time_t moment = 0) const;
  std::time_t increment(const std::time_t) const;

  void parse(std::istream& in);
};

inline datetime_t& datetime_t::operator=(const interval_t& period) {
  when = period.first();
  return *this;
}
inline datetime_t& datetime_t::operator+=(const interval_t& period) {
  when = period.increment(when);
  return *this;
}

extern std::time_t  now;
extern int	    now_year;
extern char         input_format[128];
extern const char * formats[];

bool parse_date_mask(const char * date_str, struct std::tm * result);
bool parse_date(const char * date_str, std::time_t * result,
		const int year = -1);
bool quick_parse_date(const char * date_str, std::time_t * result);

class datetime_error : public error {
 public:
  datetime_error(const std::string& reason) throw() : error(reason) {}
  virtual ~datetime_error() throw() {}
};

#endif // _DATETIME_H
