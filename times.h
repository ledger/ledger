#ifndef _TIMES_H
#define _TIMES_H

#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/date_time/posix_time/posix_time_duration.hpp>
#include <boost/date_time/posix_time/posix_time_config.hpp>
#include <boost/date_time/local_time_adjustor.hpp>

#include <ctime>
#include <string>

namespace ledger {

typedef boost::posix_time::ptime   ptime;
typedef boost::posix_time::seconds seconds;
typedef ptime::time_duration_type  time_duration;

class interval_t
{
public:
  interval_t() {}
  interval_t(const std::string& desc) {}

  operator bool() const {
    return false;
  }

  void start(const ptime& moment) {}
  ptime next() const {}

  void parse(std::istream& in) {}
};

inline ptime ptime_local_to_utc(const ptime& when) {
  struct std::tm tm_gmt = to_tm(when);
  return boost::posix_time::from_time_t(std::mktime(&tm_gmt));
}

// jww (2007-04-18): I need to make a general parsing function
// instead, and then make these into private methods.
inline ptime ptime_from_local_date_string(const std::string& date_string) {
  return ptime_local_to_utc(ptime(boost::gregorian::from_string(date_string),
				  time_duration()));
}

inline ptime ptime_from_local_time_string(const std::string& time_string) {
  return ptime_local_to_utc(boost::posix_time::time_from_string(time_string));
}

extern ptime now;

}

#endif /* _TIMES_H */
