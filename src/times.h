#ifndef _TIMES_H
#define _TIMES_H

namespace ledger {

#define SUPPORT_DATE_AND_TIME 1
#ifdef SUPPORT_DATE_AND_TIME

typedef boost::posix_time::ptime	moment_t;
typedef moment_t::time_duration_type	duration_t;

inline bool is_valid_moment(const moment_t& moment) {
  return ! moment.is_not_a_date_time();
}

#else // SUPPORT_DATE_AND_TIME

typedef boost::gregorian::date          moment_t;
typedef boost::gregorian::date_duration duration_t;

inline bool is_valid_moment(const moment_t& moment) {
  return ! moment.is_not_a_date();
}

#endif // SUPPORT_DATE_AND_TIME

extern const moment_t& now;

DECLARE_EXCEPTION(datetime_error);

class interval_t
{
public:
  interval_t() {}
  interval_t(const string&) {}

  operator bool() const {
    return false;
  }

  void start(const moment_t&) {}
  moment_t next() const { return moment_t(); }

  void parse(std::istream&) {}
};

#if 0
inline moment_t ptime_local_to_utc(const moment_t& when) {
  struct std::tm tm_gmt = to_tm(when);
  return boost::posix_time::from_time_t(std::mktime(&tm_gmt));
}

// jww (2007-04-18): I need to make a general parsing function
// instead, and then make these into private methods.
inline moment_t ptime_from_local_date_string(const string& date_string) {
  return ptime_local_to_utc(moment_t(boost::gregorian::from_string(date_string),
				     time_duration()));
}

inline moment_t ptime_from_local_time_string(const string& time_string) {
  return ptime_local_to_utc(boost::posix_time::time_from_string(time_string));
}
#endif

moment_t parse_datetime(const char * str);

inline moment_t parse_datetime(const string& str) {
  return parse_datetime(str.c_str());
}

extern const ptime time_now;
extern const date  date_now;
extern bool        day_before_month;

#if 0
struct intorchar
{
  int	      ival;
  string sval;

  intorchar() : ival(-1) {}
  intorchar(int val) : ival(val) {}
  intorchar(const string& val) : ival(-1), sval(val) {}
  intorchar(const intorchar& o) : ival(o.ival), sval(o.sval) {}
};

ledger::moment_t parse_abs_datetime(std::istream& input);
#endif

} // namespace ledger

#endif // _TIMES_H
