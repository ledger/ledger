#define BOOST_TEST_DYN_LINK
#define BOOST_TEST_MODULE util
#include <boost/test/unit_test.hpp>

#include <system.hh>

#include "utils.h"
#include "times.h"

using namespace ledger;

struct times_fixture {
  times_fixture() {
    times_initialize();
  }
  ~times_fixture() {
    times_shutdown();
  }
};

BOOST_FIXTURE_TEST_SUITE(times, times_fixture)

BOOST_AUTO_TEST_CASE(testConstructors)
{
#ifndef NOT_FOR_PYTHON
  std::time_t now         = std::time(NULL);
  struct tm * moment      = std::localtime(&now);
  std::time_t localMoment = std::mktime(moment);
#endif // NOT_FOR_PYTHON

#ifndef NOT_FOR_PYTHON
  date_t d0;
  date_t d1;
  datetime_t d3;
  date_t d4;
  date_t d5;
  date_t d6;
  date_t d7;
  date_t d8;
  date_t d9;

#if 0
  date_t d10;
  date_t d11;
  date_t d12;
  date_t d13;
  date_t d14;
  datetime_t d15;
#endif
#endif // NOT_FOR_PYTHON

  d1 = parse_date("1990/01/01");
#ifndef NOT_FOR_PYTHON
  d3 = boost::posix_time::from_time_t(localMoment);
#endif // NOT_FOR_PYTHON
  d4 = parse_date("2006/12/25");
  d5 = parse_date("12/25");
  d6 = parse_date("2006.12.25");
  d7 = parse_date("12.25");
  d8 = parse_date("2006-12-25");
  d9 = parse_date("12-25");

#ifndef NOT_FOR_PYTHON
#if 0
  d10 = parse_date("tue");
  d11 = parse_date("tuesday");
  d12 = parse_date("feb");
  d13 = parse_date("february");
  d14 = parse_date("2006");
  d15 = d3;
#endif
#endif // NOT_FOR_PYTHON

#ifndef NOT_FOR_PYTHON
  BOOST_CHECK(d0.is_not_a_date());
  BOOST_CHECK(! d1.is_not_a_date());
  BOOST_CHECK(! d4.is_not_a_date());
#endif // NOT_FOR_PYTHON

  BOOST_CHECK(CURRENT_DATE() > d1);
  BOOST_CHECK(CURRENT_DATE() > d4);

#ifndef NOT_FOR_PYTHON
#if 0
  BOOST_CHECK_EQUAL(d3, d15);
#endif
#endif // NOT_FOR_PYTHON
  BOOST_CHECK_EQUAL(d4, d6);
  BOOST_CHECK_EQUAL(d4, d8);
  BOOST_CHECK_EQUAL(d5, d7);
  BOOST_CHECK_EQUAL(d5, d9);
#ifndef NOT_FOR_PYTHON
#if 0
  BOOST_CHECK_EQUAL(d10, d11);
  BOOST_CHECK_EQUAL(d12, d13);
  
  BOOST_CHECK_THROW(parse_date("2007/02/29"), boost::gregorian::bad_day_of_month);
  //BOOST_CHECK_THROW(parse_date("2007/13/01"), datetime_error);
  //BOOST_CHECK_THROW(parse_date("2007/00/01"), datetime_error);
  BOOST_CHECK_THROW(parse_date("2007/01/00"), boost::gregorian::bad_day_of_month);
  //BOOST_CHECK_THROW(parse_date("2007/00/00"), boost::gregorian::bad_day_of_month);
  //BOOST_CHECK_THROW(parse_date("2007/05/32"), boost::gregorian::bad_day_of_month);

  BOOST_CHECK_THROW(parse_date("2006x/12/25"), datetime_error);
  BOOST_CHECK_THROW(parse_date("2006/12x/25"), datetime_error);
  BOOST_CHECK_THROW(parse_date("2006/12/25x"), datetime_error);

  BOOST_CHECK_THROW(parse_date("feb/12/25"), datetime_error);
  BOOST_CHECK_THROW(parse_date("2006/mon/25"), datetime_error);
  BOOST_CHECK_THROW(parse_date("2006/12/web"), datetime_error);

  BOOST_CHECK_THROW(parse_date("12*25"), datetime_error);

  BOOST_CHECK_THROW(parse_date("tuf"), datetime_error);
  BOOST_CHECK_THROW(parse_date("tufsday"), datetime_error);
  BOOST_CHECK_THROW(parse_date("fec"), datetime_error);
  BOOST_CHECK_THROW(parse_date("fecruary"), datetime_error);
  BOOST_CHECK_THROW(parse_date("207x"), datetime_error);
  BOOST_CHECK_THROW(parse_date("hello"), datetime_error);

  d1 = parse_date("2002-02-02");
  d1 = parse_date("2002/02/02");
  d1 = parse_date("2002.02.02");
  d1 = parse_date("02-02-2002");
  d1 = parse_date("02/02/2002");
  d1 = parse_date("02.02.2002");
  d1 = parse_date("02-02-02");
  d1 = parse_date("02/02/02");
  d1 = parse_date("02.02.02");
  d1 = parse_date("02-02");
  d1 = parse_date("02/02");
  d1 = parse_date("02.02");
  d1 = parse_date("20020202");
  d1 = parse_date("20020202T023318");
  d1 = parse_date("20020202T023318-0700");
  d1 = parse_date("20020202T023318-0100");
  d1 = parse_date("02-Feb-2002");
  d1 = parse_date("2002-Feb-02");
  d1 = parse_date("02 Feb 2002");
  d1 = parse_date("02-Feb-2002");
  d1 = parse_date("02 February 2002");
  d1 = parse_date("02-February-2002");
  d1 = parse_date("2002 Feb 02");
  d1 = parse_date("2002-Feb-02");
  d1 = parse_date("2002 February 02");
  d1 = parse_date("2002-February-02");
  d1 = parse_date("02 Feb");
  d1 = parse_date("02-Feb");
  d1 = parse_date("02 February");
  d1 = parse_date("02-February");
  d1 = parse_date("Feb 02");
  d1 = parse_date("Feb-02");
  d1 = parse_date("February 02");
  d1 = parse_date("February-02");
  d1 = parse_date("Feb 02, 2002");
  d1 = parse_date("February 02, 2002");
  d1 = parse_date("2002-02-02 12:00:00");
  d1 = parse_date("2002-02-02 12:00:00 AM");
  d1 = parse_date("2002-02-02 12:00 AM");
  d1 = parse_date("2002-02-02 12:00AM");
  d1 = parse_date("2002-02-02 12p");
  d1 = parse_date("2002-02-02 12a");

  BOOST_CHECK(d1.valid());
#endif // NOT_FOR_PYTHON
#endif
}

BOOST_AUTO_TEST_SUITE_END()
