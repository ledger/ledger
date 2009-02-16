#include "t_times.h"

#include "utils.h"

using namespace ledger;

CPPUNIT_TEST_SUITE_NAMED_REGISTRATION(DateTimeTestCase, "util");

void DateTimeTestCase::setUp() {
}
void DateTimeTestCase::tearDown() {
}

void DateTimeTestCase::testConstructors()
{
#ifndef NOT_FOR_PYTHON
  std::time_t now	  = std::time(NULL);
  struct tm * moment	  = std::localtime(&now);
  std::time_t localMoment = std::mktime(moment);
#endif // NOT_FOR_PYTHON

#ifndef NOT_FOR_PYTHON
  datetime_t d0;
  datetime_t d1;
  datetime_t d3;
  datetime_t d4;
  datetime_t d5;
  datetime_t d6;
  datetime_t d7;
  datetime_t d8;
  datetime_t d9;

  datetime_t d10;
  datetime_t d11;
  datetime_t d12;
  datetime_t d13;
  datetime_t d14;
  datetime_t d15;
#endif // NOT_FOR_PYTHON

  d1 = parse_datetime("1990/01/01");
#ifndef NOT_FOR_PYTHON
  d3 = boost::posix_time::from_time_t(localMoment);
#endif // NOT_FOR_PYTHON
  d4 = parse_datetime("2006/12/25");
  d5 = parse_datetime("12/25");
  d6 = parse_datetime("2006.12.25");
  d7 = parse_datetime("12.25");
  d8 = parse_datetime("2006-12-25");
  d9 = parse_datetime("12-25");

  d10 = parse_datetime("tue");
  d11 = parse_datetime("tuesday");
  d12 = parse_datetime("feb");
  d13 = parse_datetime("february");
  d14 = parse_datetime("2006");
#ifndef NOT_FOR_PYTHON
  d15 = d3;
#endif // NOT_FOR_PYTHON

#ifndef NOT_FOR_PYTHON
  assertTrue(d0.is_not_a_date_time());
  assertFalse(d1.is_not_a_date_time());
  assertFalse(d4.is_not_a_date_time());
#endif // NOT_FOR_PYTHON

  assertTrue(CURRENT_TIME() > d1);
  assertTrue(CURRENT_TIME() > d4);

#ifndef NOT_FOR_PYTHON
  assertEqual(d3, d15);
#endif // NOT_FOR_PYTHON
  assertEqual(d4, d6);
  assertEqual(d4, d8);
  assertEqual(d5, d7);
  assertEqual(d5, d9);
  assertEqual(d10, d11);
  assertEqual(d12, d13);
  
#if 0
#ifndef NOT_FOR_PYTHON
  assertThrow(parse_datetime("2007/02/29"), boost::gregorian::bad_day_of_month);
  //assertThrow(parse_datetime("2007/13/01"), datetime_error);
  //assertThrow(parse_datetime("2007/00/01"), datetime_error);
  assertThrow(parse_datetime("2007/01/00"), boost::gregorian::bad_day_of_month);
  //assertThrow(parse_datetime("2007/00/00"), boost::gregorian::bad_day_of_month);
  //assertThrow(parse_datetime("2007/05/32"), boost::gregorian::bad_day_of_month);

  assertThrow(parse_datetime("2006x/12/25"), datetime_error);
  assertThrow(parse_datetime("2006/12x/25"), datetime_error);
  assertThrow(parse_datetime("2006/12/25x"), datetime_error);

  assertThrow(parse_datetime("feb/12/25"), datetime_error);
  assertThrow(parse_datetime("2006/mon/25"), datetime_error);
  assertThrow(parse_datetime("2006/12/web"), datetime_error);

  assertThrow(parse_datetime("12*25"), datetime_error);

  assertThrow(parse_datetime("tuf"), datetime_error);
  assertThrow(parse_datetime("tufsday"), datetime_error);
  assertThrow(parse_datetime("fec"), datetime_error);
  assertThrow(parse_datetime("fecruary"), datetime_error);
  assertThrow(parse_datetime("207x"), datetime_error);
  assertThrow(parse_datetime("hello"), datetime_error);

  d1 = parse_datetime("2002-02-02");
  d1 = parse_datetime("2002/02/02");
  d1 = parse_datetime("2002.02.02");
  d1 = parse_datetime("02-02-2002");
  d1 = parse_datetime("02/02/2002");
  d1 = parse_datetime("02.02.2002");
  d1 = parse_datetime("02-02-02");
  d1 = parse_datetime("02/02/02");
  d1 = parse_datetime("02.02.02");
  d1 = parse_datetime("02-02");
  d1 = parse_datetime("02/02");
  d1 = parse_datetime("02.02");
  d1 = parse_datetime("20020202");
  d1 = parse_datetime("20020202T023318");
  d1 = parse_datetime("20020202T023318-0700");
  d1 = parse_datetime("20020202T023318-0100");
  d1 = parse_datetime("02-Feb-2002");
  d1 = parse_datetime("2002-Feb-02");
  d1 = parse_datetime("02 Feb 2002");
  d1 = parse_datetime("02-Feb-2002");
  d1 = parse_datetime("02 February 2002");
  d1 = parse_datetime("02-February-2002");
  d1 = parse_datetime("2002 Feb 02");
  d1 = parse_datetime("2002-Feb-02");
  d1 = parse_datetime("2002 February 02");
  d1 = parse_datetime("2002-February-02");
  d1 = parse_datetime("02 Feb");
  d1 = parse_datetime("02-Feb");
  d1 = parse_datetime("02 February");
  d1 = parse_datetime("02-February");
  d1 = parse_datetime("Feb 02");
  d1 = parse_datetime("Feb-02");
  d1 = parse_datetime("February 02");
  d1 = parse_datetime("February-02");
  d1 = parse_datetime("Feb 02, 2002");
  d1 = parse_datetime("February 02, 2002");
  d1 = parse_datetime("2002-02-02 12:00:00");
  d1 = parse_datetime("2002-02-02 12:00:00 AM");
  d1 = parse_datetime("2002-02-02 12:00 AM");
  d1 = parse_datetime("2002-02-02 12:00AM");
  d1 = parse_datetime("2002-02-02 12p");
  d1 = parse_datetime("2002-02-02 12a");

  assertValid(d1);
#endif // NOT_FOR_PYTHON
#endif
}
