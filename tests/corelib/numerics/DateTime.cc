#include "DateTimeTest.h"
#include "ledger.h"
#include "acconf.h"

#include <ctime>

using namespace ledger;

CPPUNIT_TEST_SUITE_NAMED_REGISTRATION(DateTimeTestCase, "numerics");

void DateTimeTestCase::setUp() {}
void DateTimeTestCase::tearDown() {}

void DateTimeTestCase::testConstructors()
{
  std::time_t time_t_now = std::time(NULL);
  struct tm * moment = std::localtime(&time_t_now);

  std::time_t localMoment = std::mktime(moment);

  ptime d0;
  ptime d1(parse_datetime("1990/01/01"));
  ptime d3(boost::posix_time::from_time_t(localMoment));
  ptime d4(parse_datetime("2006/12/25"));
  ptime d5(parse_datetime("12/25"));
  ptime d6(parse_datetime("2006.12.25"));
  ptime d7(parse_datetime("12.25"));
  ptime d8(parse_datetime("2006-12-25"));
  ptime d9(parse_datetime("12-25"));
#if 0
  ptime d10(parse_datetime("tue"));
  ptime d11(parse_datetime("tuesday"));
  ptime d12(parse_datetime("feb"));
  ptime d13(parse_datetime("february"));
  ptime d14(parse_datetime("2006"));
#endif
  ptime d15(d3);

  assertTrue(d0.is_not_a_date_time());
  assertFalse(d1.is_not_a_date_time());
  assertFalse(d4.is_not_a_date_time());

  assertTrue(now > d1);
  assertTrue(now <= d3);
  assertTrue(now > d4);

  assertEqual(d3, d15);
  assertEqual(d4, d6);
  assertEqual(d4, d8);
  assertEqual(d5, d7);
  assertEqual(d5, d9);
#if 0
  assertEqual(d10, d11);
  assertEqual(d12, d13);
#endif
  
  assertThrow(parse_datetime("2007/02/29"), datetime_error *);
  assertThrow(parse_datetime("2007/13/01"), datetime_error *);
  assertThrow(parse_datetime("2007/00/01"), datetime_error *);
  assertThrow(parse_datetime("2007/01/00"), datetime_error *);
  assertThrow(parse_datetime("2007/00/00"), datetime_error *);
  assertThrow(parse_datetime("2007/05/32"), datetime_error *);

  assertThrow(parse_datetime("2006x/12/25"), datetime_error *);
  assertThrow(parse_datetime("2006/12x/25"), datetime_error *);
  assertThrow(parse_datetime("2006/12/25x"), datetime_error *);

  assertThrow(parse_datetime("feb/12/25"), datetime_error *);
  assertThrow(parse_datetime("2006/mon/25"), datetime_error *);
  assertThrow(parse_datetime("2006/12/web"), datetime_error *);

  assertThrow(parse_datetime("12*25"), datetime_error *);

  assertThrow(parse_datetime("tuf"), datetime_error *);
  assertThrow(parse_datetime("tufsday"), datetime_error *);
  assertThrow(parse_datetime("fec"), datetime_error *);
  assertThrow(parse_datetime("fecruary"), datetime_error *);
  assertThrow(parse_datetime("207x"), datetime_error *);
  assertThrow(parse_datetime("hello"), datetime_error *);

  interval_t i1;
  interval_t i2;
}
