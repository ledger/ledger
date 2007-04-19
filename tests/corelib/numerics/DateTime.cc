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
  struct tm moment;
  std::memset(&moment, INT_MAX, sizeof(struct tm));

#ifdef HAVE_STRPTIME
  strptime("2006/12/25 00:00:00", "%Y/%m/%d %H:%M:%S", &moment);
#endif

#ifdef HAVE_TIMEGM
  std::time_t gmtMoment	  = timegm(&moment);
#endif
  std::time_t localMoment = std::mktime(&moment);

  date_t d1;
#ifdef HAVE_TIMEGM
  date_t d2(gmtMoment);
#endif
  date_t d3(localMoment);
  date_t d4("2006/12/25");
  date_t d5("12/25");
  date_t d6("2006.12.25");
  date_t d7("12.25");
  date_t d8("2006-12-25");
  date_t d9("12-25");
  date_t d10("tue");
  date_t d11("tuesday");
  date_t d12("feb");
  date_t d13("february");
  date_t d14("2006");
  date_t d15(d3);

#ifdef HAVE_TIMEGM
  if (std::memcmp(&gmtMoment, &localMoment, sizeof(std::time_t)) == 0)
    assertEqual(d2, d3);
  else
    assertNotEqual(d2, d3);
#endif

  assertFalse(d1);
  assertTrue(d4);

  assertTrue(date_t::now > d1);
  assertTrue(date_t::now > d3);
  assertTrue(date_t::now > d4);

  assertEqual(d3, d4);
  assertEqual(d3, d15);
  assertEqual(d4, d6);
  assertEqual(d4, d8);
  assertEqual(d5, d7);
  assertEqual(d5, d9);
  assertEqual(d10, d11);
  assertEqual(d12, d13);
  
  assertThrow(date_t("2007/02/29"), date_error *);
  assertThrow(date_t("2007/13/01"), date_error *);
  assertThrow(date_t("2007/00/01"), date_error *);
  assertThrow(date_t("2007/01/00"), date_error *);
  assertThrow(date_t("2007/00/00"), date_error *);
  assertThrow(date_t("2007/05/32"), date_error *);

  assertThrow(date_t("2006x/12/25"), date_error *);
  assertThrow(date_t("2006/12x/25"), date_error *);
  assertThrow(date_t("2006/12/25x"), date_error *);

  assertThrow(date_t("feb/12/25"), date_error *);
  assertThrow(date_t("2006/mon/25"), date_error *);
  assertThrow(date_t("2006/12/web"), date_error *);

  assertThrow(date_t("12*25"), date_error *);

  assertThrow(date_t("tuf"), date_error *);
  assertThrow(date_t("tufsday"), date_error *);
  assertThrow(date_t("fec"), date_error *);
  assertThrow(date_t("fecruary"), date_error *);
  assertThrow(date_t("207x"), date_error *);
  assertThrow(date_t("hello"), date_error *);

  datetime_t dt1;
  datetime_t dt2;
  datetime_t dt3;
  datetime_t dt4;
  datetime_t dt5;
  
  interval_t i1;
  interval_t i2;
}
