#ifndef _DATETIMETEST_H
#define _DATETIMETEST_H

#include "UnitTests.h"

class DateTimeTestCase : public CPPUNIT_NS::TestCase
{
  CPPUNIT_TEST_SUITE(DateTimeTestCase);

  CPPUNIT_TEST(testConstructors);

  CPPUNIT_TEST_SUITE_END();

public:
  DateTimeTestCase() {}
  virtual ~DateTimeTestCase() {}

  virtual void setUp();
  virtual void tearDown();

  void testConstructors();

private:
  DateTimeTestCase(const DateTimeTestCase &copy);
  void operator=(const DateTimeTestCase &copy);
};

#endif /* _DATETIMETEST_H */
