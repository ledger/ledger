#ifndef _T_UTILS_H
#define _T_UTILS_H

#include "UnitTests.h"

class UtilitiesTestCase : public CPPUNIT_NS::TestCase
{
  CPPUNIT_TEST_SUITE(UtilitiesTestCase);

  //CPPUNIT_TEST(testConstructors);

  CPPUNIT_TEST_SUITE_END();

public:
  UtilitiesTestCase() {}
  virtual ~UtilitiesTestCase() {}

  //virtual void setUp();
  //virtual void tearDown();

  //void testConstructors();

private:
  UtilitiesTestCase(const UtilitiesTestCase &copy);
  void operator=(const UtilitiesTestCase &copy);
};

#endif /* _T_UTILS_H */
