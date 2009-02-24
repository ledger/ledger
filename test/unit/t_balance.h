#ifndef _T_BALANCE_H
#define _T_BALANCE_H

#include "UnitTests.h"

class BalanceTestCase : public CPPUNIT_NS::TestCase
{
  CPPUNIT_TEST_SUITE(BalanceTestCase);

  //CPPUNIT_TEST(testConstructors);

  CPPUNIT_TEST_SUITE_END();

public:
  BalanceTestCase() {}
  virtual ~BalanceTestCase() {}

  virtual void setUp();
  virtual void tearDown();

  //void testConstructors();

private:
  BalanceTestCase(const BalanceTestCase &copy);
  void operator=(const BalanceTestCase &copy);
};

#endif // _T_BALANCE_H
