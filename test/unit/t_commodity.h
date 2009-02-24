#ifndef _T_COMMMODITY_H
#define _T_COMMMODITY_H

#include "UnitTests.h"

class CommodityTestCase : public CPPUNIT_NS::TestCase
{
  CPPUNIT_TEST_SUITE(CommodityTestCase);

  CPPUNIT_TEST(testPriceHistory);

  CPPUNIT_TEST_SUITE_END();

public:
  CommodityTestCase() {}
  virtual ~CommodityTestCase() {}

  virtual void setUp();
  virtual void tearDown();

  void testPriceHistory();

private:
  CommodityTestCase(const CommodityTestCase &copy);
  void operator=(const CommodityTestCase &copy);
};

#endif // _T_COMMMODITY_H
