#ifndef _COMMMODITY_H
#define _COMMMODITY_H

#include "UnitTests.h"

class CommodityTestCase : public CPPUNIT_NS::TestCase
{
  CPPUNIT_TEST_SUITE(CommodityTestCase);

  CPPUNIT_TEST(testPriceHistory);
  CPPUNIT_TEST(testLots);
  CPPUNIT_TEST(testScalingBase);
  CPPUNIT_TEST(testReduction);

  CPPUNIT_TEST_SUITE_END();

public:
  ledger::session_t session;

  CommodityTestCase() {}
  virtual ~CommodityTestCase() {}

  virtual void setUp();
  virtual void tearDown();

  void testPriceHistory();
  void testLots();
  void testScalingBase();
  void testReduction();

private:
  CommodityTestCase(const CommodityTestCase &copy);
  void operator=(const CommodityTestCase &copy);
};

#endif /* _COMMMODITY_H */
