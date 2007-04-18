#include "Commodity.h"
#include "ledger.h"

using namespace ledger;

CPPUNIT_TEST_SUITE_NAMED_REGISTRATION(CommodityTestCase, "numerics");

void CommodityTestCase::setUp() {}
void CommodityTestCase::tearDown() {}

void CommodityTestCase::testConstructors()
{

}

void CommodityTestCase::testPriceHistory()
{
  // jww (2007-04-17): tbd
  amount_t x1("100.10 AAPL");

  assertEqual(x1, x1.value(datetime_t()));

  assertValid(x1);
}

void CommodityTestCase::testLots()
{
  // jww (2007-04-17): tbd
}

void CommodityTestCase::testScalingBase()
{
  // jww (2007-04-17): tbd
}

void CommodityTestCase::testReduction()
{
  // jww (2007-04-17): tbd
}

