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
  datetime_t jan17_07("2007/01/17 00:00:00");
  datetime_t feb27_07("2007/02/27 18:00:00");
  datetime_t feb28_07("2007/02/28 06:00:00");
  datetime_t feb28_07sbm("2007/02/28 11:59:59");
  datetime_t mar01_07("2007/03/01 00:00:00");
  datetime_t apr15_07("2007/04/15 13:00:00");

  // jww (2007-04-17): tbd
  amount_t x1("100.10 AAPL");

  // Commodities cannot be constructed by themselves, since a great
  // deal of their state depends on how they were seen to be used.
  commodity_t& aapl(x1.commodity());

  aapl.add_price(datetime_t(), amount_t("$10.20"));

  assertEqual(amount_t("$1021.02"), x1.value(datetime_t()));

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

