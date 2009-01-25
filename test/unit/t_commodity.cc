#include "t_commodity.h"

#include "utils.h"
#include "amount.h"

using namespace ledger;

CPPUNIT_TEST_SUITE_NAMED_REGISTRATION(CommodityTestCase, "math");

void CommodityTestCase::setUp() {
  amount_t::initialize();
}

void CommodityTestCase::tearDown() {
  amount_t::shutdown();
}

void CommodityTestCase::testPriceHistory()
{
  datetime_t jan17_05    = parse_datetime("2005/01/17 00:00:00");
  datetime_t jan17_06    = parse_datetime("2006/01/17 00:00:00");
  datetime_t jan17_07    = parse_datetime("2007/01/17 00:00:00");
  datetime_t feb27_07    = parse_datetime("2007/02/27 18:00:00");
  datetime_t feb28_07    = parse_datetime("2007/02/28 06:00:00");
  datetime_t feb28_07sbm = parse_datetime("2007/02/28 11:59:59");
  datetime_t mar01_07    = parse_datetime("2007/03/01 00:00:00");
  datetime_t apr15_07    = parse_datetime("2007/04/15 13:00:00");

  amount_t x0;
  amount_t x1("100.10 AAPL");

  assertThrow(x0.value(), amount_error);
  assertFalse(x1.value());

  // Commodities cannot be constructed by themselves, since a great deal
  // of their state depends on how they were seen to be used.
  commodity_t& aapl(x1.commodity());

  aapl.add_price(jan17_07,    amount_t("$10.20"));
  aapl.add_price(feb27_07,    amount_t("$13.40"));
  aapl.add_price(feb28_07,    amount_t("$18.33"));
  aapl.add_price(feb28_07sbm, amount_t("$18.30"));
  aapl.add_price(mar01_07,    amount_t("$19.50"));
  aapl.add_price(apr15_07,    amount_t("$21.22"));
  aapl.add_price(jan17_05,    amount_t("EUR 23.00"));
  aapl.add_price(jan17_06,    amount_t("CAD 25.00"));

  commodity_t& euro(amount_t("EUR 1.00").commodity());

  euro.add_price(feb27_07, amount_t("CAD 1.40"));
  euro.add_price(jan17_05, amount_t("$0.78"));

  commodity_t& cad(amount_t("CAD 1.00").commodity());

  cad.add_price(jan17_06, amount_t("$1.11"));

  optional<amount_t> amt = x1.value(feb28_07sbm);
  assertTrue(amt);
  assertEqual(amount_t("$1831.83"), *amt);

  amt = x1.value(current_time);
  assertTrue(amt);
  assertEqual(amount_t("$2124.12"), *amt);

  amt = x1.value(current_time, euro);
  assertTrue(amt);
  assertEqual(amount_t("EUR 1366.87"), *amt);

  // Add a newer Euro pricing
  aapl.add_price(jan17_07, amount_t("EUR 23.00"));

  amt = x1.value(current_time, euro);
  assertTrue(amt);
  assertEqual(amount_t("EUR 2302.30"), *amt);

  amt = x1.value(current_time, cad);
  assertTrue(amt);
  assertEqual(amount_t("CAD 3223.22"), *amt);

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

