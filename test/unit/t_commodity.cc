#include "t_commodity.h"

CPPUNIT_TEST_SUITE_NAMED_REGISTRATION(CommodityTestCase, "numerics");

void CommodityTestCase::setUp() {
  ledger::set_session_context(&session);
}
void CommodityTestCase::tearDown() {
  ledger::set_session_context();
}

void CommodityTestCase::testPriceHistory()
{
  datetime_t jan17_07    = parse_datetime("2007/01/17 00:00:00");
  datetime_t feb27_07    = parse_datetime("2007/02/27 18:00:00");
  datetime_t feb28_07    = parse_datetime("2007/02/28 06:00:00");
  datetime_t feb28_07sbm = parse_datetime("2007/02/28 11:59:59");
  datetime_t mar01_07    = parse_datetime("2007/03/01 00:00:00");
  datetime_t apr15_07    = parse_datetime("2007/04/15 13:00:00");

  // jww (2007-04-17): tbd
  amount_t x0;
  amount_t x1("100.10 AAPL");

  assertThrow(x0.value(), amount_error);
  assertFalse(x1.value());

  // Commodities cannot be constructed by themselves, since a great
  // deal of their state depends on how they were seen to be used.
  commodity_t& aapl(x1.commodity());

  aapl.add_price(jan17_07, amount_t("$10.20"));
  aapl.add_price(feb27_07, amount_t("$13.40"));
  aapl.add_price(feb28_07, amount_t("$18.33"));
  aapl.add_price(feb28_07sbm, amount_t("$18.30"));
  aapl.add_price(mar01_07, amount_t("$19.50"));
  aapl.add_price(apr15_07, amount_t("$21.22"));
  aapl.add_price(apr15_07, amount_t("EUR 23.00"));

  optional<amount_t> amt1 = x1.value(feb28_07sbm);
  assertTrue(amt1);
  assertEqual(amount_t("$1831.83"), *amt1);

  commodity_t& euro(amount_t("EUR 1.00").commodity());

  optional<amount_t> amt2 = x1.value(current_time, euro);
  assertTrue(amt2);
  assertEqual(amount_t("EUR 2302.30"), *amt2);

  optional<amount_t> amt3 = x1.value(current_time);
  assertTrue(amt3);
  assertEqual(amount_t("$2124.12"), *amt3);

  euro.add_price(feb27_07, amount_t("CAD 40.00"));

  commodity_t& cad(amount_t("CAD 1.00").commodity());

  optional<amount_t> amt4 = x1.value(current_time, cad);
  assertTrue(amt4);
  assertEqual(amount_t("CAD 92092.00"), *amt4);

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

