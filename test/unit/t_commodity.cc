#include "t_commodity.h"

#include "commodity.h"

using namespace ledger;

CPPUNIT_TEST_SUITE_NAMED_REGISTRATION(CommodityTestCase, "math");

void CommodityTestCase::setUp() {
  amount_t::initialize();
  amount_t::stream_fullstrings = true;
}

void CommodityTestCase::tearDown() {
  amount_t::shutdown();
}

void CommodityTestCase::testPriceHistory()
{
#ifndef NOT_FOR_PYTHON
  datetime_t jan17_05;
  datetime_t jan17_06;
  datetime_t jan17_07;
  datetime_t feb27_07;
  datetime_t feb28_07;
  datetime_t feb28_07sbm;
  datetime_t mar01_07;
  datetime_t apr15_07;
#endif // NOT_FOR_PYTHON

  jan17_05    = parse_datetime("2005/01/17 00:00:00");
  jan17_06    = parse_datetime("2006/01/17 00:00:00");
  jan17_07    = parse_datetime("2007/01/17 00:00:00");
  feb27_07    = parse_datetime("2007/02/27 18:00:00");
  feb28_07    = parse_datetime("2007/02/28 06:00:00");
  feb28_07sbm = parse_datetime("2007/02/28 11:59:59");
  mar01_07    = parse_datetime("2007/03/01 00:00:00");
  apr15_07    = parse_datetime("2007/04/15 13:00:00");

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

  amount_t one_euro("EUR 1.00");
  commodity_t& euro(one_euro.commodity());

  euro.add_price(feb27_07, amount_t("CAD 1.40"));
  euro.add_price(jan17_05, amount_t("$0.78"));

  amount_t one_cad("CAD 1.00");
  commodity_t& cad(one_cad.commodity());

  cad.add_price(jan17_06, amount_t("$1.11"));

#ifndef NOT_FOR_PYTHON
  optional<amount_t> amt = x1.value(false, feb28_07sbm);
  assertTrue(amt);
  assertEqual(amount_t("$1831.83"), *amt);

  amt = x1.value(false, CURRENT_TIME());
  assertTrue(amt);
  assertEqual(string("$2124.12"), amt->to_string());
#ifdef INTEGER_MATH
  assertEqual(string("$2124.12"), amt->to_fullstring());
#else
  assertEqual(string("$2124.1220"), amt->to_fullstring());
#endif

  amt = x1.value(false, CURRENT_TIME(), euro);
  assertTrue(amt);
  assertEqual(string("EUR 1366.87"), amt->rounded().to_string());

  // Add a newer Euro pricing
  aapl.add_price(jan17_07, amount_t("EUR 23.00"));

  amt = x1.value(false, CURRENT_TIME(), euro);
  assertTrue(amt);
  assertEqual(string("EUR 2302.30"), amt->to_string());

  amt = x1.value(false, CURRENT_TIME(), cad);
  assertTrue(amt);
  assertEqual(string("CAD 3223.22"), amt->to_string());
#endif // NOT_FOR_PYTHON

  assertValid(x1);
}

void CommodityTestCase::testLots()
{
}

void CommodityTestCase::testScalingBase()
{
}

void CommodityTestCase::testReduction()
{
}

