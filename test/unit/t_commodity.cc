#define BOOST_TEST_DYN_LINK
//#define BOOST_TEST_MODULE commodity
#include <boost/test/unit_test.hpp>

#include <system.hh>

#include "amount.h"
#include "commodity.h"

using namespace ledger;

struct commodity_fixture {
  commodity_fixture() {
  times_initialize();
  amount_t::initialize();
  amount_t::stream_fullstrings = true;
  }

  ~commodity_fixture() {
  amount_t::shutdown();
  times_shutdown();
  }
};

BOOST_FIXTURE_TEST_SUITE(commodity, commodity_fixture)

BOOST_AUTO_TEST_CASE(testPriceHistory)
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

  BOOST_CHECK_THROW(x0.value(), amount_error);
#ifndef NOT_FOR_PYTHON
  BOOST_CHECK(! x1.value());
#endif

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
  optional<amount_t> amt = x1.value(feb28_07sbm);
  BOOST_CHECK(amt);
  BOOST_CHECK_EQUAL(amount_t("$1831.83"), *amt);

  amt = x1.value(CURRENT_TIME());
  BOOST_CHECK(amt);
  BOOST_CHECK_EQUAL(string("$2124.12"), amt->to_string());
#ifdef INTEGER_MATH
  BOOST_CHECK_EQUAL(string("$2124.12"), amt->to_fullstring());
#else
  BOOST_CHECK_EQUAL(string("$2124.122"), amt->to_fullstring());
#endif

  amt = x1.value(CURRENT_TIME(), &euro);
  BOOST_CHECK(amt);
  BOOST_CHECK_EQUAL(string("EUR 1787.50"), amt->rounded().to_string());

  // Add a newer Euro pricing
  aapl.add_price(jan17_07, amount_t("EUR 23.00"));

  amt = x1.value(CURRENT_TIME(), &euro);
  BOOST_CHECK(amt);
  BOOST_CHECK_EQUAL(string("EUR 2302.30"), amt->to_string());

  amt = x1.value(CURRENT_TIME(), &cad);
  BOOST_CHECK(amt);
  BOOST_CHECK_EQUAL(string("CAD 3223.22"), amt->to_string());
#endif // NOT_FOR_PYTHON

  BOOST_CHECK(x1.valid());
}


BOOST_AUTO_TEST_SUITE_END()
