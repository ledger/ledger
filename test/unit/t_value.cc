#define BOOST_TEST_DYN_LINK

#include <boost/test/unit_test.hpp>

#include <system.hh>

#include "value.h"

using namespace ledger;

struct value_fixture {
  value_fixture() {
  times_initialize();
  amount_t::initialize();
  value_t::initialize();


  // Cause the display precision for dollars to be initialized to 2.
  amount_t x1("$1.00");
  BOOST_CHECK(x1);

  amount_t::stream_fullstrings = true; // make reports from UnitTests accurate
  }

  ~value_fixture()
  {
  amount_t::stream_fullstrings = false;
  amount_t::shutdown();
  times_shutdown();
  value_t::shutdown();
  }
};

BOOST_FIXTURE_TEST_SUITE(value, value_fixture)

BOOST_AUTO_TEST_CASE(testConstructors)
{
  value_t v1;
  value_t v2(true);
  value_t v3(boost::posix_time::from_time_t(time_t(NULL)));
  value_t v4(date_t(parse_date("2014/08/14")));
  value_t v5(2L);
  value_t v6(4UL);
  value_t v7(1.00);
  value_t v8(amount_t("4 GBP"));
  value_t v9(balance_t("3 EUR"));
  value_t v10(mask_t("regex"));
  value_t v11(new value_t::sequence_t());
  value_t v12(string("$1"));
  value_t v13("2 CAD");
  value_t v14("comment",true);
  value_t v15(string("tag"),true);

  BOOST_CHECK(v1.valid());
  BOOST_CHECK(v2.valid());
  BOOST_CHECK(v3.valid());
  BOOST_CHECK(v4.valid());
  BOOST_CHECK(v5.valid());
  BOOST_CHECK(v6.valid());
  BOOST_CHECK(v7.valid());
  BOOST_CHECK(v8.valid());
  BOOST_CHECK(v9.valid());
  BOOST_CHECK(v10.valid());
  BOOST_CHECK(v11.valid());
  BOOST_CHECK(v12.valid());
  BOOST_CHECK(v13.valid());
  BOOST_CHECK(v14.valid());
  BOOST_CHECK(v15.valid());
}

BOOST_AUTO_TEST_SUITE_END()

