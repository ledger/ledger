#define BOOST_TEST_DYN_LINK
//#define BOOST_TEST_MODULE balance
#include <boost/test/unit_test.hpp>

#include <system.hh>

#include "balance.h"

using namespace ledger;

struct balance_fixture {
  balance_fixture() {
  times_initialize();
  amount_t::initialize();

  // Cause the display precision for dollars to be initialized to 2.
  amount_t x1("$1.00");
  BOOST_CHECK(x1);

  amount_t::stream_fullstrings = true; // make reports from UnitTests accurate
  }

  ~balance_fixture()
  {
  amount_t::stream_fullstrings = false;
  amount_t::shutdown();
  times_shutdown();
  }
};

//BOOST_FIXTURE_TEST_SUITE(balance, balance_fixture)

//BOOST_AUTO_TEST_SUITE_END()
