#include "t_balance.h"

#include "utils.h"
#include "amount.h"

using namespace ledger;

CPPUNIT_TEST_SUITE_NAMED_REGISTRATION(BalanceTestCase, "math");

void BalanceTestCase::setUp()
{
  amount_t::initialize();

  // Cause the display precision for dollars to be initialized to 2.
  amount_t x1("$1.00");
  assertTrue(x1);

  amount_t::stream_fullstrings = true; // make reports from UnitTests accurate
}

void BalanceTestCase::tearDown()
{
  amount_t::stream_fullstrings = false;
  amount_t::shutdown();
}
