#include "t_balance.h"

CPPUNIT_TEST_SUITE_NAMED_REGISTRATION(BalanceTestCase, "numerics");

void BalanceTestCase::setUp()
{
  //ledger::set_session_context(&session);

  // Cause the display precision for dollars to be initialized to 2.
  amount_t x1("$1.00");
  assertTrue(x1);

  amount_t::stream_fullstrings = true; // make reports from UnitTests accurate
}

void BalanceTestCase::tearDown()
{
  amount_t::stream_fullstrings = false;

  //ledger::set_session_context();
}

void BalanceTestCase::testConstructors()
{
}
