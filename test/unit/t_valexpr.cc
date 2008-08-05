#include "t_valexpr.h"

CPPUNIT_TEST_SUITE_NAMED_REGISTRATION(ValueExprTestCase, "numerics");

void ValueExprTestCase::setUp()
{
  ledger::set_session_context(&session);

  // Cause the display precision for dollars to be initialized to 2.
  amount_t x1("$1.00");
  assertTrue(x1);

  amount_t::stream_fullstrings = true; // make reports from UnitTests accurate
}

void ValueExprTestCase::tearDown()
{
  amount_t::stream_fullstrings = false;

  ledger::set_session_context();
}

void ValueExprTestCase::testConstructors()
{
}
