#include "t_expr.h"

#include "expr.h"

using namespace ledger;

CPPUNIT_TEST_SUITE_NAMED_REGISTRATION(ValueExprTestCase, "expr");

void ValueExprTestCase::setUp()
{
  amount_t::initialize();
#ifndef NOT_FOR_PYTHON
  expr_t::initialize();
#endif // NOT_FOR_PYTHON
}

void ValueExprTestCase::tearDown()
{
#ifndef NOT_FOR_PYTHON
  expr_t::shutdown();
#endif // NOT_FOR_PYTHON
  amount_t::shutdown();
}

void ValueExprTestCase::testConstructors()
{
  int x = 1;
}
