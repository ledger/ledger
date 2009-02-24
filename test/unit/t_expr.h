#ifndef _T_EXPR_H
#define _T_EXPR_H

#include "UnitTests.h"

class ValueExprTestCase : public CPPUNIT_NS::TestCase
{
  CPPUNIT_TEST_SUITE(ValueExprTestCase);

  //CPPUNIT_TEST(testConstructors);

  CPPUNIT_TEST_SUITE_END();

public:
  ValueExprTestCase() {}
  virtual ~ValueExprTestCase() {}

  virtual void setUp();
  virtual void tearDown();

  //void testConstructors();

private:
  ValueExprTestCase(const ValueExprTestCase &copy);
  void operator=(const ValueExprTestCase &copy);
};

#endif // _T_EXPR_H
