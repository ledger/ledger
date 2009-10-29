#ifndef _T_EXPR_H
#define _T_EXPR_H

#include "UnitTests.h"

class ValueExprTestCase : public CPPUNIT_NS::TestCase
{
  CPPUNIT_TEST_SUITE(ValueExprTestCase);

  //CPPUNIT_TEST(testConstructors);
  CPPUNIT_TEST(testPredicateTokenizer1);
  CPPUNIT_TEST(testPredicateTokenizer2);
  CPPUNIT_TEST(testPredicateTokenizer3);
  CPPUNIT_TEST(testPredicateTokenizer4);
  CPPUNIT_TEST(testPredicateTokenizer5);
  CPPUNIT_TEST(testPredicateTokenizer6);
  CPPUNIT_TEST(testPredicateTokenizer7);
  CPPUNIT_TEST(testPredicateTokenizer8);
  CPPUNIT_TEST(testPredicateTokenizer9);
  CPPUNIT_TEST(testPredicateTokenizer10);
  CPPUNIT_TEST(testPredicateTokenizer11);
  CPPUNIT_TEST(testPredicateTokenizer12);
  CPPUNIT_TEST(testPredicateTokenizer13);
  CPPUNIT_TEST(testPredicateTokenizer14);
  CPPUNIT_TEST(testPredicateTokenizer15);
  CPPUNIT_TEST(testPredicateTokenizer16);

  CPPUNIT_TEST_SUITE_END();

public:
  ValueExprTestCase() {}
  virtual ~ValueExprTestCase() {}

  virtual void setUp();
  virtual void tearDown();

  //void testConstructors();
  void testPredicateTokenizer1();
  void testPredicateTokenizer2();
  void testPredicateTokenizer3();
  void testPredicateTokenizer4();
  void testPredicateTokenizer5();
  void testPredicateTokenizer6();
  void testPredicateTokenizer7();
  void testPredicateTokenizer8();
  void testPredicateTokenizer9();
  void testPredicateTokenizer10();
  void testPredicateTokenizer11();
  void testPredicateTokenizer12();
  void testPredicateTokenizer13();
  void testPredicateTokenizer14();
  void testPredicateTokenizer15();
  void testPredicateTokenizer16();

private:
  ValueExprTestCase(const ValueExprTestCase &copy);
  void operator=(const ValueExprTestCase &copy);
};

#endif // _T_EXPR_H
