#ifndef _BASICAMOUNTTEST_H
#define _BASICAMOUNTTEST_H

#include "UnitTests.h"

class BasicAmountTest : public CPPUNIT_NS::TestCase
{
  CPPUNIT_TEST_SUITE(BasicAmountTest);

  CPPUNIT_TEST(testConstructors);
  CPPUNIT_TEST(testNegation);
  CPPUNIT_TEST(testAssignment);
  CPPUNIT_TEST(testEquality);
  CPPUNIT_TEST(testIntegerAddition);
  CPPUNIT_TEST(testFractionalAddition);
  CPPUNIT_TEST(testIntegerSubtraction);
  CPPUNIT_TEST(testFractionalSubtraction);
  CPPUNIT_TEST(testIntegerMultiplication);
  CPPUNIT_TEST(testFractionalMultiplication);
  CPPUNIT_TEST(testIntegerDivision);
  CPPUNIT_TEST(testFractionalDivision);

  CPPUNIT_TEST_SUITE_END();

public:
  BasicAmountTest() {}
  virtual ~BasicAmountTest() {}

  virtual void setUp();
  virtual void tearDown();

  void testConstructors();
  void testNegation();
  void testAssignment();
  void testEquality();
  void testIntegerAddition();
  void testFractionalAddition();
  void testIntegerSubtraction();
  void testFractionalSubtraction();
  void testIntegerMultiplication();
  void testFractionalMultiplication();
  void testIntegerDivision();
  void testFractionalDivision();

private:
  BasicAmountTest(const BasicAmountTest &copy);
  void operator=(const BasicAmountTest &copy);
};

#endif /* _BASICAMOUNTTEST_H */
