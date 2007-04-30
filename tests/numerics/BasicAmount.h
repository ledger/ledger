#ifndef _BASICAMOUNT_H
#define _BASICAMOUNT_H

#include "UnitTests.h"

class BasicAmountTestCase : public CPPUNIT_NS::TestCase
{
  CPPUNIT_TEST_SUITE(BasicAmountTestCase);

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
  CPPUNIT_TEST(testIntegerConversion);
  CPPUNIT_TEST(testFractionalConversion);
  CPPUNIT_TEST(testFractionalRound);
  CPPUNIT_TEST(testTruth);
  CPPUNIT_TEST(testForZero);
  CPPUNIT_TEST(testComparisons);
  CPPUNIT_TEST(testSign);
  CPPUNIT_TEST(testAbs);
  CPPUNIT_TEST(testPrinting);

  CPPUNIT_TEST_SUITE_END();

public:
  BasicAmountTestCase() {}
  virtual ~BasicAmountTestCase() {}

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
  void testIntegerConversion();
  void testFractionalConversion();
  void testFractionalRound();
  void testTruth();
  void testForZero();
  void testComparisons();
  void testSign();
  void testAbs();
  void testPrinting();

private:
  BasicAmountTestCase(const BasicAmountTestCase &copy);
  void operator=(const BasicAmountTestCase &copy);
};

#endif /* _BASICAMOUNT_H */
