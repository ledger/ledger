#ifndef _T_AMOUNT_H
#define _T_AMOUNT_H

#include "UnitTests.h"

class AmountTestCase : public CPPUNIT_NS::TestCase
{
  CPPUNIT_TEST_SUITE(AmountTestCase);

  CPPUNIT_TEST(testConstructors);
  CPPUNIT_TEST(testCommodityConstructors);
  CPPUNIT_TEST(testParser);
  CPPUNIT_TEST(testAssignment);
  CPPUNIT_TEST(testCommodityAssignment);
  CPPUNIT_TEST(testEquality);
  CPPUNIT_TEST(testCommodityEquality);
  CPPUNIT_TEST(testComparisons);
  CPPUNIT_TEST(testCommodityComparisons);
  CPPUNIT_TEST(testIntegerAddition);
  CPPUNIT_TEST(testFractionalAddition);
  CPPUNIT_TEST(testCommodityAddition);
  CPPUNIT_TEST(testIntegerSubtraction);
  CPPUNIT_TEST(testFractionalSubtraction);
  CPPUNIT_TEST(testCommoditySubtraction);
  CPPUNIT_TEST(testIntegerMultiplication);
  CPPUNIT_TEST(testFractionalMultiplication);
  CPPUNIT_TEST(testCommodityMultiplication);
  CPPUNIT_TEST(testIntegerDivision);
  CPPUNIT_TEST(testFractionalDivision);
  CPPUNIT_TEST(testCommodityDivision);
  CPPUNIT_TEST(testNegation);
  CPPUNIT_TEST(testCommodityNegation);
  CPPUNIT_TEST(testAbs);
  CPPUNIT_TEST(testCommodityAbs);
#if 0
  CPPUNIT_TEST(testReduction);
#endif
  CPPUNIT_TEST(testSign);
  CPPUNIT_TEST(testCommoditySign);
  CPPUNIT_TEST(testTruth);
  CPPUNIT_TEST(testCommodityTruth);
  CPPUNIT_TEST(testForZero);
  CPPUNIT_TEST(testCommodityForZero);
  CPPUNIT_TEST(testIntegerConversion);
  CPPUNIT_TEST(testFractionalConversion);
  CPPUNIT_TEST(testCommodityConversion);
  CPPUNIT_TEST(testPrinting);
  CPPUNIT_TEST(testCommodityPrinting);

  CPPUNIT_TEST_SUITE_END();

public:
  AmountTestCase() {}
  virtual ~AmountTestCase() {}

  virtual void setUp();
  virtual void tearDown();

  void testConstructors();
  void testCommodityConstructors();
  void testParser();
  void testAssignment();
  void testCommodityAssignment();
  void testEquality();
  void testCommodityEquality();
  void testComparisons();
  void testCommodityComparisons();
  void testIntegerAddition();
  void testFractionalAddition();
  void testCommodityAddition();
  void testIntegerSubtraction();
  void testFractionalSubtraction();
  void testCommoditySubtraction();
  void testIntegerMultiplication();
  void testFractionalMultiplication();
  void testCommodityMultiplication();
  void testIntegerDivision();
  void testFractionalDivision();
  void testCommodityDivision();
  void testNegation();
  void testCommodityNegation();
  void testAbs();
  void testCommodityAbs();
  void testFractionalRound();
  void testCommodityRound();
  void testCommodityDisplayRound();
  void testReduction();
  void testSign();
  void testCommoditySign();
  void testTruth();
  void testCommodityTruth();
  void testForZero();
  void testCommodityForZero();
  void testIntegerConversion();
  void testFractionalConversion();
  void testCommodityConversion();
  void testPrinting();
  void testCommodityPrinting();

private:
  AmountTestCase(const AmountTestCase &copy);
  void operator=(const AmountTestCase &copy);
};

#endif // _T_AMOUNT_H
