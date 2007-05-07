#ifndef _T_AMOUNT_H
#define _T_AMOUNT_H

#include "UnitTests.h"

class BasicAmountTestCase : public CPPUNIT_NS::TestCase
{
  CPPUNIT_TEST_SUITE(BasicAmountTestCase);

  CPPUNIT_TEST(testConstructors);
  CPPUNIT_TEST(testAssignment);
  CPPUNIT_TEST(testEquality);
  CPPUNIT_TEST(testComparisons);
  CPPUNIT_TEST(testIntegerAddition);
  CPPUNIT_TEST(testFractionalAddition);
  CPPUNIT_TEST(testIntegerSubtraction);
  CPPUNIT_TEST(testFractionalSubtraction);
  CPPUNIT_TEST(testIntegerMultiplication);
  CPPUNIT_TEST(testFractionalMultiplication);
  CPPUNIT_TEST(testIntegerDivision);
  CPPUNIT_TEST(testFractionalDivision);
  CPPUNIT_TEST(testNegation);
  CPPUNIT_TEST(testAbs);
  CPPUNIT_TEST(testFractionalRound);
  CPPUNIT_TEST(testReduction);
  CPPUNIT_TEST(testSign);
  CPPUNIT_TEST(testTruth);
  CPPUNIT_TEST(testForZero);
  CPPUNIT_TEST(testIntegerConversion);
  CPPUNIT_TEST(testFractionalConversion);
  CPPUNIT_TEST(testPrinting);
  CPPUNIT_TEST(testCommodityConstructors);
  CPPUNIT_TEST(testCommodityNegation);
  CPPUNIT_TEST(testCommodityAssignment);
  CPPUNIT_TEST(testCommodityEquality);
  CPPUNIT_TEST(testCommodityAddition);
  CPPUNIT_TEST(testCommoditySubtraction);
  CPPUNIT_TEST(testCommodityMultiplication);
  CPPUNIT_TEST(testCommodityDivision);
  CPPUNIT_TEST(testCommodityConversion);
  CPPUNIT_TEST(testCommodityRound);
  CPPUNIT_TEST(testCommodityDisplayRound);
  CPPUNIT_TEST(testCommodityTruth);
  CPPUNIT_TEST(testCommodityForZero);
  CPPUNIT_TEST(testCommodityComparisons);
  CPPUNIT_TEST(testCommoditySign);
  CPPUNIT_TEST(testCommodityAbs);
  CPPUNIT_TEST(testCommodityPrinting);

  CPPUNIT_TEST_SUITE_END();

public:
  ledger::session_t session;

  BasicAmountTestCase() {}
  virtual ~BasicAmountTestCase() {}

  virtual void setUp();
  virtual void tearDown();

  void testConstructors();
  void testAssignment();
  void testEquality();
  void testComparisons();
  void testIntegerAddition();
  void testFractionalAddition();
  void testIntegerSubtraction();
  void testFractionalSubtraction();
  void testIntegerMultiplication();
  void testFractionalMultiplication();
  void testIntegerDivision();
  void testFractionalDivision();
  void testNegation();
  void testAbs();
  void testFractionalRound();
  void testReduction();
  void testSign();
  void testTruth();
  void testForZero();
  void testIntegerConversion();
  void testFractionalConversion();
  void testPrinting();
  void testCommodityConstructors();
  void testCommodityNegation();
  void testCommodityAssignment();
  void testCommodityEquality();
  void testCommodityAddition();
  void testCommoditySubtraction();
  void testCommodityMultiplication();
  void testCommodityDivision();
  void testCommodityConversion();
  void testCommodityRound();
  void testCommodityDisplayRound();
  void testCommodityTruth();
  void testCommodityForZero();
  void testCommodityComparisons();
  void testCommoditySign();
  void testCommodityAbs();
  void testCommodityPrinting();

private:
  BasicAmountTestCase(const BasicAmountTestCase &copy);
  void operator=(const BasicAmountTestCase &copy);
};

#endif // _T_AMOUNT_H
