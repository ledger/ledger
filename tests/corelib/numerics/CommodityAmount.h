#ifndef _COMMODITYAMOUNT_H
#define _COMMODITYAMOUNT_H

#include "UnitTests.h"

class CommodityAmountTestCase : public CPPUNIT_NS::TestCase
{
  CPPUNIT_TEST_SUITE(CommodityAmountTestCase);

  CPPUNIT_TEST(testConstructors);
  CPPUNIT_TEST(testNegation);
  CPPUNIT_TEST(testAssignment);
  CPPUNIT_TEST(testEquality);
  CPPUNIT_TEST(testAddition);
  CPPUNIT_TEST(testSubtraction);
  CPPUNIT_TEST(testMultiplication);
  CPPUNIT_TEST(testDivision);
  CPPUNIT_TEST(testConversion);
  CPPUNIT_TEST(testRound);
  CPPUNIT_TEST(testDisplayRound);
  CPPUNIT_TEST(testTruth);
  CPPUNIT_TEST(testForZero);
  CPPUNIT_TEST(testComparisons);
  CPPUNIT_TEST(testSign);
  CPPUNIT_TEST(testAbs);
  CPPUNIT_TEST(testPrinting);
  CPPUNIT_TEST(testPriceHistory);
  CPPUNIT_TEST(testLots);
  CPPUNIT_TEST(testScalingBase);
  CPPUNIT_TEST(testReduction);

  CPPUNIT_TEST_SUITE_END();

public:
  CommodityAmountTestCase() {}
  virtual ~CommodityAmountTestCase() {}

  virtual void setUp();
  virtual void tearDown();

  void testConstructors();
  void testNegation();
  void testAssignment();
  void testEquality();
  void testAddition();
  void testSubtraction();
  void testMultiplication();
  void testDivision();
  void testConversion();
  void testRound();
  void testDisplayRound();
  void testTruth();
  void testForZero();
  void testComparisons();
  void testSign();
  void testAbs();
  void testPrinting();
  void testPriceHistory();
  void testLots();
  void testScalingBase();
  void testReduction();

private:
  CommodityAmountTestCase(const CommodityAmountTestCase &copy);
  void operator=(const CommodityAmountTestCase &copy);
};

#endif /* _COMMODITYAMOUNT_H */
