#include "CommodityAmount.h"
#include "ledger.h"

using namespace ledger;

CPPUNIT_TEST_SUITE_NAMED_REGISTRATION(CommodityAmountTestCase, "numerics");

inline amount_t internalAmount(const std::string& value) {
  amount_t temp;
  temp.parse(value, AMOUNT_PARSE_NO_MIGRATE);
  return temp;
}

void CommodityAmountTestCase::setUp() {}
void CommodityAmountTestCase::tearDown() {}

void CommodityAmountTestCase::testConstructors()
{
  amount_t x1("$123.45");
  amount_t x2("-$123.45");
  amount_t x3("$-123.45");
  amount_t x4("DM 123.45");
  amount_t x5("-DM 123.45");
  amount_t x6("DM -123.45");
  amount_t x7("123.45 euro");
  amount_t x8("-123.45 euro");
  amount_t x9("123.45€");
  amount_t x10("-123.45€");

  assertEqual(amount_t("$123.45"), x1);
  assertEqual(amount_t("-$123.45"), x2);
  assertEqual(amount_t("$-123.45"), x3);
  assertEqual(amount_t("DM 123.45"), x4);
  assertEqual(amount_t("-DM 123.45"), x5);
  assertEqual(amount_t("DM -123.45"), x6);
  assertEqual(amount_t("123.45 euro"), x7);
  assertEqual(amount_t("-123.45 euro"), x8);
  assertEqual(amount_t("123.45€"), x9);
  assertEqual(amount_t("-123.45€"), x10);

  assertEqual(std::string("$123.45"), x1.to_string());
  assertEqual(std::string("$-123.45"), x2.to_string());
  assertEqual(std::string("$-123.45"), x3.to_string());
  assertEqual(std::string("DM 123.45"), x4.to_string());
  assertEqual(std::string("DM -123.45"), x5.to_string());
  assertEqual(std::string("DM -123.45"), x6.to_string());
  assertEqual(std::string("123.45 euro"), x7.to_string());
  assertEqual(std::string("-123.45 euro"), x8.to_string());
  assertEqual(std::string("123.45€"), x9.to_string());
  assertEqual(std::string("-123.45€"), x10.to_string());

  CPPUNIT_ASSERT(x1.valid());
  CPPUNIT_ASSERT(x2.valid());
  CPPUNIT_ASSERT(x3.valid());
  CPPUNIT_ASSERT(x4.valid());
  CPPUNIT_ASSERT(x5.valid());
  CPPUNIT_ASSERT(x6.valid());
  CPPUNIT_ASSERT(x7.valid());
  CPPUNIT_ASSERT(x8.valid());
  CPPUNIT_ASSERT(x9.valid());
  CPPUNIT_ASSERT(x10.valid());
}

void CommodityAmountTestCase::testNegation()
{
  amount_t x1("$123.45");
  amount_t x2("-$123.45");
  amount_t x3("$-123.45");
  amount_t x4("DM 123.45");
  amount_t x5("-DM 123.45");
  amount_t x6("DM -123.45");
  amount_t x7("123.45 euro");
  amount_t x8("-123.45 euro");
  amount_t x9("123.45€");
  amount_t x10("-123.45€");

  assertEqual(amount_t("$-123.45"), - x1);
  assertEqual(amount_t("$123.45"), - x2);
  assertEqual(amount_t("$123.45"), - x3);
  assertEqual(amount_t("DM -123.45"), - x4);
  assertEqual(amount_t("DM 123.45"), - x5);
  assertEqual(amount_t("DM 123.45"), - x6);
  assertEqual(amount_t("-123.45 euro"), - x7);
  assertEqual(amount_t("123.45 euro"), - x8);
  assertEqual(amount_t("-123.45€"), - x9);
  assertEqual(amount_t("123.45€"), - x10);

  assertEqual(std::string("$-123.45"), (- x1).to_string());
  assertEqual(std::string("$123.45"), (- x2).to_string());
  assertEqual(std::string("$123.45"), (- x3).to_string());
  assertEqual(std::string("DM -123.45"), (- x4).to_string());
  assertEqual(std::string("DM 123.45"), (- x5).to_string());
  assertEqual(std::string("DM 123.45"), (- x6).to_string());
  assertEqual(std::string("-123.45 euro"), (- x7).to_string());
  assertEqual(std::string("123.45 euro"), (- x8).to_string());
  assertEqual(std::string("-123.45€"), (- x9).to_string());
  assertEqual(std::string("123.45€"), (- x10).to_string());

  CPPUNIT_ASSERT(x1.valid());
  CPPUNIT_ASSERT(x2.valid());
  CPPUNIT_ASSERT(x3.valid());
  CPPUNIT_ASSERT(x4.valid());
  CPPUNIT_ASSERT(x5.valid());
  CPPUNIT_ASSERT(x6.valid());
  CPPUNIT_ASSERT(x7.valid());
  CPPUNIT_ASSERT(x8.valid());
  CPPUNIT_ASSERT(x9.valid());
  CPPUNIT_ASSERT(x10.valid());
}

void CommodityAmountTestCase::testAssignment()
{
  amount_t x1  = "$123.45";
  amount_t x2  = "-$123.45";
  amount_t x3  = "$-123.45";
  amount_t x4  = "DM 123.45";
  amount_t x5  = "-DM 123.45";
  amount_t x6  = "DM -123.45";
  amount_t x7  = "123.45 euro";
  amount_t x8  = "-123.45 euro";
  amount_t x9  = "123.45€";
  amount_t x10 = "-123.45€";

  assertEqual(amount_t("$123.45"), x1);
  assertEqual(amount_t("-$123.45"), x2);
  assertEqual(amount_t("$-123.45"), x3);
  assertEqual(amount_t("DM 123.45"), x4);
  assertEqual(amount_t("-DM 123.45"), x5);
  assertEqual(amount_t("DM -123.45"), x6);
  assertEqual(amount_t("123.45 euro"), x7);
  assertEqual(amount_t("-123.45 euro"), x8);
  assertEqual(amount_t("123.45€"), x9);
  assertEqual(amount_t("-123.45€"), x10);

  assertEqual(std::string("$123.45"), x1.to_string());
  assertEqual(std::string("$-123.45"), x2.to_string());
  assertEqual(std::string("$-123.45"), x3.to_string());
  assertEqual(std::string("DM 123.45"), x4.to_string());
  assertEqual(std::string("DM -123.45"), x5.to_string());
  assertEqual(std::string("DM -123.45"), x6.to_string());
  assertEqual(std::string("123.45 euro"), x7.to_string());
  assertEqual(std::string("-123.45 euro"), x8.to_string());
  assertEqual(std::string("123.45€"), x9.to_string());
  assertEqual(std::string("-123.45€"), x10.to_string());

  CPPUNIT_ASSERT(x1.valid());
  CPPUNIT_ASSERT(x2.valid());
  CPPUNIT_ASSERT(x3.valid());
  CPPUNIT_ASSERT(x4.valid());
  CPPUNIT_ASSERT(x5.valid());
  CPPUNIT_ASSERT(x6.valid());
  CPPUNIT_ASSERT(x7.valid());
  CPPUNIT_ASSERT(x8.valid());
  CPPUNIT_ASSERT(x9.valid());
  CPPUNIT_ASSERT(x10.valid());
}

void CommodityAmountTestCase::testEquality()
{
  amount_t x1  = "$123.45";
  amount_t x2  = "-$123.45";
  amount_t x3  = "$-123.45";
  amount_t x4  = "DM 123.45";
  amount_t x5  = "-DM 123.45";
  amount_t x6  = "DM -123.45";
  amount_t x7  = "123.45 euro";
  amount_t x8  = "-123.45 euro";
  amount_t x9  = "123.45€";
  amount_t x10 = "-123.45€";

  CPPUNIT_ASSERT(x1 != x2);
  CPPUNIT_ASSERT(x1 != x4);
  CPPUNIT_ASSERT(x1 != x7);
  CPPUNIT_ASSERT(x1 != x9);
  CPPUNIT_ASSERT(x2 == x3);
  CPPUNIT_ASSERT(x4 != x5);
  CPPUNIT_ASSERT(x5 == x6);
  CPPUNIT_ASSERT(x7 == - x8);
  CPPUNIT_ASSERT(x9 == - x10);

  CPPUNIT_ASSERT(x1.valid());
  CPPUNIT_ASSERT(x2.valid());
  CPPUNIT_ASSERT(x3.valid());
  CPPUNIT_ASSERT(x4.valid());
  CPPUNIT_ASSERT(x5.valid());
  CPPUNIT_ASSERT(x6.valid());
  CPPUNIT_ASSERT(x7.valid());
  CPPUNIT_ASSERT(x8.valid());
  CPPUNIT_ASSERT(x9.valid());
  CPPUNIT_ASSERT(x10.valid());
}

void CommodityAmountTestCase::testAddition()
{
  // jww (2007-04-16): tbd
  amount_t x1(123.123);
  amount_t y1(456.456);

  assertEqual(amount_t(579.579), x1 + y1);
  assertEqual(amount_t(579.579), x1 + 456.456);
  assertEqual(amount_t(579.579), 456.456 + x1);

  x1 += amount_t(456.456);
  assertEqual(amount_t(579.579), x1);
  x1 += 456.456;
  assertEqual(amount_t(1036.035), x1);
  x1 += 456L;
  assertEqual(amount_t(1492.035), x1);

  amount_t x2("123456789123456789.123456789123456789");

  assertEqual(amount_t("246913578246913578.246913578246913578"), x2 + x2);

  CPPUNIT_ASSERT(x1.valid());
  CPPUNIT_ASSERT(y1.valid());
  CPPUNIT_ASSERT(x2.valid());
}

void CommodityAmountTestCase::testSubtraction()
{
  // jww (2007-04-16): tbd
  amount_t x1(123.123);
  amount_t y1(456.456);

  assertEqual(amount_t(-333.333), x1 - y1);
  assertEqual(amount_t(333.333), y1 - x1);

  x1 -= amount_t(456.456);
  assertEqual(amount_t(-333.333), x1);
  x1 -= 456.456;
  assertEqual(amount_t(-789.789), x1);
  x1 -= 456L;
  assertEqual(amount_t(-1245.789), x1);

  amount_t x2("123456789123456789.123456789123456789");
  amount_t y2("9872345982459.248974239578");

  assertEqual(amount_t("123446916777474329.874482549545456789"), x2 - y2);
  assertEqual(amount_t("-123446916777474329.874482549545456789"), y2 - x2);

  CPPUNIT_ASSERT(x1.valid());
  CPPUNIT_ASSERT(y1.valid());
  CPPUNIT_ASSERT(x2.valid());
  CPPUNIT_ASSERT(y2.valid());
}

void CommodityAmountTestCase::testMultiplication()
{
  // jww (2007-04-16): tbd
  amount_t x1(123.123);
  amount_t y1(456.456);

  assertEqual(amount_t(0L), x1 * 0L);
  assertEqual(amount_t(0L), amount_t(0L) * x1);
  assertEqual(amount_t(0L), 0L * x1);
  assertEqual(x1, x1 * 1L);
  assertEqual(x1, amount_t(1L) * x1);
  assertEqual(x1, 1L * x1);
  assertEqual(- x1, x1 * -1L);
  assertEqual(- x1, amount_t(-1L) * x1);
  assertEqual(- x1, -1L * x1);
  assertEqual(amount_t("56200.232088"), x1 * y1);
  assertEqual(amount_t("56200.232088"), y1 * x1);
  assertEqual(amount_t("56200.232088"), x1 * 456.456);
  assertEqual(amount_t("56200.232088"), amount_t(456.456) * x1);
  assertEqual(amount_t("56200.232088"), 456.456 * x1);

  x1 *= amount_t(123.123);
  assertEqual(amount_t("15159.273129"), x1);
  x1 *= 123.123;
  assertEqual(amount_t("1866455.185461867"), x1);
  x1 *= 123L;
  assertEqual(amount_t("229573987.811809641"), x1);

  amount_t x2("123456789123456789.123456789123456789");

  assertEqual(amount_t("15241578780673678546105778311537878.046486820281054720515622620750190521"),
	      x2 * x2);

  CPPUNIT_ASSERT(x1.valid());
  CPPUNIT_ASSERT(y1.valid());
  CPPUNIT_ASSERT(x2.valid());
}

void CommodityAmountTestCase::testDivision()
{
  // jww (2007-04-16): tbd
  amount_t x1(123.123);
  amount_t y1(456.456);

  assertThrow(x1 / 0L, amount_error *);
  assertEqual(amount_t("0.008121"), amount_t(1.0) / x1);
  assertEqual(amount_t("0.008121"), 1.0 / x1);
  assertEqual(x1, x1 / 1.0);
  assertEqual(amount_t("0.008121"), amount_t(1.0) / x1);
  assertEqual(amount_t("0.008121"), 1.0 / x1);
  assertEqual(- x1, x1 / -1.0);
  assertEqual(- amount_t("0.008121"), amount_t(-1.0) / x1);
  assertEqual(- amount_t("0.008121"), -1.0 / x1);
  assertEqual(amount_t("0.269736842105"), x1 / y1);
  assertEqual(amount_t("3.707317073170"), y1 / x1);
  assertEqual(amount_t("0.269736842105"), x1 / 456.456);
  assertEqual(amount_t("3.707317073170"), amount_t(456.456) / x1);
  assertEqual(amount_t("3.707317073170"), 456.456 / x1);

  x1 /= amount_t(456.456);
  assertEqual(amount_t("0.269736842105"), x1);
  x1 /= 456.456;
  assertEqual(amount_t("0.0005909372252856792330476541"), x1);
  x1 /= 456L;
  assertEqual(amount_t("0.00000129591496773175270405187302631578947368421052631578947368421"), x1);

  amount_t x4("1234567891234567.89123456789");
  amount_t y4("56.789");

  assertEqual(amount_t(1.0), x4 / x4);
  assertEqual(amount_t("21739560323910.7554497273748437197344556164"),
	      x4 / y4);

  CPPUNIT_ASSERT(x1.valid());
  CPPUNIT_ASSERT(y1.valid());
  CPPUNIT_ASSERT(x4.valid());
  CPPUNIT_ASSERT(y4.valid());
}

void CommodityAmountTestCase::testConversion()
{
  // jww (2007-04-16): tbd
  amount_t x1(1234.56);

  assertEqual(true, bool(x1));
  assertEqual(1234L, long(x1));
  assertEqual(1234.56, double(x1));
  assertEqual(std::string("1234.56"), x1.to_string());
  assertEqual(std::string("1234.56"), x1.quantity_string());

  CPPUNIT_ASSERT(x1.valid());
}

void CommodityAmountTestCase::testRound()
{
  // jww (2007-04-16): tbd
  amount_t x1("1234.567890");

  assertEqual(amount_t("1234.56789"), x1.round(6));
  assertEqual(amount_t("1234.56789"), x1.round(5));
  assertEqual(amount_t("1234.5679"), x1.round(4));
  assertEqual(amount_t("1234.568"), x1.round(3));
  assertEqual(amount_t("1234.57"), x1.round(2));
  assertEqual(amount_t("1234.6"), x1.round(1));
  assertEqual(amount_t("1235"), x1.round(0));

  amount_t x2("9876.543210");

  assertEqual(amount_t("9876.543210"), x2.round(6));
  assertEqual(amount_t("9876.54321"), x2.round(5));
  assertEqual(amount_t("9876.5432"), x2.round(4));
  assertEqual(amount_t("9876.543"), x2.round(3));
  assertEqual(amount_t("9876.54"), x2.round(2));
  assertEqual(amount_t("9876.5"), x2.round(1));
  assertEqual(amount_t("9877"), x2.round(0));

  amount_t x3("-1234.567890");

  assertEqual(amount_t("-1234.56789"), x3.round(6));
  assertEqual(amount_t("-1234.56789"), x3.round(5));
  assertEqual(amount_t("-1234.5679"), x3.round(4));
  assertEqual(amount_t("-1234.568"), x3.round(3));
  assertEqual(amount_t("-1234.57"), x3.round(2));
  assertEqual(amount_t("-1234.6"), x3.round(1));
  assertEqual(amount_t("-1235"), x3.round(0));

  amount_t x4("-9876.543210");

  assertEqual(amount_t("-9876.543210"), x4.round(6));
  assertEqual(amount_t("-9876.54321"), x4.round(5));
  assertEqual(amount_t("-9876.5432"), x4.round(4));
  assertEqual(amount_t("-9876.543"), x4.round(3));
  assertEqual(amount_t("-9876.54"), x4.round(2));
  assertEqual(amount_t("-9876.5"), x4.round(1));
  assertEqual(amount_t("-9877"), x4.round(0));

  CPPUNIT_ASSERT(x1.valid());
  CPPUNIT_ASSERT(x2.valid());
  CPPUNIT_ASSERT(x3.valid());
  CPPUNIT_ASSERT(x4.valid());
}

void CommodityAmountTestCase::testDisplayRound()
{
  amount_t x1("$0.85");

  x1 *= 0.19;

  CPPUNIT_ASSERT(amount_t("$0.16") != x1);
  assertEqual(internalAmount("$0.1615"), x1);
  assertEqual(std::string("$0.16"), x1.to_string());

  x1 *= 7L;

  CPPUNIT_ASSERT(amount_t("$1.13") != x1);
  assertEqual(internalAmount("$1.1305"), x1);
  assertEqual(std::string("$1.13"), x1.to_string());
}

void CommodityAmountTestCase::testTruth()
{
  // jww (2007-04-16): tbd
  amount_t x0;
  amount_t x1("1234");
  amount_t x2("1234.56");

  if (x0)
    CPPUNIT_ASSERT(false);
  else
    CPPUNIT_ASSERT(true);

  if (x1)
    CPPUNIT_ASSERT(true);
  else
    CPPUNIT_ASSERT(false);

  if (x2)
    CPPUNIT_ASSERT(true);
  else
    CPPUNIT_ASSERT(false);

  CPPUNIT_ASSERT(x0.valid());
  CPPUNIT_ASSERT(x1.valid());
  CPPUNIT_ASSERT(x2.valid());
}

void CommodityAmountTestCase::testForZero()
{
  // jww (2007-04-16): tbd
  amount_t x0;
  amount_t x1("0.000000000000000000001");

  CPPUNIT_ASSERT(! x0);
  CPPUNIT_ASSERT(x1);
  CPPUNIT_ASSERT(x0.zero());
  CPPUNIT_ASSERT(x0.realzero());
  CPPUNIT_ASSERT(! x1.zero());
  CPPUNIT_ASSERT(! x1.realzero());

  CPPUNIT_ASSERT(x0.valid());
  CPPUNIT_ASSERT(x1.valid());
}

void CommodityAmountTestCase::testComparisons()
{
  // jww (2007-04-16): tbd
  amount_t x0;
  amount_t x1(-123L);
  amount_t x2(123L);
  amount_t x3(-123.45);
  amount_t x4(123.45);
  amount_t x5("-123.45");
  amount_t x6("123.45");

  CPPUNIT_ASSERT(x0 > x1);
  CPPUNIT_ASSERT(x0 < x2);
  CPPUNIT_ASSERT(x0 > x3);
  CPPUNIT_ASSERT(x0 < x4);
  CPPUNIT_ASSERT(x0 > x5);
  CPPUNIT_ASSERT(x0 < x6);

  CPPUNIT_ASSERT(x1 > x3);
  CPPUNIT_ASSERT(x3 <= x5);
  CPPUNIT_ASSERT(x3 >= x5);
  CPPUNIT_ASSERT(x3 < x1);
  CPPUNIT_ASSERT(x3 < x4);

  CPPUNIT_ASSERT(x0.valid());
  CPPUNIT_ASSERT(x1.valid());
  CPPUNIT_ASSERT(x2.valid());
  CPPUNIT_ASSERT(x3.valid());
  CPPUNIT_ASSERT(x4.valid());
  CPPUNIT_ASSERT(x5.valid());
  CPPUNIT_ASSERT(x6.valid());
}

void CommodityAmountTestCase::testSign()
{
  // jww (2007-04-16): tbd
  amount_t x0;
  amount_t x1("0.0000000000000000000000000000000000001");
  amount_t x2("-0.0000000000000000000000000000000000001");
  amount_t x3("1");
  amount_t x4("-1");

  CPPUNIT_ASSERT(! x0.sign());
  CPPUNIT_ASSERT(x1.sign() > 0);
  CPPUNIT_ASSERT(x2.sign() < 0);
  CPPUNIT_ASSERT(x3.sign() > 0);
  CPPUNIT_ASSERT(x4.sign() < 0);

  CPPUNIT_ASSERT(x0.valid());
  CPPUNIT_ASSERT(x1.valid());
  CPPUNIT_ASSERT(x2.valid());
  CPPUNIT_ASSERT(x3.valid());
  CPPUNIT_ASSERT(x4.valid());
}

void CommodityAmountTestCase::testAbs()
{
  // jww (2007-04-16): tbd
  amount_t x0;
  amount_t x1(-1234L);
  amount_t x2(1234L);

  assertEqual(amount_t(), abs(x0));
  assertEqual(amount_t(1234L), abs(x1));
  assertEqual(amount_t(1234L), abs(x2));

  x0.abs();
  x1.abs();
  x2.abs();

  assertEqual(amount_t(), x0);
  assertEqual(amount_t(1234L), x1);
  assertEqual(amount_t(1234L), x2);

  CPPUNIT_ASSERT(x0.valid());
  CPPUNIT_ASSERT(x1.valid());
  CPPUNIT_ASSERT(x2.valid());
}

void CommodityAmountTestCase::testPrinting()
{
  // jww (2007-04-16): tbd
  amount_t x0;
  amount_t x1("982340823.380238098235098235098235098");

  {
    std::ostringstream bufstr;
    bufstr << x0;

    assertEqual(std::string("0"), bufstr.str());
  }

  {
    std::ostringstream bufstr;
    bufstr << x1;

    assertEqual(std::string("982340823.380238098235098235098235098"),
		bufstr.str());
  }

  CPPUNIT_ASSERT(x0.valid());
  CPPUNIT_ASSERT(x1.valid());
}
