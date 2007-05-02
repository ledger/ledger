#include "BasicAmount.h"

CPPUNIT_TEST_SUITE_NAMED_REGISTRATION(BasicAmountTestCase, "numerics");

void BasicAmountTestCase::setUp() {
  ledger::initialize();
}
void BasicAmountTestCase::tearDown() {
  ledger::shutdown();
}

void BasicAmountTestCase::testConstructors()
{
  amount_t x0;
  amount_t x1(123456L);
  amount_t x2(123456UL);
  amount_t x3(123.456);
  amount_t x5("123456");
  amount_t x6("123.456");
  amount_t x7(std::string("123456"));
  amount_t x8(std::string("123.456"));
  amount_t x9(x3);
  amount_t x10(x6);
  amount_t x11(x8);

  assertEqual(amount_t(0L), x0);
  assertEqual(amount_t(), x0);
  assertEqual(amount_t("0"), x0);
  assertEqual(amount_t("0.0"), x0);
  assertEqual(x2, x1);
  assertEqual(x5, x1);
  assertEqual(x7, x1);
  assertEqual(x6, x3);
  assertEqual(x8, x3);
  assertEqual(x10, x3);
  assertEqual(x10, x9);

  CPPUNIT_ASSERT(x0.valid());
  CPPUNIT_ASSERT(x1.valid());
  CPPUNIT_ASSERT(x2.valid());
  CPPUNIT_ASSERT(x3.valid());
  CPPUNIT_ASSERT(x5.valid());
  CPPUNIT_ASSERT(x6.valid());
  CPPUNIT_ASSERT(x7.valid());
  CPPUNIT_ASSERT(x8.valid());
  CPPUNIT_ASSERT(x9.valid());
  CPPUNIT_ASSERT(x10.valid());
  CPPUNIT_ASSERT(x11.valid());
}

void BasicAmountTestCase::testAssignment()
{
  amount_t x0;
  amount_t x1  = 123456L;
  amount_t x2  = 123456UL;
  amount_t x3  = 123.456;
  amount_t x5  = "123456";
  amount_t x6  = "123.456";
  amount_t x7  = string("123456");
  amount_t x8  = string("123.456");
  amount_t x9  = x3;
  amount_t x10 = amount_t(x6);

  assertEqual(amount_t(0L), x0);
  assertEqual(x2, x1);
  assertEqual(x5, x1);
  assertEqual(x7, x1);
  assertEqual(x6, x3);
  assertEqual(x8, x3);
  assertEqual(x10, x3);
  assertEqual(x10, x9);

  x0  = amount_t();
  x1  = 123456L;
  x2  = 123456UL;
  x3  = 123.456;
  x5  = "123456";
  x6  = "123.456";
  x7  = std::string("123456");
  x8  = std::string("123.456");
  x9  = x3;
  x10 = amount_t(x6);

  assertEqual(amount_t(0L), x0);
  assertEqual(x2, x1);
  assertEqual(x5, x1);
  assertEqual(x7, x1);
  assertEqual(x6, x3);
  assertEqual(x8, x3);
  assertEqual(x10, x3);
  assertEqual(x10, x9);

  CPPUNIT_ASSERT(x0.valid());
  CPPUNIT_ASSERT(x1.valid());
  CPPUNIT_ASSERT(x2.valid());
  CPPUNIT_ASSERT(x3.valid());
  CPPUNIT_ASSERT(x5.valid());
  CPPUNIT_ASSERT(x6.valid());
  CPPUNIT_ASSERT(x7.valid());
  CPPUNIT_ASSERT(x8.valid());
  CPPUNIT_ASSERT(x9.valid());
  CPPUNIT_ASSERT(x10.valid());
}

void BasicAmountTestCase::testEquality()
{
  amount_t x1(123456L);
  amount_t x2(456789L);
  amount_t x3(333333L);
  amount_t x4(123456.0);
  amount_t x5("123456.0");
  amount_t x6(123456.0F);

  CPPUNIT_ASSERT(x1 == 123456L);
  CPPUNIT_ASSERT(x1 != x2);
  CPPUNIT_ASSERT(x1 == (x2 - x3));
  CPPUNIT_ASSERT(x1 == x4);
  CPPUNIT_ASSERT(x4 == x5);
  CPPUNIT_ASSERT(x4 == x6);

  CPPUNIT_ASSERT(x1.valid());
  CPPUNIT_ASSERT(x2.valid());
  CPPUNIT_ASSERT(x3.valid());
  CPPUNIT_ASSERT(x4.valid());
  CPPUNIT_ASSERT(x5.valid());
  CPPUNIT_ASSERT(x6.valid());
}

void BasicAmountTestCase::testComparisons()
{
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

void BasicAmountTestCase::testIntegerAddition()
{
  amount_t x1(123L);
  amount_t y1(456L);

  assertEqual(amount_t(579L), x1 + y1);
  assertEqual(amount_t(579L), x1 + 456L);
  assertEqual(amount_t(579L), 456L + x1);

  x1 += amount_t(456L);
  assertEqual(amount_t(579L), x1);
  x1 += 456L;
  assertEqual(amount_t(1035L), x1);

  amount_t x4("123456789123456789123456789");

  assertEqual(amount_t("246913578246913578246913578"), x4 + x4);

  CPPUNIT_ASSERT(x1.valid());
  CPPUNIT_ASSERT(y1.valid());
  CPPUNIT_ASSERT(x4.valid());
}

void BasicAmountTestCase::testFractionalAddition()
{
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

void BasicAmountTestCase::testIntegerSubtraction()
{
  amount_t x1(123L);
  amount_t y1(456L);

  assertEqual(amount_t(333L), y1 - x1);
  assertEqual(amount_t(-333L), x1 - y1);
  assertEqual(amount_t(23L), x1 - 100L);
  assertEqual(amount_t(-23L), 100L - x1);

  x1 -= amount_t(456L);
  assertEqual(amount_t(-333L), x1);
  x1 -= 456L;
  assertEqual(amount_t(-789L), x1);

  amount_t x4("123456789123456789123456789");
  amount_t y4("8238725986235986");

  assertEqual(amount_t("123456789115218063137220803"), x4 - y4);
  assertEqual(amount_t("-123456789115218063137220803"), y4 - x4);

  CPPUNIT_ASSERT(x1.valid());
  CPPUNIT_ASSERT(y1.valid());
  CPPUNIT_ASSERT(x4.valid());
  CPPUNIT_ASSERT(y4.valid());
}

void BasicAmountTestCase::testFractionalSubtraction()
{
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

void BasicAmountTestCase::testIntegerMultiplication()
{
  amount_t x1(123L);
  amount_t y1(456L);

  assertEqual(amount_t(0L), x1 * 0L);
  assertEqual(amount_t(0L), amount_t(0L) * x1);
  assertEqual(amount_t(0L), 0L * x1);
  assertEqual(x1, x1 * 1L);
  assertEqual(x1, amount_t(1L) * x1);
  assertEqual(x1, 1L * x1);
  assertEqual(- x1, x1 * -1L);
  assertEqual(- x1, amount_t(-1L) * x1);
  assertEqual(- x1, -1L * x1);
  assertEqual(amount_t(56088L), x1 * y1);
  assertEqual(amount_t(56088L), y1 * x1);
  assertEqual(amount_t(56088L), x1 * 456L);
  assertEqual(amount_t(56088L), amount_t(456L) * x1);
  assertEqual(amount_t(56088L), 456L * x1);

  x1 *= amount_t(123L);
  assertEqual(amount_t(15129L), x1);
  x1 *= 123L;
  assertEqual(amount_t(1860867L), x1);

  amount_t x4("123456789123456789123456789");

  assertEqual(amount_t("15241578780673678546105778281054720515622620750190521"),
	      x4 * x4);

  CPPUNIT_ASSERT(x1.valid());
  CPPUNIT_ASSERT(y1.valid());
  CPPUNIT_ASSERT(x4.valid());
}

void BasicAmountTestCase::testFractionalMultiplication()
{
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

void BasicAmountTestCase::testIntegerDivision()
{
  amount_t x1(123L);
  amount_t y1(456L);

  assertThrow(x1 / 0L, amount_error);
  assertEqual(amount_t(0L), amount_t(0L) / x1);
  assertEqual(amount_t(0L), 0L / x1);
  assertEqual(x1, x1 / 1L);
  assertEqual(amount_t("0.008130"), amount_t(1L) / x1);
  assertEqual(amount_t("0.008130"), 1L / x1);
  assertEqual(- x1, x1 / -1L);
  assertEqual(- amount_t("0.008130"), amount_t(-1L) / x1);
  assertEqual(- amount_t("0.008130"), -1L / x1);
  assertEqual(amount_t("0.269737"), x1 / y1);
  assertEqual(amount_t("3.707317"), y1 / x1);
  assertEqual(amount_t("0.269737"), x1 / 456L);
  assertEqual(amount_t("3.707317"), amount_t(456L) / x1);
  assertEqual(amount_t("3.707317"), 456L / x1);

  x1 /= amount_t(456L);
  assertEqual(amount_t("0.269737"), x1);
  x1 /= 456L;
  assertEqual(amount_t("0.00059152850877193"), x1);

  amount_t x4("123456789123456789123456789");
  amount_t y4("56");

  assertEqual(amount_t(1L), x4 / x4);
  assertEqual(amount_t("2204585520061728377204585.517857"), x4 / y4);

  CPPUNIT_ASSERT(x1.valid());
  CPPUNIT_ASSERT(y1.valid());
  CPPUNIT_ASSERT(x4.valid());
  CPPUNIT_ASSERT(y4.valid());
}

void BasicAmountTestCase::testFractionalDivision()
{
  amount_t x1(123.123);
  amount_t y1(456.456);

  assertThrow(x1 / 0L, amount_error);
  assertEqual(amount_t("0.008121959"), amount_t(1.0) / x1);
  assertEqual(amount_t("0.008121959"), 1.0 / x1);
  assertEqual(x1, x1 / 1.0);
  assertEqual(amount_t("0.008121959"), amount_t(1.0) / x1);
  assertEqual(amount_t("0.008121959"), 1.0 / x1);
  assertEqual(- x1, x1 / -1.0);
  assertEqual(- amount_t("0.008121959"), amount_t(-1.0) / x1);
  assertEqual(- amount_t("0.008121959"), -1.0 / x1);
  assertEqual(amount_t("0.269736842105263"), x1 / y1);
  assertEqual(amount_t("3.707317073170732"), y1 / x1);
  assertEqual(amount_t("0.269736842105263"), x1 / 456.456);
  assertEqual(amount_t("3.707317073170732"), amount_t(456.456) / x1);
  assertEqual(amount_t("3.707317073170732"), 456.456 / x1);

  x1 /= amount_t(456.456);
  assertEqual(amount_t("0.269736842105263"), x1);
  x1 /= 456.456;
  assertEqual(amount_t("0.000590937225286255411255411255411255411"), x1);
  x1 /= 456L;
  assertEqual(amount_t("0.000001295914967733016252753094858358016252192982456140350877192982456140350877192982"), x1);

  amount_t x4("1234567891234567.89123456789");
  amount_t y4("56.789");

  assertEqual(amount_t(1.0), x4 / x4);
  assertEqual(amount_t("21739560323910.7554497273748437197344556164046"), x4 / y4);

  CPPUNIT_ASSERT(x1.valid());
  CPPUNIT_ASSERT(y1.valid());
  CPPUNIT_ASSERT(x4.valid());
  CPPUNIT_ASSERT(y4.valid());
}

void BasicAmountTestCase::testNegation()
{
  amount_t x0;
  amount_t x1(-123456L);
  amount_t x3(-123.456);
  amount_t x5("-123456");
  amount_t x6("-123.456");
  amount_t x7(std::string("-123456"));
  amount_t x8(std::string("-123.456"));
  amount_t x9(- x3);

  assertEqual(amount_t(0L), x0);
  assertEqual(x5, x1);
  assertEqual(x7, x1);
  assertEqual(x6, x3);
  assertEqual(x8, x3);
  assertEqual(- x6, x9);
  assertEqual(x3.negate(), x9);

  amount_t x10(x9.negate());

  assertEqual(x3, x10);

  CPPUNIT_ASSERT(x0.valid());
  CPPUNIT_ASSERT(x1.valid());
  CPPUNIT_ASSERT(x3.valid());
  CPPUNIT_ASSERT(x5.valid());
  CPPUNIT_ASSERT(x6.valid());
  CPPUNIT_ASSERT(x7.valid());
  CPPUNIT_ASSERT(x8.valid());
  CPPUNIT_ASSERT(x9.valid());
  CPPUNIT_ASSERT(x10.valid());
}

void BasicAmountTestCase::testAbs()
{
  amount_t x0;
  amount_t x1(-1234L);
  amount_t x2(1234L);

  assertEqual(amount_t(), x0.abs());
  assertEqual(amount_t(1234L), x1.abs());
  assertEqual(amount_t(1234L), x2.abs());

  CPPUNIT_ASSERT(x0.valid());
  CPPUNIT_ASSERT(x1.valid());
  CPPUNIT_ASSERT(x2.valid());
}

void BasicAmountTestCase::testFractionalRound()
{
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

  amount_t x5("0.0000000000000000000000000000000000001");

  assertEqual(amount_t("0.0000000000000000000000000000000000001"),
	      x5.round(37));
  assertEqual(amount_t(), x5.round(36));

  CPPUNIT_ASSERT(x1.valid());
  CPPUNIT_ASSERT(x2.valid());
  CPPUNIT_ASSERT(x3.valid());
  CPPUNIT_ASSERT(x4.valid());
}

void BasicAmountTestCase::testReduction()
{
  amount_t x1("60s");
  amount_t x2("600s");
  amount_t x3("6000s");
  amount_t x4("360000s");
  amount_t x5("10m");		// 600s
  amount_t x6("100m");		// 6000s
  amount_t x7("1000m");		// 60000s
  amount_t x8("10000m");	// 600000s
  amount_t x9("10h");		// 36000s
  amount_t x10("100h");		// 360000s
  amount_t x11("1000h");	// 3600000s
  amount_t x12("10000h");	// 36000000s

  assertEqual(x2, x5);
  assertEqual(x3, x6);
  assertEqual(x4, x10);
}

void BasicAmountTestCase::testSign()
{
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

void BasicAmountTestCase::testTruth()
{
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

void BasicAmountTestCase::testForZero()
{
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

void BasicAmountTestCase::testIntegerConversion()
{
  amount_t x1(123456L);

  assertEqual(true, bool(x1));
  assertEqual(123456L, x1.to_long());
  assertEqual(123456.0, x1.to_double());
  assertEqual(string("123456"), x1.to_string());
  assertEqual(string("123456"), x1.quantity_string());

  CPPUNIT_ASSERT(x1.valid());
}

void BasicAmountTestCase::testFractionalConversion()
{
  amount_t x1(1234.56);

  assertEqual(true, bool(x1));
  assertEqual(1234L, x1.to_long());
  assertEqual(1234.56, x1.to_double());
  assertEqual(string("1234.56"), x1.to_string());
  assertEqual(string("1234.56"), x1.quantity_string());

  CPPUNIT_ASSERT(x1.valid());
}

void BasicAmountTestCase::testPrinting()
{
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
