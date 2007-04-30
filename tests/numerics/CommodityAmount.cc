#include "CommodityAmount.h"

#define internalAmount(x) amount_t::exact(x)

CPPUNIT_TEST_SUITE_NAMED_REGISTRATION(CommodityAmountTestCase, "numerics");

void CommodityAmountTestCase::setUp()
{
  ledger::initialize();

  // Cause the display precision for dollars to be initialized to 2.
  amount_t x1("$1.00");
  assertTrue(x1);
  amount_t::full_strings = true; // makes error reports from UnitTests accurate
}

void CommodityAmountTestCase::tearDown()
{
  amount_t::full_strings = false;
  ledger::shutdown();
}

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

  assertEqual(string("$123.45"), x1.to_string());
  assertEqual(string("$-123.45"), x2.to_string());
  assertEqual(string("$-123.45"), x3.to_string());
  assertEqual(string("DM 123.45"), x4.to_string());
  assertEqual(string("DM -123.45"), x5.to_string());
  assertEqual(string("DM -123.45"), x6.to_string());
  assertEqual(string("123.45 euro"), x7.to_string());
  assertEqual(string("-123.45 euro"), x8.to_string());
  assertEqual(string("123.45€"), x9.to_string());
  assertEqual(string("-123.45€"), x10.to_string());

  assertValid(x1);
  assertValid(x2);
  assertValid(x3);
  assertValid(x4);
  assertValid(x5);
  assertValid(x6);
  assertValid(x7);
  assertValid(x8);
  assertValid(x9);
  assertValid(x10);
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

  assertEqual(amount_t("$-123.45"), x1.negate());
  assertEqual(amount_t("$123.45"), x2.negate());
  assertEqual(amount_t("$123.45"), x3.negate());

  assertEqual(string("$-123.45"), (- x1).to_string());
  assertEqual(string("$123.45"), (- x2).to_string());
  assertEqual(string("$123.45"), (- x3).to_string());
  assertEqual(string("DM -123.45"), (- x4).to_string());
  assertEqual(string("DM 123.45"), (- x5).to_string());
  assertEqual(string("DM 123.45"), (- x6).to_string());
  assertEqual(string("-123.45 euro"), (- x7).to_string());
  assertEqual(string("123.45 euro"), (- x8).to_string());
  assertEqual(string("-123.45€"), (- x9).to_string());
  assertEqual(string("123.45€"), (- x10).to_string());

  assertEqual(amount_t("$-123.45"), x1.negate());
  assertEqual(amount_t("$123.45"), x2.negate());
  assertEqual(amount_t("$123.45"), x3.negate());

  assertValid(x1);
  assertValid(x2);
  assertValid(x3);
  assertValid(x4);
  assertValid(x5);
  assertValid(x6);
  assertValid(x7);
  assertValid(x8);
  assertValid(x9);
  assertValid(x10);
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

  assertEqual(string("$123.45"), x1.to_string());
  assertEqual(string("$-123.45"), x2.to_string());
  assertEqual(string("$-123.45"), x3.to_string());
  assertEqual(string("DM 123.45"), x4.to_string());
  assertEqual(string("DM -123.45"), x5.to_string());
  assertEqual(string("DM -123.45"), x6.to_string());
  assertEqual(string("123.45 euro"), x7.to_string());
  assertEqual(string("-123.45 euro"), x8.to_string());
  assertEqual(string("123.45€"), x9.to_string());
  assertEqual(string("-123.45€"), x10.to_string());

  assertValid(x1);
  assertValid(x2);
  assertValid(x3);
  assertValid(x4);
  assertValid(x5);
  assertValid(x6);
  assertValid(x7);
  assertValid(x8);
  assertValid(x9);
  assertValid(x10);
}

void CommodityAmountTestCase::testEquality()
{
  amount_t x0;
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

  assertTrue(x0.null());
  assertTrue(x0.zero());
  assertTrue(x0.realzero());
  assertTrue(x0.sign() == 0);
  assertTrue(x0.compare(x1) < 0);
  assertTrue(x0.compare(x2) > 0);
  assertTrue(x0.compare(x0) == 0);

  assertTrue(x1 != x2);
  assertTrue(x1 != x4);
  assertTrue(x1 != x7);
  assertTrue(x1 != x9);
  assertTrue(x2 == x3);
  assertTrue(x4 != x5);
  assertTrue(x5 == x6);
  assertTrue(x7 == - x8);
  assertTrue(x9 == - x10);

  assertValid(x0);
  assertValid(x1);
  assertValid(x2);
  assertValid(x3);
  assertValid(x4);
  assertValid(x5);
  assertValid(x6);
  assertValid(x7);
  assertValid(x8);
  assertValid(x9);
  assertValid(x10);
}

void CommodityAmountTestCase::testAddition()
{
  amount_t x0;
  amount_t x1("$123.45");
  amount_t x2(internalAmount("$123.456789"));
  amount_t x3("DM 123.45");
  amount_t x4("123.45 euro");
  amount_t x5("123.45€");
  amount_t x6("123.45");

  assertEqual(amount_t("$246.90"), x1 + x1);
  assertNotEqual(amount_t("$246.91"), x1 + x2);
  assertEqual(internalAmount("$246.906789"), x1 + x2);

  // Converting to string drops internal precision
  assertEqual(string("$246.90"), (x1 + x1).to_string());
  assertEqual(string("$246.91"), (x1 + x2).to_string());

  assertThrow(x1 + x0, amount_error);
  assertThrow(x1 + x3, amount_error);
  assertThrow(x1 + x4, amount_error);
  assertThrow(x1 + x5, amount_error);
  assertThrow(x1 + x6, amount_error);
  assertThrow(x1 + 123.45, amount_error);
  assertThrow(x1 + 123L, amount_error);

  assertEqual(amount_t("DM 246.90"), x3 + x3);
  assertEqual(amount_t("246.90 euro"), x4 + x4);
  assertEqual(amount_t("246.90€"), x5 + x5);

  assertEqual(string("DM 246.90"), (x3 + x3).to_string());
  assertEqual(string("246.90 euro"), (x4 + x4).to_string());
  assertEqual(string("246.90€"), (x5 + x5).to_string());

  x1 += amount_t("$456.45");
  assertEqual(amount_t("$579.90"), x1);
  x1 += amount_t("$456.45");
  assertEqual(amount_t("$1036.35"), x1);
  x1 += amount_t("$456");
  assertEqual(amount_t("$1492.35"), x1);

  amount_t x7(internalAmount("$123456789123456789.123456789123456789"));

  assertEqual(internalAmount("$246913578246913578.246913578246913578"), x7 + x7);

  assertValid(x1);
  assertValid(x2);
  assertValid(x3);
  assertValid(x4);
  assertValid(x5);
  assertValid(x6);
  assertValid(x7);
}

void CommodityAmountTestCase::testSubtraction()
{
  amount_t x0;
  amount_t x1("$123.45");
  amount_t x2(internalAmount("$123.456789"));
  amount_t x3("DM 123.45");
  amount_t x4("123.45 euro");
  amount_t x5("123.45€");
  amount_t x6("123.45");

  assertNotEqual(amount_t(), x1 - x1);
  assertEqual(amount_t("$0"), x1 - x1);
  assertEqual(amount_t("$23.45"), x1 - amount_t("$100.00"));
  assertEqual(amount_t("$-23.45"), amount_t("$100.00") - x1);
  assertNotEqual(amount_t("$-0.01"), x1 - x2);
  assertEqual(internalAmount("$-0.006789"), x1 - x2);

  // Converting to string drops internal precision.  If an amount is
  // zero, it drops the commodity as well.
  assertEqual(string("$0.00"), (x1 - x1).to_string());
  assertEqual(string("$-0.01"), (x1 - x2).to_string());

  assertThrow(x1 - x0, amount_error);
  assertThrow(x1 - x3, amount_error);
  assertThrow(x1 - x4, amount_error);
  assertThrow(x1 - x5, amount_error);
  assertThrow(x1 - x6, amount_error);
  assertThrow(x1 - 123.45, amount_error);
  assertThrow(x1 - 123L, amount_error);

  assertEqual(amount_t("DM 0.00"), x3 - x3);
  assertEqual(amount_t("DM 23.45"), x3 - amount_t("DM 100.00"));
  assertEqual(amount_t("DM -23.45"), amount_t("DM 100.00") - x3);
  assertEqual(amount_t("0.00 euro"), x4 - x4);
  assertEqual(amount_t("23.45 euro"), x4 - amount_t("100.00 euro"));
  assertEqual(amount_t("-23.45 euro"), amount_t("100.00 euro") - x4);
  assertEqual(amount_t("0.00€"), x5 - x5);
  assertEqual(amount_t("23.45€"), x5 - amount_t("100.00€"));
  assertEqual(amount_t("-23.45€"), amount_t("100.00€") - x5);

  assertEqual(string("DM 0.00"), (x3 - x3).to_string());
  assertEqual(string("DM 23.45"), (x3 - amount_t("DM 100.00")).to_string());
  assertEqual(string("DM -23.45"), (amount_t("DM 100.00") - x3).to_string());
  assertEqual(string("0.00 euro"), (x4 - x4).to_string());
  assertEqual(string("23.45 euro"), (x4 - amount_t("100.00 euro")).to_string());
  assertEqual(string("-23.45 euro"), (amount_t("100.00 euro") - x4).to_string());
  assertEqual(string("0.00€"), (x5 - x5).to_string());
  assertEqual(string("23.45€"), (x5 - amount_t("100.00€")).to_string());
  assertEqual(string("-23.45€"), (amount_t("100.00€") - x5).to_string());

  x1 -= amount_t("$456.45");
  assertEqual(amount_t("$-333.00"), x1);
  x1 -= amount_t("$456.45");
  assertEqual(amount_t("$-789.45"), x1);
  x1 -= amount_t("$456");
  assertEqual(amount_t("$-1245.45"), x1);

  amount_t x7(internalAmount("$123456789123456789.123456789123456789"));
  amount_t x8(internalAmount("$2354974984698.98459845984598"));

  assertEqual(internalAmount("$123454434148472090.138858329277476789"), x7 - x8);
  assertEqual(string("$123454434148472090.138858329277476789"), (x7 - x8).to_string());
  assertEqual(string("$123454434148472090.14"),
	      (amount_t("$1.00") * (x7 - x8)).to_string());
  assertEqual(internalAmount("$-123454434148472090.138858329277476789"), x8 - x7);
  assertEqual(string("$-123454434148472090.138858329277476789"), (x8 - x7).to_string());
  assertEqual(string("$-123454434148472090.14"),
	      (amount_t("$1.00") * (x8 - x7)).to_string());

  assertValid(x1);
  assertValid(x2);
  assertValid(x3);
  assertValid(x4);
  assertValid(x5);
  assertValid(x6);
  assertValid(x7);
  assertValid(x8);
}

void CommodityAmountTestCase::testMultiplication()
{
  amount_t x1("$123.12");
  amount_t y1("$456.45");
  amount_t x2(internalAmount("$123.456789"));
  amount_t x3("DM 123.45");
  amount_t x4("123.45 euro");
  amount_t x5("123.45€");

  assertEqual(amount_t("$0.00"), x1 * 0L);
  assertEqual(amount_t("$0.00"), 0L * x1);
  assertEqual(x1, x1 * 1L);
  assertEqual(x1, 1L * x1);
  assertEqual(- x1, x1 * -1L);
  assertEqual(- x1, -1L * x1);
  assertEqual(internalAmount("$56198.124"), x1 * y1);
  assertEqual(string("$56198.12"), (x1 * y1).to_string());
  assertEqual(internalAmount("$56198.124"), y1 * x1);
  assertEqual(string("$56198.12"), (y1 * x1).to_string());

  // Internal amounts retain their precision, even when being
  // converted to strings
  assertEqual(internalAmount("$15199.99986168"), x1 * x2);
  assertEqual(internalAmount("$15199.99986168"), x2 * x1);
  assertEqual(string("$15200.00"), (x1 * x2).to_string());
  assertEqual(string("$15199.99986168"), (x2 * x1).to_string());

  assertThrow(x1 * x3, amount_error);
  assertThrow(x1 * x4, amount_error);
  assertThrow(x1 * x5, amount_error);

  x1 *= amount_t("123.12");
  assertEqual(internalAmount("$15158.5344"), x1);
  assertEqual(string("$15158.53"), x1.to_string());
  x1 *= 123.12;
  assertEqual(internalAmount("$1866318.755328"), x1);
  assertEqual(string("$1866318.76"), x1.to_string());
  x1 *= 123L;
  assertEqual(internalAmount("$229557206.905344"), x1);
  assertEqual(string("$229557206.91"), x1.to_string());

  amount_t x7(internalAmount("$123456789123456789.123456789123456789"));

  assertEqual(internalAmount("$15241578780673678546105778311537878.046486820281054720515622620750190521"),
	      x7 * x7);

  assertValid(x1);
  assertValid(x2);
  assertValid(x3);
  assertValid(x4);
  assertValid(x5);
  assertValid(x7);
}

void CommodityAmountTestCase::testDivision()
{
  amount_t x1("$123.12");
  amount_t y1("$456.45");
  amount_t x2(internalAmount("$123.456789"));
  amount_t x3("DM 123.45");
  amount_t x4("123.45 euro");
  amount_t x5("123.45€");

  assertThrow(x1 / 0L, amount_error);
  assertEqual(amount_t("$0.00"), 0L / x1);
  assertEqual(x1, x1 / 1L);
  assertEqual(internalAmount("$0.00812216"), 1L / x1);
  assertEqual(- x1, x1 / -1L);
  assertEqual(internalAmount("$-0.00812216"), -1L / x1);
  assertEqual(internalAmount("$0.26973382"), x1 / y1);
  assertEqual(string("$0.27"), (x1 / y1).to_string());
  assertEqual(internalAmount("$3.70735867"), y1 / x1);
  assertEqual(string("$3.71"), (y1 / x1).to_string());

  // Internal amounts retain their precision, even when being
  // converted to strings
  assertEqual(internalAmount("$0.99727201"), x1 / x2);
  assertEqual(internalAmount("$1.00273545321637426901"), x2 / x1);
  assertEqual(string("$1.00"), (x1 / x2).to_string());
  assertEqual(string("$1.00273545321637426901"), (x2 / x1).to_string());

  assertThrow(x1 / x3, amount_error);
  assertThrow(x1 / x4, amount_error);
  assertThrow(x1 / x5, amount_error);

  x1 /= amount_t("123.12");
  assertEqual(internalAmount("$1.00"), x1);
  assertEqual(string("$1.00"), x1.to_string());
  x1 /= 123.12;
  assertEqual(internalAmount("$0.00812216"), x1);
  assertEqual(string("$0.01"), x1.to_string());
  x1 /= 123L;
  assertEqual(internalAmount("$0.00006603"), x1);
  assertEqual(string("$0.00"), x1.to_string());

  amount_t x6(internalAmount("$237235987235987.98723987235978"));
  amount_t x7(internalAmount("$123456789123456789.123456789123456789"));

  assertEqual(amount_t("$1"), x7 / x7);
  assertEqual(internalAmount("$0.0019216115121765559608381226612019501046413574469262"),
	      x6 / x7);
  assertEqual(internalAmount("$520.39654928343335571379527154924040947271699678158689736256"),
	      x7 / x6);

  assertValid(x1);
  assertValid(x2);
  assertValid(x3);
  assertValid(x4);
  assertValid(x5);
  assertValid(x6);
  assertValid(x7);
}

void CommodityAmountTestCase::testConversion()
{
  amount_t x1("$1234.56");

  assertEqual(true, bool(x1));
  assertEqual(1234L, long(x1));
  assertEqual(1234.56, double(x1));
  assertEqual(string("$1234.56"), x1.to_string());
  assertEqual(string("1234.56"), x1.quantity_string());

  assertValid(x1);
}

void CommodityAmountTestCase::testRound()
{
  amount_t x1(internalAmount("$1234.567890"));

  assertEqual(internalAmount("$1234.56789"), x1.round(6));
  assertEqual(internalAmount("$1234.56789"), x1.round(5));
  assertEqual(internalAmount("$1234.5679"), x1.round(4));
  assertEqual(internalAmount("$1234.568"), x1.round(3));
  assertEqual(amount_t("$1234.57"), x1.round(2));
  assertEqual(amount_t("$1234.6"), x1.round(1));
  assertEqual(amount_t("$1235"), x1.round(0));

  amount_t x2(internalAmount("$9876.543210"));

  assertEqual(internalAmount("$9876.543210"), x2.round(6));
  assertEqual(internalAmount("$9876.54321"), x2.round(5));
  assertEqual(internalAmount("$9876.5432"), x2.round(4));
  assertEqual(internalAmount("$9876.543"), x2.round(3));
  assertEqual(amount_t("$9876.54"), x2.round(2));
  assertEqual(amount_t("$9876.5"), x2.round(1));
  assertEqual(amount_t("$9877"), x2.round(0));

  amount_t x3(internalAmount("$-1234.567890"));

  assertEqual(internalAmount("$-1234.56789"), x3.round(6));
  assertEqual(internalAmount("$-1234.56789"), x3.round(5));
  assertEqual(internalAmount("$-1234.5679"), x3.round(4));
  assertEqual(internalAmount("$-1234.568"), x3.round(3));
  assertEqual(amount_t("$-1234.57"), x3.round(2));
  assertEqual(amount_t("$-1234.6"), x3.round(1));
  assertEqual(amount_t("$-1235"), x3.round(0));

  amount_t x4(internalAmount("$-9876.543210"));

  assertEqual(internalAmount("$-9876.543210"), x4.round(6));
  assertEqual(internalAmount("$-9876.54321"), x4.round(5));
  assertEqual(internalAmount("$-9876.5432"), x4.round(4));
  assertEqual(internalAmount("$-9876.543"), x4.round(3));
  assertEqual(amount_t("$-9876.54"), x4.round(2));
  assertEqual(amount_t("$-9876.5"), x4.round(1));
  assertEqual(amount_t("$-9877"), x4.round(0));

  amount_t x5("$123.45");

  x5 *= 100.12;

  assertEqual(internalAmount("$12359.814"), x5);
  assertEqual(string("$12359.81"), x5.to_string());
  assertEqual(string("$12359.814"), x5.to_fullstring());
  assertEqual(string("$12359.814"), x5.unround().to_string());

  assertValid(x1);
  assertValid(x2);
  assertValid(x3);
  assertValid(x4);
  assertValid(x5);
}

void CommodityAmountTestCase::testDisplayRound()
{
  amount_t x1("$0.85");
  amount_t x2("$0.1");

  x1 *= 0.19;

  assertNotEqual(amount_t("$0.16"), x1);
  assertEqual(internalAmount("$0.1615"), x1);
  assertEqual(string("$0.16"), x1.to_string());

  assertEqual(amount_t("$0.10"), x2);
  assertNotEqual(internalAmount("$0.101"), x2);
  assertEqual(string("$0.10"), x2.to_string());

  x1 *= 7L;

  assertNotEqual(amount_t("$1.13"), x1);
  assertEqual(internalAmount("$1.1305"), x1);
  assertEqual(string("$1.13"), x1.to_string());
}

void CommodityAmountTestCase::testTruth()
{
  amount_t x1("$1234");
  amount_t x2("$1234.56");

  if (x1)
    CPPUNIT_ASSERT(true);
  else
    CPPUNIT_ASSERT(false);

  if (x2)
    CPPUNIT_ASSERT(true);
  else
    CPPUNIT_ASSERT(false);

  assertValid(x1);
  assertValid(x2);
}

void CommodityAmountTestCase::testForZero()
{
  amount_t x1(internalAmount("$0.000000000000000000001"));

  assertFalse(x1);
  assertTrue(x1.zero());
  assertFalse(x1.realzero());

  assertValid(x1);
}

void CommodityAmountTestCase::testComparisons()
{
  amount_t x0;
  amount_t x1("$-123");
  amount_t x2("$123.00");
  amount_t x3(internalAmount("$-123.4544"));
  amount_t x4(internalAmount("$123.4544"));
  amount_t x5("$-123.45");
  amount_t x6("$123.45");

  assertTrue(x0 > x1);
  assertTrue(x0 < x2);
  assertTrue(x0 > x3);
  assertTrue(x0 < x4);
  assertTrue(x0 > x5);
  assertTrue(x0 < x6);

  assertTrue(x1 > x3);
  assertTrue(x3 <= x5);
  assertTrue(x3 < x5);
  assertTrue(x3 <= x5);
  assertFalse(x3 == x5);
  assertTrue(x3 < x1);
  assertTrue(x3 < x4);

  assertValid(x0);
  assertValid(x1);
  assertValid(x2);
  assertValid(x3);
  assertValid(x4);
  assertValid(x5);
  assertValid(x6);
}

void CommodityAmountTestCase::testSign()
{
  amount_t x0;
  amount_t x1(internalAmount("$0.0000000000000000000000000000000000001"));
  amount_t x2(internalAmount("$-0.0000000000000000000000000000000000001"));
  amount_t x3("$1");
  amount_t x4("$-1");

  assertFalse(x0.sign());
  assertTrue(x1.sign() != 0);
  assertTrue(x2.sign() != 0);
  assertTrue(x3.sign() > 0);
  assertTrue(x4.sign() < 0);

  assertValid(x0);
  assertValid(x1);
  assertValid(x2);
  assertValid(x3);
  assertValid(x4);
}

void CommodityAmountTestCase::testAbs()
{
  amount_t x0;
  amount_t x1("$-1234.56");
  amount_t x2("$1234.56");

  assertEqual(amount_t(), x0.abs());
  assertEqual(amount_t("$1234.56"), x1.abs());
  assertEqual(amount_t("$1234.56"), x2.abs());

  assertValid(x0);
  assertValid(x1);
  assertValid(x2);
}

void CommodityAmountTestCase::testPrinting()
{
  amount_t x0;
  amount_t x1(internalAmount("$982340823.386238098235098235098235098"));
  amount_t x2("$982340823.38");

  {
    std::ostringstream bufstr;
    bufstr << x0;

    assertEqual(std::string("0"), bufstr.str());
  }

  {
    std::ostringstream bufstr;
    bufstr << x1;

    assertEqual(std::string("$982340823.386238098235098235098235098"),
		bufstr.str());
  }

  {
    std::ostringstream bufstr;
    bufstr << (x1 * x2).to_string();

    assertEqual(std::string("$964993493285024293.18099172508158508135413499124"),
		bufstr.str());
  }

  {
    std::ostringstream bufstr;
    bufstr << (x2 * x1).to_string();

    assertEqual(std::string("$964993493285024293.18"), bufstr.str());
  }

  assertValid(x0);
  assertValid(x1);
  assertValid(x2);
}

