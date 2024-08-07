#define BOOST_TEST_DYN_LINK
#define BOOST_TEST_MODULE math
#include <boost/test/unit_test.hpp>

#include <system.hh>

#include "amount.h"
#include "commodity.h"

#define internalAmount(x) amount_t::exact(x)

using namespace ledger;

struct amount_fixture {
  amount_fixture() {
  times_initialize();
  amount_t::initialize();

  // Cause the display precision for dollars to be initialized to 2.
  amount_t x1("$1.00");
  BOOST_CHECK(x1);

  amount_t::stream_fullstrings = true; // make reports from UnitTests accurate
  }

  ~amount_fixture() {
  amount_t::stream_fullstrings = false;
  amount_t::shutdown();
  times_shutdown();
  }
};

BOOST_FIXTURE_TEST_SUITE(amount, amount_fixture)

BOOST_AUTO_TEST_CASE(testParser)
{
  amount_t x0;
  amount_t x1;
  amount_t x2;
  amount_t x3;
  amount_t x4("123.456");
  amount_t x5(x4);
  amount_t x6(x4);
  amount_t x7(x4);
  amount_t x8("$123.45");
  amount_t x9(x8);
  amount_t x10(x8);
  amount_t x11(x8);
  amount_t x12("$100");

  BOOST_CHECK_EQUAL(amount_t::precision_t(2), x12.commodity().precision());

#ifndef NOT_FOR_PYTHON
  string buf("$100...");
  std::istringstream input(buf);
  amount_t x13;
  x13.parse(input);
  BOOST_CHECK_EQUAL(x12, x13);
#endif // NOT_FOR_PYTHON

  amount_t x14;
  BOOST_CHECK_THROW(x14.parse("DM"), amount_error);

  amount_t x15("$1.000.000,00"); // parsing this switches us to European

  amount_t x16("$2000");
  BOOST_CHECK_EQUAL(string("$2.000,00"), x16.to_string());
  x16.parse("$2000,00");
  BOOST_CHECK_EQUAL(string("$2.000,00"), x16.to_string());

  // Since use of a decimal-comma is an additive quality, we must switch back
  // to decimal-period manually.
  x15.commodity().drop_flags(COMMODITY_STYLE_DECIMAL_COMMA);

  amount_t x17("$1,000,000.00"); // parsing this switches back to American

  amount_t x18("$2000");
  BOOST_CHECK_EQUAL(string("$2,000.00"), x18.to_string());
  x18.parse("$2,000");
  BOOST_CHECK_EQUAL(string("$2,000.00"), x18.to_string());

  BOOST_CHECK_EQUAL(x15, x17);

  amount_t x19("EUR 1000");
  amount_t x20("EUR 1000");

  BOOST_CHECK_EQUAL(string("EUR 1000"), x19.to_string());
  BOOST_CHECK_EQUAL(string("EUR 1000"), x20.to_string());

  x1.parse("$100.0000", PARSE_NO_MIGRATE);
  BOOST_CHECK_EQUAL(amount_t::precision_t(2), x12.commodity().precision());
  BOOST_CHECK_EQUAL(x1.commodity(), x12.commodity());
  BOOST_CHECK_EQUAL(x1, x12);

  x0.parse("$100.0000");
  BOOST_CHECK_EQUAL(amount_t::precision_t(4), x12.commodity().precision());
  BOOST_CHECK_EQUAL(x0.commodity(), x12.commodity());
  BOOST_CHECK_EQUAL(x0, x12);

  x2.parse("$100.00", PARSE_NO_REDUCE);
  BOOST_CHECK_EQUAL(x2, x12);
  x3.parse("$100.00", PARSE_NO_MIGRATE | PARSE_NO_REDUCE);
  BOOST_CHECK_EQUAL(x3, x12);

  x4.parse("$100.00");
  BOOST_CHECK_EQUAL(x4, x12);
  x5.parse("$100.00", PARSE_NO_MIGRATE);
  BOOST_CHECK_EQUAL(x5, x12);
  x6.parse("$100.00", PARSE_NO_REDUCE);
  BOOST_CHECK_EQUAL(x6, x12);
  x7.parse("$100.00", PARSE_NO_MIGRATE | PARSE_NO_REDUCE);
  BOOST_CHECK_EQUAL(x7, x12);

  x8.parse("$100.00");
  BOOST_CHECK_EQUAL(x8, x12);
  x9.parse("$100.00", PARSE_NO_MIGRATE);
  BOOST_CHECK_EQUAL(x9, x12);
  x10.parse("$100.00", PARSE_NO_REDUCE);
  BOOST_CHECK_EQUAL(x10, x12);
  x11.parse("$100.00", PARSE_NO_MIGRATE | PARSE_NO_REDUCE);
  BOOST_CHECK_EQUAL(x11, x12);

  BOOST_CHECK(x0.valid());
  BOOST_CHECK(x1.valid());
  BOOST_CHECK(x2.valid());
  BOOST_CHECK(x3.valid());
  BOOST_CHECK(x5.valid());
  BOOST_CHECK(x6.valid());
  BOOST_CHECK(x7.valid());
  BOOST_CHECK(x8.valid());
  BOOST_CHECK(x9.valid());
  BOOST_CHECK(x10.valid());
  BOOST_CHECK(x11.valid());
  BOOST_CHECK(x12.valid());
}

BOOST_AUTO_TEST_CASE(testConstructors)
{
  amount_t x0;
  amount_t x1(123456L);
  amount_t x2(123456UL);
  amount_t x3("123.456");
  amount_t x5("123456");
  amount_t x6("123.456");
  amount_t x7(string("123456"));
  amount_t x8(string("123.456"));
  amount_t x9(x3);
  amount_t x10(x6);
  amount_t x11(x8);

  BOOST_CHECK_EQUAL(amount_t(), x0);
  BOOST_CHECK_NE(amount_t("0"), x0);
  BOOST_CHECK_NE(amount_t("0.0"), x0);
  BOOST_CHECK_EQUAL(x2, x1);
  BOOST_CHECK_EQUAL(x5, x1);
  BOOST_CHECK_EQUAL(x7, x1);
  BOOST_CHECK_EQUAL(x6, x3);
  BOOST_CHECK_EQUAL(x8, x3);
  BOOST_CHECK_EQUAL(x10, x3);
  BOOST_CHECK_EQUAL(x10, x9);

  BOOST_CHECK(x0.valid());
  BOOST_CHECK(x1.valid());
  BOOST_CHECK(x2.valid());
  BOOST_CHECK(x3.valid());
  BOOST_CHECK(x5.valid());
  BOOST_CHECK(x6.valid());
  BOOST_CHECK(x7.valid());
  BOOST_CHECK(x8.valid());
  BOOST_CHECK(x9.valid());
  BOOST_CHECK(x10.valid());
  BOOST_CHECK(x11.valid());
}

BOOST_AUTO_TEST_CASE(testCommodityConstructors)
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

  BOOST_CHECK_EQUAL(amount_t("$123.45"), x1);
  BOOST_CHECK_EQUAL(amount_t("-$123.45"), x2);
  BOOST_CHECK_EQUAL(amount_t("$-123.45"), x3);
  BOOST_CHECK_EQUAL(amount_t("DM 123.45"), x4);
  BOOST_CHECK_EQUAL(amount_t("-DM 123.45"), x5);
  BOOST_CHECK_EQUAL(amount_t("DM -123.45"), x6);
  BOOST_CHECK_EQUAL(amount_t("123.45 euro"), x7);
  BOOST_CHECK_EQUAL(amount_t("-123.45 euro"), x8);
  BOOST_CHECK_EQUAL(amount_t("123.45€"), x9);
  BOOST_CHECK_EQUAL(amount_t("-123.45€"), x10);

  BOOST_CHECK_EQUAL(string("$123.45"), x1.to_string());
  BOOST_CHECK_EQUAL(string("$-123.45"), x2.to_string());
  BOOST_CHECK_EQUAL(string("$-123.45"), x3.to_string());
  BOOST_CHECK_EQUAL(string("DM 123.45"), x4.to_string());
  BOOST_CHECK_EQUAL(string("DM -123.45"), x5.to_string());
  BOOST_CHECK_EQUAL(string("DM -123.45"), x6.to_string());
  BOOST_CHECK_EQUAL(string("123.45 euro"), x7.to_string());
  BOOST_CHECK_EQUAL(string("-123.45 euro"), x8.to_string());
  BOOST_CHECK_EQUAL(string("123.45€"), x9.to_string());
  BOOST_CHECK_EQUAL(string("-123.45€"), x10.to_string());

  BOOST_CHECK(x1.valid());
  BOOST_CHECK(x2.valid());
  BOOST_CHECK(x3.valid());
  BOOST_CHECK(x4.valid());
  BOOST_CHECK(x5.valid());
  BOOST_CHECK(x6.valid());
  BOOST_CHECK(x7.valid());
  BOOST_CHECK(x8.valid());
  BOOST_CHECK(x9.valid());
  BOOST_CHECK(x10.valid());
}

#ifndef NOT_FOR_PYTHON

BOOST_AUTO_TEST_CASE(testAssignment)
{
  amount_t x0;
  amount_t x1;
  amount_t x2;
  amount_t x3;
  amount_t x5;
  amount_t x6;
  amount_t x7;
  amount_t x8;
  amount_t x9;
  amount_t x10;

  x1  = 123456L;
  x2  = 123456UL;
  x3  = "123.456";
  x5  = "123456";
  x6  = "123.456";
  x7  = string("123456");
  x8  = string("123.456");
  x9  = x3;
  x10 = amount_t(x6);

  BOOST_CHECK_EQUAL(x2, x1);
  BOOST_CHECK_EQUAL(x5, x1);
  BOOST_CHECK_EQUAL(x7, x1);
  BOOST_CHECK_EQUAL(x6, x3);
  BOOST_CHECK_EQUAL(x8, x3);
  BOOST_CHECK_EQUAL(x10, x3);
  BOOST_CHECK_EQUAL(x10, x9);

  BOOST_CHECK(! x1.is_null());
  x1 = x0;                      // sets x1 back to uninitialized state
  BOOST_CHECK(x0.is_null());
  BOOST_CHECK(x1.is_null());

  BOOST_CHECK(x0.valid());
  BOOST_CHECK(x1.valid());
  BOOST_CHECK(x2.valid());
  BOOST_CHECK(x3.valid());
  BOOST_CHECK(x5.valid());
  BOOST_CHECK(x6.valid());
  BOOST_CHECK(x7.valid());
  BOOST_CHECK(x8.valid());
  BOOST_CHECK(x9.valid());
  BOOST_CHECK(x10.valid());
}

BOOST_AUTO_TEST_CASE(testCommodityAssignment)
{
  amount_t x1;
  amount_t x2;
  amount_t x3;
  amount_t x4;
  amount_t x5;
  amount_t x6;
  amount_t x7;
  amount_t x8;
  amount_t x9;
  amount_t x10;

  x1  = "$123.45";
  x2  = "-$123.45";
  x3  = "$-123.45";
  x4  = "DM 123.45";
  x5  = "-DM 123.45";
  x6  = "DM -123.45";
  x7  = "123.45 euro";
  x8  = "-123.45 euro";
  x9  = "123.45€";
  x10 = "-123.45€";

  BOOST_CHECK_EQUAL(amount_t("$123.45"), x1);
  BOOST_CHECK_EQUAL(amount_t("-$123.45"), x2);
  BOOST_CHECK_EQUAL(amount_t("$-123.45"), x3);
  BOOST_CHECK_EQUAL(amount_t("DM 123.45"), x4);
  BOOST_CHECK_EQUAL(amount_t("-DM 123.45"), x5);
  BOOST_CHECK_EQUAL(amount_t("DM -123.45"), x6);
  BOOST_CHECK_EQUAL(amount_t("123.45 euro"), x7);
  BOOST_CHECK_EQUAL(amount_t("-123.45 euro"), x8);
  BOOST_CHECK_EQUAL(amount_t("123.45€"), x9);
  BOOST_CHECK_EQUAL(amount_t("-123.45€"), x10);

  BOOST_CHECK_EQUAL(string("$123.45"), x1.to_string());
  BOOST_CHECK_EQUAL(string("$-123.45"), x2.to_string());
  BOOST_CHECK_EQUAL(string("$-123.45"), x3.to_string());
  BOOST_CHECK_EQUAL(string("DM 123.45"), x4.to_string());
  BOOST_CHECK_EQUAL(string("DM -123.45"), x5.to_string());
  BOOST_CHECK_EQUAL(string("DM -123.45"), x6.to_string());
  BOOST_CHECK_EQUAL(string("123.45 euro"), x7.to_string());
  BOOST_CHECK_EQUAL(string("-123.45 euro"), x8.to_string());
  BOOST_CHECK_EQUAL(string("123.45€"), x9.to_string());
  BOOST_CHECK_EQUAL(string("-123.45€"), x10.to_string());

  BOOST_CHECK(x1.valid());
  BOOST_CHECK(x2.valid());
  BOOST_CHECK(x3.valid());
  BOOST_CHECK(x4.valid());
  BOOST_CHECK(x5.valid());
  BOOST_CHECK(x6.valid());
  BOOST_CHECK(x7.valid());
  BOOST_CHECK(x8.valid());
  BOOST_CHECK(x9.valid());
  BOOST_CHECK(x10.valid());
}

#endif // NOT_FOR_PYTHON

BOOST_AUTO_TEST_CASE(testEquality)
{
  amount_t x1(123456L);
  amount_t x2(456789L);
  amount_t x3(333333L);
  amount_t x4("123456.0");
  amount_t x5("123456.0");
  amount_t x6("123456.0");

  BOOST_CHECK(x1 == 123456L);
  BOOST_CHECK(x1 != x2);
  BOOST_CHECK(x1 == (x2 - x3));
  BOOST_CHECK(x1 == x4);
  BOOST_CHECK(x4 == x5);
  BOOST_CHECK(x4 == x6);

  BOOST_CHECK(x1 == 123456L);
  BOOST_CHECK(123456L == x1);
  BOOST_CHECK(x1 == 123456UL);
  BOOST_CHECK(123456UL == x1);
  BOOST_CHECK(x1 == amount_t("123456.0"));
  BOOST_CHECK(amount_t("123456.0") == x1);

  BOOST_CHECK(x1.valid());
  BOOST_CHECK(x2.valid());
  BOOST_CHECK(x3.valid());
  BOOST_CHECK(x4.valid());
  BOOST_CHECK(x5.valid());
  BOOST_CHECK(x6.valid());
}

BOOST_AUTO_TEST_CASE(testCommodityEquality)
{
  amount_t x0;
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

  BOOST_CHECK(x0.is_null());
  BOOST_CHECK_THROW(x0.is_zero(), amount_error);
  BOOST_CHECK_THROW(x0.is_realzero(), amount_error);
  BOOST_CHECK_THROW(x0.sign(), amount_error);
  BOOST_CHECK_THROW(x0.compare(x1), amount_error);
  BOOST_CHECK_THROW(x0.compare(x2), amount_error);
  BOOST_CHECK_THROW(x0.compare(x0), amount_error);

  BOOST_CHECK(x1 != x2);
  BOOST_CHECK(x1 != x4);
  BOOST_CHECK(x1 != x7);
  BOOST_CHECK(x1 != x9);
  BOOST_CHECK(x2 == x3);
  BOOST_CHECK(x4 != x5);
  BOOST_CHECK(x5 == x6);
  BOOST_CHECK(x7 == - x8);
  BOOST_CHECK(x9 == - x10);

  BOOST_CHECK(x0.valid());
  BOOST_CHECK(x1.valid());
  BOOST_CHECK(x2.valid());
  BOOST_CHECK(x3.valid());
  BOOST_CHECK(x4.valid());
  BOOST_CHECK(x5.valid());
  BOOST_CHECK(x6.valid());
  BOOST_CHECK(x7.valid());
  BOOST_CHECK(x8.valid());
  BOOST_CHECK(x9.valid());
  BOOST_CHECK(x10.valid());
}

BOOST_AUTO_TEST_CASE(testComparisons)
{
  amount_t x0;
  amount_t x1(-123L);
  amount_t x2(123L);
  amount_t x3("-123.45");
  amount_t x4("123.45");
  amount_t x5("-123.45");
  amount_t x6("123.45");

  BOOST_CHECK_THROW(x0 > x1, amount_error);
  BOOST_CHECK_THROW(x0 < x2, amount_error);
  BOOST_CHECK_THROW(x0 > x3, amount_error);
  BOOST_CHECK_THROW(x0 < x4, amount_error);
  BOOST_CHECK_THROW(x0 > x5, amount_error);
  BOOST_CHECK_THROW(x0 < x6, amount_error);

  BOOST_CHECK(x1 > x3);
  BOOST_CHECK(x3 <= x5);
  BOOST_CHECK(x3 >= x5);
  BOOST_CHECK(x3 < x1);
  BOOST_CHECK(x3 < x4);

  BOOST_CHECK(x1 < 100L);
  BOOST_CHECK(100L > x1);
  BOOST_CHECK(x1 < 100UL);
  BOOST_CHECK(100UL > x1);
#ifndef NOT_FOR_PYTHON
  BOOST_CHECK(x1 < 100.0);
  BOOST_CHECK(100.0 > x1);
#endif // NOT_FOR_PYTHON

  BOOST_CHECK(x0.valid());
  BOOST_CHECK(x1.valid());
  BOOST_CHECK(x2.valid());
  BOOST_CHECK(x3.valid());
  BOOST_CHECK(x4.valid());
  BOOST_CHECK(x5.valid());
  BOOST_CHECK(x6.valid());
}

BOOST_AUTO_TEST_CASE(testCommodityComparisons)
{
  amount_t x1("$-123");
  amount_t x2("$123.00");
  amount_t x3(internalAmount("$-123.4544"));
  amount_t x4(internalAmount("$123.4544"));
  amount_t x5("$-123.45");
  amount_t x6("$123.45");
  amount_t x7("DM 123.45");

  BOOST_CHECK(x1 > x3);
  BOOST_CHECK(x3 <= x5);
  BOOST_CHECK(x3 < x5);
  BOOST_CHECK(x3 <= x5);
#ifndef NOT_FOR_PYTHON
  BOOST_CHECK(! (x3 == x5));
#endif
  BOOST_CHECK(x3 < x1);
  BOOST_CHECK(x3 < x4);
#ifndef NOT_FOR_PYTHON
  BOOST_CHECK(! (x6 == x7));
#endif
  BOOST_CHECK_THROW(x6 < x7, amount_error);

  BOOST_CHECK(x1.valid());
  BOOST_CHECK(x2.valid());
  BOOST_CHECK(x3.valid());
  BOOST_CHECK(x4.valid());
  BOOST_CHECK(x5.valid());
  BOOST_CHECK(x6.valid());
}

BOOST_AUTO_TEST_CASE(testIntegerAddition)
{
  amount_t x0;
  amount_t x1(123L);
  amount_t y1(456L);

  BOOST_CHECK_EQUAL(amount_t(579L), x1 + y1);
  BOOST_CHECK_EQUAL(amount_t(579L), x1 + 456L);
  BOOST_CHECK_EQUAL(amount_t(579L), 456L + x1);

  x1 += amount_t(456L);
  BOOST_CHECK_EQUAL(amount_t(579L), x1);
  x1 += 456L;
  BOOST_CHECK_EQUAL(amount_t(1035L), x1);

  amount_t x4("123456789123456789123456789");

  BOOST_CHECK_EQUAL(amount_t("246913578246913578246913578"), x4 + x4);

  BOOST_CHECK(x0.valid());
  BOOST_CHECK(x1.valid());
  BOOST_CHECK(y1.valid());
  BOOST_CHECK(x4.valid());
}

BOOST_AUTO_TEST_CASE(testFractionalAddition)
{
  amount_t x1("123.123");
  amount_t y1("456.456");

  BOOST_CHECK_EQUAL(amount_t("579.579"), x1 + y1);
  BOOST_CHECK_EQUAL(amount_t("579.579"), x1 + amount_t("456.456"));
  BOOST_CHECK_EQUAL(amount_t("579.579"), amount_t("456.456") + x1);

  x1 += amount_t("456.456");
  BOOST_CHECK_EQUAL(amount_t("579.579"), x1);
  x1 += amount_t("456.456");
  BOOST_CHECK_EQUAL(amount_t("1036.035"), x1);
  x1 += 456L;
  BOOST_CHECK_EQUAL(amount_t("1492.035"), x1);

  amount_t x2("123456789123456789.123456789123456789");

  BOOST_CHECK_EQUAL(amount_t("246913578246913578.246913578246913578"), x2 + x2);

  BOOST_CHECK(x1.valid());
  BOOST_CHECK(y1.valid());
  BOOST_CHECK(x2.valid());
}

BOOST_AUTO_TEST_CASE(testCommodityAddition)
{
  amount_t x0;
  amount_t x1("$123.45");
  amount_t x2(internalAmount("$123.456789"));
  amount_t x3("DM 123.45");
  amount_t x4("123.45 euro");
  amount_t x5("123.45€");
  amount_t x6("123.45");

  BOOST_CHECK_EQUAL(amount_t("$246.90"), x1 + x1);
  BOOST_CHECK_NE(amount_t("$246.91"), x1 + x2);
  BOOST_CHECK_EQUAL(internalAmount("$246.906789"), x1 + x2);

  // Converting to string drops internal precision
  BOOST_CHECK_EQUAL(string("$246.90"), (x1 + x1).to_string());
  BOOST_CHECK_EQUAL(string("$246.91"), (x1 + x2).to_string());

  BOOST_CHECK_THROW(x1 + x0, amount_error);
  BOOST_CHECK_THROW(x0 + x1, amount_error);
  BOOST_CHECK_THROW(x0 + x0, amount_error);
  BOOST_CHECK_THROW(x1 + x3, amount_error);
  BOOST_CHECK_THROW(x1 + x4, amount_error);
  BOOST_CHECK_THROW(x1 + x5, amount_error);
  BOOST_CHECK_EQUAL(string("$246.90"), (x1 + x6).to_string());
#ifndef NOT_FOR_PYTHON
  BOOST_CHECK_EQUAL(string("$246.90"), (x1 + 123.45).to_string());
#endif // NOT_FOR_PYTHON
  BOOST_CHECK_EQUAL(string("$246.45"), (x1 + 123L).to_string());

  BOOST_CHECK_EQUAL(amount_t("DM 246.90"), x3 + x3);
  BOOST_CHECK_EQUAL(amount_t("246.90 euro"), x4 + x4);
  BOOST_CHECK_EQUAL(amount_t("246.90€"), x5 + x5);

  BOOST_CHECK_EQUAL(string("DM 246.90"), (x3 + x3).to_string());
  BOOST_CHECK_EQUAL(string("246.90 euro"), (x4 + x4).to_string());
  BOOST_CHECK_EQUAL(string("246.90€"), (x5 + x5).to_string());

  x1 += amount_t("$456.45");
  BOOST_CHECK_EQUAL(amount_t("$579.90"), x1);
  x1 += amount_t("$456.45");
  BOOST_CHECK_EQUAL(amount_t("$1036.35"), x1);
  x1 += amount_t("$456");
  BOOST_CHECK_EQUAL(amount_t("$1492.35"), x1);

  amount_t x7(internalAmount("$123456789123456789.123456789123456789"));

  BOOST_CHECK_EQUAL(internalAmount("$246913578246913578.246913578246913578"), x7 + x7);

  BOOST_CHECK(x1.valid());
  BOOST_CHECK(x2.valid());
  BOOST_CHECK(x3.valid());
  BOOST_CHECK(x4.valid());
  BOOST_CHECK(x5.valid());
  BOOST_CHECK(x6.valid());
  BOOST_CHECK(x7.valid());
}

BOOST_AUTO_TEST_CASE(testIntegerSubtraction)
{
  amount_t x1(123L);
  amount_t y1(456L);

  BOOST_CHECK_EQUAL(amount_t(333L), y1 - x1);
  BOOST_CHECK_EQUAL(amount_t(-333L), x1 - y1);
  BOOST_CHECK_EQUAL(amount_t(23L), x1 - 100L);
  BOOST_CHECK_EQUAL(amount_t(-23L), 100L - x1);

  x1 -= amount_t(456L);
  BOOST_CHECK_EQUAL(amount_t(-333L), x1);
  x1 -= 456L;
  BOOST_CHECK_EQUAL(amount_t(-789L), x1);

  amount_t x4("123456789123456789123456789");
  amount_t y4("8238725986235986");

  BOOST_CHECK_EQUAL(amount_t("123456789115218063137220803"), x4 - y4);
  BOOST_CHECK_EQUAL(amount_t("-123456789115218063137220803"), y4 - x4);

  BOOST_CHECK(x1.valid());
  BOOST_CHECK(y1.valid());
  BOOST_CHECK(x4.valid());
  BOOST_CHECK(y4.valid());
}

BOOST_AUTO_TEST_CASE(testFractionalSubtraction)
{
  amount_t x1("123.123");
  amount_t y1("456.456");

  BOOST_CHECK_EQUAL(amount_t("-333.333"), x1 - y1);
  BOOST_CHECK_EQUAL(amount_t("333.333"), y1 - x1);

  x1 -= amount_t("456.456");
  BOOST_CHECK_EQUAL(amount_t("-333.333"), x1);
  x1 -= amount_t("456.456");
  BOOST_CHECK_EQUAL(amount_t("-789.789"), x1);
  x1 -= 456L;
  BOOST_CHECK_EQUAL(amount_t("-1245.789"), x1);

  amount_t x2("123456789123456789.123456789123456789");
  amount_t y2("9872345982459.248974239578");

  BOOST_CHECK_EQUAL(amount_t("123446916777474329.874482549545456789"), x2 - y2);
  BOOST_CHECK_EQUAL(amount_t("-123446916777474329.874482549545456789"), y2 - x2);

  BOOST_CHECK(x1.valid());
  BOOST_CHECK(y1.valid());
  BOOST_CHECK(x2.valid());
  BOOST_CHECK(y2.valid());
}

BOOST_AUTO_TEST_CASE(testCommoditySubtraction)
{
  amount_t x0;
  amount_t x1("$123.45");
  amount_t x2(internalAmount("$123.456789"));
  amount_t x3("DM 123.45");
  amount_t x4("123.45 euro");
  amount_t x5("123.45€");
  amount_t x6("123.45");

  BOOST_CHECK_NE(amount_t(), x1 - x1);
  BOOST_CHECK_EQUAL(amount_t("$0"), x1 - x1);
  BOOST_CHECK_EQUAL(amount_t("$23.45"), x1 - amount_t("$100.00"));
  BOOST_CHECK_EQUAL(amount_t("$-23.45"), amount_t("$100.00") - x1);
  BOOST_CHECK_NE(amount_t("$-0.01"), x1 - x2);
  BOOST_CHECK_EQUAL(internalAmount("$-0.006789"), x1 - x2);

  // Converting to string drops internal precision.  If an amount is
  // zero, it drops the commodity as well.
  BOOST_CHECK_EQUAL(string("$0.00"), (x1 - x1).to_string());
  BOOST_CHECK_EQUAL(string("$-0.01"), (x1 - x2).to_string());

  BOOST_CHECK_THROW(x1 - x0, amount_error);
  BOOST_CHECK_THROW(x0 - x1, amount_error);
  BOOST_CHECK_THROW(x0 - x0, amount_error);
  BOOST_CHECK_THROW(x1 - x3, amount_error);
  BOOST_CHECK_THROW(x1 - x4, amount_error);
  BOOST_CHECK_THROW(x1 - x5, amount_error);
  BOOST_CHECK_EQUAL(string("$0.00"), (x1 - x6).to_string());
#ifndef NOT_FOR_PYTHON
  BOOST_CHECK_EQUAL(string("$-0.00"), (x1 - 123.45).to_string());
#endif // NOT_FOR_PYTHON
  BOOST_CHECK_EQUAL(string("$0.45"), (x1 - 123L).to_string());

  BOOST_CHECK_EQUAL(amount_t("DM 0.00"), x3 - x3);
  BOOST_CHECK_EQUAL(amount_t("DM 23.45"), x3 - amount_t("DM 100.00"));
  BOOST_CHECK_EQUAL(amount_t("DM -23.45"), amount_t("DM 100.00") - x3);
  BOOST_CHECK_EQUAL(amount_t("0.00 euro"), x4 - x4);
  BOOST_CHECK_EQUAL(amount_t("23.45 euro"), x4 - amount_t("100.00 euro"));
  BOOST_CHECK_EQUAL(amount_t("-23.45 euro"), amount_t("100.00 euro") - x4);
  BOOST_CHECK_EQUAL(amount_t("0.00€"), x5 - x5);
  BOOST_CHECK_EQUAL(amount_t("23.45€"), x5 - amount_t("100.00€"));
  BOOST_CHECK_EQUAL(amount_t("-23.45€"), amount_t("100.00€") - x5);

  BOOST_CHECK_EQUAL(string("DM 0.00"), (x3 - x3).to_string());
  BOOST_CHECK_EQUAL(string("DM 23.45"), (x3 - amount_t("DM 100.00")).to_string());
  BOOST_CHECK_EQUAL(string("DM -23.45"), (amount_t("DM 100.00") - x3).to_string());
  BOOST_CHECK_EQUAL(string("0.00 euro"), (x4 - x4).to_string());
  BOOST_CHECK_EQUAL(string("23.45 euro"), (x4 - amount_t("100.00 euro")).to_string());
  BOOST_CHECK_EQUAL(string("-23.45 euro"), (amount_t("100.00 euro") - x4).to_string());
  BOOST_CHECK_EQUAL(string("0.00€"), (x5 - x5).to_string());
  BOOST_CHECK_EQUAL(string("23.45€"), (x5 - amount_t("100.00€")).to_string());
  BOOST_CHECK_EQUAL(string("-23.45€"), (amount_t("100.00€") - x5).to_string());

  x1 -= amount_t("$456.45");
  BOOST_CHECK_EQUAL(amount_t("$-333.00"), x1);
  x1 -= amount_t("$456.45");
  BOOST_CHECK_EQUAL(amount_t("$-789.45"), x1);
  x1 -= amount_t("$456");
  BOOST_CHECK_EQUAL(amount_t("$-1245.45"), x1);

  amount_t x7(internalAmount("$123456789123456789.123456789123456789"));
  amount_t x8(internalAmount("$2354974984698.98459845984598"));

  BOOST_CHECK_EQUAL(internalAmount("$123454434148472090.138858329277476789"), x7 - x8);
  BOOST_CHECK_EQUAL(string("$123454434148472090.138858329277476789"), (x7 - x8).to_string());
  BOOST_CHECK_EQUAL(string("$123454434148472090.14"),
              (amount_t("$1.00") * (x7 - x8)).to_string());
  BOOST_CHECK_EQUAL(internalAmount("$-123454434148472090.138858329277476789"), x8 - x7);
  BOOST_CHECK_EQUAL(string("$-123454434148472090.138858329277476789"), (x8 - x7).to_string());
  BOOST_CHECK_EQUAL(string("$-123454434148472090.14"),
              (amount_t("$1.00") * (x8 - x7)).to_string());

  BOOST_CHECK(x1.valid());
  BOOST_CHECK(x2.valid());
  BOOST_CHECK(x3.valid());
  BOOST_CHECK(x4.valid());
  BOOST_CHECK(x5.valid());
  BOOST_CHECK(x6.valid());
  BOOST_CHECK(x7.valid());
  BOOST_CHECK(x8.valid());
}

BOOST_AUTO_TEST_CASE(testIntegerMultiplication)
{
  amount_t x1(123L);
  amount_t y1(456L);

  BOOST_CHECK_EQUAL(amount_t(0L), x1 * 0L);
  BOOST_CHECK_EQUAL(amount_t(0L), amount_t(0L) * x1);
  BOOST_CHECK_EQUAL(amount_t(0L), 0L * x1);
  BOOST_CHECK_EQUAL(x1, x1 * 1L);
  BOOST_CHECK_EQUAL(x1, amount_t(1L) * x1);
  BOOST_CHECK_EQUAL(x1, 1L * x1);
  BOOST_CHECK_EQUAL(- x1, x1 * -1L);
  BOOST_CHECK_EQUAL(- x1, amount_t(-1L) * x1);
  BOOST_CHECK_EQUAL(- x1, -1L * x1);
  BOOST_CHECK_EQUAL(amount_t(56088L), x1 * y1);
  BOOST_CHECK_EQUAL(amount_t(56088L), y1 * x1);
  BOOST_CHECK_EQUAL(amount_t(56088L), x1 * 456L);
  BOOST_CHECK_EQUAL(amount_t(56088L), amount_t(456L) * x1);
  BOOST_CHECK_EQUAL(amount_t(56088L), 456L * x1);

  x1 *= amount_t(123L);
  BOOST_CHECK_EQUAL(amount_t(15129L), x1);
  x1 *= 123L;
  BOOST_CHECK_EQUAL(amount_t(1860867L), x1);

  amount_t x4("123456789123456789123456789");

  BOOST_CHECK_EQUAL(amount_t("15241578780673678546105778281054720515622620750190521"),
              x4 * x4);

  BOOST_CHECK(x1.valid());
  BOOST_CHECK(y1.valid());
  BOOST_CHECK(x4.valid());
}

BOOST_AUTO_TEST_CASE(testFractionalMultiplication)
{
  amount_t x1("123.123");
  amount_t y1("456.456");

  BOOST_CHECK_EQUAL(amount_t(0L), x1 * 0L);
  BOOST_CHECK_EQUAL(amount_t(0L), amount_t(0L) * x1);
  BOOST_CHECK_EQUAL(amount_t(0L), 0L * x1);
  BOOST_CHECK_EQUAL(x1, x1 * 1L);
  BOOST_CHECK_EQUAL(x1, amount_t(1L) * x1);
  BOOST_CHECK_EQUAL(x1, 1L * x1);
  BOOST_CHECK_EQUAL(- x1, x1 * -1L);
  BOOST_CHECK_EQUAL(- x1, amount_t(-1L) * x1);
  BOOST_CHECK_EQUAL(- x1, -1L * x1);
  BOOST_CHECK_EQUAL(amount_t("56200.232088"), x1 * y1);
  BOOST_CHECK_EQUAL(amount_t("56200.232088"), y1 * x1);
  BOOST_CHECK_EQUAL(amount_t("56200.232088"), x1 * amount_t("456.456"));
  BOOST_CHECK_EQUAL(amount_t("56200.232088"), amount_t("456.456") * x1);
  BOOST_CHECK_EQUAL(amount_t("56200.232088"), amount_t("456.456") * x1);

  x1 *= amount_t("123.123");
  BOOST_CHECK_EQUAL(amount_t("15159.273129"), x1);
  x1 *= amount_t("123.123");
  BOOST_CHECK_EQUAL(amount_t("1866455.185461867"), x1);
  x1 *= 123L;
  BOOST_CHECK_EQUAL(amount_t("229573987.811809641"), x1);

  amount_t x2("123456789123456789.123456789123456789");

  BOOST_CHECK_EQUAL(amount_t("15241578780673678546105778311537878.046486820281054720515622620750190521"),
              x2 * x2);

  BOOST_CHECK(x1.valid());
  BOOST_CHECK(y1.valid());
  BOOST_CHECK(x2.valid());
}

BOOST_AUTO_TEST_CASE(testCommodityMultiplication)
{
  amount_t x0;
  amount_t x1("$123.12");
  amount_t y1("$456.45");
  amount_t x2(internalAmount("$123.456789"));
  amount_t x3("DM 123.45");
  amount_t x4("123.45 euro");
  amount_t x5("123.45€");

  BOOST_CHECK_EQUAL(amount_t("$0.00"), x1 * 0L);
  BOOST_CHECK_EQUAL(amount_t("$0.00"), 0L * x1);
  BOOST_CHECK_EQUAL(x1, x1 * 1L);
  BOOST_CHECK_EQUAL(x1, 1L * x1);
  BOOST_CHECK_EQUAL(- x1, x1 * -1L);
  BOOST_CHECK_EQUAL(- x1, -1L * x1);
  BOOST_CHECK_EQUAL(internalAmount("$56198.124"), x1 * y1);
  BOOST_CHECK_EQUAL(string("$56198.12"), (x1 * y1).to_string());
  BOOST_CHECK_EQUAL(internalAmount("$56198.124"), y1 * x1);
  BOOST_CHECK_EQUAL(string("$56198.12"), (y1 * x1).to_string());

  // Internal amounts retain their precision, even when being
  // converted to strings
  BOOST_CHECK_EQUAL(internalAmount("$15199.99986168"), x1 * x2);
  BOOST_CHECK_EQUAL(internalAmount("$15199.99986168"), x2 * x1);
  BOOST_CHECK_EQUAL(string("$15200.00"), (x1 * x2).to_string());
  BOOST_CHECK_EQUAL(string("$15199.99986168"), (x2 * x1).to_string());

  BOOST_CHECK_THROW(x1 * x0, amount_error);
  BOOST_CHECK_THROW(x0 * x1, amount_error);
  BOOST_CHECK_THROW(x0 * x0, amount_error);
  //BOOST_CHECK_THROW(x1 * x3, amount_error);
  //BOOST_CHECK_THROW(x1 * x4, amount_error);
  //BOOST_CHECK_THROW(x1 * x5, amount_error);

  x1 *= amount_t("123.12");
  BOOST_CHECK_EQUAL(internalAmount("$15158.5344"), x1);
  BOOST_CHECK_EQUAL(string("$15158.53"), x1.to_string());
  x1 *= amount_t("123.12");
  BOOST_CHECK_EQUAL(internalAmount("$1866318.755328"), x1);
  BOOST_CHECK_EQUAL(string("$1866318.76"), x1.to_string());
  x1 *= 123L;
  BOOST_CHECK_EQUAL(internalAmount("$229557206.905344"), x1);
  BOOST_CHECK_EQUAL(string("$229557206.91"), x1.to_string());

  amount_t x7(internalAmount("$123456789123456789.123456789123456789"));

  BOOST_CHECK_EQUAL(internalAmount("$15241578780673678546105778311537878.046486820281054720515622620750190521"),
              x7 * x7);

  BOOST_CHECK(x1.valid());
  BOOST_CHECK(x2.valid());
  BOOST_CHECK(x3.valid());
  BOOST_CHECK(x4.valid());
  BOOST_CHECK(x5.valid());
  BOOST_CHECK(x7.valid());
}

BOOST_AUTO_TEST_CASE(testIntegerDivision)
{
  amount_t x1(123L);
  amount_t y1(456L);

  BOOST_CHECK_THROW(x1 / 0L, amount_error);
  BOOST_CHECK_EQUAL(amount_t(0L), amount_t(0L) / x1);
  BOOST_CHECK_EQUAL(amount_t(0L), 0L / x1);
  BOOST_CHECK_EQUAL(x1, x1 / 1L);
  BOOST_CHECK_EQUAL(string("0.00813"), (amount_t(1L) / x1).to_string());
  BOOST_CHECK_EQUAL(string("0.00813"), (1L / x1).to_string());
  BOOST_CHECK_EQUAL(- x1, x1 / -1L);
  BOOST_CHECK_EQUAL(string("-0.00813"), (amount_t(-1L) / x1).to_string());
  BOOST_CHECK_EQUAL(string("-0.00813"), (-1L / x1).to_string());
  BOOST_CHECK_EQUAL(string("0.269737"), (x1 / y1).to_string());
  BOOST_CHECK_EQUAL(string("3.707317"), (y1 / x1).to_string());
  BOOST_CHECK_EQUAL(string("0.269737"), (x1 / 456L).to_string());
  BOOST_CHECK_EQUAL(string("3.707317"), (amount_t(456L) / x1).to_string());
  BOOST_CHECK_EQUAL(string("3.707317"), (456L / x1).to_string());

  x1 /= amount_t(456L);
  BOOST_CHECK_EQUAL(string("0.269737"), x1.to_string());
  x1 /= 456L;
  BOOST_CHECK_EQUAL(string("0.000591528163"), x1.to_string());

  amount_t x4("123456789123456789123456789");
  amount_t y4("56");

  BOOST_CHECK_EQUAL(amount_t(1L), x4 / x4);
  BOOST_CHECK_EQUAL(string("2204585520061728377204585.517857"), (x4 / y4).to_string());

  BOOST_CHECK_EQUAL(amount_t("0.000000000000000000000000000001"),
              amount_t("10") / amount_t("10000000000000000000000000000000"));

  BOOST_CHECK(x1.valid());
  BOOST_CHECK(y1.valid());
  BOOST_CHECK(x4.valid());
  BOOST_CHECK(y4.valid());
}

BOOST_AUTO_TEST_CASE(testFractionalDivision)
{
  amount_t x1("123.123");
  amount_t y1("456.456");

  BOOST_CHECK_THROW(x1 / 0L, amount_error);
  BOOST_CHECK_EQUAL(string("0.0081219593"), (amount_t("1.0") / x1).to_string());
  BOOST_CHECK_EQUAL(string("0.0081219593"), (amount_t("1.0") / x1).to_string());
  BOOST_CHECK_EQUAL(x1, x1 / amount_t("1.0"));
  BOOST_CHECK_EQUAL(string("0.0081219593"), (amount_t("1.0") / x1).to_string());
  BOOST_CHECK_EQUAL(string("0.0081219593"), (amount_t("1.0") / x1).to_string());
  BOOST_CHECK_EQUAL(- x1, x1 / amount_t("-1.0"));
  BOOST_CHECK_EQUAL(string("-0.0081219593"), (amount_t("-1.0") / x1).to_string());
  BOOST_CHECK_EQUAL(string("-0.0081219593"), (amount_t("-1.0") / x1).to_string());
  BOOST_CHECK_EQUAL(string("0.269736842105"), (x1 / y1).to_string());
  BOOST_CHECK_EQUAL(string("3.707317073171"), (y1 / x1).to_string());
  BOOST_CHECK_EQUAL(string("0.269736842105"), (x1 / amount_t("456.456")).to_string());
  BOOST_CHECK_EQUAL(string("3.707317073171"), (amount_t("456.456") / x1).to_string());
  BOOST_CHECK_EQUAL(string("3.707317073171"), (amount_t("456.456") / x1).to_string());

  x1 /= amount_t("456.456");
  BOOST_CHECK_EQUAL(string("0.269736842105"), x1.to_string());
  x1 /= amount_t("456.456");
  BOOST_CHECK_EQUAL(string("0.000590937225286255757"), x1.to_string());
  x1 /= 456L;
  BOOST_CHECK_EQUAL(string("0.000001295914967733017011337"), x1.to_string());

  amount_t x4("1234567891234567.89123456789");
  amount_t y4("56.789");

  BOOST_CHECK_EQUAL(amount_t("1.0"), x4 / x4);
  BOOST_CHECK_EQUAL(string("21739560323910.75544972737484371973"), (x4 / y4).to_string());

  BOOST_CHECK(x1.valid());
  BOOST_CHECK(y1.valid());
  BOOST_CHECK(x4.valid());
  BOOST_CHECK(y4.valid());
}

BOOST_AUTO_TEST_CASE(testCommodityDivision)
{
  amount_t x0;
  amount_t x1("$123.12");
  amount_t y1("$456.45");
  amount_t x2(internalAmount("$123.456789"));
  amount_t x3("DM 123.45");
  amount_t x4("123.45 euro");
  amount_t x5("123.45€");

  BOOST_CHECK_THROW(x1 / 0L, amount_error);
  BOOST_CHECK_EQUAL(amount_t("$0.00"), 0L / x1);
  BOOST_CHECK_EQUAL(x1, x1 / 1L);
  BOOST_CHECK_EQUAL(string("$0.00812216"), (1L / x1).to_fullstring());
  BOOST_CHECK_EQUAL(- x1, x1 / -1L);
  BOOST_CHECK_EQUAL(string("$-0.00812216"), (-1L / x1).to_fullstring());
  BOOST_CHECK_EQUAL(string("$0.26973382"), (x1 / y1).to_fullstring());
  BOOST_CHECK_EQUAL(string("$0.27"), (x1 / y1).to_string());
  BOOST_CHECK_EQUAL(string("$3.70735867"), (y1 / x1).to_fullstring());
  BOOST_CHECK_EQUAL(string("$3.71"), (y1 / x1).to_string());

  // Internal amounts retain their precision, even when being
  // converted to strings
  BOOST_CHECK_EQUAL(string("$0.99727201"), (x1 / x2).to_fullstring());
  BOOST_CHECK_EQUAL(string("$1.00273545321637"), (x2 / x1).to_fullstring());
  BOOST_CHECK_EQUAL(string("$1.00"), (x1 / x2).to_string());
  BOOST_CHECK_EQUAL(string("$1.00273545321637"), (x2 / x1).to_string());

  BOOST_CHECK_THROW(x1 / x0, amount_error);
  BOOST_CHECK_THROW(x0 / x1, amount_error);
  BOOST_CHECK_THROW(x0 / x0, amount_error);
  //BOOST_CHECK_THROW(x1 / x3, amount_error);
  //BOOST_CHECK_THROW(x1 / x4, amount_error);
  //BOOST_CHECK_THROW(x1 / x5, amount_error);

  x1 /= amount_t("123.12");
  BOOST_CHECK_EQUAL(string("$1.00"), x1.to_string());
  x1 /= amount_t("123.12");
  BOOST_CHECK_EQUAL(string("$0.00812216"), x1.to_fullstring());
  BOOST_CHECK_EQUAL(string("$0.01"), x1.to_string());
  x1 /= 123L;
  BOOST_CHECK_EQUAL(string("$0.00006603"), x1.to_fullstring());
  BOOST_CHECK_EQUAL(string("$0.00"), x1.to_string());

  amount_t x6(internalAmount("$237235987235987.98723987235978"));
  amount_t x7(internalAmount("$123456789123456789.123456789123456789"));

  BOOST_CHECK_EQUAL(amount_t("$1"), x7 / x7);
  BOOST_CHECK_EQUAL(string("$0.0019216115121765559608381226612019501"),
              (x6 / x7).to_fullstring());
  BOOST_CHECK_EQUAL(string("$520.39654928343335571379527154924040947272"),
              (x7 / x6).to_fullstring());

  BOOST_CHECK(x1.valid());
  BOOST_CHECK(x2.valid());
  BOOST_CHECK(x3.valid());
  BOOST_CHECK(x4.valid());
  BOOST_CHECK(x5.valid());
  BOOST_CHECK(x6.valid());
  BOOST_CHECK(x7.valid());
}

BOOST_AUTO_TEST_CASE(testNegation)
{
  amount_t x0;
  amount_t x1(-123456L);
  amount_t x3("-123.456");
  amount_t x5("-123456");
  amount_t x6("-123.456");
  amount_t x7(string("-123456"));
  amount_t x8(string("-123.456"));
  amount_t x9(- x3);

  BOOST_CHECK_THROW(x0.negated(), amount_error);
  BOOST_CHECK_EQUAL(x5, x1);
  BOOST_CHECK_EQUAL(x7, x1);
  BOOST_CHECK_EQUAL(x6, x3);
  BOOST_CHECK_EQUAL(x8, x3);
  BOOST_CHECK_EQUAL(- x6, x9);
  BOOST_CHECK_EQUAL(x3.negated(), x9);

  amount_t x10(x9.negated());

  BOOST_CHECK_EQUAL(x3, x10);

  BOOST_CHECK(x1.valid());
  BOOST_CHECK(x3.valid());
  BOOST_CHECK(x5.valid());
  BOOST_CHECK(x6.valid());
  BOOST_CHECK(x7.valid());
  BOOST_CHECK(x8.valid());
  BOOST_CHECK(x9.valid());
  BOOST_CHECK(x10.valid());
}

BOOST_AUTO_TEST_CASE(testCommodityNegation)
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

  BOOST_CHECK_EQUAL(amount_t("$-123.45"), - x1);
  BOOST_CHECK_EQUAL(amount_t("$123.45"), - x2);
  BOOST_CHECK_EQUAL(amount_t("$123.45"), - x3);
  BOOST_CHECK_EQUAL(amount_t("DM -123.45"), - x4);
  BOOST_CHECK_EQUAL(amount_t("DM 123.45"), - x5);
  BOOST_CHECK_EQUAL(amount_t("DM 123.45"), - x6);
  BOOST_CHECK_EQUAL(amount_t("-123.45 euro"), - x7);
  BOOST_CHECK_EQUAL(amount_t("123.45 euro"), - x8);
  BOOST_CHECK_EQUAL(amount_t("-123.45€"), - x9);
  BOOST_CHECK_EQUAL(amount_t("123.45€"), - x10);

  BOOST_CHECK_EQUAL(amount_t("$-123.45"), x1.negated());
  BOOST_CHECK_EQUAL(amount_t("$123.45"), x2.negated());
  BOOST_CHECK_EQUAL(amount_t("$123.45"), x3.negated());

  BOOST_CHECK_EQUAL(string("$-123.45"), (- x1).to_string());
  BOOST_CHECK_EQUAL(string("$123.45"), (- x2).to_string());
  BOOST_CHECK_EQUAL(string("$123.45"), (- x3).to_string());
  BOOST_CHECK_EQUAL(string("DM -123.45"), (- x4).to_string());
  BOOST_CHECK_EQUAL(string("DM 123.45"), (- x5).to_string());
  BOOST_CHECK_EQUAL(string("DM 123.45"), (- x6).to_string());
  BOOST_CHECK_EQUAL(string("-123.45 euro"), (- x7).to_string());
  BOOST_CHECK_EQUAL(string("123.45 euro"), (- x8).to_string());
  BOOST_CHECK_EQUAL(string("-123.45€"), (- x9).to_string());
  BOOST_CHECK_EQUAL(string("123.45€"), (- x10).to_string());

  BOOST_CHECK_EQUAL(amount_t("$-123.45"), x1.negated());
  BOOST_CHECK_EQUAL(amount_t("$123.45"), x2.negated());
  BOOST_CHECK_EQUAL(amount_t("$123.45"), x3.negated());

  BOOST_CHECK(x1.valid());
  BOOST_CHECK(x2.valid());
  BOOST_CHECK(x3.valid());
  BOOST_CHECK(x4.valid());
  BOOST_CHECK(x5.valid());
  BOOST_CHECK(x6.valid());
  BOOST_CHECK(x7.valid());
  BOOST_CHECK(x8.valid());
  BOOST_CHECK(x9.valid());
  BOOST_CHECK(x10.valid());
}

BOOST_AUTO_TEST_CASE(testAbs)
{
  amount_t x0;
  amount_t x1(-1234L);
  amount_t x2(1234L);

  BOOST_CHECK_THROW(x0.abs(), amount_error);
  BOOST_CHECK_EQUAL(amount_t(1234L), x1.abs());
  BOOST_CHECK_EQUAL(amount_t(1234L), x2.abs());

  BOOST_CHECK(x0.valid());
  BOOST_CHECK(x1.valid());
  BOOST_CHECK(x2.valid());
}

BOOST_AUTO_TEST_CASE(testCommodityAbs)
{
  amount_t x1("$-1234.56");
  amount_t x2("$1234.56");

  BOOST_CHECK_EQUAL(amount_t("$1234.56"), x1.abs());
  BOOST_CHECK_EQUAL(amount_t("$1234.56"), x2.abs());

  BOOST_CHECK(x1.valid());
  BOOST_CHECK(x2.valid());
}

BOOST_AUTO_TEST_CASE(testFloor)
{
  amount_t x0;
  amount_t x1("123.123");
  amount_t x2("-123.123");

  BOOST_CHECK_THROW(x0.floored(), amount_error);
  BOOST_CHECK_EQUAL(amount_t(123L), x1.floored());
  BOOST_CHECK_EQUAL(amount_t(-124L), x2.floored());

  BOOST_CHECK(x0.valid());
  BOOST_CHECK(x1.valid());
  BOOST_CHECK(x2.valid());
}

BOOST_AUTO_TEST_CASE(testCommodityFloor)
{
  amount_t x1("$1234.56");
  amount_t x2("$-1234.56");

  BOOST_CHECK_EQUAL(amount_t("$1234"), x1.floored());
  BOOST_CHECK_EQUAL(amount_t("$-1235"), x2.floored());

  BOOST_CHECK(x1.valid());
  BOOST_CHECK(x2.valid());
}

BOOST_AUTO_TEST_CASE(testCeiling)
{
  amount_t x0;
  amount_t x1("123.123");
  amount_t x2("-123.123");

  BOOST_CHECK_THROW(x0.ceilinged(), amount_error);
  BOOST_CHECK_EQUAL(amount_t(124L), x1.ceilinged());
  BOOST_CHECK_EQUAL(amount_t(-123L), x2.ceilinged());

  BOOST_CHECK(x0.valid());
  BOOST_CHECK(x1.valid());
  BOOST_CHECK(x2.valid());
}

BOOST_AUTO_TEST_CASE(testCommodityCeiling)
{
  amount_t x1("$1234.56");
  amount_t x2("$-1234.56");

  BOOST_CHECK_EQUAL(amount_t("$1235"), x1.ceilinged());
  BOOST_CHECK_EQUAL(amount_t("$-1234"), x2.ceilinged());

  BOOST_CHECK(x1.valid());
  BOOST_CHECK(x2.valid());
}

BOOST_AUTO_TEST_CASE(testRoundto)
{
  amount_t a1("$ 123.123");
  amount_t a2(a1);
  a2.in_place_roundto(2);
  BOOST_CHECK_EQUAL(amount_t("$ 123.123"), a1);
  // Should it be "$ 123.12"?
  BOOST_CHECK_EQUAL(amount_t("$ 123.120"), a2);
  BOOST_CHECK_EQUAL(amount_t("$ 123.120"), a1.roundto(2));

  // `in_place_roundto` code based on conversion to double
  // had an issue with values close to halves
  // due to 0.49999999 constant.
  BOOST_CHECK_EQUAL(amount_t("1.1499999999").roundto(1), amount_t("1.1"));
  BOOST_CHECK_EQUAL(amount_t("1.1499000").roundto(1), amount_t("1.1"));
  BOOST_CHECK_EQUAL(amount_t("2.2499999999").roundto(1), amount_t("2.2"));
  BOOST_CHECK_EQUAL(amount_t("2.2499000").roundto(1), amount_t("2.2"));
  BOOST_CHECK_EQUAL(amount_t("-2.1500000001").roundto(1), amount_t("-2.2"));
  BOOST_CHECK_EQUAL(amount_t("-2.15001").roundto(1), amount_t("-2.2"));
  BOOST_CHECK_EQUAL(amount_t("-3.2500000001").roundto(1), amount_t("-3.3"));
  BOOST_CHECK_EQUAL(amount_t("-3.25001").roundto(1), amount_t("-3.3"));
}

#ifndef NOT_FOR_PYTHON
#if 0
BOOST_AUTO_TEST_CASE(testReduction)
{
  amount_t x0;
  amount_t x1("60s");
  amount_t x2("600s");
  amount_t x3("6000s");
  amount_t x4("360000s");
  amount_t x5("10m");           // 600s
  amount_t x6("100m");          // 6000s
  amount_t x7("1000m");         // 60000s
  amount_t x8("10000m");        // 600000s
  amount_t x9("10h");           // 36000s
  amount_t x10("100h");         // 360000s
  amount_t x11("1000h");        // 3600000s
  amount_t x12("10000h");       // 36000000s

  BOOST_CHECK_THROW(x0.reduce(), amount_error);
  BOOST_CHECK_THROW(x0.unreduce(), amount_error);
  BOOST_CHECK_EQUAL(x2, x5.reduce());
  BOOST_CHECK_EQUAL(x3, x6.reduce());
  BOOST_CHECK_EQUAL(x10, x4.reduce());
  BOOST_CHECK_EQUAL(string("100.0h"), x4.unreduce().to_string());
}
#endif
#endif // NOT_FOR_PYTHON

BOOST_AUTO_TEST_CASE(testSign)
{
  amount_t x0;
  amount_t x1("0.0000000000000000000000000000000000001");
  amount_t x2("-0.0000000000000000000000000000000000001");
  amount_t x3("1");
  amount_t x4("-1");

  BOOST_CHECK_THROW(x0.sign(), amount_error);
  BOOST_CHECK(x1.sign() > 0);
  BOOST_CHECK(x2.sign() < 0);
  BOOST_CHECK(x3.sign() > 0);
  BOOST_CHECK(x4.sign() < 0);

  BOOST_CHECK(x0.valid());
  BOOST_CHECK(x1.valid());
  BOOST_CHECK(x2.valid());
  BOOST_CHECK(x3.valid());
  BOOST_CHECK(x4.valid());
}

BOOST_AUTO_TEST_CASE(testCommoditySign)
{
  amount_t x1(internalAmount("$0.0000000000000000000000000000000000001"));
  amount_t x2(internalAmount("$-0.0000000000000000000000000000000000001"));
  amount_t x3("$1");
  amount_t x4("$-1");

  BOOST_CHECK(x1.sign() != 0);
  BOOST_CHECK(x2.sign() != 0);
  BOOST_CHECK(x3.sign() > 0);
  BOOST_CHECK(x4.sign() < 0);

  BOOST_CHECK(x1.valid());
  BOOST_CHECK(x2.valid());
  BOOST_CHECK(x3.valid());
  BOOST_CHECK(x4.valid());
}

BOOST_AUTO_TEST_CASE(testTruth)
{
  amount_t x0;
  amount_t x1("1234");
  amount_t x2("1234.56");

#ifndef NOT_FOR_PYTHON
  BOOST_CHECK_THROW(x0.operator bool(), amount_error);
#endif // NOT_FOR_PYTHON

  BOOST_CHECK(x1);
  BOOST_CHECK(x2);

  BOOST_CHECK(x0.valid());
  BOOST_CHECK(x1.valid());
  BOOST_CHECK(x2.valid());
}

BOOST_AUTO_TEST_CASE(testCommodityTruth)
{
  amount_t x1("$1234");
  amount_t x2("$1234.56");

  if (x1)
    BOOST_CHECK(true);

  if (x2)
    BOOST_CHECK(true);

  BOOST_CHECK(x1.valid());
  BOOST_CHECK(x2.valid());
}

BOOST_AUTO_TEST_CASE(testForZero)
{
  amount_t x0;
  amount_t x1("0.000000000000000000001");

  BOOST_CHECK(x1);
  BOOST_CHECK_THROW(x0.is_zero(), amount_error);
  BOOST_CHECK_THROW(x0.is_realzero(), amount_error);
  BOOST_CHECK(! x1.is_zero());
  BOOST_CHECK(! x1.is_realzero());

  BOOST_CHECK(x0.valid());
  BOOST_CHECK(x1.valid());
}

BOOST_AUTO_TEST_CASE(testCommodityForZero)
{
  amount_t x1(internalAmount("$0.000000000000000000001"));

  BOOST_CHECK(x1);               // an internal amount never betrays its precision
  BOOST_CHECK(! x1.is_zero());
  BOOST_CHECK(! x1.is_realzero());

  BOOST_CHECK(x1.valid());
}

BOOST_AUTO_TEST_CASE(testIntegerConversion)
{
  amount_t x0;
  amount_t x1(123456L);
  amount_t x2("12345682348723487324");

  BOOST_CHECK_THROW(x0.to_long(), amount_error);
  BOOST_CHECK_THROW(x0.to_double(), amount_error);
  BOOST_CHECK(! x2.fits_in_long());
  BOOST_CHECK_EQUAL(123456L, x1.to_long());
  BOOST_CHECK_EQUAL(123456.0, x1.to_double());
  BOOST_CHECK_EQUAL(string("123456"), x1.to_string());
  BOOST_CHECK_EQUAL(string("123456"), x1.quantity_string());

  BOOST_CHECK(x1.valid());
}

BOOST_AUTO_TEST_CASE(testFractionalConversion)
{
  amount_t x1("1234.56");
  amount_t x2("1234.5683787634678348734");

  BOOST_CHECK_EQUAL(1235L, x1.to_long());
  BOOST_CHECK_EQUAL(1234.56, x1.to_double());
  BOOST_CHECK_EQUAL(string("1234.56"), x1.to_string());
  BOOST_CHECK_EQUAL(string("1234.56"), x1.quantity_string());

  BOOST_CHECK(x1.valid());
}

BOOST_AUTO_TEST_CASE(testCommodityConversion)
{
  amount_t x1("$1234.56");

  BOOST_CHECK_EQUAL(1235L, x1.to_long());
  BOOST_CHECK_EQUAL(1234.56, x1.to_double());
  BOOST_CHECK_EQUAL(string("$1234.56"), x1.to_string());
  BOOST_CHECK_EQUAL(string("1234.56"), x1.quantity_string());

  BOOST_CHECK(x1.valid());
}

#ifndef NOT_FOR_PYTHON

BOOST_AUTO_TEST_CASE(testPrinting)
{
  amount_t x0;
  amount_t x1("982340823.380238098235098235098235098");

  {
  std::ostringstream bufstr;
  x0.print(bufstr);
  BOOST_CHECK_EQUAL(std::string("<null>"), bufstr.str());
  }

  {
  std::ostringstream bufstr;
  x1.print(bufstr);

  BOOST_CHECK_EQUAL(std::string("982340823.380238098235098235098235098"),
              bufstr.str());
  }

  BOOST_CHECK(x0.valid());
  BOOST_CHECK(x1.valid());
}

BOOST_AUTO_TEST_CASE(testCommodityPrinting)
{
  amount_t x1(internalAmount("$982340823.386238098235098235098235098"));
  amount_t x2("$982340823.38");

  {
  std::ostringstream bufstr;
  x1.print(bufstr);

  BOOST_CHECK_EQUAL(std::string("$982340823.386238098235098235098235098"),
              bufstr.str());
  }

  {
  std::ostringstream bufstr;
  (x1 * x2).print(bufstr);

  BOOST_CHECK_EQUAL(std::string("$964993493285024293.18099172508158508135413499124"),
              bufstr.str());
  }

  {
  std::ostringstream bufstr;
  (x2 * x1).print(bufstr);

  BOOST_CHECK_EQUAL(std::string("$964993493285024293.18"), bufstr.str());
  }

  BOOST_CHECK(x1.valid());
  BOOST_CHECK(x2.valid());
}

#endif // NOT_FOR_PYTHON

BOOST_AUTO_TEST_SUITE_END()
