#define BOOST_TEST_DYN_LINK
#define BOOST_TEST_MODULE math
#include <boost/test/unit_test.hpp>

#include <system.hh>

#include "amount.h"
#include "commodity.h"
#include "annotate.h"

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
  (void)x13.parse(input);
  BOOST_CHECK_EQUAL(x12, x13);
#endif // NOT_FOR_PYTHON

  amount_t x14;
  BOOST_CHECK_THROW((void)x14.parse("DM"), amount_error);

  amount_t x15("$1.000.000,00"); // parsing this switches us to European

  amount_t x16("$2000");
  BOOST_CHECK_EQUAL(string("$2.000,00"), x16.to_string());
  (void)x16.parse("$2000,00");
  BOOST_CHECK_EQUAL(string("$2.000,00"), x16.to_string());

  // Since use of a decimal-comma is an additive quality, we must switch back
  // to decimal-period manually.
  x15.commodity().drop_flags(COMMODITY_STYLE_DECIMAL_COMMA);

  amount_t x17("$1,000,000.00"); // parsing this switches back to American

  amount_t x18("$2000");
  BOOST_CHECK_EQUAL(string("$2,000.00"), x18.to_string());
  (void)x18.parse("$2,000");
  BOOST_CHECK_EQUAL(string("$2,000.00"), x18.to_string());

  BOOST_CHECK_EQUAL(x15, x17);

  amount_t x19("EUR 1000");
  amount_t x20("EUR 1000");

  BOOST_CHECK_EQUAL(string("EUR 1000"), x19.to_string());
  BOOST_CHECK_EQUAL(string("EUR 1000"), x20.to_string());

  (void)x1.parse("$100.0000", PARSE_NO_MIGRATE);
  BOOST_CHECK_EQUAL(amount_t::precision_t(2), x12.commodity().precision());
  BOOST_CHECK_EQUAL(x1.commodity(), x12.commodity());
  BOOST_CHECK_EQUAL(x1, x12);

  (void)x0.parse("$100.0000");
  BOOST_CHECK_EQUAL(amount_t::precision_t(4), x12.commodity().precision());
  BOOST_CHECK_EQUAL(x0.commodity(), x12.commodity());
  BOOST_CHECK_EQUAL(x0, x12);

  (void)x2.parse("$100.00", PARSE_NO_REDUCE);
  BOOST_CHECK_EQUAL(x2, x12);
  (void)x3.parse("$100.00", PARSE_NO_MIGRATE | PARSE_NO_REDUCE);
  BOOST_CHECK_EQUAL(x3, x12);

  (void)x4.parse("$100.00");
  BOOST_CHECK_EQUAL(x4, x12);
  (void)x5.parse("$100.00", PARSE_NO_MIGRATE);
  BOOST_CHECK_EQUAL(x5, x12);
  (void)x6.parse("$100.00", PARSE_NO_REDUCE);
  BOOST_CHECK_EQUAL(x6, x12);
  (void)x7.parse("$100.00", PARSE_NO_MIGRATE | PARSE_NO_REDUCE);
  BOOST_CHECK_EQUAL(x7, x12);

  (void)x8.parse("$100.00");
  BOOST_CHECK_EQUAL(x8, x12);
  (void)x9.parse("$100.00", PARSE_NO_MIGRATE);
  BOOST_CHECK_EQUAL(x9, x12);
  (void)x10.parse("$100.00", PARSE_NO_REDUCE);
  BOOST_CHECK_EQUAL(x10, x12);
  (void)x11.parse("$100.00", PARSE_NO_MIGRATE | PARSE_NO_REDUCE);
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
  const amount_t& x9(x3);
  const amount_t& x10(x6);
  const amount_t& x11(x8);

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
  BOOST_CHECK_THROW((void)x0.is_zero(), amount_error);
  BOOST_CHECK_THROW((void)x0.is_realzero(), amount_error);
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

  BOOST_CHECK_THROW((void)x0.negated(), amount_error);
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

  BOOST_CHECK_THROW((void)x0.abs(), amount_error);
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

  BOOST_CHECK_THROW((void)x0.floored(), amount_error);
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

  BOOST_CHECK_THROW((void)x0.ceilinged(), amount_error);
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
  BOOST_CHECK_THROW((void)x0.is_zero(), amount_error);
  BOOST_CHECK_THROW((void)x0.is_realzero(), amount_error);
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

  BOOST_CHECK_THROW((void)x0.to_long(), amount_error);
  BOOST_CHECK_THROW((void)x0.to_double(), amount_error);
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

BOOST_AUTO_TEST_CASE(testKeepPrecision)
{
  // An amount created via exact() (internalAmount) has keep_precision true
  amount_t x1(internalAmount("$1.234567"));
  BOOST_CHECK(x1.keep_precision());
  BOOST_CHECK_EQUAL(string("$1.234567"), x1.to_string());

  // After setting keep_precision to false, display uses commodity precision
  x1.set_keep_precision(false);
  BOOST_CHECK(!x1.keep_precision());
  BOOST_CHECK_EQUAL(string("$1.23"), x1.to_string());

  // Setting it back to true restores full precision display
  x1.set_keep_precision(true);
  BOOST_CHECK(x1.keep_precision());
  BOOST_CHECK_EQUAL(string("$1.234567"), x1.to_string());

  // A normally parsed commodity amount does not keep precision
  amount_t x2("$1.23");
  BOOST_CHECK(!x2.keep_precision());
  x2.set_keep_precision(true);
  BOOST_CHECK(x2.keep_precision());

  BOOST_CHECK(x1.valid());
  BOOST_CHECK(x2.valid());
}

BOOST_AUTO_TEST_CASE(testUnrounded)
{
  // Start with a normal commodity amount (not exact), keep_precision is false
  amount_t x0("$1.23");
  BOOST_CHECK(!x0.keep_precision());
  amount_t x0u = x0.unrounded();
  BOOST_CHECK(x0u.keep_precision());
  BOOST_CHECK_EQUAL(string("$1.23"), x0u.to_string());

  // With an internal amount that has extra precision
  amount_t x1(internalAmount("$1.23456789"));
  // internalAmount (exact) already sets keep_precision
  BOOST_CHECK(x1.keep_precision());

  // Set keep_precision to false to simulate a rounded amount
  x1.set_keep_precision(false);
  BOOST_CHECK(!x1.keep_precision());
  BOOST_CHECK_EQUAL(string("$1.23"), x1.to_string());

  // unrounded should set keep_precision and show full precision
  amount_t x2 = x1.unrounded();
  BOOST_CHECK(x2.keep_precision());
  BOOST_CHECK_EQUAL(string("$1.23456789"), x2.to_string());

  // The original should remain unchanged
  BOOST_CHECK(!x1.keep_precision());

  BOOST_CHECK(x1.valid());
  BOOST_CHECK(x2.valid());
}

BOOST_AUTO_TEST_CASE(testAnnotation)
{
  amount_t x1("100 AAPL");

  BOOST_CHECK(!x1.has_annotation());

  annotation_t ann;
  ann.price = amount_t("$50.00");
  x1.annotate(ann);

  BOOST_CHECK(x1.has_annotation());
  BOOST_CHECK(x1.annotation().price);
  BOOST_CHECK_EQUAL(*x1.annotation().price, amount_t("$50.00"));

  // Test strip_annotations with default keep_details (all false) strips everything
  amount_t x2 = x1.strip_annotations(keep_details_t());
  BOOST_CHECK(!x2.has_annotation());
  BOOST_CHECK_EQUAL(x1.number(), x2.number());

  // Test strip_annotations keeping price
  amount_t x3 = x1.strip_annotations(keep_details_t(true, false, false));
  BOOST_CHECK(x3.has_annotation());
  BOOST_CHECK(x3.annotation().price);
  BOOST_CHECK_EQUAL(*x3.annotation().price, amount_t("$50.00"));

  BOOST_CHECK(x1.valid());
  BOOST_CHECK(x2.valid());
  BOOST_CHECK(x3.valid());
}

BOOST_AUTO_TEST_CASE(testAnnotationWithDate)
{
  amount_t x1("100 AAPL");

  annotation_t ann;
  ann.price = amount_t("$50.00");
  ann.date = parse_date("2024/01/15");
  ann.tag = string("lot1");
  x1.annotate(ann);

  BOOST_CHECK(x1.has_annotation());
  BOOST_CHECK(x1.annotation().price);
  BOOST_CHECK(x1.annotation().date);
  BOOST_CHECK(x1.annotation().tag);
  BOOST_CHECK_EQUAL(*x1.annotation().price, amount_t("$50.00"));
  BOOST_CHECK_EQUAL(*x1.annotation().tag, string("lot1"));

  // Strip keeping only price
  keep_details_t kd(true, false, false);
  amount_t x2 = x1.strip_annotations(kd);
  BOOST_CHECK(x2.has_annotation());
  BOOST_CHECK(x2.annotation().price);
  BOOST_CHECK(!x2.annotation().date);
  BOOST_CHECK(!x2.annotation().tag);

  // Strip keeping nothing
  amount_t x3 = x1.strip_annotations(keep_details_t());
  BOOST_CHECK(!x3.has_annotation());

  // Strip keeping everything
  keep_details_t kd_all(true, true, true);
  amount_t x4 = x1.strip_annotations(kd_all);
  BOOST_CHECK(x4.has_annotation());
  BOOST_CHECK(x4.annotation().price);
  BOOST_CHECK(x4.annotation().date);
  BOOST_CHECK(x4.annotation().tag);

  BOOST_CHECK(x1.valid());
  BOOST_CHECK(x2.valid());
  BOOST_CHECK(x3.valid());
  BOOST_CHECK(x4.valid());
}

#endif // NOT_FOR_PYTHON

BOOST_AUTO_TEST_CASE(testDisplayPrecision)
{
  amount_t x1("$1.00");

  // display_precision follows commodity precision
  BOOST_CHECK_EQUAL(amount_t::precision_t(2), x1.display_precision());

  amount_t x2("123.456");
  // uncommoditized amount display_precision equals its own precision
  BOOST_CHECK_EQUAL(amount_t::precision_t(3), x2.display_precision());

  BOOST_CHECK(x1.valid());
  BOOST_CHECK(x2.valid());
}

BOOST_AUTO_TEST_CASE(testAmountNumber)
{
  amount_t x1("$123.45");
  amount_t x2 = x1.number();

  BOOST_CHECK_EQUAL(string("123.45"), x2.to_string());
  BOOST_CHECK(!x2.has_commodity());
  BOOST_CHECK(x1.has_commodity());

  // number() of an uncommoditized amount returns itself
  amount_t x3("123.45");
  BOOST_CHECK_EQUAL(x3, x3.number());

  BOOST_CHECK(x1.valid());
  BOOST_CHECK(x2.valid());
  BOOST_CHECK(x3.valid());
}

BOOST_AUTO_TEST_CASE(testWithCommodity)
{
  amount_t x1("123.45");
  amount_t x2("EUR 1.00");

  BOOST_CHECK(!x1.has_commodity());
  amount_t x3 = x1.with_commodity(x2.commodity());
  BOOST_CHECK(x3.has_commodity());
  BOOST_CHECK_EQUAL(x3.commodity(), x2.commodity());

  // The numeric value should be preserved
  BOOST_CHECK_EQUAL(x1.number(), x3.number());

  BOOST_CHECK(x1.valid());
  BOOST_CHECK(x2.valid());
  BOOST_CHECK(x3.valid());
}

BOOST_AUTO_TEST_CASE(testClearCommodity)
{
  amount_t x1("$123.45");
  BOOST_CHECK(x1.has_commodity());

  x1.clear_commodity();
  BOOST_CHECK(!x1.has_commodity());

  // After clearing commodity, the numeric value should remain
  BOOST_CHECK_EQUAL(amount_t("123.45"), x1);

  BOOST_CHECK(x1.valid());
}

// ---------------------------------------------------------------------------
// Cover amount.cc line 526-540: precision/keep_precision/set_keep_precision
// on uninitialized amounts
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testPrecisionUninitialized)
{
  amount_t x1;
  BOOST_CHECK_THROW(x1.precision(), amount_error);
}

BOOST_AUTO_TEST_CASE(testKeepPrecisionUninitialized)
{
  amount_t x1;
  BOOST_CHECK_THROW(x1.keep_precision(), amount_error);
}

BOOST_AUTO_TEST_CASE(testSetKeepPrecisionUninitialized)
{
  amount_t x1;
  BOOST_CHECK_THROW(x1.set_keep_precision(true), amount_error);
}

BOOST_AUTO_TEST_CASE(testDisplayPrecisionUninitialized)
{
  amount_t x1;
  BOOST_CHECK_THROW(x1.display_precision(), amount_error);
}

// ---------------------------------------------------------------------------
// Cover amount.cc line 571: in_place_invert uninitialized
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testInPlaceInvertUninitialized)
{
  amount_t x1;
  BOOST_CHECK_THROW(x1.in_place_invert(), amount_error);
}

// ---------------------------------------------------------------------------
// Cover amount.cc line 581: in_place_round uninitialized
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testInPlaceRoundUninitialized)
{
  amount_t x1;
  BOOST_CHECK_THROW(x1.in_place_round(), amount_error);
}

// ---------------------------------------------------------------------------
// Cover amount.cc line 591: in_place_truncate uninitialized
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testInPlaceTruncateUninitialized)
{
  amount_t x1;
  BOOST_CHECK_THROW(x1.in_place_truncate(), amount_error);
}

// ---------------------------------------------------------------------------
// Cover amount.cc lines 609-625: in_place_floor/ceiling uninitialized
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testInPlaceFloorUninitialized)
{
  amount_t x1;
  BOOST_CHECK_THROW(x1.in_place_floor(), amount_error);
}

BOOST_AUTO_TEST_CASE(testInPlaceCeilingUninitialized)
{
  amount_t x1;
  BOOST_CHECK_THROW(x1.in_place_ceiling(), amount_error);
}

// ---------------------------------------------------------------------------
// Cover amount.cc line 694: in_place_unround uninitialized
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testInPlaceUnroundUninitialized)
{
  amount_t x1;
  BOOST_CHECK_THROW(x1.in_place_unround(), amount_error);
}

// ---------------------------------------------------------------------------
// Cover amount.cc line 707: in_place_reduce uninitialized
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testInPlaceReduceUninitialized)
{
  amount_t x1;
  BOOST_CHECK_THROW(x1.in_place_reduce(), amount_error);
}

// ---------------------------------------------------------------------------
// Cover amount.cc line 717: in_place_unreduce uninitialized
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testInPlaceUnreduceUninitialized)
{
  amount_t x1;
  BOOST_CHECK_THROW(x1.in_place_unreduce(), amount_error);
}

// ---------------------------------------------------------------------------
// Cover amount.cc line 894: annotate uninitialized
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testAnnotateUninitialized)
{
  amount_t x1;
  annotation_t ann;
  BOOST_CHECK_THROW(x1.annotate(ann), amount_error);
}

// ---------------------------------------------------------------------------
// Cover amount.cc line 911/918: has_annotation/annotation uninitialized
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testHasAnnotationUninitialized)
{
  amount_t x1;
  BOOST_CHECK_THROW(x1.has_annotation(), amount_error);
}

BOOST_AUTO_TEST_CASE(testAnnotationUninitialized)
{
  amount_t x1;
  BOOST_CHECK_THROW(x1.annotation(), amount_error);
}

// ---------------------------------------------------------------------------
// Cover amount.cc line 927-931: annotation on unannotated amount
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testAnnotationUnannotated)
{
  amount_t x1("$100");
  BOOST_CHECK(!x1.has_annotation());
  BOOST_CHECK_THROW(x1.annotation(), amount_error);
}

// ---------------------------------------------------------------------------
// Cover amount.cc line 939: strip_annotations uninitialized
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testStripAnnotationsUninitialized)
{
  amount_t x1;
  keep_details_t kd;
  BOOST_CHECK_THROW(x1.strip_annotations(kd), amount_error);
}

// ---------------------------------------------------------------------------
// Cover amount.cc line 565: in_place_negate uninitialized
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testInPlaceNegateUninitialized)
{
  amount_t x1;
  BOOST_CHECK_THROW(x1.in_place_negate(), amount_error);
}

// ---------------------------------------------------------------------------
// Cover amount.cc line 786/820: sign and is_zero on uninitialized
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testSignUninitialized)
{
  amount_t x1;
  BOOST_CHECK_THROW((void)x1.sign(), amount_error);
}

BOOST_AUTO_TEST_CASE(testIsZeroUninitialized)
{
  amount_t x1;
  BOOST_CHECK_THROW((void)x1.is_zero(), amount_error);
}

// ---------------------------------------------------------------------------
// Cover amount.cc line 992: to_long on uninitialized
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testToLongUninitialized)
{
  amount_t x1;
  BOOST_CHECK_THROW(x1.to_long(), amount_error);
}

// ---------------------------------------------------------------------------
// Cover amount.cc line 653: in_place_roundto with places
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testInPlaceRoundto)
{
  amount_t x1("123.456789");
  x1.in_place_roundto(2);
  BOOST_CHECK_EQUAL(string("123.46"), x1.to_string());
}

// ---------------------------------------------------------------------------
// Cover amount.cc lines 1323-1333: valid() edge cases
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testValidNull)
{
  amount_t x1;
  BOOST_CHECK(x1.valid()); // null amount is valid

  amount_t x2("$100");
  BOOST_CHECK(x2.valid());
}

// -----------------------------------------------------------------------
// Coverage for amount.cc lines 84-89: bigint_t::valid() edge cases
// Already implicitly tested through amount_t operations, but we ensure
// the constructor paths are covered
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testAmountDoubleConstructor)
{
  // Cover amount.cc line 339-343: amount_t(const double)
  amount_t x1(3.14);
  BOOST_CHECK(!x1.is_null());
  // Double constructor sets precision to extend_by_digits
  BOOST_CHECK(x1.valid());
}

// -----------------------------------------------------------------------
// Coverage for amount.cc lines 346-350: amount_t(const unsigned long)
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testAmountUnsignedLongConstructor)
{
  amount_t x1(static_cast<unsigned long>(42));
  BOOST_CHECK(!x1.is_null());
  BOOST_CHECK(x1.valid());
}

// -----------------------------------------------------------------------
// Coverage for amount.cc lines 358-366: operator= assignment
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testAmountAssignment)
{
  amount_t x1("$100.00");
  amount_t x2;

  // Assign initialized to uninitialized
  x2 = x1;
  BOOST_CHECK(!x2.is_null());
  BOOST_CHECK_EQUAL(x1, x2);

  // Self-assignment
  x1 = x1;
  BOOST_CHECK(!x1.is_null());

  // Assign uninitialized to initialized
  amount_t x3;
  x2 = x3;
  BOOST_CHECK(x2.is_null());
}

// -----------------------------------------------------------------------
// Coverage for amount.cc lines 288-289: _copy with BIGINT_BULK_ALLOC
// Already tested implicitly but let's ensure _dup path is hit
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testAmountDup)
{
  amount_t x1("$100.00");
  amount_t x2 = x1;  // share the bigint

  // Modify x1, forcing a _dup
  x1 += amount_t("$1.00");
  BOOST_CHECK(x1 != x2);
}

// -----------------------------------------------------------------------
// Coverage for amount.cc lines 318, 329: _clear and _release paths
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testAmountClear)
{
  amount_t x1("$100.00");
  x1 = amount_t();  // Clear by assigning null
  BOOST_CHECK(x1.is_null());
}

// -----------------------------------------------------------------------
// Coverage for amount.cc lines 202, 211, 218, 226-229:
// stream_out_mpq precision and zeros handling
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testAmountPrecisionDisplay)
{
  // Amount with trailing zeros that should be trimmed
  amount_t x1("$100.10");
  std::ostringstream out;
  x1.print(out);
  BOOST_CHECK(!out.str().empty());

  // Large precision
  amount_t x2("$0.123456789");
  std::ostringstream out2;
  x2.print(out2);
  BOOST_CHECK(!out2.str().empty());
}

// -----------------------------------------------------------------------
// Coverage for amount.cc: print with commodity
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testAmountPrintWithCommodity)
{
  // Use a commodity that already exists in the default pool
  amount_t x1("$10000.00");

  std::ostringstream out;
  x1.print(out);
  std::string result = out.str();
  BOOST_CHECK(!result.empty());
  BOOST_CHECK(result.find("10") != std::string::npos);
}

// -----------------------------------------------------------------------
// Coverage for amount.cc line 136: amount constructor precision
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testAmountLongConstructor)
{
  amount_t x1(static_cast<long>(999));
  BOOST_CHECK(!x1.is_null());
  BOOST_CHECK(x1.valid());
  BOOST_CHECK_EQUAL(x1.to_long(), 999L);
}

// -----------------------------------------------------------------------
// Coverage W7: amount_t constructor from double (lines 84-85, 339-344)
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testAmountDoubleConstructorW7)
{
  // Lines 339-344: amount_t(const double val)
  amount_t x1(3.14159);
  BOOST_CHECK(!x1.is_null());
  BOOST_CHECK(x1.valid());
  // Double amounts have approximate precision
  BOOST_CHECK(x1.to_double() > 3.14);
  BOOST_CHECK(x1.to_double() < 3.15);

  amount_t x2(0.0);
  BOOST_CHECK(x2.is_zero());

  amount_t x3(-99.99);
  BOOST_CHECK(x3.sign() < 0);
}

// -----------------------------------------------------------------------
// Coverage W7: amount_t constructor from unsigned long (lines 346-350)
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testAmountUnsignedLongConstructorW7)
{
  // Lines 346-350: amount_t(const unsigned long val)
  amount_t x1(static_cast<unsigned long>(42));
  BOOST_CHECK(!x1.is_null());
  BOOST_CHECK(x1.valid());
  BOOST_CHECK_EQUAL(x1.to_long(), 42L);

  amount_t x2(static_cast<unsigned long>(0));
  BOOST_CHECK(x2.is_zero());

  amount_t x3(static_cast<unsigned long>(999999));
  BOOST_CHECK_EQUAL(x3.to_long(), 999999L);
}

// -----------------------------------------------------------------------
// Coverage W7: amount_t assignment operator (lines 358-366)
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testAmountAssignmentW7)
{
  // Test self-assignment (line 359: if (this != &amt))
  amount_t x1("$10.00");
  x1 = x1;  // self-assignment should be safe
  BOOST_CHECK_EQUAL(x1.to_string(), "$10.00");

  // Test assignment from null amount (line 363: _clear())
  amount_t x2("$20.00");
  amount_t x3;  // null amount
  x2 = x3;
  BOOST_CHECK(x2.is_null());

  // Test assignment of value to null
  amount_t x4;
  amount_t x5("$30.00");
  x4 = x5;
  BOOST_CHECK_EQUAL(x4.to_string(), "$30.00");
}

// -----------------------------------------------------------------------
// Coverage W7: amount_t comparison operators (lines 288-329, 388-397)
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testAmountComparisonEdgesW7)
{
  // Test operator== with one null operand (line 389-390)
  amount_t x1("$10.00");
  amount_t x2;
  BOOST_CHECK(!(x1 == x2));
  BOOST_CHECK(!(x2 == x1));

  // Test operator== with both null (line 391-392)
  amount_t x3;
  BOOST_CHECK(x2 == x3);

  // Test operator== with different commodities (line 393-394)
  amount_t x4("10 AAPL");
  amount_t x5("10 MSFT");
  BOOST_CHECK(!(x4 == x5));
}

// -----------------------------------------------------------------------
// Coverage W7: in_place_truncate with places < 0 (lines 608-610, 623-625)
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testAmountTruncateEdgesW7)
{
  // Test truncation with no commodity (display_precision returns 0)
  amount_t x1("123.456");
  x1.set_keep_precision(true);
  x1.in_place_truncate();
  // After truncation the amount should be truncated to display precision
  BOOST_CHECK(x1.valid());
}

// -----------------------------------------------------------------------
// Coverage W7: in_place_roundto with places == 0 (line 653)
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testAmountRoundtoEdgesW7)
{
  // Test roundto with 0 places
  amount_t x1("123.456");
  x1.in_place_roundto(0);
  BOOST_CHECK_EQUAL(x1.to_long(), 123L);

  // Test roundto with negative places
  amount_t x2("1567.89");
  x2.in_place_roundto(-2);
  BOOST_CHECK(x2.valid());

  // Test roundto rounding up
  amount_t x3("5.5");
  x3.in_place_roundto(0);
  BOOST_CHECK_EQUAL(x3.to_long(), 6L);

  // Test roundto rounding down
  amount_t x4("5.4");
  x4.in_place_roundto(0);
  BOOST_CHECK_EQUAL(x4.to_long(), 5L);
}

// -----------------------------------------------------------------------
// Coverage W7: price() returning nullopt (line 820)
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testAmountPriceNulloptW7)
{
  // Line 820: price() returns nullopt when no annotation
  amount_t x1("$10.00");
  std::optional<amount_t> p = x1.price();
  BOOST_CHECK(!p);

  // Amount without commodity also returns nullopt
  amount_t x2("10.00");
  std::optional<amount_t> p2 = x2.price();
  BOOST_CHECK(!p2);
}

// -----------------------------------------------------------------------
// Coverage W7: parse with PARSE_NO_ANNOT (lines 1034, 1047)
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testAmountParseNoAnnotW7)
{
  // Lines 1034, 1047: parse with PARSE_NO_ANNOT flag
  amount_t x1;
  (void)x1.parse("$10.00 {$11.00}", PARSE_NO_ANNOT);
  BOOST_CHECK_EQUAL(x1.to_string(), "$10.00");
  // With PARSE_NO_ANNOT the annotation is not parsed
  BOOST_CHECK(!x1.has_annotation());
}

// -----------------------------------------------------------------------
// Coverage W7: amount_t::valid() edge cases (lines 1322-1334)
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testAmountValidW7)
{
  // Line 1322-1324: amount with valid quantity
  amount_t x1("$10.00");
  BOOST_CHECK(x1.valid());

  // Line 1331-1333: null amount is valid
  amount_t x2;
  BOOST_CHECK(x2.valid());
}

// -----------------------------------------------------------------------
// Coverage W7: print with AMOUNT_PRINT_ELIDE_COMMODITY_QUOTES (line 1296)
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testAmountPrintElideCommodityQuotesW7)
{
  // Test printing with the elide_commodity_quotes flag
  amount_t x1("1.00 \"MY STOCK\"");
  std::ostringstream out;
  x1.print(out, AMOUNT_PRINT_ELIDE_COMMODITY_QUOTES);
  string result = out.str();
  BOOST_CHECK(!result.empty());
}

// -----------------------------------------------------------------------
// Coverage W7: _copy with BIGINT_BULK_ALLOC (lines 287-289)
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testAmountCopyBulkAllocW7)
{
  // Create amounts and exercise the copy path
  amount_t x1("$10.00");
  const amount_t& x2(x1);  // copy constructor
  BOOST_CHECK_EQUAL(x1, x2);

  // Assign over an existing amount
  amount_t x3("$20.00");
  x3 = x1;
  BOOST_CHECK_EQUAL(x3, x1);
}

// -----------------------------------------------------------------------
// Coverage W7: parse negative amount with commodity prefix (line 1015-1018)
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testAmountParseNegativePrefixW7)
{
  // Line 1015-1018: negative sign handling
  amount_t x1;
  (void)x1.parse("-$10.00");
  BOOST_CHECK(x1.sign() < 0);
  BOOST_CHECK_EQUAL(x1.to_string(), "$-10.00");

  amount_t x2;
  (void)x2.parse("-10.00 EUR");
  BOOST_CHECK(x2.sign() < 0);
}

// -----------------------------------------------------------------------
// Coverage W7: in_place_multiply/divide edge cases (lines 472-478)
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testAmountMultiplyDivideEdgesW7)
{
  // Test multiply where commodity is taken from second operand
  amount_t x1(10L);  // no commodity
  amount_t x2("$5.00");
  x1 *= x2;
  BOOST_CHECK(x1.has_commodity());
  BOOST_CHECK_EQUAL(x1.commodity().symbol(), "$");

  // Test divide where commodity is taken from second operand
  amount_t x3(100L);
  amount_t x4("$5.00");
  x3 /= x4;
  BOOST_CHECK(x3.has_commodity());
}

// -----------------------------------------------------------------------
// Coverage W7: parse with apostrophe separators (line 1191)
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testAmountParseApostropheSepW7)
{
  amount_t x1;
  (void)x1.parse("CHF 1'234.56");
  BOOST_CHECK(!x1.is_null());
  BOOST_CHECK(x1.valid());
}

// -----------------------------------------------------------------------
// Coverage W7: parse decimal comma edge cases (lines 1124-1175)
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testAmountParseDecimalCommaEdgesW7)
{
  // Line 1126: incorrect use of thousand-mark period
  // Create a commodity with decimal comma style
  amount_t setup("EUR 1.234,56");
  // Now parsing with EUR should use decimal comma style
  amount_t x1("EUR 5,00");
  BOOST_CHECK(x1.valid());

  // Reset style for other tests
  setup.commodity().drop_flags(COMMODITY_STYLE_DECIMAL_COMMA);
}

// -----------------------------------------------------------------------
// Coverage W8: amount_t(double) constructor (lines 84-85, 88-89 not reachable
// directly but line 339-343: double ctor)
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testDoubleConstructorW8)
{
  // Lines 339-343: amount_t(const double val)
  amount_t x1(3.14);
  BOOST_CHECK(!x1.is_null());
  BOOST_CHECK(x1.valid());
  BOOST_CHECK_EQUAL(x1.sign(), 1);

  amount_t x2(-2.718);
  BOOST_CHECK(!x2.is_null());
  BOOST_CHECK(x2.valid());
  BOOST_CHECK_EQUAL(x2.sign(), -1);

  amount_t x3(0.0);
  BOOST_CHECK(!x3.is_null());
  BOOST_CHECK(x3.valid());
  BOOST_CHECK_EQUAL(x3.sign(), 0);
}

// -----------------------------------------------------------------------
// Coverage W8: amount_t(unsigned long) constructor (lines 346-349)
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testUnsignedLongConstructorW8)
{
  // Lines 346-349: amount_t(const unsigned long val)
  amount_t x1(42UL);
  BOOST_CHECK(!x1.is_null());
  BOOST_CHECK(x1.valid());
  BOOST_CHECK_EQUAL(x1, amount_t(42L));

  amount_t x2(0UL);
  BOOST_CHECK(!x2.is_null());
  BOOST_CHECK(x2.valid());
  BOOST_CHECK_EQUAL(x2.sign(), 0);
}

// -----------------------------------------------------------------------
// Coverage W8: copy constructor edge cases (lines 117-136: _copy with
// BIGINT_BULK_ALLOC flag, line 288-289)
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testCopyEdgeCasesW8)
{
  // Test copy assignment from initialized to uninitialized
  amount_t x1("$100.00");
  amount_t x2;
  x2 = x1;
  BOOST_CHECK_EQUAL(x1, x2);

  // Test self-assignment
  x1 = x1;
  BOOST_CHECK_EQUAL(x1, x2);

  // Test assignment from uninitialized to initialized (line 362-363: _clear)
  amount_t x3("$200.00");
  amount_t x4;
  x3 = x4;
  BOOST_CHECK(x3.is_null());
}

// -----------------------------------------------------------------------
// Coverage W8: assignment operators (lines 202-257)
// These include assignment from double/long, and _clear path
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testAssignmentFromDoubleW8)
{
  // Assignment from double
  amount_t x1;
  x1 = 3.14;
  BOOST_CHECK(!x1.is_null());
  BOOST_CHECK(x1.valid());

  // Assignment from double to already-set amount
  amount_t x2(100L);
  x2 = 2.718;
  BOOST_CHECK(x2.valid());

  // Assignment from unsigned long
  amount_t x3;
  x3 = 42UL;
  BOOST_CHECK_EQUAL(x3, amount_t(42L));
}

// -----------------------------------------------------------------------
// Coverage W8: Comparison operators with double (lines 288-289, 318, 329)
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testComparisonWithDoubleW8)
{
  amount_t x1(100L);

  // operator< with double
  BOOST_CHECK(x1 < 200.0);
  BOOST_CHECK(!(x1 < 50.0));

  // operator> with double
  BOOST_CHECK(x1 > 50.0);
  BOOST_CHECK(!(x1 > 200.0));

  // operator== with double
  BOOST_CHECK(x1 == 100.0);
  BOOST_CHECK(!(x1 == 99.0));
}

// -----------------------------------------------------------------------
// Coverage W8: in_place_truncate negative places (lines 609-610, 624-625)
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testTruncateEdgeCasesW8)
{
  // in_place_truncate with places < 0 (line 608-610 and 623-625)
  amount_t x1("123.456");
  x1.set_keep_precision(true);
  amount_t x2(x1);
  x2.in_place_truncate();
  BOOST_CHECK(x2.valid());

  // Test truncating a commodity amount where display_precision is 0
  amount_t x3("$123");
  x3.in_place_truncate();
  BOOST_CHECK(x3.valid());
}

// -----------------------------------------------------------------------
// Coverage W8: in_place_roundto with 0 places (line 653)
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testRoundtoZeroPlacesW8)
{
  // Line 653: throw for uninitialized
  amount_t x0;
  BOOST_CHECK_THROW(x0.in_place_roundto(2), amount_error);

  // Roundto with 0 places
  amount_t x1("123.456");
  x1.in_place_roundto(0);
  BOOST_CHECK_EQUAL(x1, amount_t(123L));

  // Roundto with negative places
  amount_t x2("12345.0");
  x2.in_place_roundto(-2);
  BOOST_CHECK(x2.valid());
}

// -----------------------------------------------------------------------
// Coverage W8: annotate on commodity (line 911)
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testAnnotateW8)
{
  // Line 911: annotate code path
  amount_t x1("10 AAPL");
  annotation_t ann(amount_t("$50.00"));
  x1.annotate(ann);
  BOOST_CHECK(x1.has_annotation());
  BOOST_CHECK(x1.valid());

  // Annotating an amount without commodity is a no-op (line 896)
  amount_t x2(100L);
  x2.annotate(ann);
  BOOST_CHECK(!x2.has_annotation());
}

// -----------------------------------------------------------------------
// Coverage W8: parse() with PARSE_NO_ANNOT flag (lines 1237-1240)
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testParseNoAnnotW8)
{
  // Lines 1034-1036 (PARSE_NO_ANNOT flag) - parse with annotation details
  amount_t x1;
  (void)x1.parse("10 AAPL {$50.00}", PARSE_NO_ANNOT);
  BOOST_CHECK(x1.valid());
  // With NO_ANNOT, the annotation should not be parsed
  BOOST_CHECK(!x1.has_annotation());

  // Parse prefix commodity with PARSE_NO_ANNOT
  amount_t x2;
  (void)x2.parse("$100.00 {$50.00}", PARSE_NO_ANNOT);
  BOOST_CHECK(x2.valid());
}

// -----------------------------------------------------------------------
// Coverage W8: parse() negative amount with prefix commodity (lines 1126,
// 1138, 1152, 1156, 1169, 1191)
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testParseNegativePrefixW8)
{
  // Line 1015-1018: negative prefix parsing
  amount_t x1("- $100.00");
  BOOST_CHECK_EQUAL(x1.sign(), -1);
  BOOST_CHECK(x1.valid());
}

// -----------------------------------------------------------------------
// Coverage W8: parse() with comma-based amounts triggering error paths
// (lines 1152, 1156)
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testParseCommaErrorsW8)
{
  // Line 1152: too many commas -- two commas confirm thousands, then
  // a third period after is blocked
  BOOST_CHECK_THROW(amount_t("1,234,567,89.00"), amount_error);

  // Line 1156: incorrect use of decimal comma
  // Create a decimal comma commodity first
  amount_t setup("SEK 1,00");
  // Now try period after comma in decimal comma style => line 1156
  BOOST_CHECK_THROW(amount_t("SEK 1.234,567,89"), amount_error);
  setup.commodity().drop_flags(COMMODITY_STYLE_DECIMAL_COMMA);
}

// -----------------------------------------------------------------------
// Coverage W8: parse() with thousand-mark period error (line 1126)
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testParseThousandPeriodErrorW8)
{
  // Line 1126: incorrect use of thousand-mark period
  // Create a decimal comma commodity
  amount_t setup2("NOK 1.234,56");
  // Try a period that doesn't sit on a 3-digit boundary
  BOOST_CHECK_THROW(amount_t("NOK 1.23,45"), amount_error);
  setup2.commodity().drop_flags(COMMODITY_STYLE_DECIMAL_COMMA);
}

// -----------------------------------------------------------------------
// Coverage W8: parse() with thousand-mark comma error (line 1169)
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testParseThousandCommaErrorW8)
{
  // Line 1169: incorrect use of thousand-mark comma
  // This happens when we have a previous comma or period and then
  // an incorrectly placed comma appears
  BOOST_CHECK_THROW(amount_t("1.234,56,78"), amount_error);
}

// -----------------------------------------------------------------------
// Coverage W8: print() with AMOUNT_PRINT_ELIDE_COMMODITY (lines 1323-1333)
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testPrintElideCommodityW8)
{
  amount_t x1("$123.45");
  {
    std::ostringstream out;
    x1.print(out, AMOUNT_PRINT_ELIDE_COMMODITY_QUOTES);
    BOOST_CHECK(!out.str().empty());
  }

  // Test printing a null amount
  amount_t x0;
  {
    std::ostringstream out;
    x0.print(out);
    BOOST_CHECK_EQUAL(out.str(), "<null>");
  }
}

// -----------------------------------------------------------------------
// Coverage W8: value() with price chain (line 786)
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testValuePriceChainW8)
{
  // Exercise the value() method (line 786 area)
  amount_t x1("100 AAPL2");
  commodity_t& aapl(x1.commodity());

  datetime_t now = parse_datetime("2020/01/01 00:00:00");
  aapl.add_price(now, amount_t("$10.00"));

  std::optional<amount_t> val = x1.value(now);
  BOOST_CHECK(val);
  BOOST_CHECK_EQUAL(string("$1000.00"), val->to_string());
}

// -----------------------------------------------------------------------
// Coverage W8: parse with soft fail (line 992 - unget path)
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testParseSoftFailW8)
{
  amount_t x1;
  // PARSE_SOFT_FAIL should return false instead of throwing
  std::istringstream in("notanumber");
  bool result = x1.parse(in, PARSE_SOFT_FAIL);
  BOOST_CHECK(!result);
}

// -----------------------------------------------------------------------
// Coverage W11: amount.cc lines 84-85, 88-89 (bigint_t::valid)
// These are only reachable through internal corruption, but we test
// the valid() method indirectly through amount_t::valid()
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testAmountValidW11)
{
  amount_t x1("$10.00");
  BOOST_CHECK(x1.valid());

  amount_t x2(100L);
  BOOST_CHECK(x2.valid());

  amount_t x3(3.14);
  BOOST_CHECK(x3.valid());

  amount_t x4;
  BOOST_CHECK(x4.valid());
}

// -----------------------------------------------------------------------
// Coverage W11: amount.cc lines 339-343 (amount from double)
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testAmountFromDoubleW11)
{
  amount_t x1(1.5);
  BOOST_CHECK(x1);
  BOOST_CHECK(!x1.is_zero());

  amount_t x2(0.0);
  BOOST_CHECK(x2.is_zero());

  amount_t x3(-2.75);
  BOOST_CHECK(x3.sign() < 0);

  amount_t x4(100.0);
  BOOST_CHECK(x4 > amount_t(99L));
}

// -----------------------------------------------------------------------
// Coverage W11: amount.cc lines 226-229 (exception in stream_out_mpq)
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testAmountDivisionW11)
{
  amount_t x1("$100.00");
  amount_t x2("$3.00");
  amount_t result = x1 / x2;
  BOOST_CHECK(!result.is_zero());

  // Division of plain numbers
  amount_t x3(100L);
  amount_t x4(7L);
  amount_t result2 = x3 / x4;
  BOOST_CHECK(!result2.is_zero());

  // Division by zero
  amount_t x5("$100.00");
  amount_t x6("$0.00");
  BOOST_CHECK_THROW(x5 / x6, amount_error);
}

// -----------------------------------------------------------------------
// Coverage W11: amount.cc lines 288-289 (copy with BIGINT_BULK_ALLOC)
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testAmountCopyW11)
{
  amount_t x1("$100.00");
  const amount_t& x2(x1);
  BOOST_CHECK_EQUAL(x1, x2);

  amount_t x3;
  x3 = x1;
  BOOST_CHECK_EQUAL(x1, x3);

  // Self-assignment
  amount_t x4("$50.00");
  x4 = x4;
  BOOST_CHECK_EQUAL(x4, amount_t("$50.00"));
}

// -----------------------------------------------------------------------
// Coverage W11: amount.cc lines 318, 329 (clear and release paths)
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testAmountClearW11)
{
  amount_t x1("$100.00");
  BOOST_CHECK(x1.valid());
  x1 = amount_t();  // assign empty amount
  BOOST_CHECK(x1.valid());
}

// -----------------------------------------------------------------------
// Coverage W11: amount.cc lines 608-610, 624-625 (truncate with
// negative/zero display_precision)
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testAmountTruncateW11)
{
  amount_t x1("$123.456789");
  x1.in_place_truncate();
  BOOST_CHECK(!x1.is_zero());

  // Truncate a plain number
  amount_t x2("123.456789");
  x2.set_keep_precision(true);
  x2.in_place_truncate();
  BOOST_CHECK(!x2.is_zero());
}

// -----------------------------------------------------------------------
// Coverage W11: amount.cc line 786 (amount print path)
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testAmountPrintCommodityW11)
{
  amount_t x1("$100.00");
  std::ostringstream out;
  x1.print(out);
  BOOST_CHECK_EQUAL(out.str(), "$100.00");

  // Print with suffixed commodity
  amount_t x2("100.00 EUR");
  std::ostringstream out2;
  x2.print(out2);
  BOOST_CHECK(out2.str().find("EUR") != string::npos);
}

// -----------------------------------------------------------------------
// Coverage W11: amount.cc line 911 (annotate assert false path)
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testAmountAnnotateW11)
{
  amount_t x1("10 AAPL3");
  annotation_t details;
  details.price = amount_t("$100.00");
  x1.annotate(details);
  BOOST_CHECK(x1.has_annotation());
  BOOST_CHECK_EQUAL(x1.annotation().price->to_string(), "$100.00");
}

// -----------------------------------------------------------------------
// Coverage W11: amount.cc line 992 (stream parse path)
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testAmountStreamParseW11)
{
  std::istringstream in("$100.00 ");
  amount_t x1;
  (void)x1.parse(in);
  BOOST_CHECK_EQUAL(x1.to_string(), "$100.00");

  // Parse with negative sign
  std::istringstream in2("-$50.00");
  amount_t x2;
  (void)x2.parse(in2);
  BOOST_CHECK(x2.sign() < 0);
}

// -----------------------------------------------------------------------
// Coverage W11: amount.cc lines 1323-1324, 1328-1329, 1332-1333
// (valid() error checks - these are internal validity checks)
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testAmountValidChecksW11)
{
  // Valid with quantity
  amount_t x1("$100.00");
  BOOST_CHECK(x1.valid());

  // Valid with no quantity (null amount)
  amount_t x2;
  BOOST_CHECK(x2.valid());

  // Both valid checks pass
  amount_t x3(42L);
  BOOST_CHECK(x3.valid());
}

// -----------------------------------------------------------------------
// Coverage W11: amount.cc lines 105-108, 117, 126, 136
// (stream_out_mpq precision paths)
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testAmountPrecisionPathsW11)
{
  // High precision amount
  amount_t x1("$100.123456789");
  std::ostringstream out;
  x1.print(out);
  BOOST_CHECK(!out.str().empty());

  // Amount with commodity precision
  amount_t x2("1000.00 EUR");
  std::ostringstream out2;
  x2.print(out2);
  BOOST_CHECK(!out2.str().empty());
}

// -----------------------------------------------------------------------
// Coverage W11: amount.cc line 202, 211, 218 (arithmetic edge cases)
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testAmountArithmeticEdgeCasesW11)
{
  // Adding amounts with same commodity
  amount_t x1("$10.00");
  amount_t x2("$20.00");
  x1 += x2;
  BOOST_CHECK_EQUAL(x1, amount_t("$30.00"));

  // Subtracting amounts
  amount_t x3("$50.00");
  amount_t x4("$20.00");
  x3 -= x4;
  BOOST_CHECK_EQUAL(x3, amount_t("$30.00"));

  // Multiplying amounts
  amount_t x5("$10.00");
  amount_t x6(3L);
  x5 *= x6;
  BOOST_CHECK_EQUAL(x5, amount_t("$30.00"));

  // Dividing amounts
  amount_t x7("$30.00");
  amount_t x8("$10.00");
  amount_t result = x7 / x8;
  BOOST_CHECK(!result.is_zero());
}

// -----------------------------------------------------------------------
// Coverage W11: amount.cc comparison paths (line 288-289)
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testAmountCompareW11)
{
  // Equality with same commodity
  amount_t x1("$10.00");
  amount_t x2("$10.00");
  BOOST_CHECK(x1 == x2);

  // Inequality
  amount_t x3("$10.00");
  amount_t x4("$20.00");
  BOOST_CHECK(x3 < x4);

  // Compare null amounts - both null
  amount_t x5;
  amount_t x6;
  BOOST_CHECK_THROW(x5.compare(x6), amount_error);

  // Compare initialized to null
  amount_t x7("$10.00");
  amount_t x8;
  BOOST_CHECK_THROW(x7.compare(x8), amount_error);
}

BOOST_AUTO_TEST_SUITE_END()
