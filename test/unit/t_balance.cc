#define BOOST_TEST_DYN_LINK
//#define BOOST_TEST_MODULE balance
#include <boost/test/unit_test.hpp>

#include <system.hh>

#include "balance.h"

using namespace ledger;

struct balance_fixture {
  balance_fixture() {
  times_initialize();
  amount_t::initialize();

  // Cause the display precision for dollars to be initialized to 2.
  amount_t x1("$1.00");
  BOOST_CHECK(x1);

  amount_t::stream_fullstrings = true; // make reports from UnitTests accurate
  }

  ~balance_fixture()
  {
  amount_t::stream_fullstrings = false;
  amount_t::shutdown();
  times_shutdown();
  }
};

BOOST_FIXTURE_TEST_SUITE(balance, balance_fixture)

BOOST_AUTO_TEST_CASE(testConstructors)
{
  balance_t b0;
  balance_t b1(1.00);
  balance_t b2(123456UL);
  balance_t b3(12345L);
  balance_t b4(string ("EUR 123"));
  balance_t b5("$ 456");
  balance_t b6;
  balance_t b7(amount_t("$ 1.00"));
  balance_t b8(b7);

  BOOST_CHECK_EQUAL(balance_t(), b0);
  BOOST_CHECK_NE(balance_t("0"), b0);
  BOOST_CHECK_NE(balance_t("0.0"), b0);
  BOOST_CHECK_EQUAL(b2, 123456UL);
  BOOST_CHECK_EQUAL(b3, 12345L);
  BOOST_CHECK_EQUAL(b4, "EUR 123");
  BOOST_CHECK_EQUAL(b5, string("$ 456"));
  BOOST_CHECK_EQUAL(b7, b8);
  BOOST_CHECK_EQUAL(b8, amount_t("$ 1.00"));

  b5 = "euro 2345";
  b6 = string("DM -34532");
  b7 = amount_t("$ 1.00");

  b8 = b5;
  BOOST_CHECK_EQUAL(b5, b8);

  BOOST_CHECK(b0.valid());
  BOOST_CHECK(b1.valid());
  BOOST_CHECK(b2.valid());
  BOOST_CHECK(b3.valid());
  BOOST_CHECK(b4.valid());
  BOOST_CHECK(b5.valid());
  BOOST_CHECK(b6.valid());
  BOOST_CHECK(b7.valid());
  BOOST_CHECK(b8.valid());
}

BOOST_AUTO_TEST_CASE(testAddition)
{
  amount_t a0;
  amount_t a1("$1");
  amount_t a2("2 EUR");
  amount_t a3("0.00 CAD");
  amount_t a4("$2");

  balance_t b0;
  balance_t b1(1.00);
  balance_t b2(2UL);
  balance_t b3(2L);
  balance_t b4;
  balance_t b5;

  b0 += b1;
  b2 += b3;
  b3 += a1;
  b3 += a2;
  b4 += b3;
  b5 += a1;
  b5 += a4;

  BOOST_CHECK_EQUAL(balance_t(1.00), b0);
  BOOST_CHECK_EQUAL(b3 += a3, b4);
  BOOST_CHECK_EQUAL(balance_t(4L), b2);
  BOOST_CHECK_EQUAL(balance_t() += amount_t("$3"), b5);

  BOOST_CHECK_THROW(b3 += a0, balance_error);

  BOOST_CHECK(b0.valid());
  BOOST_CHECK(b1.valid());
  BOOST_CHECK(b2.valid());
  BOOST_CHECK(b3.valid());
  BOOST_CHECK(b4.valid());
  BOOST_CHECK(b5.valid());
}

BOOST_AUTO_TEST_CASE(testSubtraction)
{
  amount_t a0;
  amount_t a1("$1");
  amount_t a2("2 EUR");
  amount_t a3("0.00 CAD");
  amount_t a4("$2");

  balance_t b0;
  balance_t b1(1.00);
  balance_t b2(2UL);
  balance_t b3(2L);
  balance_t b4;
  balance_t b5;

  b0 -= b1;
  b2 -= b3;
  b3 -= a1;
  b3 -= a2;
  b4 = b3;
  b5 -= a1;
  b5 -= a4;

  BOOST_CHECK_EQUAL(balance_t(-1.00), b0);
  BOOST_CHECK_EQUAL(b3 -= a3, b4);
  BOOST_CHECK_EQUAL(balance_t(), b2);
  BOOST_CHECK_EQUAL(b3 -= b2, b3);
  BOOST_CHECK_EQUAL(balance_t() -= amount_t("$3"), b5);

  BOOST_CHECK_THROW(b3 -= a0, balance_error);

  BOOST_CHECK(b0.valid());
  BOOST_CHECK(b1.valid());
  BOOST_CHECK(b2.valid());
  BOOST_CHECK(b3.valid());
  BOOST_CHECK(b4.valid());
  BOOST_CHECK(b5.valid());
}

BOOST_AUTO_TEST_CASE(testEqaulity)
{
  amount_t a0;
  amount_t a1("$1");
  amount_t a2("2 EUR");
  amount_t a3("0.00 CAD");

  balance_t b0;
  balance_t b1(1.00);
  balance_t b2(2UL);
  balance_t b3(2L);
  balance_t b4("EUR 2");
  balance_t b5("$-1");
  balance_t b6("0.00");
  balance_t b7("0.00");


  BOOST_CHECK(b2 == b3);
  BOOST_CHECK(b4 == a2);
  BOOST_CHECK(b1 == "1.00");
  BOOST_CHECK(b5 == amount_t("-$1"));
  BOOST_CHECK(!(b6 == "0"));
  BOOST_CHECK(!(b6 == a3));
  BOOST_CHECK(!(b6 == "0.00"));
  BOOST_CHECK(b6 == b7);

  b4 += b5;
  b5 += a2;

  BOOST_CHECK(b4 == b5);

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-value"
  BOOST_CHECK_THROW(b0 == a0, balance_error);
#pragma GCC diagnostic pop

  BOOST_CHECK(b0.valid());
  BOOST_CHECK(b1.valid());
  BOOST_CHECK(b2.valid());
  BOOST_CHECK(b3.valid());
  BOOST_CHECK(b4.valid());
  BOOST_CHECK(b5.valid());
  BOOST_CHECK(b6.valid());
  BOOST_CHECK(b7.valid());
}

BOOST_AUTO_TEST_CASE(testMultiplication)
{
  amount_t a0;
  amount_t a1("0.00");

  balance_t b0;
  balance_t b1(1.00);
  balance_t b2(2UL);
  balance_t b3("CAD -3");
  balance_t b4("EUR 4.99999");
  balance_t b5("$1");
  balance_t b6;

  BOOST_CHECK_EQUAL(b1 *= 2.00, amount_t(2.00));
  BOOST_CHECK_EQUAL(b2 *= 2L, amount_t(4L));
  BOOST_CHECK_EQUAL(b2 *= 2UL, amount_t(8UL));
  BOOST_CHECK_EQUAL(b3 *= amount_t("-8 CAD"), amount_t("CAD 24"));
  BOOST_CHECK_EQUAL(b0 *= 2UL, b0);
  BOOST_CHECK_EQUAL(b0 *= a1, a1);

  b6 += b3;
  b3 += b4;
  b3 += b5;
  b3 *= 2L;
  b6 *= 2L;
  b4 *= 2L;
  b5 *= 2L;
  b6 += b4;
  b6 += b5;

  BOOST_CHECK_EQUAL(b3, b6);

  BOOST_CHECK_THROW(b1 *= a0 , balance_error);
  BOOST_CHECK_THROW(b4 *= amount_t("1 CAD") , balance_error);
  BOOST_CHECK_THROW(b3 *= amount_t("1 CAD") , balance_error);

  BOOST_CHECK(b0.valid());
  BOOST_CHECK(b1.valid());
  BOOST_CHECK(b2.valid());
  BOOST_CHECK(b3.valid());
  BOOST_CHECK(b4.valid());
  BOOST_CHECK(b5.valid());
  BOOST_CHECK(b6.valid());
}

BOOST_AUTO_TEST_CASE(testDivision)
{
  amount_t a0;
  amount_t a1("0.00");

  balance_t b0;
  balance_t b1(4.00);
  balance_t b2(4UL);
  balance_t b3("CAD -24");
  balance_t b4("EUR 4");
  balance_t b5("$2");
  balance_t b6;

  BOOST_CHECK_EQUAL(b1 /= 2.00, amount_t(2.00));
  BOOST_CHECK_EQUAL(b2 /= 2L, amount_t(2L));
  BOOST_CHECK_EQUAL(b2 /= 2UL, amount_t(1UL));
  BOOST_CHECK_EQUAL(b3 /= amount_t("-3 CAD"), amount_t("CAD 8"));
  BOOST_CHECK_EQUAL(b0 /= 2UL, b0);

  b6 += b3;
  b3 += b4;
  b3 += b5;
  b3 /= 2L;
  b6 /= 2L;
  b4 /= 2L;
  b5 /= 2L;
  b6 += b4;
  b6 += b5;

  BOOST_CHECK_EQUAL(b3, b6);

  BOOST_CHECK_THROW(b1 /= a0 , balance_error);
  BOOST_CHECK_THROW(b1 /= a1 , balance_error);
  BOOST_CHECK_THROW(b4 /= amount_t("1 CAD") , balance_error);
  BOOST_CHECK_THROW(b3 /= amount_t("1 CAD") , balance_error);

  BOOST_CHECK(b0.valid());
  BOOST_CHECK(b1.valid());
  BOOST_CHECK(b2.valid());
  BOOST_CHECK(b3.valid());
  BOOST_CHECK(b4.valid());
  BOOST_CHECK(b5.valid());
  BOOST_CHECK(b6.valid());
}

BOOST_AUTO_TEST_CASE(testNegation)
{
  amount_t a1("0.00");
  amount_t a2("$ 123");
  amount_t a3("EUR 456");

  balance_t b0;
  balance_t b1;
  balance_t b2;
  balance_t b3;

  b1 += a1;
  b1 += a2;
  b1 += a3;
  b2 += -a1;
  b2 += -a2;
  b2 += -a3;
  b3 = -b1;

  BOOST_CHECK_EQUAL(b0.negated(), b0);
  BOOST_CHECK_EQUAL(b2, b3);
  BOOST_CHECK_EQUAL(b2, -b1);
  BOOST_CHECK_EQUAL(b2.negated(), b1);

  b2.in_place_negate();

  BOOST_CHECK_EQUAL(b2, b1);
  BOOST_CHECK_EQUAL(b1, -b3);

  BOOST_CHECK(b0.valid());
  BOOST_CHECK(b1.valid());
  BOOST_CHECK(b2.valid());
  BOOST_CHECK(b3.valid());
}

BOOST_AUTO_TEST_CASE(testAbs)
{
  amount_t a1("0.00");
  amount_t a2("$ 123");
  amount_t a3("EUR 456");

  balance_t b0;
  balance_t b1;
  balance_t b2;

  b1 += a1;
  b1 += a2;
  b1 += a3;
  b2 += -a1;
  b2 += -a2;
  b2 += -a3;

  BOOST_CHECK_EQUAL(b0.abs(), b0);
  BOOST_CHECK_EQUAL(b2.abs(), b1.abs());

  BOOST_CHECK(b0.valid());
  BOOST_CHECK(b1.valid());
  BOOST_CHECK(b2.valid());
}

BOOST_AUTO_TEST_CASE(testCeiling)
{
  amount_t a1("0.00");
  amount_t a2("$ 123.123");
  amount_t a3("EUR 456.56");
  amount_t a4(-a1);
  amount_t a5(-a2);
  amount_t a6(-a3);

  balance_t b0;
  balance_t b1;
  balance_t b2;
  balance_t b3;
  balance_t b4;

  b1 += a1;
  b1 += a2;
  b1 += a3;
  b2 += -a1;
  b2 += -a2;
  b2 += -a3;

  b3 += a1.ceilinged();
  b3 += a2.ceilinged();
  b3 += a3.ceilinged();
  b4 += a4.ceilinged();
  b4 += a5.ceilinged();
  b4 += a6.ceilinged();

  BOOST_CHECK_EQUAL(b0.ceilinged(), b0);
  BOOST_CHECK_EQUAL(b2.ceilinged(), b4);
  BOOST_CHECK_EQUAL(b1.ceilinged(), b3);

  b1.in_place_ceiling();
  BOOST_CHECK_EQUAL(b1, b3);

  BOOST_CHECK(b0.valid());
  BOOST_CHECK(b1.valid());
  BOOST_CHECK(b2.valid());
  BOOST_CHECK(b3.valid());
  BOOST_CHECK(b4.valid());
}

BOOST_AUTO_TEST_CASE(testFloor)
{
  amount_t a1("0.00");
  amount_t a2("$ 123.123");
  amount_t a3("EUR 456.56");
  amount_t a4(-a1);
  amount_t a5(-a2);
  amount_t a6(-a3);

  balance_t b0;
  balance_t b1;
  balance_t b2;
  balance_t b3;
  balance_t b4;

  b1 += a1;
  b1 += a2;
  b1 += a3;
  b2 += -a1;
  b2 += -a2;
  b2 += -a3;

  b3 += a1.floored();
  b3 += a2.floored();
  b3 += a3.floored();
  b4 += a4.floored();
  b4 += a5.floored();
  b4 += a6.floored();

  BOOST_CHECK_EQUAL(b0.floored(), b0);
  BOOST_CHECK_EQUAL(b2.floored(), b4);
  BOOST_CHECK_EQUAL(b1.floored(), b3);

  b1.in_place_floor();
  BOOST_CHECK_EQUAL(b1, b3);

  BOOST_CHECK(b0.valid());
  BOOST_CHECK(b1.valid());
  BOOST_CHECK(b2.valid());
  BOOST_CHECK(b3.valid());
  BOOST_CHECK(b4.valid());
}

BOOST_AUTO_TEST_CASE(testRoundto)
{
  amount_t a1("0.00");
  amount_t a2("$ 123.123");
  amount_t a3("EUR 456.567");
  amount_t a4("0.00");
  amount_t a5("$ 123.12");
  amount_t a6("EUR 456.57");

  balance_t b0;
  balance_t b1;
  balance_t b2;
  balance_t b3;
  balance_t b4;

  b1 += a1;
  b1 += a2;
  b1 += a3;
  b2 += a4;
  b2 += a5;
  b2 += a6;

  a1.in_place_roundto(2);
  a2.in_place_roundto(2);
  a3.in_place_roundto(2);
  a4.in_place_roundto(2);
  a5.in_place_roundto(2);
  a6.in_place_roundto(2);

  b3 += a1;
  b3 += a2;
  b3 += a3;
  b4 += a4;
  b4 += a5;
  b4 += a6;

  BOOST_CHECK_EQUAL(b0.roundto(2), b0);
  BOOST_CHECK_EQUAL(b2.roundto(2), b4);
  BOOST_CHECK_EQUAL(b1.roundto(2), b4);

  b1.in_place_roundto(2);
  BOOST_CHECK_EQUAL(b1, b3);

  BOOST_CHECK(b0.valid());
  BOOST_CHECK(b1.valid());
  BOOST_CHECK(b2.valid());
  BOOST_CHECK(b3.valid());
  BOOST_CHECK(b4.valid());
}

BOOST_AUTO_TEST_CASE(testTruth)
{
  amount_t a1("0.00");
  amount_t a2("$ 123");
  amount_t a3("EUR 456");

  balance_t b0;
  balance_t b1;

  b1 += a1;
  b1 += a2;
  b1 += a3;

  BOOST_CHECK(!b0);
  BOOST_CHECK(b1);

  BOOST_CHECK(b0.valid());
  BOOST_CHECK(b1.valid());
}

BOOST_AUTO_TEST_CASE(testForZero)
{
  amount_t a1("0.00");
  amount_t a2("$ 123");
  amount_t a3("EUR 456");

  balance_t b0;
  balance_t b1;

  b1 += a1;
  b1 += a2;
  b1 += a3;

  BOOST_CHECK(b0.is_empty());
  BOOST_CHECK(b0.is_zero());
  BOOST_CHECK(b0.is_realzero());
  BOOST_CHECK(!b0.is_nonzero());
  BOOST_CHECK(!b1.is_empty());
  BOOST_CHECK(!b1.is_zero());
  BOOST_CHECK(!b1.is_realzero());
  BOOST_CHECK(b1.is_nonzero());

  BOOST_CHECK(b0.valid());
  BOOST_CHECK(b1.valid());
}

BOOST_AUTO_TEST_SUITE_END()
