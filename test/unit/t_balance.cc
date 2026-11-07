#define BOOST_TEST_DYN_LINK
//#define BOOST_TEST_MODULE balance
#include <boost/test/unit_test.hpp>

#include <system.hh>

#include "balance.h"
#include "commodity.h"
#include "annotate.h"
#include "pool.h"

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

BOOST_AUTO_TEST_CASE(testStripAnnotations)
{
  // Create an annotated amount: 10 AAPL with a price annotation
  amount_t a1("10 AAPL");
  annotation_t ann1;
  ann1.price = amount_t("$50.00");
  a1.annotate(ann1);

  BOOST_CHECK(a1.has_annotation());

  // Build a balance with the annotated amount
  balance_t b1;
  b1 += a1;

  // Strip all annotations (keep nothing)
  keep_details_t keep_none;
  balance_t b2 = b1.strip_annotations(keep_none);

  // After stripping, the amount should no longer carry annotation info
  // but the commodity count should remain at 1
  BOOST_CHECK_EQUAL(b2.commodity_count(), 1);

  // The stripped amount should equal 10 AAPL without annotation
  amount_t a_plain("10 AAPL");
  BOOST_CHECK_EQUAL(b2, a_plain);

  // Keep all annotations
  keep_details_t keep_all(true, true, true, false);
  balance_t b3 = b1.strip_annotations(keep_all);
  BOOST_CHECK_EQUAL(b3.commodity_count(), 1);

  // Stripping an empty balance should yield an empty balance
  balance_t b_empty;
  balance_t b_stripped = b_empty.strip_annotations(keep_none);
  BOOST_CHECK(b_stripped.is_empty());

  // Balance with non-annotated amounts should be unchanged
  balance_t b4;
  b4 += amount_t("$100.00");
  balance_t b5 = b4.strip_annotations(keep_none);
  BOOST_CHECK_EQUAL(b4, b5);

  BOOST_CHECK(b1.valid());
  BOOST_CHECK(b2.valid());
  BOOST_CHECK(b3.valid());
  BOOST_CHECK(b_empty.valid());
  BOOST_CHECK(b_stripped.valid());
  BOOST_CHECK(b4.valid());
  BOOST_CHECK(b5.valid());
}

BOOST_AUTO_TEST_CASE(testCommodityAmount)
{
  // Build a multi-commodity balance
  balance_t b1;
  b1 += amount_t("$100.00");
  b1 += amount_t("EUR 50");
  b1 += amount_t("CAD 75");

  // Extract the dollar amount
  optional<amount_t> dollar_amt =
    b1.commodity_amount(amount_t("$1").commodity());
  BOOST_CHECK(dollar_amt);
  BOOST_CHECK_EQUAL(*dollar_amt, amount_t("$100.00"));

  // Extract the EUR amount
  optional<amount_t> eur_amt =
    b1.commodity_amount(amount_t("EUR 1").commodity());
  BOOST_CHECK(eur_amt);
  BOOST_CHECK_EQUAL(*eur_amt, amount_t("EUR 50"));

  // Extract the CAD amount
  optional<amount_t> cad_amt =
    b1.commodity_amount(amount_t("CAD 1").commodity());
  BOOST_CHECK(cad_amt);
  BOOST_CHECK_EQUAL(*cad_amt, amount_t("CAD 75"));

  // Looking up a commodity not in the balance returns none
  optional<amount_t> gbp_amt =
    b1.commodity_amount(amount_t("GBP 1").commodity());
  BOOST_CHECK(!gbp_amt);

  // Single-commodity balance: calling with no argument returns the amount
  balance_t b2;
  b2 += amount_t("$200.00");
  optional<amount_t> single_amt = b2.commodity_amount();
  BOOST_CHECK(single_amt);
  BOOST_CHECK_EQUAL(*single_amt, amount_t("$200.00"));

  // Empty balance: calling with no argument returns none
  balance_t b3;
  optional<amount_t> empty_amt = b3.commodity_amount();
  BOOST_CHECK(!empty_amt);

  BOOST_CHECK(b1.valid());
  BOOST_CHECK(b2.valid());
  BOOST_CHECK(b3.valid());
}

BOOST_AUTO_TEST_CASE(testCommodityCount)
{
  balance_t b0;
  BOOST_CHECK_EQUAL(b0.commodity_count(), 0);

  b0 += amount_t("$100.00");
  BOOST_CHECK_EQUAL(b0.commodity_count(), 1);

  b0 += amount_t("EUR 50");
  BOOST_CHECK_EQUAL(b0.commodity_count(), 2);

  b0 += amount_t("CAD 75");
  BOOST_CHECK_EQUAL(b0.commodity_count(), 3);

  // Adding more of an existing commodity should not increase the count
  b0 += amount_t("$50.00");
  BOOST_CHECK_EQUAL(b0.commodity_count(), 3);

  // Subtracting the exact amount of a commodity removes it from the map
  b0 -= amount_t("CAD 75");
  BOOST_CHECK_EQUAL(b0.commodity_count(), 2);

  BOOST_CHECK(b0.valid());
}

BOOST_AUTO_TEST_CASE(testSingleAmount)
{
  // single_amount() returns true when balance has exactly one commodity
  balance_t b1;
  b1 += amount_t("$100.00");
  BOOST_CHECK(b1.single_amount());

  // single_amount() returns false for empty balance
  balance_t b0;
  BOOST_CHECK(!b0.single_amount());

  // single_amount() returns false for multi-commodity balance
  balance_t b2;
  b2 += amount_t("$100.00");
  b2 += amount_t("EUR 50");
  BOOST_CHECK(!b2.single_amount());

  BOOST_CHECK(b0.valid());
  BOOST_CHECK(b1.valid());
  BOOST_CHECK(b2.valid());
}

BOOST_AUTO_TEST_CASE(testBalancePrint)
{
  // Test printing a single-commodity balance
  balance_t b1;
  b1 += amount_t("$100.00");
  std::ostringstream out1;
  b1.print(out1);
  // With no width specified, print uses -1 (no justification)
  BOOST_CHECK(!out1.str().empty());
  // The output should contain the dollar amount
  BOOST_CHECK(out1.str().find("100") != std::string::npos);

  // Test printing a multi-commodity balance
  balance_t b2;
  b2 += amount_t("$100.00");
  b2 += amount_t("EUR 50");
  std::ostringstream out2;
  b2.print(out2, 20);
  std::string printed = out2.str();
  // Should contain both commodities
  BOOST_CHECK(printed.find("100") != std::string::npos);
  BOOST_CHECK(printed.find("EUR") != std::string::npos);

  // Test printing an empty balance (should print zero)
  balance_t b3;
  std::ostringstream out3;
  b3.print(out3, 10);
  BOOST_CHECK(out3.str().find("0") != std::string::npos);

  // Test printing with right justification
  balance_t b4;
  b4 += amount_t("$50.00");
  std::ostringstream out4;
  b4.print(out4, 20, -1, AMOUNT_PRINT_RIGHT_JUSTIFY);
  std::string justified = out4.str();
  BOOST_CHECK(!justified.empty());
  BOOST_CHECK(justified.find("50") != std::string::npos);

  BOOST_CHECK(b1.valid());
  BOOST_CHECK(b2.valid());
  BOOST_CHECK(b3.valid());
  BOOST_CHECK(b4.valid());
}

BOOST_AUTO_TEST_CASE(testBalancePrintColorizesNegativeAmounts)
{
  // Only the negative amount takes the color, and only when asked to.
  balance_t b1;
  b1 += amount_t("$-1.00");
  b1 += amount_t("EUR 50");

  std::ostringstream colored;
  b1.print(colored, 8, -1, AMOUNT_PRINT_RIGHT_JUSTIFY | AMOUNT_PRINT_COLORIZE, "\033[38;5;196m");
  BOOST_CHECK_EQUAL(colored.str(), "  \033[38;5;196m$-1.00\033[0m\n  EUR 50");

  std::ostringstream uncolored;
  b1.print(uncolored, 8, -1, AMOUNT_PRINT_RIGHT_JUSTIFY, "\033[38;5;196m");
  BOOST_CHECK_EQUAL(uncolored.str(), "  $-1.00\n  EUR 50");

  BOOST_CHECK(b1.valid());
}

BOOST_AUTO_TEST_CASE(testToAmount)
{
  // Single-commodity balance converts to amount_t
  balance_t b1;
  b1 += amount_t("$100.00");
  amount_t a1 = b1.to_amount();
  BOOST_CHECK_EQUAL(a1, amount_t("$100.00"));

  // Empty balance throws
  balance_t b2;
  BOOST_CHECK_THROW(b2.to_amount(), balance_error);

  // Multi-commodity balance throws
  balance_t b3;
  b3 += amount_t("$100.00");
  b3 += amount_t("EUR 50");
  BOOST_CHECK_THROW(b3.to_amount(), balance_error);

  // Verify to_string() works via string conversion
  balance_t b4;
  b4 += amount_t("$42.00");
  std::string s = b4.to_string();
  BOOST_CHECK(!s.empty());
  BOOST_CHECK(s.find("42") != std::string::npos);

  BOOST_CHECK(b1.valid());
  BOOST_CHECK(b2.valid());
  BOOST_CHECK(b3.valid());
  BOOST_CHECK(b4.valid());
}

BOOST_AUTO_TEST_CASE(testMapSortedAmounts)
{
  // Build a multi-commodity balance
  balance_t b1;
  b1 += amount_t("$100.00");
  b1 += amount_t("EUR 50");
  b1 += amount_t("CAD 75");

  // Collect amounts via map_sorted_amounts
  std::vector<amount_t> collected;
  b1.map_sorted_amounts([&](const amount_t& amt) {
    collected.push_back(amt);
  });

  // Should have collected exactly 3 amounts
  BOOST_CHECK_EQUAL(collected.size(), 3);

  // Verify all three commodity amounts are present
  bool found_dollar = false, found_eur = false, found_cad = false;
  for (const auto& amt : collected) {
    if (amt.commodity().symbol() == "$")
      found_dollar = true;
    else if (amt.commodity().symbol() == "EUR")
      found_eur = true;
    else if (amt.commodity().symbol() == "CAD")
      found_cad = true;
  }
  BOOST_CHECK(found_dollar);
  BOOST_CHECK(found_eur);
  BOOST_CHECK(found_cad);

  // Verify amounts are sorted by commodity (alphabetical by symbol)
  // CAD < EUR < $ (prefix symbols sort after alphabetic ones)
  for (std::size_t i = 1; i < collected.size(); i++) {
    BOOST_CHECK(commodity_t::compare_by_commodity()(&collected[i-1], &collected[i]) < 0);
  }

  // Empty balance: map_sorted_amounts should not invoke the callback
  balance_t b2;
  std::vector<amount_t> empty_collected;
  b2.map_sorted_amounts([&](const amount_t& amt) {
    empty_collected.push_back(amt);
  });
  BOOST_CHECK(empty_collected.empty());

  // Single-commodity balance: should invoke callback exactly once
  balance_t b3;
  b3 += amount_t("$200.00");
  std::vector<amount_t> single_collected;
  b3.map_sorted_amounts([&](const amount_t& amt) {
    single_collected.push_back(amt);
  });
  BOOST_CHECK_EQUAL(single_collected.size(), 1);
  BOOST_CHECK_EQUAL(single_collected[0], amount_t("$200.00"));

  // Also test sorted_amounts directly
  balance_t::amounts_array sorted;
  b1.sorted_amounts(sorted);
  BOOST_CHECK_EQUAL(sorted.size(), 3);
  for (std::size_t i = 1; i < sorted.size(); i++) {
    BOOST_CHECK(commodity_t::compare_by_commodity()(sorted[i-1], sorted[i]) < 0);
  }

  BOOST_CHECK(b1.valid());
  BOOST_CHECK(b2.valid());
  BOOST_CHECK(b3.valid());
}

BOOST_AUTO_TEST_CASE(testTruncatedAndUnrounded)
{
  amount_t a1("$ 123.456");
  amount_t a2("EUR 789.123");

  balance_t b1;
  b1 += a1;
  b1 += a2;

  // Test truncated(): each component amount should be truncated
  balance_t b_trunc = b1.truncated();
  balance_t b_trunc_expected;
  b_trunc_expected += a1.truncated();
  b_trunc_expected += a2.truncated();
  BOOST_CHECK_EQUAL(b_trunc, b_trunc_expected);

  // Test in_place_truncate()
  balance_t b2(b1);
  b2.in_place_truncate();
  BOOST_CHECK_EQUAL(b2, b_trunc_expected);

  // Test unrounded(): each component amount should be unrounded
  balance_t b_unround = b1.unrounded();
  balance_t b_unround_expected;
  b_unround_expected += a1.unrounded();
  b_unround_expected += a2.unrounded();
  BOOST_CHECK_EQUAL(b_unround, b_unround_expected);

  // Test in_place_unround()
  balance_t b3(b1);
  b3.in_place_unround();
  BOOST_CHECK_EQUAL(b3, b_unround_expected);

  // Test on empty balance
  balance_t b_empty;
  BOOST_CHECK_EQUAL(b_empty.truncated(), b_empty);
  BOOST_CHECK_EQUAL(b_empty.unrounded(), b_empty);

  // Test reduced() and unreduced()
  balance_t b4;
  b4 += amount_t("$100.00");
  balance_t b_reduced = b4.reduced();
  BOOST_CHECK(b_reduced.valid());

  balance_t b_unreduced = b4.unreduced();
  BOOST_CHECK(b_unreduced.valid());

  // Test in_place_reduce() and in_place_unreduce()
  balance_t b5(b4);
  b5.in_place_reduce();
  BOOST_CHECK(b5.valid());

  balance_t b6(b4);
  b6.in_place_unreduce();
  BOOST_CHECK(b6.valid());

  // Test number() strips commodities from all component amounts
  balance_t b7;
  b7 += amount_t("$100.00");
  b7 += amount_t("EUR 50");
  balance_t b_number = b7.number();
  // The number balance should contain commodity-less amounts
  // that sum together since they share the null commodity
  BOOST_CHECK(b_number.valid());

  BOOST_CHECK(b1.valid());
  BOOST_CHECK(b2.valid());
  BOOST_CHECK(b3.valid());
  BOOST_CHECK(b_trunc.valid());
  BOOST_CHECK(b_unround.valid());
  BOOST_CHECK(b_empty.valid());
  BOOST_CHECK(b4.valid());
  BOOST_CHECK(b5.valid());
  BOOST_CHECK(b6.valid());
  BOOST_CHECK(b7.valid());
  BOOST_CHECK(b_number.valid());
}

BOOST_AUTO_TEST_CASE(testFirstSortedAmount)
{
  balance_t empty;
  BOOST_CHECK(empty.first_sorted_amount() == nullptr);

  // The first amount in commodity order, whatever order the map holds.
  balance_t b;
  b += amount_t("5 EUR");
  b += amount_t("$10.00");
  b += amount_t("3 CAD");
  balance_t::amounts_array sorted;
  b.sorted_amounts(sorted);
  BOOST_REQUIRE_EQUAL(sorted.size(), 3U);
  BOOST_CHECK_EQUAL(sorted.front(), b.first_sorted_amount());
  BOOST_CHECK_EQUAL(*b.first_sorted_amount(), amount_t("$10.00"));

  // FIFO and LIFO put the oldest or the newest lot first.
  balance_t lots;
  lots += amount_t("10 AAPL {$5} [2024/02/01]");
  lots += amount_t("10 AAPL {$6} [2024/01/01]");
  lots += amount_t("10 AAPL {$4} [2024/03/01]");
  lots.sort_order = balance_t::lot_sort_order::fifo;
  BOOST_CHECK_EQUAL(*lots.first_sorted_amount(), amount_t("10 AAPL {$6} [2024/01/01]"));
  lots.sort_order = balance_t::lot_sort_order::lifo;
  BOOST_CHECK_EQUAL(*lots.first_sorted_amount(), amount_t("10 AAPL {$4} [2024/03/01]"));
}

BOOST_AUTO_TEST_CASE(testStripAnnotationsMergeOrder)
{
  // A plain amount kept at full precision and a lot of the same commodity
  // merge when the lot is stripped, and the merged amount keeps full
  // precision whichever of the two the component map lists first.  The
  // map's order is a hash of commodity addresses, so forty commodities give
  // forty orders: a merge that let the first part's flags win would fail
  // here with overwhelming probability.
  for (int i = 0; i < 40; i++) {
    const string sym = string(1, static_cast<char>('A' + i / 26)) +
                       string(1, static_cast<char>('A' + i % 26)) + "X";
    amount_t plain("199.99977356 " + sym);
    plain.set_keep_precision(true);
    balance_t b;
    b += plain;
    b += amount_t("400.00 " + sym + " {$4.8799} [2010/05/19]");

    balance_t stripped = b.strip_annotations(keep_details_t());
    BOOST_REQUIRE_EQUAL(stripped.amounts.size(), 1U);
    BOOST_CHECK(stripped.first_sorted_amount()->keep_precision());
    BOOST_CHECK_EQUAL(*stripped.first_sorted_amount(), amount_t("599.99977356 " + sym));
  }
}

BOOST_AUTO_TEST_CASE(testSelfAddition)
{
  // b += b doubles every component, also when a copy shares b's map nodes:
  // the walk over the right-hand side must not read nodes the additions
  // rewrite in place.  b -= b empties the balance.
  auto symbol = [](int i) {
    return string(1, static_cast<char>('A' + i / 26 / 26 % 26)) +
           string(1, static_cast<char>('A' + i / 26 % 26)) +
           string(1, static_cast<char>('A' + i % 26)) + "Q";
  };
  balance_t b;
  for (int i = 0; i < 300; i++)
    b += amount_t("1 " + symbol(i));
  balance_t copy(b);

  b += b;
  BOOST_CHECK_EQUAL(b.amounts.size(), 300U);
  for (const balance_t::amounts_map::value_type& pair : b.amounts)
    BOOST_CHECK_EQUAL(pair.second.number(), amount_t(2L));
  BOOST_CHECK_EQUAL(copy.amounts.size(), 300U);
  for (const balance_t::amounts_map::value_type& pair : copy.amounts)
    BOOST_CHECK_EQUAL(pair.second.number(), amount_t(1L));

  balance_t shared(b);
  b -= b;
  BOOST_CHECK(b.amounts.empty());
  BOOST_CHECK_EQUAL(shared.amounts.size(), 300U);
  copy -= copy;
  BOOST_CHECK(copy.amounts.empty());
}

BOOST_AUTO_TEST_CASE(testAssertThrows)
{
#if !NO_ASSERTS
  // utils.h replaces assert() with one that throws.  A third-party header
  // that includes <cassert> (immer, reached through system.hh) must not put
  // the C library's assert() back in a translation unit using balance_t.
  BOOST_CHECK_THROW(assert(false), std::logic_error);
#endif
}

// -----------------------------------------------------------------------
// Lots the pool has had to give equal annotations (duplicate_lots, see
// commodity_pool_t::reannotate) are one component of a balance
// -----------------------------------------------------------------------

namespace {

/// Two lots of RDUP {$5} [2024/01/02] (($7)): one written with its
/// valuation, the other written without it and then given it, as an
/// account's `value` does.  RDUP {$6} [2024/01/02] is a lot outside the
/// pair.
struct duplicate_lots_t {
  commodity_t* spelled;
  commodity_t* reannotated;
  commodity_t* outsider;

  duplicate_lots_t() {
    commodity_pool_t& pool(*commodity_pool_t::current_pool);
    const annotation_t written(amount_t("$5"), parse_date("2024/01/02"));
    annotation_t valued(written);
    valued.value_expr = expr_t("$7");
    spelled = pool.find_or_create("RDUP", valued);
    reannotated = pool.find_or_create("RDUP", written);
    pool.reannotate(as_annotated_commodity(*reannotated), valued);
    outsider = pool.find_or_create("RDUP", annotation_t(amount_t("$6"), parse_date("2024/01/02")));
    BOOST_REQUIRE(spelled != reannotated);
    BOOST_REQUIRE(*spelled == *reannotated);
    BOOST_REQUIRE_EQUAL(pool.duplicate_lots.size(), 2U);
  }

  static amount_t of(commodity_t* lot, long quantity) {
    amount_t amount(quantity);
    amount.set_commodity(*lot);
    return amount;
  }
};

} // namespace

BOOST_AUTO_TEST_CASE(testDuplicateLotsAdd)
{
  // Amounts of the two lots of a pair add up in one component, kept under
  // the lot that carried the annotation first (the spelled one here) unless
  // the balance already holds the pair under the other; a lot outside the
  // pair keeps a component of its own.
  duplicate_lots_t lots;

  balance_t b;
  b += duplicate_lots_t::of(lots.spelled, 10);
  b += duplicate_lots_t::of(lots.reannotated, 5);
  b += duplicate_lots_t::of(lots.outsider, 3);
  BOOST_CHECK_EQUAL(b.amounts.size(), 2U);
  BOOST_CHECK(b.component_key(*lots.reannotated) == lots.spelled);
  BOOST_CHECK(b.component_key(*lots.outsider) == lots.outsider);
  BOOST_REQUIRE(b.amounts.find(lots.spelled) != nullptr);
  BOOST_CHECK_EQUAL(b.amounts.find(lots.spelled)->number(), amount_t(15L));
  BOOST_REQUIRE(b.amounts.find(lots.outsider) != nullptr);
  BOOST_CHECK_EQUAL(b.amounts.find(lots.outsider)->number(), amount_t(3L));

  balance_t c;
  c += duplicate_lots_t::of(lots.reannotated, 5);
  c += duplicate_lots_t::of(lots.spelled, 10);
  BOOST_CHECK_EQUAL(c.amounts.size(), 1U);
  BOOST_CHECK(c.component_key(*lots.reannotated) == lots.spelled);
  BOOST_CHECK(c.component_key(*lots.spelled) == lots.spelled);
  BOOST_REQUIRE(c.amounts.find(lots.spelled) != nullptr);
  BOOST_CHECK_EQUAL(c.amounts.find(lots.spelled)->number(), amount_t(15L));

  // Adding a whole balance looks each component up the same way.
  balance_t d;
  d += duplicate_lots_t::of(lots.reannotated, 1);
  d += b;
  BOOST_CHECK_EQUAL(d.amounts.size(), 2U);
  BOOST_REQUIRE(d.amounts.find(lots.spelled) != nullptr);
  BOOST_CHECK_EQUAL(d.amounts.find(lots.spelled)->number(), amount_t(16L));
}

BOOST_AUTO_TEST_CASE(testDuplicateLotsSubtract)
{
  // A sale through either lot of a pair draws on the one component and,
  // once it nets to zero, removes it; a lot outside the pair is subtracted
  // into a component of its own.
  duplicate_lots_t lots;

  balance_t b;
  b += duplicate_lots_t::of(lots.spelled, 10);
  b -= duplicate_lots_t::of(lots.reannotated, 4);
  b -= duplicate_lots_t::of(lots.outsider, 2);
  BOOST_CHECK_EQUAL(b.amounts.size(), 2U);
  BOOST_REQUIRE(b.amounts.find(lots.spelled) != nullptr);
  BOOST_CHECK_EQUAL(b.amounts.find(lots.spelled)->number(), amount_t(6L));
  BOOST_REQUIRE(b.amounts.find(lots.outsider) != nullptr);
  BOOST_CHECK_EQUAL(b.amounts.find(lots.outsider)->number(), amount_t(-2L));

  b -= duplicate_lots_t::of(lots.reannotated, 6);
  BOOST_CHECK_EQUAL(b.amounts.size(), 1U);
  BOOST_CHECK(b.amounts.find(lots.spelled) == nullptr);
  BOOST_CHECK(b.amounts.find(lots.reannotated) == nullptr);

  // Subtracting a whole balance looks each component up the same way.
  balance_t held;
  held += duplicate_lots_t::of(lots.spelled, 10);
  balance_t sold;
  sold += duplicate_lots_t::of(lots.reannotated, 10);
  held -= sold;
  BOOST_CHECK(held.is_empty());
}

BOOST_AUTO_TEST_CASE(testDuplicateLotsMerge)
{
  // merge(), which rebuilds a balance from transformed components, puts
  // the two lots of a pair in one component too, and a lot outside the
  // pair in its own.
  duplicate_lots_t lots;

  balance_t b;
  b.merge(duplicate_lots_t::of(lots.reannotated, 7));
  b.merge(duplicate_lots_t::of(lots.spelled, 3));
  b.merge(duplicate_lots_t::of(lots.outsider, 1));
  BOOST_CHECK_EQUAL(b.amounts.size(), 2U);
  BOOST_CHECK(b.component_key(*lots.reannotated) == lots.spelled);
  BOOST_REQUIRE(b.amounts.find(lots.spelled) != nullptr);
  BOOST_CHECK_EQUAL(b.amounts.find(lots.spelled)->number(), amount_t(10L));
  BOOST_REQUIRE(b.amounts.find(lots.outsider) != nullptr);
  BOOST_CHECK_EQUAL(b.amounts.find(lots.outsider)->number(), amount_t(1L));
}

BOOST_AUTO_TEST_CASE(testDuplicateLotsCommodityAmount)
{
  // Asked for either lot of a pair, a balance answers with the component
  // it holds; asked for a lot outside the pair, it has none.
  duplicate_lots_t lots;

  balance_t b;
  b += duplicate_lots_t::of(lots.spelled, 10);

  optional<amount_t> by_other = b.commodity_amount(*lots.reannotated);
  BOOST_REQUIRE(by_other);
  BOOST_CHECK(&by_other->commodity() == lots.spelled);
  BOOST_CHECK_EQUAL(by_other->number(), amount_t(10L));

  optional<amount_t> by_own = b.commodity_amount(*lots.spelled);
  BOOST_REQUIRE(by_own);
  BOOST_CHECK(&by_own->commodity() == lots.spelled);
  BOOST_CHECK_EQUAL(by_own->number(), amount_t(10L));

  BOOST_CHECK(!b.commodity_amount(*lots.outsider));
}

BOOST_AUTO_TEST_CASE(testComponentKeyOutsideDuplicateLots)
{
  // Even with a pair of duplicate lots in the pool, any other commodity is
  // its own key, whatever the balance holds: only a lot of a pair is ever
  // looked for by its annotation.
  duplicate_lots_t lots;
  commodity_pool_t& pool(*commodity_pool_t::current_pool);
  commodity_t* absent =
      pool.find_or_create("RDUP", annotation_t(amount_t("$8"), parse_date("2024/01/02")));
  commodity_t& base(lots.outsider->referent());

  balance_t one;
  one += duplicate_lots_t::of(lots.outsider, 2);
  BOOST_REQUIRE_EQUAL(one.amounts.size(), 1U);
  BOOST_CHECK(one.component_key(*lots.outsider) == lots.outsider);
  BOOST_CHECK(one.component_key(*absent) == absent);
  BOOST_CHECK(one.component_key(base) == &base);

  balance_t pair_held;
  pair_held += duplicate_lots_t::of(lots.spelled, 1);
  BOOST_CHECK(pair_held.component_key(*lots.outsider) == lots.outsider);
  BOOST_CHECK(pair_held.component_key(*absent) == absent);
  BOOST_CHECK(pair_held.component_key(base) == &base);
}

BOOST_AUTO_TEST_CASE(testUnreduceMergeKeepsPrecision)
{
  // Unreducing can turn two components into one commodity: 90m and 5400s
  // are both 1.5h.  The merged amount keeps full precision when either
  // part keeps it, whichever of the two the component map visits first;
  // giving the flag to each part in turn exercises both orders.
  amount_t::parse_conversion("1.0m", "60s");
  amount_t::parse_conversion("1.00h", "60m");

  for (int keeper = 0; keeper < 3; ++keeper) {
    amount_t minutes;
    (void)minutes.parse("90m", PARSE_NO_REDUCE);
    amount_t seconds;
    (void)seconds.parse("5400s", PARSE_NO_REDUCE);
    if (keeper == 0)
      minutes.set_keep_precision(true);
    else if (keeper == 1)
      seconds.set_keep_precision(true);

    balance_t b;
    b += minutes;
    b += seconds;
    BOOST_REQUIRE_EQUAL(b.amounts.size(), 2U);
    BOOST_REQUIRE_EQUAL(minutes.unreduced().commodity().symbol(), "h");
    BOOST_REQUIRE_EQUAL(seconds.unreduced().commodity().symbol(), "h");
    BOOST_REQUIRE_EQUAL(minutes.unreduced().keep_precision(), keeper == 0);
    BOOST_REQUIRE_EQUAL(seconds.unreduced().keep_precision(), keeper == 1);

    b.in_place_unreduce();
    BOOST_REQUIRE_EQUAL(b.amounts.size(), 1U);
    const amount_t hours(b.amounts.begin()->second);
    BOOST_CHECK_EQUAL(hours.commodity().symbol(), "h");
    BOOST_CHECK_EQUAL(hours.number(), amount_t(3L));
    // With neither part keeping full precision, neither does the sum.
    BOOST_CHECK_EQUAL(hours.keep_precision(), keeper < 2);
  }
}

BOOST_AUTO_TEST_SUITE_END()
