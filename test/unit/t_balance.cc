#define BOOST_TEST_DYN_LINK
//#define BOOST_TEST_MODULE balance
#include <boost/test/unit_test.hpp>

#include <system.hh>

#include "balance.h"
#include "commodity.h"
#include "annotate.h"

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

BOOST_AUTO_TEST_SUITE_END()
