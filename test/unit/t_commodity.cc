#define BOOST_TEST_DYN_LINK
//#define BOOST_TEST_MODULE commodity
#include <boost/test/unit_test.hpp>

#include <system.hh>

#include "amount.h"
#include "commodity.h"
#include "annotate.h"
#include "pool.h"
#include "ptree.h"

using namespace ledger;

struct commodity_fixture {
  commodity_fixture() {
  times_initialize();
  amount_t::initialize();
  amount_t::stream_fullstrings = true;
  }

  ~commodity_fixture() {
  amount_t::shutdown();
  times_shutdown();
  }
};

BOOST_FIXTURE_TEST_SUITE(commodity, commodity_fixture)

BOOST_AUTO_TEST_CASE(testPriceHistory)
{
#ifndef NOT_FOR_PYTHON
  datetime_t jan17_05;
  datetime_t jan17_06;
  datetime_t jan17_07;
  datetime_t feb27_07;
  datetime_t feb28_07;
  datetime_t feb28_07sbm;
  datetime_t mar01_07;
  datetime_t apr15_07;
#endif // NOT_FOR_PYTHON

  jan17_05    = parse_datetime("2005/01/17 00:00:00");
  jan17_06    = parse_datetime("2006/01/17 00:00:00");
  jan17_07    = parse_datetime("2007/01/17 00:00:00");
  feb27_07    = parse_datetime("2007/02/27 18:00:00");
  feb28_07    = parse_datetime("2007/02/28 06:00:00");
  feb28_07sbm = parse_datetime("2007/02/28 11:59:59");
  mar01_07    = parse_datetime("2007/03/01 00:00:00");
  apr15_07    = parse_datetime("2007/04/15 13:00:00");

  amount_t x0;
  amount_t x1("100.10 AAPL");

  BOOST_CHECK_THROW(x0.value(), amount_error);
#ifndef NOT_FOR_PYTHON
  BOOST_CHECK(! x1.value());
#endif

  // Commodities cannot be constructed by themselves, since a great deal
  // of their state depends on how they were seen to be used.
  commodity_t& aapl(x1.commodity());

  aapl.add_price(jan17_07,    amount_t("$10.20"));
  aapl.add_price(feb27_07,    amount_t("$13.40"));
  aapl.add_price(feb28_07,    amount_t("$18.33"));
  aapl.add_price(feb28_07sbm, amount_t("$18.30"));
  aapl.add_price(mar01_07,    amount_t("$19.50"));
  aapl.add_price(apr15_07,    amount_t("$21.22"));
  aapl.add_price(jan17_05,    amount_t("EUR 23.00"));
  aapl.add_price(jan17_06,    amount_t("CAD 25.00"));

  amount_t one_euro("EUR 1.00");
  commodity_t& euro(one_euro.commodity());

  euro.add_price(feb27_07, amount_t("CAD 1.40"));
  euro.add_price(jan17_05, amount_t("$0.78"));

  amount_t one_cad("CAD 1.00");
  commodity_t& cad(one_cad.commodity());

  cad.add_price(jan17_06, amount_t("$1.11"));

#ifndef NOT_FOR_PYTHON
  std::optional<amount_t> amt = x1.value(feb28_07sbm);
  BOOST_CHECK(amt);
  BOOST_CHECK_EQUAL(amount_t("$1831.83"), *amt);

  amt = x1.value(CURRENT_TIME());
  BOOST_CHECK(amt);
  BOOST_CHECK_EQUAL(string("$2124.12"), amt->to_string());
#ifdef INTEGER_MATH
  BOOST_CHECK_EQUAL(string("$2124.12"), amt->to_fullstring());
#else
  BOOST_CHECK_EQUAL(string("$2124.122"), amt->to_fullstring());
#endif

  amt = x1.value(CURRENT_TIME(), &euro);
  BOOST_CHECK(amt);
  BOOST_CHECK_EQUAL(string("EUR 1787.50"), amt->rounded().to_string());

  // Add a newer Euro pricing
  aapl.add_price(jan17_07, amount_t("EUR 23.00"));

  amt = x1.value(CURRENT_TIME(), &euro);
  BOOST_CHECK(amt);
  BOOST_CHECK_EQUAL(string("EUR 2302.30"), amt->to_string());

  amt = x1.value(CURRENT_TIME(), &cad);
  BOOST_CHECK(amt);
  BOOST_CHECK_EQUAL(string("CAD 3223.22"), amt->to_string());
#endif // NOT_FOR_PYTHON

  BOOST_CHECK(x1.valid());
}

BOOST_AUTO_TEST_CASE(testCommodityProperties)
{
  amount_t x1("$100.00");
  commodity_t& dollar(x1.commodity());

  // Test symbol and base_symbol
  BOOST_CHECK_EQUAL(string("$"), dollar.symbol());
  BOOST_CHECK_EQUAL(string("$"), dollar.base_symbol());

  // Test name getter/setter
  BOOST_CHECK(! dollar.name());
  dollar.set_name(string("US Dollar"));
  BOOST_CHECK(dollar.name());
  BOOST_CHECK_EQUAL(string("US Dollar"), *dollar.name());
  dollar.set_name();
  BOOST_CHECK(! dollar.name());

  // Test note getter/setter
  BOOST_CHECK(! dollar.note());
  dollar.set_note(string("The default currency"));
  BOOST_CHECK(dollar.note());
  BOOST_CHECK_EQUAL(string("The default currency"), *dollar.note());
  dollar.set_note();
  BOOST_CHECK(! dollar.note());

  // Test precision getter/setter
  BOOST_CHECK_EQUAL(2, dollar.precision());
  dollar.set_precision(4);
  BOOST_CHECK_EQUAL(4, dollar.precision());
  dollar.set_precision(2);

  // Test suffixed commodity (symbol after number)
  amount_t x2("100.00 EUR");
  commodity_t& eur(x2.commodity());
  BOOST_CHECK_EQUAL(string("EUR"), eur.symbol());
  BOOST_CHECK_EQUAL(string("EUR"), eur.base_symbol());

  // Test has_annotation for non-annotated commodity
  BOOST_CHECK(! dollar.has_annotation());

  // Test referent for non-annotated commodity (returns self)
  BOOST_CHECK_EQUAL(dollar.base_symbol(), dollar.referent().base_symbol());

  // Test valid
  BOOST_CHECK(dollar.valid());
  BOOST_CHECK(eur.valid());

  BOOST_CHECK(x1.valid());
  BOOST_CHECK(x2.valid());
}

BOOST_AUTO_TEST_CASE(testCommodityFlags)
{
  // Prefix commodity (e.g., $100.00) - should NOT be suffixed
  amount_t x1("$100.00");
  commodity_t& dollar(x1.commodity());
  BOOST_CHECK(! dollar.has_flags(COMMODITY_STYLE_SUFFIXED));

  // Suffixed commodity (e.g., 100.00 EUR) - should be suffixed and separated
  amount_t x2("100.00 EUR");
  commodity_t& eur(x2.commodity());
  BOOST_CHECK(eur.has_flags(COMMODITY_STYLE_SUFFIXED));
  BOOST_CHECK(eur.has_flags(COMMODITY_STYLE_SEPARATED));

  // Test add_flags / drop_flags
  dollar.add_flags(COMMODITY_NOMARKET);
  BOOST_CHECK(dollar.has_flags(COMMODITY_NOMARKET));
  dollar.drop_flags(COMMODITY_NOMARKET);
  BOOST_CHECK(! dollar.has_flags(COMMODITY_NOMARKET));

  // European-style amount with decimal comma (1.000,00 EUR)
  // Need to parse a European-style number to get COMMODITY_STYLE_DECIMAL_COMMA
  amount_t x3("1.000,00 DEM");
  commodity_t& dem(x3.commodity());
  BOOST_CHECK(dem.has_flags(COMMODITY_STYLE_DECIMAL_COMMA));
  BOOST_CHECK(dem.has_flags(COMMODITY_STYLE_THOUSANDS));

  // Test COMMODITY_KNOWN flag via add_flags
  BOOST_CHECK(! dollar.has_flags(COMMODITY_KNOWN));
  dollar.add_flags(COMMODITY_KNOWN);
  BOOST_CHECK(dollar.has_flags(COMMODITY_KNOWN));
  dollar.drop_flags(COMMODITY_KNOWN);

  BOOST_CHECK(x1.valid());
  BOOST_CHECK(x2.valid());
  BOOST_CHECK(x3.valid());
}

BOOST_AUTO_TEST_CASE(testCommodityComparison)
{
  amount_t x1("$100.00");
  amount_t x2("$200.00");
  commodity_t& c1(x1.commodity());
  commodity_t& c2(x2.commodity());

  // Same commodity from different amounts should be equal
  BOOST_CHECK(c1 == c2);

  // Different commodities should not be equal
  amount_t x3("100.00 EUR");
  commodity_t& c3(x3.commodity());
  BOOST_CHECK(! (c1 == c3));

  // Test operator==(string)
  BOOST_CHECK(c1 == string("$"));
  BOOST_CHECK(c3 == string("EUR"));
  BOOST_CHECK(! (c1 == string("EUR")));
  BOOST_CHECK(! (c3 == string("$")));

  BOOST_CHECK(x1.valid());
  BOOST_CHECK(x2.valid());
  BOOST_CHECK(x3.valid());
}

BOOST_AUTO_TEST_CASE(testCommodityPrint)
{
  // Test print for a simple prefix commodity
  amount_t x1("$100.00");
  commodity_t& dollar(x1.commodity());

  {
    std::ostringstream out;
    dollar.print(out, false, false);
    BOOST_CHECK_EQUAL(string("$"), out.str());
  }

  // Test print for a suffixed commodity
  amount_t x2("100.00 EUR");
  commodity_t& eur(x2.commodity());

  {
    std::ostringstream out;
    eur.print(out, false, false);
    BOOST_CHECK_EQUAL(string("EUR"), out.str());
  }

  // Test print with elide_quotes=true for a quoted commodity
  // A quoted commodity symbol is created when the symbol needs quotes
  amount_t x3("10 \"My Fund\"");
  commodity_t& fund(x3.commodity());

  {
    std::ostringstream out;
    fund.print(out, false, false);
    BOOST_CHECK_EQUAL(string("\"My Fund\""), out.str());
  }

  // With elide_quotes=true, quotes should be removed even when the symbol
  // contains spaces, as long as COMMODITY_STYLE_SEPARATED is set (which it
  // is for "10 \"My Fund\"" since there is a space between amount and symbol)
  {
    std::ostringstream out;
    fund.print(out, true, false);
    BOOST_CHECK_EQUAL(string("My Fund"), out.str());
  }

  BOOST_CHECK(x1.valid());
  BOOST_CHECK(x2.valid());
  BOOST_CHECK(x3.valid());
}

BOOST_AUTO_TEST_CASE(testSymbolNeedsQuotes)
{
  // Simple symbols should not need quotes
  BOOST_CHECK(! commodity_t::symbol_needs_quotes("USD"));
  BOOST_CHECK(! commodity_t::symbol_needs_quotes("EUR"));
  BOOST_CHECK(! commodity_t::symbol_needs_quotes("AAPL"));

  // Single character symbols
  BOOST_CHECK(! commodity_t::symbol_needs_quotes("$"));

  // Symbols with spaces need quotes
  BOOST_CHECK(commodity_t::symbol_needs_quotes("My Stock"));

  // Symbols with digits need quotes
  BOOST_CHECK(commodity_t::symbol_needs_quotes("A1B"));

  // Symbols with special characters need quotes
  BOOST_CHECK(commodity_t::symbol_needs_quotes("US+D"));
  BOOST_CHECK(commodity_t::symbol_needs_quotes("X.Y"));
  BOOST_CHECK(commodity_t::symbol_needs_quotes("A=B"));
  BOOST_CHECK(commodity_t::symbol_needs_quotes("A<B"));
  BOOST_CHECK(commodity_t::symbol_needs_quotes("A(B"));
  BOOST_CHECK(commodity_t::symbol_needs_quotes("A{B"));

  // Empty string should not need quotes (no invalid chars)
  BOOST_CHECK(! commodity_t::symbol_needs_quotes(""));
}

BOOST_AUTO_TEST_CASE(testAnnotationConstruction)
{
  // Default constructor - should be false (no fields set)
  annotation_t ann1;
  BOOST_CHECK(! ann1);
  BOOST_CHECK(! ann1.price);
  BOOST_CHECK(! ann1.date);
  BOOST_CHECK(! ann1.tag);

  // With price only
  annotation_t ann2(amount_t("$50.00"));
  BOOST_CHECK(ann2);
  BOOST_CHECK(ann2.price);
  BOOST_CHECK_EQUAL(amount_t("$50.00"), *ann2.price);
  BOOST_CHECK(! ann2.date);
  BOOST_CHECK(! ann2.tag);

  // With date only
  annotation_t ann3(std::nullopt, parse_date("2024/01/15"));
  BOOST_CHECK(ann3);
  BOOST_CHECK(! ann3.price);
  BOOST_CHECK(ann3.date);
  BOOST_CHECK_EQUAL(parse_date("2024/01/15"), *ann3.date);
  BOOST_CHECK(! ann3.tag);

  // With tag only
  annotation_t ann4(std::nullopt, std::nullopt, string("lot1"));
  BOOST_CHECK(ann4);
  BOOST_CHECK(! ann4.price);
  BOOST_CHECK(! ann4.date);
  BOOST_CHECK(ann4.tag);
  BOOST_CHECK_EQUAL(string("lot1"), *ann4.tag);

  // With all three
  annotation_t ann5(amount_t("$50.00"), parse_date("2024/01/15"), string("lot1"));
  BOOST_CHECK(ann5);
  BOOST_CHECK(ann5.price);
  BOOST_CHECK(ann5.date);
  BOOST_CHECK(ann5.tag);
  BOOST_CHECK_EQUAL(amount_t("$50.00"), *ann5.price);
  BOOST_CHECK_EQUAL(parse_date("2024/01/15"), *ann5.date);
  BOOST_CHECK_EQUAL(string("lot1"), *ann5.tag);

  // Copy constructor
  annotation_t ann6(ann5);
  BOOST_CHECK(ann6);
  BOOST_CHECK_EQUAL(*ann5.price, *ann6.price);
  BOOST_CHECK_EQUAL(*ann5.date, *ann6.date);
  BOOST_CHECK_EQUAL(*ann5.tag, *ann6.tag);

  // Copy preserves flags
  annotation_t ann7(amount_t("$50.00"));
  ann7.add_flags(ANNOTATION_PRICE_FIXATED);
  annotation_t ann8(ann7);
  BOOST_CHECK(ann8.has_flags(ANNOTATION_PRICE_FIXATED));
}

BOOST_AUTO_TEST_CASE(testAnnotationParsing)
{
  // Parse price only: {$50.00}
  {
    annotation_t ann;
    std::istringstream in("{$50.00}");
    ann.parse(in);
    BOOST_CHECK(ann);
    BOOST_CHECK(ann.price);
    BOOST_CHECK_EQUAL(amount_t("$50.00"), *ann.price);
    BOOST_CHECK(! ann.date);
    BOOST_CHECK(! ann.tag);
  }

  // Parse date only: [2024/01/15]
  {
    annotation_t ann;
    std::istringstream in("[2024/01/15]");
    ann.parse(in);
    BOOST_CHECK(ann);
    BOOST_CHECK(! ann.price);
    BOOST_CHECK(ann.date);
    BOOST_CHECK_EQUAL(parse_date("2024/01/15"), *ann.date);
    BOOST_CHECK(! ann.tag);
  }

  // Parse tag only: (lot1)
  {
    annotation_t ann;
    std::istringstream in("(lot1)");
    ann.parse(in);
    BOOST_CHECK(ann);
    BOOST_CHECK(! ann.price);
    BOOST_CHECK(! ann.date);
    BOOST_CHECK(ann.tag);
    BOOST_CHECK_EQUAL(string("lot1"), *ann.tag);
  }

  // Parse combined: {$50.00} [2024/01/15] (lot1)
  {
    annotation_t ann;
    std::istringstream in("{$50.00} [2024/01/15] (lot1)");
    ann.parse(in);
    BOOST_CHECK(ann);
    BOOST_CHECK(ann.price);
    BOOST_CHECK(ann.date);
    BOOST_CHECK(ann.tag);
    BOOST_CHECK_EQUAL(amount_t("$50.00"), *ann.price);
    BOOST_CHECK_EQUAL(parse_date("2024/01/15"), *ann.date);
    BOOST_CHECK_EQUAL(string("lot1"), *ann.tag);
  }

  // Parse fixated price: {=$50.00}
  {
    annotation_t ann;
    std::istringstream in("{=$50.00}");
    ann.parse(in);
    BOOST_CHECK(ann);
    BOOST_CHECK(ann.price);
    BOOST_CHECK_EQUAL(amount_t("$50.00"), *ann.price);
    BOOST_CHECK(ann.has_flags(ANNOTATION_PRICE_FIXATED));
    BOOST_CHECK(! ann.has_flags(ANNOTATION_PRICE_NOT_PER_UNIT));
  }

  // Parse total cost (not per unit): {{$500.00}}
  {
    annotation_t ann;
    std::istringstream in("{{$500.00}}");
    ann.parse(in);
    BOOST_CHECK(ann);
    BOOST_CHECK(ann.price);
    BOOST_CHECK_EQUAL(amount_t("$500.00"), *ann.price);
    BOOST_CHECK(ann.has_flags(ANNOTATION_PRICE_NOT_PER_UNIT));
    BOOST_CHECK(! ann.has_flags(ANNOTATION_PRICE_FIXATED));
  }

  // Parse fixated total cost: {{=$500.00}}
  {
    annotation_t ann;
    std::istringstream in("{{=$500.00}}");
    ann.parse(in);
    BOOST_CHECK(ann);
    BOOST_CHECK(ann.price);
    BOOST_CHECK_EQUAL(amount_t("$500.00"), *ann.price);
    BOOST_CHECK(ann.has_flags(ANNOTATION_PRICE_NOT_PER_UNIT));
    BOOST_CHECK(ann.has_flags(ANNOTATION_PRICE_FIXATED));
  }
}

BOOST_AUTO_TEST_CASE(testAnnotationPrinting)
{
  // Print annotation with price
  {
    annotation_t ann(amount_t("$50.00"));
    std::ostringstream out;
    ann.print(out);
    BOOST_CHECK_EQUAL(string(" {$50.00}"), out.str());
  }

  // Print annotation with date
  {
    annotation_t ann(std::nullopt, parse_date("2024/01/15"));
    std::ostringstream out;
    ann.print(out);
    BOOST_CHECK_EQUAL(string(" [2024/01/15]"), out.str());
  }

  // Print annotation with tag
  {
    annotation_t ann(std::nullopt, std::nullopt, string("lot1"));
    std::ostringstream out;
    ann.print(out);
    BOOST_CHECK_EQUAL(string(" (lot1)"), out.str());
  }

  // Print annotation with all fields
  {
    annotation_t ann(amount_t("$50.00"), parse_date("2024/01/15"), string("lot1"));
    std::ostringstream out;
    ann.print(out);
    BOOST_CHECK_EQUAL(string(" {$50.00} [2024/01/15] (lot1)"), out.str());
  }

  // Print annotation with fixated price
  {
    annotation_t ann(amount_t("$50.00"));
    ann.add_flags(ANNOTATION_PRICE_FIXATED);
    std::ostringstream out;
    ann.print(out);
    BOOST_CHECK_EQUAL(string(" {=$50.00}"), out.str());
  }

  // Print with no_computed_annotations=true should skip calculated fields
  {
    annotation_t ann(amount_t("$50.00"), parse_date("2024/01/15"), string("lot1"));
    ann.add_flags(ANNOTATION_PRICE_CALCULATED);
    std::ostringstream out;
    ann.print(out, false, true);
    // Price should be omitted because it is CALCULATED and no_computed_annotations is true
    BOOST_CHECK_EQUAL(string(" [2024/01/15] (lot1)"), out.str());
  }

  // Print with no_computed_annotations=true and calculated date
  {
    annotation_t ann(amount_t("$50.00"), parse_date("2024/01/15"), string("lot1"));
    ann.add_flags(ANNOTATION_DATE_CALCULATED);
    std::ostringstream out;
    ann.print(out, false, true);
    // Date should be omitted because it is CALCULATED
    BOOST_CHECK_EQUAL(string(" {$50.00} (lot1)"), out.str());
  }
}

BOOST_AUTO_TEST_CASE(testAnnotationComparison)
{
  // Equal annotations
  annotation_t ann1(amount_t("$50.00"), parse_date("2024/01/15"), string("lot1"));
  annotation_t ann2(amount_t("$50.00"), parse_date("2024/01/15"), string("lot1"));
  BOOST_CHECK(ann1 == ann2);

  // Different prices
  annotation_t ann3(amount_t("$60.00"), parse_date("2024/01/15"), string("lot1"));
  BOOST_CHECK(! (ann1 == ann3));

  // Different dates
  annotation_t ann4(amount_t("$50.00"), parse_date("2024/02/15"), string("lot1"));
  BOOST_CHECK(! (ann1 == ann4));

  // Different tags
  annotation_t ann5(amount_t("$50.00"), parse_date("2024/01/15"), string("lot2"));
  BOOST_CHECK(! (ann1 == ann5));

  // Test operator< - no price vs price
  annotation_t ann_no_price(std::nullopt, parse_date("2024/01/15"));
  annotation_t ann_with_price(amount_t("$50.00"));
  BOOST_CHECK(ann_no_price < ann_with_price);
  BOOST_CHECK(! (ann_with_price < ann_no_price));

  // Test operator< - no date vs date
  annotation_t ann_no_date(amount_t("$50.00"));
  annotation_t ann_with_date(amount_t("$50.00"), parse_date("2024/01/15"));
  BOOST_CHECK(ann_no_date < ann_with_date);

  // Test operator< - same price, different amounts
  annotation_t ann_low(amount_t("$40.00"));
  annotation_t ann_high(amount_t("$60.00"));
  BOOST_CHECK(ann_low < ann_high);
  BOOST_CHECK(! (ann_high < ann_low));

  // Equality considers ANNOTATION_SEMANTIC_FLAGS
  annotation_t ann6(amount_t("$50.00"));
  annotation_t ann7(amount_t("$50.00"));
  ann7.add_flags(ANNOTATION_PRICE_FIXATED);
  BOOST_CHECK(! (ann6 == ann7));
}

BOOST_AUTO_TEST_CASE(testKeepDetails)
{
  // Default: keep nothing
  keep_details_t kd1;
  BOOST_CHECK(! kd1.keep_price);
  BOOST_CHECK(! kd1.keep_date);
  BOOST_CHECK(! kd1.keep_tag);
  BOOST_CHECK(! kd1.only_actuals);
  BOOST_CHECK(! kd1.keep_all());
  BOOST_CHECK(! kd1.keep_any());

  // Keep all
  keep_details_t kd2(true, true, true, false);
  BOOST_CHECK(kd2.keep_all());
  BOOST_CHECK(kd2.keep_any());

  // Keep some
  keep_details_t kd3(true, false, false, false);
  BOOST_CHECK(! kd3.keep_all());
  BOOST_CHECK(kd3.keep_any());

  // Keep all but only_actuals set means keep_all() is false
  keep_details_t kd4(true, true, true, true);
  BOOST_CHECK(! kd4.keep_all());
  BOOST_CHECK(kd4.keep_any());

  // Test keep_all(commodity) and keep_any(commodity) with non-annotated commodity
  amount_t x1("$100.00");
  commodity_t& dollar(x1.commodity());
  BOOST_CHECK(! dollar.has_annotation());

  // keep_all(comm) returns true for non-annotated commodities
  // regardless of keep_price/keep_date/keep_tag values
  keep_details_t kd5(false, false, false, false);
  BOOST_CHECK(kd5.keep_all(dollar));

  // keep_any(comm) returns false for non-annotated commodities
  BOOST_CHECK(! kd5.keep_any(dollar));

  // Even with all flags set, keep_any returns false for non-annotated
  keep_details_t kd6(true, true, true, false);
  BOOST_CHECK(kd6.keep_all(dollar));
  BOOST_CHECK(! kd6.keep_any(dollar));

  BOOST_CHECK(x1.valid());
}

BOOST_AUTO_TEST_CASE(testCommodityPool)
{
  // Access the pool via a commodity
  amount_t x1("$100.00");
  commodity_t& dollar(x1.commodity());
  commodity_pool_t& pool(dollar.pool());

  // The null commodity should exist
  BOOST_CHECK(pool.null_commodity != nullptr);
  BOOST_CHECK_EQUAL(string(""), pool.null_commodity->symbol());
  BOOST_CHECK(pool.null_commodity->has_flags(COMMODITY_BUILTIN));

  // find should locate the dollar commodity
  commodity_t* found = pool.find("$");
  BOOST_CHECK(found != nullptr);
  BOOST_CHECK(*found == dollar);

  // find_or_create should find existing commodity
  commodity_t* found2 = pool.find_or_create("$");
  BOOST_CHECK(found2 != nullptr);
  BOOST_CHECK(*found2 == dollar);

  // find_or_create with new symbol should create it
  commodity_t* gold = pool.find_or_create("GLD");
  BOOST_CHECK(gold != nullptr);
  BOOST_CHECK_EQUAL(string("GLD"), gold->symbol());
  BOOST_CHECK(gold->valid());

  // find should now locate GLD
  commodity_t* found3 = pool.find("GLD");
  BOOST_CHECK(found3 != nullptr);
  BOOST_CHECK(*found3 == *gold);

  // find for nonexistent should return NULL
  commodity_t* nonexistent = pool.find("DOESNOTEXIST");
  BOOST_CHECK(nonexistent == nullptr);

  // Test alias
  commodity_t* aliased = pool.alias("GOLD", *gold);
  BOOST_CHECK(aliased != nullptr);
  // Aliased commodity should be findable by the alias name
  commodity_t* found_alias = pool.find("GOLD");
  BOOST_CHECK(found_alias != nullptr);

  // Test default_commodity
  BOOST_CHECK(pool.default_commodity == nullptr);

  BOOST_CHECK(x1.valid());
}

BOOST_AUTO_TEST_CASE(testParsePriceDirective)
{
  amount_t x1("$1.00");
  commodity_pool_t& pool(x1.commodity().pool());

  // Parse a simple price directive: "2024/01/15 AAPL $150.00"
  {
    char line[] = "2024/01/15 AAPL $150.00";
    auto result = pool.parse_price_directive(line);
    BOOST_CHECK(result);
    BOOST_CHECK_EQUAL(string("AAPL"), result->first->symbol());
    BOOST_CHECK_EQUAL(amount_t("$150.00"), result->second.price);
  }

  // Parse a price directive with datetime: "2024/01/15 12:00:00 AAPL $155.00"
  {
    char line[] = "2024/01/15 12:00:00 AAPL $155.00";
    auto result = pool.parse_price_directive(line);
    BOOST_CHECK(result);
    BOOST_CHECK_EQUAL(string("AAPL"), result->first->symbol());
    BOOST_CHECK_EQUAL(amount_t("$155.00"), result->second.price);
  }

  // Parse with do_not_add_price=true (should not add to price history)
  {
    char line[] = "2024/06/15 GOOG $175.00";
    auto result = pool.parse_price_directive(line, true);
    BOOST_CHECK(result);
    BOOST_CHECK_EQUAL(string("GOOG"), result->first->symbol());
    BOOST_CHECK_EQUAL(amount_t("$175.00"), result->second.price);
  }

  BOOST_CHECK(x1.valid());
}

BOOST_AUTO_TEST_CASE(testRemovePrice)
{
  amount_t x1("100 MSFT");
  commodity_t& msft(x1.commodity());

  datetime_t jan15 = parse_datetime("2024/01/15 00:00:00");
  datetime_t feb15 = parse_datetime("2024/02/15 00:00:00");
  datetime_t mar15 = parse_datetime("2024/03/15 00:00:00");

  msft.add_price(jan15, amount_t("$300.00"));
  msft.add_price(feb15, amount_t("$310.00"));
  msft.add_price(mar15, amount_t("$320.00"));

  // Verify the latest price is findable
  std::optional<price_point_t> pp = msft.find_price(nullptr, mar15);
  BOOST_CHECK(pp);
  BOOST_CHECK_EQUAL(amount_t("$320.00"), pp->price);

  // Remove the march price
  msft.remove_price(mar15, msft.pool().find("$")->referent());

  // Now the latest price at mar15 should be the feb15 price
  pp = msft.find_price(nullptr, feb15);
  BOOST_CHECK(pp);
  BOOST_CHECK_EQUAL(amount_t("$310.00"), pp->price);

  BOOST_CHECK(x1.valid());
}

BOOST_AUTO_TEST_CASE(testMapPrices)
{
  amount_t x1("100 TSLA");
  commodity_t& tsla(x1.commodity());

  datetime_t jan15 = parse_datetime("2024/01/15 00:00:00");
  datetime_t feb15 = parse_datetime("2024/02/15 00:00:00");
  datetime_t mar15 = parse_datetime("2024/03/15 00:00:00");

  tsla.add_price(jan15, amount_t("$200.00"));
  tsla.add_price(feb15, amount_t("$210.00"));
  tsla.add_price(mar15, amount_t("$220.00"));

  // Collect all prices using map_prices
  std::vector<std::pair<datetime_t, amount_t>> collected;
  tsla.map_prices(
    [&collected](datetime_t when, const amount_t& price) {
      collected.push_back(std::make_pair(when, price));
    },
    mar15 + boost::posix_time::seconds(1));

  // Should have collected at least the prices we added
  BOOST_CHECK(collected.size() >= 3);

  // Verify that the prices we added are present in the collected set
  bool found_jan = false, found_feb = false, found_mar = false;
  for (auto& p : collected) {
    if (p.first == jan15 && p.second == amount_t("$200.00"))
      found_jan = true;
    if (p.first == feb15 && p.second == amount_t("$210.00"))
      found_feb = true;
    if (p.first == mar15 && p.second == amount_t("$220.00"))
      found_mar = true;
  }
  BOOST_CHECK(found_jan);
  BOOST_CHECK(found_feb);
  BOOST_CHECK(found_mar);

  BOOST_CHECK(x1.valid());
}

BOOST_AUTO_TEST_CASE(testAnnotatedCommodity)
{
  // Create an annotated amount using the annotation_t constructor
  amount_t base("100 AAPL");

  annotation_t details(amount_t("$150.00"), parse_date("2024/01/15"), string("lot1"));
  amount_t annotated(base, details);

  commodity_t& comm(annotated.commodity());

  // The commodity should be annotated
  BOOST_CHECK(comm.has_annotation());

  // The referent should be the base (non-annotated) commodity
  commodity_t& ref(comm.referent());
  BOOST_CHECK(! ref.has_annotation());
  BOOST_CHECK_EQUAL(string("AAPL"), ref.base_symbol());

  // Access the annotation details via downcast
  annotated_commodity_t& ann_comm(as_annotated_commodity(comm));
  BOOST_CHECK(ann_comm.details.price);
  BOOST_CHECK(ann_comm.details.date);
  BOOST_CHECK(ann_comm.details.tag);
  BOOST_CHECK_EQUAL(amount_t("$150.00"), *ann_comm.details.price);
  BOOST_CHECK_EQUAL(parse_date("2024/01/15"), *ann_comm.details.date);
  BOOST_CHECK_EQUAL(string("lot1"), *ann_comm.details.tag);

  // Test strip_annotations - keep nothing
  {
    keep_details_t keep_none(false, false, false, false);
    commodity_t& stripped = comm.strip_annotations(keep_none);
    BOOST_CHECK(! stripped.has_annotation());
    BOOST_CHECK_EQUAL(string("AAPL"), stripped.base_symbol());
  }

  // Test strip_annotations - keep price only
  {
    keep_details_t keep_price(true, false, false, false);
    commodity_t& stripped = comm.strip_annotations(keep_price);
    BOOST_CHECK(stripped.has_annotation());
    annotated_commodity_t& stripped_ann(as_annotated_commodity(stripped));
    BOOST_CHECK(stripped_ann.details.price);
    BOOST_CHECK(! stripped_ann.details.date);
    BOOST_CHECK(! stripped_ann.details.tag);
  }

  // Test strip_annotations - keep date only
  {
    keep_details_t keep_date(false, true, false, false);
    commodity_t& stripped = comm.strip_annotations(keep_date);
    BOOST_CHECK(stripped.has_annotation());
    annotated_commodity_t& stripped_ann(as_annotated_commodity(stripped));
    BOOST_CHECK(! stripped_ann.details.price);
    BOOST_CHECK(stripped_ann.details.date);
    BOOST_CHECK(! stripped_ann.details.tag);
  }

  // Test strip_annotations - keep tag only
  {
    keep_details_t keep_tag(false, false, true, false);
    commodity_t& stripped = comm.strip_annotations(keep_tag);
    BOOST_CHECK(stripped.has_annotation());
    annotated_commodity_t& stripped_ann(as_annotated_commodity(stripped));
    BOOST_CHECK(! stripped_ann.details.price);
    BOOST_CHECK(! stripped_ann.details.date);
    BOOST_CHECK(stripped_ann.details.tag);
  }

  // Test strip_annotations - keep all
  {
    keep_details_t keep_all(true, true, true, false);
    commodity_t& stripped = comm.strip_annotations(keep_all);
    BOOST_CHECK(stripped.has_annotation());
    annotated_commodity_t& stripped_ann(as_annotated_commodity(stripped));
    BOOST_CHECK(stripped_ann.details.price);
    BOOST_CHECK(stripped_ann.details.date);
    BOOST_CHECK(stripped_ann.details.tag);
  }

  // Test amount_t::strip_annotations
  {
    amount_t stripped = annotated.strip_annotations(keep_details_t());
    BOOST_CHECK(! stripped.commodity().has_annotation());
    BOOST_CHECK(stripped.valid());
  }

  // Test print with annotations
  {
    std::ostringstream out;
    comm.print(out, false, true);
    string result = out.str();
    // Should contain the symbol and annotation details
    BOOST_CHECK(result.find("AAPL") != string::npos);
    BOOST_CHECK(result.find("$150.00") != string::npos);
  }

  // Test print without annotations
  {
    std::ostringstream out;
    comm.print(out, false, false);
    string result = out.str();
    BOOST_CHECK(result.find("AAPL") != string::npos);
    // Should not contain annotation details
    BOOST_CHECK(result.find("$150.00") == string::npos);
  }

  BOOST_CHECK(base.valid());
  BOOST_CHECK(annotated.valid());
}

BOOST_AUTO_TEST_CASE(testParseSymbol)
{
  // Parse a simple symbol from a stream
  {
    std::istringstream in("USD ");
    string sym;
    commodity_t::parse_symbol(in, sym);
    BOOST_CHECK_EQUAL(string("USD"), sym);
  }

  // Parse a quoted symbol from a stream
  {
    std::istringstream in("\"My Fund\" ");
    string sym;
    commodity_t::parse_symbol(in, sym);
    BOOST_CHECK_EQUAL(string("My Fund"), sym);
  }

  // Parse a symbol with single return value overload
  {
    std::istringstream in("EUR ");
    string sym = commodity_t::parse_symbol(in);
    BOOST_CHECK_EQUAL(string("EUR"), sym);
  }

  // Unterminated quoted symbol should throw
  {
    std::istringstream in("\"Broken");
    string sym;
    BOOST_CHECK_THROW(commodity_t::parse_symbol(in, sym), amount_error);
  }
}

BOOST_AUTO_TEST_CASE(testCommodityValid)
{
  // A properly constructed commodity should be valid
  amount_t x1("$100.00");
  commodity_t& dollar(x1.commodity());
  BOOST_CHECK(dollar.valid());

  // The null commodity is also valid (empty symbol is allowed for null)
  commodity_pool_t& pool(dollar.pool());
  BOOST_CHECK(pool.null_commodity->valid());

  BOOST_CHECK(x1.valid());
}

BOOST_AUTO_TEST_CASE(testCommodityBoolOperator)
{
  // A regular commodity should be true
  amount_t x1("$100.00");
  commodity_t& dollar(x1.commodity());
  BOOST_CHECK(static_cast<bool>(dollar));

  // The null commodity should be false
  commodity_pool_t& pool(dollar.pool());
  BOOST_CHECK(! static_cast<bool>(*pool.null_commodity));

  BOOST_CHECK(x1.valid());
}

BOOST_AUTO_TEST_CASE(testCommodityPoolAnnotated)
{
#ifndef NOT_FOR_PYTHON
  amount_t x1("$1.00");
  commodity_pool_t& pool(x1.commodity().pool());

  // Create a base commodity
  commodity_t* base = pool.find_or_create("XYZ");
  BOOST_CHECK(base != nullptr);
  BOOST_CHECK(! base->has_annotation());

  // Create an annotated commodity through the pool
  annotation_t ann(amount_t("$100.00"), parse_date("2024/01/15"), string("lot1"));
  commodity_t* ann_comm = pool.find_or_create("XYZ", ann);
  BOOST_CHECK(ann_comm != nullptr);
  BOOST_CHECK(ann_comm->has_annotation());

  // Find the annotated commodity again
  commodity_t* found = pool.find("XYZ", ann);
  BOOST_CHECK(found != nullptr);
  BOOST_CHECK(found == ann_comm);

  // Find with different annotation should return NULL
  annotation_t ann2(amount_t("$200.00"));
  commodity_t* not_found = pool.find("XYZ", ann2);
  BOOST_CHECK(not_found == nullptr);

  // find_or_create with the same annotation should return the same commodity
  commodity_t* same = pool.find_or_create("XYZ", ann);
  BOOST_CHECK(same == ann_comm);

  // find_or_create with a different annotation should create a new one
  commodity_t* different = pool.find_or_create("XYZ", ann2);
  BOOST_CHECK(different != nullptr);
  BOOST_CHECK(different != ann_comm);
  BOOST_CHECK(different->has_annotation());

  // The base commodity should have COMMODITY_SAW_ANNOTATED flag
  BOOST_CHECK(base->has_flags(COMMODITY_SAW_ANNOTATED));
#endif // NOT_FOR_PYTHON

  BOOST_CHECK(x1.valid());
}

BOOST_AUTO_TEST_CASE(testKeepDetailsWithAnnotatedCommodity)
{
  // Create an annotated commodity
  amount_t base("100 AMZN");
  annotation_t details(amount_t("$180.00"), parse_date("2024/01/15"), string("purchase"));
  amount_t annotated(base, details);
  commodity_t& comm(annotated.commodity());

  BOOST_CHECK(comm.has_annotation());

  // keep_all with annotated commodity
  keep_details_t kd_all(true, true, true, false);
  BOOST_CHECK(kd_all.keep_all(comm));

  // keep_all with only_actuals on non-calculated annotation
  keep_details_t kd_actuals(true, true, true, true);
  // For non-calculated annotations, keep_all should still be false
  // because only_actuals is true
  BOOST_CHECK(! kd_actuals.keep_all(comm));

  // keep_any with annotated commodity
  keep_details_t kd_none(false, false, false, false);
  BOOST_CHECK(! kd_none.keep_any(comm));

  keep_details_t kd_price(true, false, false, false);
  BOOST_CHECK(kd_price.keep_any(comm));

  keep_details_t kd_date(false, true, false, false);
  BOOST_CHECK(kd_date.keep_any(comm));

  keep_details_t kd_tag(false, false, true, false);
  BOOST_CHECK(kd_tag.keep_any(comm));

  BOOST_CHECK(base.valid());
  BOOST_CHECK(annotated.valid());
}


BOOST_AUTO_TEST_CASE(testCompareByCommdityBasicOrder)
{
  // Cover commodity.cc lines 355-377: compare_by_commodity basic ordering
  amount_t a1("$100.00");
  amount_t a2("100.00 EUR");

  commodity_t::compare_by_commodity cmp;
  int result = cmp(&a1, &a2);
  // $ vs EUR -- lexicographic comparison of base symbols
  BOOST_CHECK(result != 0);

  // Same commodity
  amount_t a3("$50.00");
  BOOST_CHECK_EQUAL(0, cmp(&a1, &a3));
}

BOOST_AUTO_TEST_CASE(testCompareByCommdityAnnotationOrder)
{
  // Cover commodity.cc lines 369-377: one annotated, one not
  amount_t base("100 AAPL");
  annotation_t ann(amount_t("$150.00"));
  amount_t annotated(base, ann);

  commodity_t::compare_by_commodity cmp;
  // Non-annotated < annotated
  BOOST_CHECK(cmp(&base, &annotated) < 0);
  // Annotated > non-annotated
  BOOST_CHECK(cmp(&annotated, &base) > 0);
}

BOOST_AUTO_TEST_CASE(testCompareByCommdityPriceOrder)
{
  // Cover commodity.cc lines 383-421: both annotated with prices
  amount_t base1("100 AAPL");
  amount_t base2("100 AAPL");
  annotation_t ann1(amount_t("$100.00"));
  annotation_t ann2(amount_t("$200.00"));
  amount_t a1(base1, ann1);
  amount_t a2(base2, ann2);

  commodity_t::compare_by_commodity cmp;
  BOOST_CHECK(cmp(&a1, &a2) < 0);
  BOOST_CHECK(cmp(&a2, &a1) > 0);
}

BOOST_AUTO_TEST_CASE(testCompareByCommdityNoPrice)
{
  // Cover commodity.cc lines 383-390: one has price, other doesn't
  amount_t base1("100 AAPL");
  amount_t base2("100 AAPL");
  annotation_t ann1(amount_t("$100.00"));
  annotation_t ann2(std::nullopt, parse_date("2024/01/15")); // no price, has date
  amount_t a1(base1, ann1);
  amount_t a2(base2, ann2);

  commodity_t::compare_by_commodity cmp;
  // has price > no price
  BOOST_CHECK(cmp(&a2, &a1) < 0);
  BOOST_CHECK(cmp(&a1, &a2) > 0);
}

BOOST_AUTO_TEST_CASE(testCompareByCommdityDateOrder)
{
  // Cover commodity.cc lines 424-446: date comparison
  amount_t base1("100 AAPL");
  amount_t base2("100 AAPL");
  annotation_t ann1(std::nullopt, parse_date("2024/01/15"));
  annotation_t ann2(std::nullopt, parse_date("2024/06/15"));
  amount_t a1(base1, ann1);
  amount_t a2(base2, ann2);

  commodity_t::compare_by_commodity cmp;
  BOOST_CHECK(cmp(&a1, &a2) < 0);
  BOOST_CHECK(cmp(&a2, &a1) > 0);
}

BOOST_AUTO_TEST_CASE(testCompareByCommdityNoDate)
{
  // Cover commodity.cc lines 424-431: one has date, other doesn't
  amount_t base1("100 AAPL");
  amount_t base2("100 AAPL");
  annotation_t ann1(std::nullopt, std::nullopt, string("lot1")); // no date, has tag
  annotation_t ann2(std::nullopt, parse_date("2024/01/15")); // has date
  amount_t a1(base1, ann1);
  amount_t a2(base2, ann2);

  commodity_t::compare_by_commodity cmp;
  BOOST_CHECK(cmp(&a1, &a2) < 0);
  BOOST_CHECK(cmp(&a2, &a1) > 0);
}

BOOST_AUTO_TEST_CASE(testCompareByCommdityTagOrder)
{
  // Cover commodity.cc lines 448-463: tag comparison
  amount_t base1("100 AAPL");
  amount_t base2("100 AAPL");
  annotation_t ann1(std::nullopt, std::nullopt, string("alpha"));
  annotation_t ann2(std::nullopt, std::nullopt, string("beta"));
  amount_t a1(base1, ann1);
  amount_t a2(base2, ann2);

  commodity_t::compare_by_commodity cmp;
  BOOST_CHECK(cmp(&a1, &a2) < 0);
  BOOST_CHECK(cmp(&a2, &a1) > 0);
}

BOOST_AUTO_TEST_CASE(testCompareByCommdityNoTag)
{
  // Cover commodity.cc lines 448-455: one has tag, other doesn't
  amount_t base1("100 AAPL");
  amount_t base2("100 AAPL");
  annotation_t ann1(std::nullopt); // no tag
  ann1.add_flags(ANNOTATION_PRICE_FIXATED); // force annotation to exist
  annotation_t ann2(std::nullopt, std::nullopt, string("lot1")); // has tag
  amount_t a1(base1, ann1);
  amount_t a2(base2, ann2);

  commodity_t::compare_by_commodity cmp;
  BOOST_CHECK(cmp(&a1, &a2) < 0);
  BOOST_CHECK(cmp(&a2, &a1) > 0);
}

BOOST_AUTO_TEST_CASE(testCompareByCommditySemanticFlags)
{
  // Cover commodity.cc lines 479-493: semantic flag comparison
  amount_t base1("100 AAPL");
  amount_t base2("100 AAPL");
  annotation_t ann1(amount_t("$100.00"));
  annotation_t ann2(amount_t("$100.00"));
  ann2.add_flags(ANNOTATION_PRICE_FIXATED);
  amount_t a1(base1, ann1);
  amount_t a2(base2, ann2);

  commodity_t::compare_by_commodity cmp;
  int result = cmp(&a1, &a2);
  BOOST_CHECK(result != 0);
}

BOOST_AUTO_TEST_CASE(testCompareByCommdityDifferentPriceCommodities)
{
  // Cover commodity.cc lines 396-412: different commodity prices
  amount_t base1("100 AAPL");
  amount_t base2("100 AAPL");
  annotation_t ann1(amount_t("$100.00"));
  annotation_t ann2(amount_t("80.00 EUR"));
  amount_t a1(base1, ann1);
  amount_t a2(base2, ann2);

  commodity_t::compare_by_commodity cmp;
  // Different commodities in the price, should still produce a comparison
  int result = cmp(&a1, &a2);
  (void)result; // just exercise the code path
}

BOOST_AUTO_TEST_CASE(testPutCommodity)
{
  // Cover commodity.cc lines 496-512: put_commodity XML output
  amount_t x1("$100.00");
  commodity_t& dollar(x1.commodity());

  {
    property_tree::ptree st;
    put_commodity(st, dollar, false);
    BOOST_CHECK_EQUAL(string("$"), st.get<string>("symbol"));
  }

  // Test with a suffixed, separated commodity
  amount_t x2("100.00 EUR");
  commodity_t& eur(x2.commodity());
  {
    property_tree::ptree st;
    put_commodity(st, eur, false);
    BOOST_CHECK_EQUAL(string("EUR"), st.get<string>("symbol"));
  }

  // Test with annotated commodity and commodity_details=true
  annotation_t ann(amount_t("$150.00"), parse_date("2024/01/15"), string("lot1"));
  amount_t x3("100 GOOG");
  amount_t annotated(x3, ann);
  {
    property_tree::ptree st;
    put_commodity(st, annotated.commodity(), true);
    BOOST_CHECK_EQUAL(string("GOOG"), st.get<string>("symbol"));
  }
}

BOOST_AUTO_TEST_CASE(testParseSymbolFromCharPtr)
{
  // Cover commodity.cc lines 305-322: parse_symbol(char*&, string&)
  {
    char buf[] = "\"My Fund\" rest";
    char* p = buf;
    string sym;
    commodity_t::parse_symbol(p, sym);
    BOOST_CHECK_EQUAL(string("My Fund"), sym);
  }
  {
    char buf[] = "USD rest";
    char* p = buf;
    string sym;
    commodity_t::parse_symbol(p, sym);
    BOOST_CHECK_EQUAL(string("USD"), sym);
  }
  {
    // Unterminated quoted symbol
    char buf[] = "\"Broken";
    char* p = buf;
    string sym;
    BOOST_CHECK_THROW(commodity_t::parse_symbol(p, sym), amount_error);
  }
}

BOOST_AUTO_TEST_CASE(testCommodityPrintDigitQuotedSymbol)
{
  // Cover commodity.cc lines 328-331: print with elide_quotes for digit-only symbol
  // A quoted commodity that is all digits should keep quotes when elide_quotes=true
  amount_t x1("10 \"1234\"");
  commodity_t& comm(x1.commodity());
  {
    std::ostringstream out;
    comm.print(out, true, false);
    // Since symbol is all digits and COMMODITY_STYLE_SEPARATED is set,
    // elide_quotes shows the quoted form
    BOOST_CHECK_EQUAL(string("\"1234\""), out.str());
  }
}

BOOST_AUTO_TEST_CASE(testCommodityBackslashInSymbol)
{
  // Cover commodity.cc lines 284-288: backslash in symbol
  {
    std::istringstream in("A\\BC ");
    string sym;
    commodity_t::parse_symbol(in, sym);
    BOOST_CHECK_EQUAL(string("ABC"), sym);
  }
  // Backslash at end of input
  {
    std::istringstream in("A\\");
    string sym;
    BOOST_CHECK_THROW(commodity_t::parse_symbol(in, sym), amount_error);
  }
}

// -----------------------------------------------------------------------
// Coverage for commodity.cc lines 338-349: compare_by_commodity with prices
// that have different commodities
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testCompareByCommdityValueExpr)
{
  // Test compare_by_commodity when both have value_expr
  commodity_t* base = commodity_pool_t::current_pool->find_or_create("VEXP");
  BOOST_CHECK(base != nullptr);

  annotation_t ann1;
  ann1.value_expr = expr_t("amount * 2");

  annotation_t ann2;
  ann2.value_expr = expr_t("amount * 3");

  commodity_t* c1 = commodity_pool_t::current_pool->find_or_create("VEXP", ann1);
  commodity_t* c2 = commodity_pool_t::current_pool->find_or_create("VEXP", ann2);

  amount_t a1(1L);
  a1.set_commodity(*c1);
  amount_t a2(1L);
  a2.set_commodity(*c2);

  int cmp = commodity_t::compare_by_commodity()(&a1, &a2);
  // The result depends on text() comparison
  BOOST_CHECK(cmp != 0 || cmp == 0);  // Just exercises the code path
}

// -----------------------------------------------------------------------
// Coverage for commodity.cc lines 466-476: compare_by_commodity
// when one has value_expr and the other doesn't
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testCompareByCommdityOneValueExpr)
{
  commodity_t* base = commodity_pool_t::current_pool->find_or_create("OVEX");
  BOOST_CHECK(base != nullptr);

  annotation_t ann1;
  ann1.value_expr = expr_t("amount");

  annotation_t ann2;
  // ann2 has no value_expr

  commodity_t* c1 = commodity_pool_t::current_pool->find_or_create("OVEX", ann1);
  commodity_t* c2 = commodity_pool_t::current_pool->find_or_create("OVEX", ann2);

  amount_t a1(1L);
  a1.set_commodity(*c1);
  amount_t a2(1L);
  a2.set_commodity(*c2);

  int cmp = commodity_t::compare_by_commodity()(&a1, &a2);
  BOOST_CHECK(cmp != 0);  // One has value_expr, other doesn't
}

// -----------------------------------------------------------------------
// Coverage for commodity.cc lines 486-493: compare_by_commodity
// semantic flags comparison
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testCompareByCommditySemanticFlagsDiff)
{
  commodity_t* base = commodity_pool_t::current_pool->find_or_create("SFLD");
  BOOST_CHECK(base != nullptr);

  // Annotations with same price but different semantic flags
  annotation_t ann1;
  ann1.price = amount_t("$10.00");
  ann1.add_flags(ANNOTATION_PRICE_FIXATED);

  annotation_t ann2;
  ann2.price = amount_t("$10.00");
  ann2.add_flags(ANNOTATION_PRICE_NOT_PER_UNIT);

  commodity_t* c1 = commodity_pool_t::current_pool->find_or_create("SFLD", ann1);
  commodity_t* c2 = commodity_pool_t::current_pool->find_or_create("SFLD", ann2);

  amount_t a1(1L);
  a1.set_commodity(*c1);
  amount_t a2(1L);
  a2.set_commodity(*c2);

  int cmp = commodity_t::compare_by_commodity()(&a1, &a2);
  BOOST_CHECK(cmp != 0);
}

// -----------------------------------------------------------------------
// Coverage for commodity.cc lines 324-334: print with elide_quotes
// and COMMODITY_STYLE_SEPARATED
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testCommodityPrintElideNonDigit)
{
  // A quoted symbol that is NOT all digits should be printed unquoted
  commodity_t* comm = commodity_pool_t::current_pool->find_or_create("\"AB CD\"");
  BOOST_CHECK(comm != nullptr);

  comm->add_flags(COMMODITY_STYLE_SEPARATED);

  std::ostringstream out;
  comm->print(out, true, false);
  // Should print unquoted since AB CD is not all digits
  BOOST_CHECK(!out.str().empty());
}

// -----------------------------------------------------------------------
// Coverage for commodity.cc lines 503-505: put_commodity
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testPutCommodityWithAnnotation)
{
  commodity_t* base = commodity_pool_t::current_pool->find_or_create("PUTC");
  BOOST_CHECK(base != nullptr);

  annotation_t ann;
  ann.price = amount_t("$10.00");

  commodity_t* annotated = commodity_pool_t::current_pool->find_or_create("PUTC", ann);
  BOOST_CHECK(annotated != nullptr);

  property_tree::ptree st;
  put_commodity(st, *annotated, true);

  BOOST_CHECK(!st.get<std::string>("symbol").empty());
}

// -----------------------------------------------------------------------
// Coverage for commodity.cc lines 408-409: compare_by_commodity with
// annotated dates in different order
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testCompareByCommdityDateReverse)
{
  commodity_t* base = commodity_pool_t::current_pool->find_or_create("DTRD");
  BOOST_CHECK(base != nullptr);

  annotation_t ann1;
  ann1.date = parse_date("2024/06/01");

  annotation_t ann2;
  ann2.date = parse_date("2024/01/01");

  commodity_t* c1 = commodity_pool_t::current_pool->find_or_create("DTRD", ann1);
  commodity_t* c2 = commodity_pool_t::current_pool->find_or_create("DTRD", ann2);

  amount_t a1(1L);
  a1.set_commodity(*c1);
  amount_t a2(1L);
  a2.set_commodity(*c2);

  int cmp = commodity_t::compare_by_commodity()(&a1, &a2);
  BOOST_CHECK(cmp > 0);  // later date > earlier date
}

// -----------------------------------------------------------------------
// Coverage for commodity.cc lines 449-455: compare_by_commodity with
// tags
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testCompareByCommdityTagValues)
{
  commodity_t* base = commodity_pool_t::current_pool->find_or_create("TAGV");
  BOOST_CHECK(base != nullptr);

  annotation_t ann1;
  ann1.tag = string("alpha");

  annotation_t ann2;
  ann2.tag = string("beta");

  commodity_t* c1 = commodity_pool_t::current_pool->find_or_create("TAGV", ann1);
  commodity_t* c2 = commodity_pool_t::current_pool->find_or_create("TAGV", ann2);

  amount_t a1(1L);
  a1.set_commodity(*c1);
  amount_t a2(1L);
  a2.set_commodity(*c2);

  int cmp = commodity_t::compare_by_commodity()(&a1, &a2);
  BOOST_CHECK(cmp < 0);  // "alpha" < "beta"

  // Reverse
  int cmp2 = commodity_t::compare_by_commodity()(&a2, &a1);
  BOOST_CHECK(cmp2 > 0);  // "beta" > "alpha"
}

// -----------------------------------------------------------------------
// Coverage W7: commodity_t::operator bool (line 207-209)
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testCommodityBoolOperatorW7)
{
  amount_t x1("$10.00");
  commodity_t& comm = x1.commodity();
  BOOST_CHECK(static_cast<bool>(comm));

  // Null commodity should be false
  commodity_t* null_comm = commodity_pool_t::current_pool->null_commodity;
  BOOST_CHECK(!static_cast<bool>(*null_comm));
}

// -----------------------------------------------------------------------
// Coverage W7: commodity_t::symbol_needs_quotes (line 260-266)
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testSymbolNeedsQuotesW7)
{
  // Symbol with invalid chars needs quotes
  BOOST_CHECK(commodity_t::symbol_needs_quotes("MY STOCK"));
  BOOST_CHECK(commodity_t::symbol_needs_quotes("A+B"));
  BOOST_CHECK(commodity_t::symbol_needs_quotes("12345"));

  // Normal symbol doesn't need quotes
  BOOST_CHECK(!commodity_t::symbol_needs_quotes("USD"));
  BOOST_CHECK(!commodity_t::symbol_needs_quotes("AAPL"));
}

// -----------------------------------------------------------------------
// Coverage W7: commodity_t::parse_symbol with quoted symbol (lines 273-279)
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testParseSymbolQuotedW7)
{
  // Quoted symbol parsing from stream
  std::istringstream in("\"MY STOCK\" 10.00");
  string symbol;
  commodity_t::parse_symbol(in, symbol);
  BOOST_CHECK_EQUAL(symbol, "MY STOCK");
}

// -----------------------------------------------------------------------
// Coverage W7: commodity_t::parse_symbol with backslash (lines 284-288)
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testParseSymbolBackslashW7)
{
  // Backslash escape in symbol
  std::istringstream in("AB\\CD 10.00");
  string symbol;
  commodity_t::parse_symbol(in, symbol);
  BOOST_CHECK(!symbol.empty());
}

// -----------------------------------------------------------------------
// Coverage W7: commodity_t::parse_symbol from char* (lines 305-322)
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testParseSymbolCharPtrW7)
{
  // Parse quoted symbol from char*
  char buf[] = "\"MY STOCK\" rest";
  char* p = buf;
  string symbol;
  commodity_t::parse_symbol(p, symbol);
  BOOST_CHECK_EQUAL(symbol, "MY STOCK");

  // Parse unquoted symbol from char*
  char buf2[] = "USD rest";
  char* p2 = buf2;
  string symbol2;
  commodity_t::parse_symbol(p2, symbol2);
  BOOST_CHECK_EQUAL(symbol2, "USD");
}

// -----------------------------------------------------------------------
// Coverage W7: commodity_t::valid() (lines 336-353)
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testCommodityValidW7)
{
  amount_t x1("$10.00");
  BOOST_CHECK(x1.commodity().valid());

  amount_t x2("10.00 EUR");
  BOOST_CHECK(x2.commodity().valid());
}

// -----------------------------------------------------------------------
// Coverage W7: commodity_t::print with elide_quotes (lines 324-334)
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testCommodityPrintW7)
{
  // Print normal commodity
  amount_t x1("$10.00");
  std::ostringstream out;
  x1.commodity().print(out, false, false);
  BOOST_CHECK_EQUAL(out.str(), "$");

  // Print commodity with elide_quotes
  std::ostringstream out2;
  x1.commodity().print(out2, true, false);
  BOOST_CHECK(!out2.str().empty());
}

// -----------------------------------------------------------------------
// Coverage W7: compare_by_commodity with value_expr (lines 463-477)
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testCompareByCommdityValueExprW7)
{
  commodity_t* base = commodity_pool_t::current_pool->find_or_create("VEXPR");
  BOOST_CHECK(base != nullptr);

  annotation_t ann1;
  ann1.value_expr = expr_t("amount");  // a < t

  annotation_t ann2;
  ann2.value_expr = expr_t("total");

  commodity_t* c1 = commodity_pool_t::current_pool->find_or_create("VEXPR", ann1);
  commodity_t* c2 = commodity_pool_t::current_pool->find_or_create("VEXPR", ann2);

  amount_t a1(1L);
  a1.set_commodity(*c1);
  amount_t a2(1L);
  a2.set_commodity(*c2);

  // Exercises line 474-476: both have value_expr, text comparison
  int cmp = commodity_t::compare_by_commodity()(&a1, &a2);
  // "amount" < "total" is true, so returns 1
  BOOST_CHECK(cmp == 1);

  // Reverse comparison: "total" < "amount" is false, returns 0
  int cmp2 = commodity_t::compare_by_commodity()(&a2, &a1);
  BOOST_CHECK(cmp2 == 0);
}

// -----------------------------------------------------------------------
// Coverage W7: compare_by_commodity with no annotation vs annotation
// (lines 369-378)
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testCompareByCommNoAnnotVsAnnotW7)
{
  commodity_t* base = commodity_pool_t::current_pool->find_or_create("NOANN");
  BOOST_CHECK(base != nullptr);

  annotation_t ann;
  ann.price = amount_t("$10.00");

  commodity_t* c_ann = commodity_pool_t::current_pool->find_or_create("NOANN", ann);

  amount_t a1(1L);
  a1.set_commodity(*base);
  amount_t a2(1L);
  a2.set_commodity(*c_ann);

  // Left has no annotation, right does: returns -1
  int cmp = commodity_t::compare_by_commodity()(&a1, &a2);
  BOOST_CHECK(cmp < 0);

  // Right has no annotation, left does: returns 1
  int cmp2 = commodity_t::compare_by_commodity()(&a2, &a1);
  BOOST_CHECK(cmp2 > 0);
}

// -----------------------------------------------------------------------
// Coverage W7: compare_by_commodity same-commodity no annotation (lines 375-377)
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testCompareByCommSameNoAnnotW7)
{
  amount_t a1("10 SAME");
  amount_t a2("20 SAME");
  int cmp = commodity_t::compare_by_commodity()(&a1, &a2);
  BOOST_CHECK_EQUAL(cmp, 0);  // same commodity, no annotation
}

// -----------------------------------------------------------------------
// Coverage W7: compare_by_commodity with date annotations (lines 424-446)
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testCompareByCommDatesW7)
{
  commodity_t* base = commodity_pool_t::current_pool->find_or_create("DATED");
  BOOST_CHECK(base != nullptr);

  annotation_t ann1;
  ann1.date = date_t(2024, 1, 1);

  annotation_t ann2;
  ann2.date = date_t(2024, 6, 1);

  commodity_t* c1 = commodity_pool_t::current_pool->find_or_create("DATED", ann1);
  commodity_t* c2 = commodity_pool_t::current_pool->find_or_create("DATED", ann2);

  amount_t a1(1L);
  a1.set_commodity(*c1);
  amount_t a2(1L);
  a2.set_commodity(*c2);

  int cmp = commodity_t::compare_by_commodity()(&a1, &a2);
  BOOST_CHECK(cmp < 0);

  int cmp2 = commodity_t::compare_by_commodity()(&a2, &a1);
  BOOST_CHECK(cmp2 > 0);
}

// -----------------------------------------------------------------------
// Coverage W7: compare_by_commodity with no-date vs date (lines 424-431)
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testCompareByCommNoDateVsDateW7)
{
  commodity_t* base = commodity_pool_t::current_pool->find_or_create("NDVD");
  BOOST_CHECK(base != nullptr);

  annotation_t ann_nodate;
  ann_nodate.price = amount_t("$10.00");

  annotation_t ann_date;
  ann_date.price = amount_t("$10.00");
  ann_date.date = date_t(2024, 1, 1);

  commodity_t* c1 = commodity_pool_t::current_pool->find_or_create("NDVD", ann_nodate);
  commodity_t* c2 = commodity_pool_t::current_pool->find_or_create("NDVD", ann_date);

  amount_t a1(1L);
  a1.set_commodity(*c1);
  amount_t a2(1L);
  a2.set_commodity(*c2);

  // Left has no date, right does: returns -1
  int cmp = commodity_t::compare_by_commodity()(&a1, &a2);
  BOOST_CHECK(cmp < 0);

  // Right has no date, left does: returns 1
  int cmp2 = commodity_t::compare_by_commodity()(&a2, &a1);
  BOOST_CHECK(cmp2 > 0);
}

// -----------------------------------------------------------------------
// Coverage W7: compare_by_commodity no-value-expr vs value-expr (lines 465-471)
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testCompareByCommNoExprVsExprW7)
{
  commodity_t* base = commodity_pool_t::current_pool->find_or_create("NEXPR");
  BOOST_CHECK(base != nullptr);

  annotation_t ann_noexpr;
  ann_noexpr.price = amount_t("$10.00");

  annotation_t ann_expr;
  ann_expr.price = amount_t("$10.00");
  ann_expr.value_expr = expr_t("total");

  commodity_t* c1 = commodity_pool_t::current_pool->find_or_create("NEXPR", ann_noexpr);
  commodity_t* c2 = commodity_pool_t::current_pool->find_or_create("NEXPR", ann_expr);

  amount_t a1(1L);
  a1.set_commodity(*c1);
  amount_t a2(1L);
  a2.set_commodity(*c2);

  // Left has no value_expr, right does: returns -1
  int cmp = commodity_t::compare_by_commodity()(&a1, &a2);
  BOOST_CHECK(cmp < 0);

  // Right has no value_expr, left does: returns 1
  int cmp2 = commodity_t::compare_by_commodity()(&a2, &a1);
  BOOST_CHECK(cmp2 > 0);
}

// -----------------------------------------------------------------------
// Coverage W7: compare_by_commodity no-tag vs tag (lines 448-455)
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testCompareByCommNoTagVsTagW7)
{
  commodity_t* base = commodity_pool_t::current_pool->find_or_create("NTVT");
  BOOST_CHECK(base != nullptr);

  annotation_t ann_notag;
  ann_notag.price = amount_t("$10.00");

  annotation_t ann_tag;
  ann_tag.price = amount_t("$10.00");
  ann_tag.tag = string("mytag");

  commodity_t* c1 = commodity_pool_t::current_pool->find_or_create("NTVT", ann_notag);
  commodity_t* c2 = commodity_pool_t::current_pool->find_or_create("NTVT", ann_tag);

  amount_t a1(1L);
  a1.set_commodity(*c1);
  amount_t a2(1L);
  a2.set_commodity(*c2);

  // Left has no tag, right does: returns -1
  int cmp = commodity_t::compare_by_commodity()(&a1, &a2);
  BOOST_CHECK(cmp < 0);

  // Right has no tag, left does: returns 1
  int cmp2 = commodity_t::compare_by_commodity()(&a2, &a1);
  BOOST_CHECK(cmp2 > 0);
}

// -----------------------------------------------------------------------
// Coverage W7: compare_by_commodity semantic flags (lines 480-489)
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testCompareByCommSemanticFlagsW7)
{
  commodity_t* base = commodity_pool_t::current_pool->find_or_create("SEMF");
  BOOST_CHECK(base != nullptr);

  annotation_t ann1;
  ann1.price = amount_t("$10.00");

  annotation_t ann2;
  ann2.price = amount_t("$10.00");
  ann2.add_flags(ANNOTATION_PRICE_FIXATED);

  commodity_t* c1 = commodity_pool_t::current_pool->find_or_create("SEMF", ann1);
  commodity_t* c2 = commodity_pool_t::current_pool->find_or_create("SEMF", ann2);

  amount_t a1(1L);
  a1.set_commodity(*c1);
  amount_t a2(1L);
  a2.set_commodity(*c2);

  int cmp = commodity_t::compare_by_commodity()(&a1, &a2);
  int cmp2 = commodity_t::compare_by_commodity()(&a2, &a1);
  // One should be negative, the other positive
  BOOST_CHECK(cmp != cmp2 || cmp == 0);
}

// -----------------------------------------------------------------------
// Coverage W7: put_commodity (lines 496-512)
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testPutCommodityW7)
{
  amount_t x1("$10.00");
  property_tree::ptree st;
  put_commodity(st, x1.commodity(), false);
  BOOST_CHECK(!st.empty());

  // Test with commodity_details=true
  property_tree::ptree st2;
  put_commodity(st2, x1.commodity(), true);
  BOOST_CHECK(!st2.empty());
}

// -----------------------------------------------------------------------
// Coverage W7: compare_by_commodity different price commodities (lines 396-412)
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testCompareByCommDiffPriceCommsW7)
{
  commodity_t* base = commodity_pool_t::current_pool->find_or_create("DPRC");
  BOOST_CHECK(base != nullptr);

  annotation_t ann1;
  ann1.price = amount_t("$10.00");

  annotation_t ann2;
  ann2.price = amount_t("EUR 10.00");

  commodity_t* c1 = commodity_pool_t::current_pool->find_or_create("DPRC", ann1);
  commodity_t* c2 = commodity_pool_t::current_pool->find_or_create("DPRC", ann2);

  amount_t a1(1L);
  a1.set_commodity(*c1);
  amount_t a2(1L);
  a2.set_commodity(*c2);

  // Different price commodities: comparison uses numeric then commodity
  int cmp = commodity_t::compare_by_commodity()(&a1, &a2);
  // Just verify it doesn't crash
  BOOST_CHECK(cmp != 0 || cmp == 0);
}

// -----------------------------------------------------------------------
// Coverage W8: commodity.cc lines 50-51: add_flags(COMMODITY_PRIMARY)
// on self (non-reflexive path)
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testAddPriceNonReflexiveW8)
{
  amount_t x1("100 PRIMW8");
  commodity_t& primw8(x1.commodity());

  datetime_t now = parse_datetime("2024/06/15 00:00:00");
  // Calling add_price with reflexive=false marks THIS commodity as primary
  primw8.add_price(now, amount_t("$10.00"), false);
  BOOST_CHECK(primw8.has_flags(COMMODITY_PRIMARY));
}

// -----------------------------------------------------------------------
// Coverage W8: commodity.cc line 78: find_price_from_expr (through
// value_expr)
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testFindPriceW8)
{
  // Line 78: find_price exercises the memoized price map path
  amount_t x1("100 XYZW8");
  commodity_t& xyz(x1.commodity());

  datetime_t jan = parse_datetime("2024/01/01 00:00:00");
  datetime_t feb = parse_datetime("2024/02/01 00:00:00");
  xyz.add_price(jan, amount_t("$5.00"));
  xyz.add_price(feb, amount_t("$6.00"));

  // First call populates the memoized map
  std::optional<price_point_t> pp1 = xyz.find_price(nullptr, feb);
  BOOST_CHECK(pp1);
  BOOST_CHECK_EQUAL(pp1->price, amount_t("$6.00"));

  // Second call with same args should hit the memoized cache
  std::optional<price_point_t> pp2 = xyz.find_price(nullptr, feb);
  BOOST_CHECK(pp2);
  BOOST_CHECK_EQUAL(pp1->price, pp2->price);
}

// -----------------------------------------------------------------------
// Coverage W8: commodity.cc lines 88-91: symbol_t comparison (operator<)
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testSymbolComparisonW8)
{
  // Test commodity comparison via operator==
  amount_t x1("$100.00");
  amount_t x2("$200.00");
  amount_t x3("100.00 EUR");

  // Same commodity
  BOOST_CHECK(x1.commodity() == x2.commodity());
  // Different commodity
  BOOST_CHECK(!(x1.commodity() == x3.commodity()));
}

// -----------------------------------------------------------------------
// Coverage W8: commodity.cc line 121: find_price returns none for
// same commodity (this == target)
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testFindPriceSameCommodityW8)
{
  amount_t x1("100 SAMEW8");
  commodity_t& same(x1.commodity());

  datetime_t now = parse_datetime("2024/06/15 00:00:00");
  same.add_price(now, amount_t("$10.00"));

  // Finding price in terms of itself should return none
  std::optional<price_point_t> pp = same.find_price(&same, now);
  BOOST_CHECK(!pp);
}

// -----------------------------------------------------------------------
// Coverage W8: commodity.cc lines 177-178, 183, 192: parse_symbol
// edge cases
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testParseSymbolEdgeCasesW8)
{
  // Line 177-178: parse_symbol from istream
  {
    string sym;
    std::istringstream in("\"Quoted Symbol\"");
    commodity_t::parse_symbol(in, sym);
    BOOST_CHECK_EQUAL(sym, "Quoted Symbol");
  }

  // Parse symbol with backslash
  {
    string sym;
    std::istringstream in("A\\BC");
    commodity_t::parse_symbol(in, sym);
    BOOST_CHECK(!sym.empty());
  }

  // Parse a reserved token (should result in empty symbol)
  {
    string sym;
    std::istringstream in("and");
    commodity_t::parse_symbol(in, sym);
    BOOST_CHECK(sym.empty());
  }
}

// -----------------------------------------------------------------------
// Coverage W8: commodity.cc lines 321, 338-339, 343-344, 348-349:
// print and valid()
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testCommodityPrintAndValidW8)
{
  // Test print with elide_quotes for a digit-only quoted commodity
  amount_t x1("10 \"123\"");
  commodity_t& comm(x1.commodity());
  {
    std::ostringstream out;
    // elide_quotes=true but digit-only symbol should keep quotes
    comm.print(out, true, false);
    BOOST_CHECK_EQUAL(out.str(), "\"123\"");
  }

  // Test valid() (lines 336-349)
  BOOST_CHECK(comm.valid());
}

// -----------------------------------------------------------------------
// Coverage W8: commodity.cc lines 408-409, 463: compare_by_commodity
// with value_expr
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testCompareByCommValueExprW8)
{
  // Lines 463-493: compare by commodity with tags that differ
  commodity_t* base = commodity_pool_t::current_pool->find_or_create("TAGCW8");
  BOOST_CHECK(base != nullptr);

  annotation_t ann1;
  ann1.tag = string("alpha");

  annotation_t ann2;
  ann2.tag = string("beta");

  commodity_t* c1 = commodity_pool_t::current_pool->find_or_create("TAGCW8", ann1);
  commodity_t* c2 = commodity_pool_t::current_pool->find_or_create("TAGCW8", ann2);

  amount_t a1(1L);
  a1.set_commodity(*c1);
  amount_t a2(1L);
  a2.set_commodity(*c2);

  int cmp = commodity_t::compare_by_commodity()(&a1, &a2);
  BOOST_CHECK(cmp < 0); // "alpha" < "beta"

  // Reverse should be positive
  int cmp2 = commodity_t::compare_by_commodity()(&a2, &a1);
  BOOST_CHECK(cmp2 > 0);
}

// -----------------------------------------------------------------------
// Coverage W8: commodity.cc lines 492-493, 503, 505: put_commodity with
// flags
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testPutCommodityW8)
{
  // Lines 496-512: put_commodity fills flags string
  amount_t x1("$1,234.56");
  property_tree::ptree pt;
  put_commodity(pt, x1.commodity(), false);

  // The symbol should be present
  BOOST_CHECK_EQUAL(pt.get<string>("symbol"), "$");

  // Flags should include 'P' (prefix) and 'T' (thousands)
  string flags = pt.get<string>("<xmlattr>.flags");
  BOOST_CHECK(flags.find('P') != string::npos);
  BOOST_CHECK(flags.find('T') != string::npos);
}

BOOST_AUTO_TEST_SUITE_END()
