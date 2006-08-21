#ifndef __TESTAMOUNT_H
#define __TESTAMOUNT_H

#include <cxxtest/TestSuite.h>

#include <amount.h>

using namespace ledger;

class TestAmount : public CxxTest::TestSuite
{
public:
    void testCreateAmountWithoutCommodityFromInteger()
    {
	amount_t a((long int)42);
	TS_ASSERT_EQUALS("", a.commodity().symbol());
	TS_ASSERT_EQUALS("", a.commodity().base_symbol());
	TS_ASSERT_EQUALS("42", a.quantity_string());
    }

    void testCreateAmountWithoutCommodity()
    {
	amount_t a("42");
	TS_ASSERT_EQUALS("", a.commodity().symbol());
	TS_ASSERT_EQUALS("", a.commodity().base_symbol());
	TS_ASSERT_EQUALS("42", a.quantity_string());
    }

    void testCreateAmountWithPrefixCommodity()
    {
	amount_t *a;
	a = new amount_t("$ 42");
	TS_ASSERT_EQUALS("$", a->commodity().symbol());
	TS_ASSERT_EQUALS("$", a->commodity().base_symbol());
	TS_ASSERT_EQUALS("42", a->quantity_string());
    }

    void testCreateAmountWithPostfixCommodity()
    {
	amount_t *a;
	a = new amount_t("42 GLD");
	TS_ASSERT_EQUALS("GLD", a->commodity().symbol());
	TS_ASSERT_EQUALS("GLD", a->commodity().base_symbol());
	TS_ASSERT_EQUALS("42", a->quantity_string());
    }

    void testCreateAmountWithPrefixCommodityContainingSpace()
    {
	amount_t *a;
	a = new amount_t("\"one dollar\" 42");
	TS_ASSERT_EQUALS("\"one dollar\"", a->commodity().symbol());
	TS_ASSERT_EQUALS("one dollar", a->commodity().base_symbol());
	TS_ASSERT_EQUALS("42", a->quantity_string());
    }

    void testCreateAmountWithPostfixCommodityContainingSpace()
    {
	amount_t *a;
	a = new amount_t("42 \"swedish crowns\"");
	TS_ASSERT_EQUALS("\"swedish crowns\"", a->commodity().symbol());
	TS_ASSERT_EQUALS("swedish crowns", a->commodity().base_symbol());
	TS_ASSERT_EQUALS("42", a->quantity_string());
    }

    void testCreateAmountWithDirectPrefixCommodity()
    {
	amount_t *a;
	a = new amount_t("$42");
	TS_ASSERT_EQUALS("$", a->commodity().symbol());
	TS_ASSERT_EQUALS("$", a->commodity().base_symbol());
	TS_ASSERT_EQUALS("42", a->quantity_string());
    }

    void testCreateAmountWithDirectPostfixCommodity()
    {
	amount_t *a;
	a = new amount_t("42GLD");
	TS_ASSERT_EQUALS("GLD", a->commodity().symbol());
	TS_ASSERT_EQUALS("GLD", a->commodity().base_symbol());
	TS_ASSERT_EQUALS("42", a->quantity_string());
    }

    void testCannotCreateAmountWithoutQuantity()
    {
	TS_ASSERT_THROWS(new amount_t("$"), amount_error*);
    }

    void testCreateBiiigIntegerAmount()
    {
	amount_t a("12345678901234567890123456789012345678901234567890");
	TS_ASSERT_EQUALS("12345678901234567890123456789012345678901234567890",
			a.quantity_string());
    }

    void testCreateBiiigDecimalAmount()
    {
	amount_t a("12345678.901234567890123456789012345678901234567890");
	TS_ASSERT_EQUALS("12345678.901234567890123456789012345678901234567890",
			a.quantity_string());
    }

    void testCreateCommodityAnnotatedWithEntry()
    {
	amount_t a("10 AAPL (entry 6)");
	TS_ASSERT_EQUALS("10", a.quantity_string());
	commodity_t c = a.commodity();
	TS_ASSERT_EQUALS("AAPL", c.symbol());
	TS_ASSERT_EQUALS("AAPL", c.base_symbol());
	TS_ASSERT(c.annotated);
	//TODO: check entry annotation
    }

    void testCreateCommodityAnnotatedWithEntry2()
    {
	amount_t *a = new amount_t("10 AAPL (entry 6)");
	TS_ASSERT_EQUALS("10", a->quantity_string());
	commodity_t c = a->commodity();
	TS_ASSERT_EQUALS("AAPL", c.symbol());
	TS_ASSERT_EQUALS("AAPL", c.base_symbol());
	TS_ASSERT(c.annotated);
	//TODO: check entry annotation
    }

    void testAddTwoAmountsWithoutCommodity()
    {
	amount_t a("6");
	amount_t b("9");
	TS_ASSERT_EQUALS(* new amount_t((long int)15), a+b);
    }

    void testAddTwoAmountsWithSameCommodity()
    {
	amount_t a("$ 6");
	amount_t b("$ 9");
	TS_ASSERT_EQUALS(* new amount_t("$ 15"), a+b);
    }

    void testCannotAddTwoAmountsWithDifferentCommodities()
    {
	amount_t a("$ 6");
	amount_t b("9 GLD");
	TS_ASSERT_THROWS(a+b, amount_error*);
    }

    void testCompareTwoAmountsWithSameCommodity()
    {
	amount_t a("6 SCOX");
	amount_t b("9 SCOX");
	TS_ASSERT(a < b);
	TS_ASSERT(a <= b);
	TS_ASSERT(!(a > b));
	TS_ASSERT(!(a >= b));
	TS_ASSERT(!(a == b));
    }

    void testCannotCompareTwoAmountsWithDifferentCommodities()
    {
	amount_t a("$ 6");
	amount_t b("9 GLD");

	TS_ASSERT_THROWS(a < b, amount_error*);
	TS_ASSERT_THROWS(a <= b, amount_error*);
	TS_ASSERT_THROWS(a > b, amount_error*);
	TS_ASSERT_THROWS(a >= b, amount_error*);
	TS_ASSERT(!(a == b));
    }
};

#endif // __TESTAMOUNT_H
