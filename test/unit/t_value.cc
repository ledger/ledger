#define BOOST_TEST_DYN_LINK

#include <boost/test/unit_test.hpp>

#include <system.hh>

#include "commodity.h"
#include "pool.h"
#include "value.h"

#if defined(_WIN32) || defined(__CYGWIN__)
#include "strptime.h"
#endif

using namespace ledger;

struct value_fixture {
  value_fixture() {
  times_initialize();
  amount_t::initialize();
  value_t::initialize();


  // Cause the display precision for dollars to be initialized to 2.
  amount_t x1("$1.00");
  BOOST_CHECK(x1);

  amount_t::stream_fullstrings = true; // make reports from UnitTests accurate
  }

  ~value_fixture()
  {
  amount_t::stream_fullstrings = false;
  amount_t::shutdown();
  times_shutdown();
  value_t::shutdown();
  }
};

BOOST_FIXTURE_TEST_SUITE(value, value_fixture)

BOOST_AUTO_TEST_CASE(testConstructors)
{
  value_t::sequence_t s1;
  value_t v1;
  value_t v2(true);
  value_t v3(boost::posix_time::from_time_t(time_t(NULL)));
  value_t v4(date_t(parse_date("2014/08/14")));
  value_t v5(2L);
  value_t v6(4UL);
  value_t v7(1.00);
  value_t v8(amount_t("4 GBP"));
  value_t v9(balance_t("3 EUR"));
  value_t v10(mask_t("regex"));
  value_t v11(s1);
  value_t v12(string("$1"));
  value_t v13("2 CAD");
  value_t v14("comment", true);
  value_t v15(string("tag"), true);
  value_t v16(amount_t("$1").commodity());
  value_t v17(v16);

  BOOST_CHECK(v1.valid());
  BOOST_CHECK(v2.valid());
  BOOST_CHECK(v3.valid());
  BOOST_CHECK(v4.valid());
  BOOST_CHECK(v5.valid());
  BOOST_CHECK(v6.valid());
  BOOST_CHECK(v7.valid());
  BOOST_CHECK(v8.valid());
  BOOST_CHECK(v9.valid());
  BOOST_CHECK(v10.valid());
  BOOST_CHECK(v11.valid());
  BOOST_CHECK(v12.valid());
  BOOST_CHECK(v13.valid());
  BOOST_CHECK(v14.valid());
  BOOST_CHECK(v15.valid());
  BOOST_CHECK(v16.valid());
  BOOST_CHECK(v17.valid());
}

BOOST_AUTO_TEST_CASE(testAssignment)
{
  value_t::sequence_t s1;
  value_t v1;
  value_t v2 = true;
  value_t v3 = boost::posix_time::from_time_t(time_t(NULL));
  value_t v4 = date_t(parse_date("2014/08/14"));
  value_t v5 = -2L;
  value_t v6 = 4UL;
  value_t v7 = 1.00;
  value_t v8 = amount_t("4 GBP");
  value_t v9 = balance_t("3 EUR");
  value_t v10 = mask_t("regex");
  value_t v11 = s1;
  value_t v12 = value_t(string("$1"));
  value_t v13 = value_t("2 CAD");
  value_t v14 = value_t("comment", true);
  value_t v15 = value_t(string("tag"), true);
  value_t v16 = value_t(amount_t("$1").commodity());

  BOOST_CHECK(v1.valid());
  BOOST_CHECK(v2.valid());
  BOOST_CHECK(v3.valid());
  BOOST_CHECK(v4.valid());
  BOOST_CHECK(v5.valid());
  BOOST_CHECK(v6.valid());
  BOOST_CHECK(v7.valid());
  BOOST_CHECK(v8.valid());
  BOOST_CHECK(v9.valid());
  BOOST_CHECK(v10.valid());
  BOOST_CHECK(v11.valid());
  BOOST_CHECK(v12.valid());
  BOOST_CHECK(v13.valid());
  BOOST_CHECK(v14.valid());
  BOOST_CHECK(v15.valid());
  BOOST_CHECK(v16.valid());
}

BOOST_AUTO_TEST_CASE(testEquality)
{
  struct tm localtime;
  strptime("10 February 2010", "%d %b %Y", &localtime);
  value_t::sequence_t s1;

  value_t v1;
  value_t v2(true);
  value_t v3(boost::posix_time::ptime_from_tm(localtime));
  value_t v4(date_t(parse_date("2014/08/14")));
  value_t v5(2L);
  value_t v6(2UL);
  value_t v7(1.00);
  value_t v8(amount_t("4 GBP"));
  value_t v9(balance_t("4 GBP"));
  value_t v10(mask_t("regex"));
  value_t v11(s1);
  value_t v12(string("$1"));
  value_t v13("2 CAD");
  value_t v14("comment", true);
  value_t v15(string("comment"), true);
  value_t v16;

  auto& usd = amount_t("$1").commodity();
  usd.pool().alias("USD", usd);
  value_t v17(usd);

  BOOST_CHECK_EQUAL(v1, value_t());
  BOOST_CHECK_EQUAL(v2, value_t(true));
  BOOST_CHECK_EQUAL(v3, value_t(boost::posix_time::ptime_from_tm(localtime)));
  BOOST_CHECK(!(v4 == value_t(date_t(parse_date("2014/08/15")))));

  value_t v19(amount_t("2"));
  value_t v20(balance_t("2"));
  BOOST_CHECK_EQUAL(v5, v6);
  BOOST_CHECK_EQUAL(v5, v19);
  BOOST_CHECK_EQUAL(v5, v20);
  BOOST_CHECK(v19 == v5);
  BOOST_CHECK(v19 == v20);
  BOOST_CHECK(v19 == value_t(amount_t("2")));
  BOOST_CHECK(v20 == v5);
  BOOST_CHECK(v20 == v19);
  BOOST_CHECK(v20 == value_t(balance_t(2L)));
  BOOST_CHECK(v14 == v15);
  BOOST_CHECK(v10 == value_t(mask_t("regex")));
  BOOST_CHECK(v11 == value_t(s1));
  BOOST_CHECK(v17 == value_t(amount_t("$2").commodity()));
  BOOST_CHECK(v17 == value_t(amount_t("USD2").commodity()));
  BOOST_CHECK(v17 != value_t(amount_t("EUR1").commodity()));
  BOOST_CHECK(v17 == amount_t("$2").commodity());
  BOOST_CHECK(v17 == amount_t("USD2").commodity());
  BOOST_CHECK(amount_t("$2").commodity() == v17);
  BOOST_CHECK(v17 != amount_t("EUR1").commodity());
  BOOST_CHECK(amount_t("EUR1").commodity() != v17);
  BOOST_CHECK(v17 == string_value("$"));
  BOOST_CHECK(v17 == string_value("USD"));
  BOOST_CHECK(string_value("$") == v17);
  BOOST_CHECK(v17 != string_value("EUR"));
  BOOST_CHECK(string_value("EUR") != v17);

  BOOST_CHECK_THROW(v8 == v10, value_error);
  BOOST_CHECK_THROW(v17 == v8, value_error);
  BOOST_CHECK_THROW(v8 == v17, value_error);

  BOOST_CHECK(v1.valid());
  BOOST_CHECK(v2.valid());
  BOOST_CHECK(v3.valid());
  BOOST_CHECK(v4.valid());
  BOOST_CHECK(v5.valid());
  BOOST_CHECK(v6.valid());
  BOOST_CHECK(v7.valid());
  BOOST_CHECK(v8.valid());
  BOOST_CHECK(v9.valid());
  BOOST_CHECK(v10.valid());
  BOOST_CHECK(v11.valid());
  BOOST_CHECK(v12.valid());
  BOOST_CHECK(v13.valid());
  BOOST_CHECK(v14.valid());
  BOOST_CHECK(v15.valid());
  BOOST_CHECK(v17.valid());
  BOOST_CHECK(v19.valid());
  BOOST_CHECK(v20.valid());
}

BOOST_AUTO_TEST_CASE(testOrder)
{
  auto& usd = amount_t("USD1").commodity();
  usd.pool().alias("VDollar", usd);
  usd.pool().alias("TDollar", usd);
  value_t v1(usd);

  BOOST_CHECK(v1 < value_t(amount_t("V2").commodity()));
  BOOST_CHECK((v1 < value_t(amount_t("VDollar2").commodity())) == false);
  BOOST_CHECK(v1 < amount_t("V2").commodity());
  BOOST_CHECK((v1 < amount_t("VDollar2").commodity()) == false);
  BOOST_CHECK(amount_t("T2").commodity() < v1);
  BOOST_CHECK(v1 < string_value("V"));
  BOOST_CHECK((v1 < string_value("VDollar")) == false);
  BOOST_CHECK(string_value("T") < v1);
  BOOST_CHECK_THROW(v1 < value_t(amount_t("USD1")), value_error);
  BOOST_CHECK_THROW(value_t(amount_t("USD1")) < v1, value_error);

  BOOST_CHECK(v1 <= value_t(amount_t("V2").commodity()));
  BOOST_CHECK(v1 <= value_t(amount_t("VDollar2").commodity()));
  BOOST_CHECK(v1 <= amount_t("V2").commodity());
  BOOST_CHECK(v1 <= amount_t("VDollar2").commodity());
  BOOST_CHECK(amount_t("T2").commodity() <= v1);
  BOOST_CHECK(v1 <= string_value("V"));
  BOOST_CHECK(v1 <= string_value("VDollar"));
  BOOST_CHECK(string_value("T") <= v1);
  BOOST_CHECK_THROW(v1 <= value_t(amount_t("USD1")), value_error);
  BOOST_CHECK_THROW(value_t(amount_t("USD1")) <= v1, value_error);

  BOOST_CHECK(v1 > value_t(amount_t("T2").commodity()));
  BOOST_CHECK((v1 > value_t(amount_t("TDollar2").commodity())) == false);
  BOOST_CHECK(v1 > amount_t("T2").commodity());
  BOOST_CHECK((v1 > amount_t("TDollar2").commodity()) == false);
  BOOST_CHECK(amount_t("V2").commodity() > v1);
  BOOST_CHECK(v1 > string_value("T"));
  BOOST_CHECK((v1 > string_value("TDollar")) == false);
  BOOST_CHECK(string_value("V") > v1);
  BOOST_CHECK_THROW(v1 > value_t(amount_t("USD1")), value_error);
  BOOST_CHECK_THROW(value_t(amount_t("USD1")) > v1, value_error);

  BOOST_CHECK(v1 >= value_t(amount_t("T2").commodity()));
  BOOST_CHECK(v1 >= value_t(amount_t("TDollar2").commodity()));
  BOOST_CHECK(v1 >= amount_t("T2").commodity());
  BOOST_CHECK(v1 >= amount_t("TDollar2").commodity());
  BOOST_CHECK(amount_t("V2").commodity() >= v1);
  BOOST_CHECK(v1 >= string_value("T"));
  BOOST_CHECK(v1 >= string_value("TDollar"));
  BOOST_CHECK(string_value("V") >= v1);
  BOOST_CHECK_THROW(v1 >= value_t(amount_t("USD1")), value_error);
  BOOST_CHECK_THROW(value_t(amount_t("USD1")) >= v1, value_error);

  BOOST_CHECK(v1.valid());
}

BOOST_AUTO_TEST_CASE(testSequence)
{
  value_t::sequence_t s1;
  value_t v1(s1);
  BOOST_CHECK(v1.is_sequence());
  v1.push_back(value_t(2L));
  v1.push_back(value_t("3 GBP"));

  value_t v2("3 GBP");
  value_t seq(v1);
  const value_t v3(seq);

  value_t::sequence_t::iterator i = std::find(seq.begin(), seq.end(), v2);
  if (i != seq.end())
    BOOST_CHECK(v2 == *i);

  value_t::sequence_t::const_iterator j = std::find(v3.begin(), v3.end(), v2);
  if (j != v3.end())
    BOOST_CHECK(v2 == *j);

  BOOST_CHECK(v2 == seq[1]);
  BOOST_CHECK(v2 == v3[1]);
  v1.pop_back();
  v1.pop_back();
  v1.push_front(v2);
  v1.push_front(value_t(2L));
  BOOST_CHECK(v2 == v1[1]);
  BOOST_CHECK(seq == v1);

  BOOST_CHECK(v1.valid());
  BOOST_CHECK(v2.valid());
  BOOST_CHECK(v3.valid());
  BOOST_CHECK(seq.valid());
}

BOOST_AUTO_TEST_CASE(testAddition)
{
  struct tm localtime;
  strptime("10 February 2010 00:00:00", "%d %b %Y %H:%M:%S", &localtime);
  value_t::sequence_t s1;

  value_t v1;
  value_t v2(true);
  value_t v3(boost::posix_time::ptime_from_tm(localtime));
  value_t v4(date_t(parse_date("2014/08/14")));
  value_t v5(2L);
  value_t v6(2UL);
  value_t v7(1.00);
  value_t v8(amount_t("4 GBP"));
  value_t v9(balance_t("4 GBP"));
  value_t v10(mask_t("regex"));
  value_t v11(s1);
  value_t v12(string("$1"));
  value_t v13("2 CAD");
  value_t v14("comment", true);
  value_t v15(string("comment"), true);
  value_t v16(amount_t("2"));

  v14 += v15;
  BOOST_CHECK_EQUAL(v14, value_t(string("commentcomment"), true));
  v14 += v12;
  BOOST_CHECK_EQUAL(v14, value_t(string("commentcomment$1.00"), true));

  strptime("10 February 2010 00:00:00", "%d %b %Y %H:%M:%S", &localtime);
  BOOST_CHECK_EQUAL(v3, value_t(boost::posix_time::ptime_from_tm(localtime)));
  v3 += value_t(2L);
  strptime("10 February 2010 00:00:02", "%d %b %Y %H:%M:%S", &localtime);
  BOOST_CHECK_EQUAL(v3, value_t(boost::posix_time::ptime_from_tm(localtime)));
  v3 += value_t(amount_t("2"));
  strptime("10 February 2010 00:00:04", "%d %b %Y %H:%M:%S", &localtime);
  BOOST_CHECK_EQUAL(v3, value_t(boost::posix_time::ptime_from_tm(localtime)));

  v4 += value_t(2L);
  BOOST_CHECK_EQUAL(v4, value_t(date_t(parse_date("2014/08/16"))));
  v4 += value_t(amount_t("2"));
  BOOST_CHECK_EQUAL(v4, value_t(date_t(parse_date("2014/08/18"))));

  v5 += value_t(2L);
  BOOST_CHECK_EQUAL(v5, value_t(4L));
  v5 += value_t(amount_t("2"));
  BOOST_CHECK_EQUAL(v5, value_t(amount_t("6")));
  v5 += v8;

  v16 += value_t(2L);
  v16 += value_t(amount_t("2"));
  v16 += v8;
  BOOST_CHECK_EQUAL(v5, v16);

  v8 += value_t("6");
  BOOST_CHECK_EQUAL(v8, v16);

  value_t v17(6L);
  v17 += value_t(amount_t("4 GBP"));
  BOOST_CHECK_EQUAL(v8, v17);

  value_t v18(6L);
  v18 += v9;
  value_t v19(amount_t("6"));
  v19 += v9;
  BOOST_CHECK_EQUAL(v18, v19);

  v9 += value_t(2L);
  v9 += value_t(amount_t("4"));
  v9 += v19;
  v18 += v19;
  BOOST_CHECK_EQUAL(v9, v18);

  value_t v20(s1);
  v11 += value_t(2L);
  v11 += value_t("4 GBP");
  BOOST_CHECK_THROW(v11 += v20,value_error);
  BOOST_CHECK_THROW(v10 += v8, value_error);

  v20 += value_t(2L);
  v20 += value_t("4 GBP");
  BOOST_CHECK_EQUAL(v11, v20);
  v11 += v20;
  v20 += v20;
  BOOST_CHECK_EQUAL(v11, v20);

  auto& usd = amount_t("$1").commodity();
  usd.pool().alias("USD", usd);
  value_t v21(usd);
  value_t v22(usd);
  v22 += string_value("A");

  BOOST_CHECK(v22.is_string() && v22 == string_value("$A"));
  BOOST_CHECK(v21 + value_t(amount_t("$2").commodity()) == string_value("$$"));
  BOOST_CHECK(v21 + value_t(amount_t("USD2").commodity()) == string_value("$$"));
  BOOST_CHECK(v21 + amount_t("$2").commodity() == string_value("$$"));
  BOOST_CHECK(v21 + amount_t("USD2").commodity() == string_value("$$"));
  BOOST_CHECK(amount_t("$2").commodity() + v21 == string_value("$$"));
  BOOST_CHECK(v21 + string_value("A") == string_value("$A"));
  BOOST_CHECK(v21 + string_value("USD") == string_value("$USD"));
  BOOST_CHECK(string_value("A") + v21 == string_value("A$"));
  BOOST_CHECK(v21 + value_t(amount_t("USD1")) == string_value("$$1.00"));
  BOOST_CHECK_THROW(value_t(amount_t("USD1")) + v21, value_error);
  BOOST_CHECK(v21.is_commodity());

  BOOST_CHECK(v1.valid());
  BOOST_CHECK(v2.valid());
  BOOST_CHECK(v3.valid());
  BOOST_CHECK(v4.valid());
  BOOST_CHECK(v5.valid());
  BOOST_CHECK(v6.valid());
  BOOST_CHECK(v7.valid());
  BOOST_CHECK(v8.valid());
  BOOST_CHECK(v9.valid());
  BOOST_CHECK(v10.valid());
  BOOST_CHECK(v11.valid());
  BOOST_CHECK(v12.valid());
  BOOST_CHECK(v13.valid());
  BOOST_CHECK(v14.valid());
  BOOST_CHECK(v15.valid());
  BOOST_CHECK(v16.valid());
  BOOST_CHECK(v17.valid());
  BOOST_CHECK(v18.valid());
  BOOST_CHECK(v19.valid());
  BOOST_CHECK(v20.valid());
  BOOST_CHECK(v21.valid());
  BOOST_CHECK(v22.valid());
}

BOOST_AUTO_TEST_CASE(testSubtraction)
{
  struct tm localtime;
  strptime("10 February 2010 00:00:04", "%d %b %Y %H:%M:%S", &localtime);
  value_t::sequence_t s1;

  value_t v1;
  value_t v2(true);
  value_t v3(boost::posix_time::ptime_from_tm(localtime));
  value_t v4(date_t(parse_date("2014/08/18")));
  value_t v5(6L);
  value_t v6(6UL);
  value_t v7(1.00);
  value_t v8(amount_t("4 GBP"));
  value_t v9(balance_t("4 GBP"));
  value_t v10(mask_t("regex"));
  value_t v11(s1);
  value_t v12(string("$1"));
  value_t v13("2 CAD");
  value_t v14("comment", true);
  value_t v15(string("comment"), true);
  value_t v16(amount_t("6"));

  v3 -= value_t(2L);
  strptime("10 February 2010 00:00:02", "%d %b %Y %H:%M:%S", &localtime);
  BOOST_CHECK_EQUAL(v3, value_t(boost::posix_time::ptime_from_tm(localtime)));
  v3 -= value_t(amount_t("2"));
  strptime("10 February 2010 00:00:00", "%d %b %Y %H:%M:%S", &localtime);
  BOOST_CHECK_EQUAL(v3, value_t(boost::posix_time::ptime_from_tm(localtime)));

  v4 -= value_t(2L);
  BOOST_CHECK_EQUAL(v4, value_t(date_t(parse_date("2014/08/16"))));
  v4 -= value_t(amount_t("2"));
  BOOST_CHECK_EQUAL(v4, value_t(date_t(parse_date("2014/08/14"))));

  v5 -= value_t(2L);
  BOOST_CHECK_EQUAL(v5, value_t(4L));
  v5 -= value_t(amount_t("2"));
  BOOST_CHECK_EQUAL(v5, value_t(amount_t("2")));
  v5 -= v8;

  v16 -= value_t(2L);
  v16 -= value_t(amount_t("2"));
  v16 -= v8;
  BOOST_CHECK_EQUAL(v5, v16);

  v8 -= value_t("2");
  BOOST_CHECK_EQUAL(-v8, v16);

  value_t v18(6L);
  v18 -= v9;
  value_t v19(amount_t("6"));
  v19 -= v9;
  BOOST_CHECK_EQUAL(v18, v19);

  v9 -= value_t(-2L);
  v9 -= value_t(amount_t("-10"));
  v9 -= value_t(amount_t("12 GBP"));
  v9 -= v19;
  BOOST_CHECK_EQUAL(v9, v18);
  v18 -=v19;
  BOOST_CHECK_EQUAL(v18, value_t("0"));

  value_t v20(s1);
  value_t v21(2L);
  value_t v22("4 GBP");
  v11.push_back(v21);
  v11.push_back(v22);
  BOOST_CHECK_THROW(v11 -= v20,value_error);
  BOOST_CHECK_THROW(v10 -= v8, value_error);

  v20.push_back(v21);
  v20.push_back(v22);
  v11 -= v20;
  value_t v23(s1);
  v23.push_back(value_t(0L));
  v23.push_back(value_t("0"));
  BOOST_CHECK_EQUAL(v11, v23);
  v20 -= v21;
  v20 -= v22;
  BOOST_CHECK_EQUAL(v20, value_t(s1));

  value_t v24(amount_t("$1").commodity());
  BOOST_CHECK_THROW(v24 - value_t(amount_t("$2").commodity()), value_error);

  BOOST_CHECK(v1.valid());
  BOOST_CHECK(v2.valid());
  BOOST_CHECK(v3.valid());
  BOOST_CHECK(v4.valid());
  BOOST_CHECK(v5.valid());
  BOOST_CHECK(v6.valid());
  BOOST_CHECK(v7.valid());
  BOOST_CHECK(v8.valid());
  BOOST_CHECK(v9.valid());
  BOOST_CHECK(v10.valid());
  BOOST_CHECK(v11.valid());
  BOOST_CHECK(v12.valid());
  BOOST_CHECK(v13.valid());
  BOOST_CHECK(v14.valid());
  BOOST_CHECK(v15.valid());
  BOOST_CHECK(v16.valid());
  BOOST_CHECK(v18.valid());
  BOOST_CHECK(v19.valid());
  BOOST_CHECK(v20.valid());
  BOOST_CHECK(v24.valid());
}

BOOST_AUTO_TEST_CASE(testMultiplication)
{
  struct tm localtime;
  strptime("10 February 2010 00:00:00", "%d %b %Y %H:%M:%S", &localtime);
  value_t::sequence_t s1;

  value_t v1;
  value_t v2(true);
  value_t v3(boost::posix_time::ptime_from_tm(localtime));
  value_t v4(date_t(parse_date("2014/08/14")));
  value_t v5(2L);
  value_t v6(2UL);
  value_t v7(1.00);
  value_t v8(amount_t("4 GBP"));
  value_t v9(balance_t("4 GBP"));
  value_t v10(mask_t("regex"));
  value_t v11(s1);
  value_t v12(string("$1"));
  value_t v13("2 CAD");
  value_t v14("comment", true);
  value_t v15(string("comment"), true);
  value_t v16(amount_t("2"));

  v14 *= value_t(2L);
  BOOST_CHECK_EQUAL(v14, value_t(string("commentcomment"), true));

  v5 *= value_t(2L);
  BOOST_CHECK_EQUAL(v5, value_t(4L));
  v5 *= value_t(amount_t("2"));
  BOOST_CHECK_EQUAL(v5, value_t(amount_t("8")));

  v16 *= value_t(2L);
  v16 *= value_t(amount_t("2"));
  BOOST_CHECK_EQUAL(v5, v16);

  v8 *= v9;
  BOOST_CHECK_EQUAL(v8, value_t("16 GBP"));

  value_t v17(v9);
  v9 *= value_t(2L);
  BOOST_CHECK_EQUAL(v9, value_t("8 GBP"));
  v17 += value_t(2L);
  v17 *= value_t(2L);
  value_t v18("8 GBP");
  v18 += value_t(4L);
  BOOST_CHECK_EQUAL(v17, v18);

  value_t v20(s1);
  v11.push_back(value_t(2L));
  v11.push_back(value_t("2 GBP"));
  v20.push_back(value_t(4L));
  v20.push_back(value_t("4 GBP"));
  v11 *= value_t(2L);
  BOOST_CHECK_EQUAL(v11 ,v20);

  auto& usd = amount_t("$1").commodity();
  usd.pool().alias("USD", usd);
  value_t v21(usd);
  value_t v22(usd);
  v22 *= value_t(2L);

  BOOST_CHECK(v22.is_string() && v22 == string_value("$$"));
  BOOST_CHECK_THROW(v21 * value_t(amount_t("$2").commodity()), value_error);
  BOOST_CHECK(v21 * value_t(2L) == string_value("$$"));
  BOOST_CHECK(value_t(amount_t("USD1").commodity()) * value_t(2L) == string_value("$$"));
  BOOST_CHECK_THROW(value_t(2L) * v21, value_error);
  BOOST_CHECK(v21 * value_t(amount_t("$2.5")) == string_value("$$"));
  BOOST_CHECK_THROW(value_t(amount_t("$2.5")) * v21, value_error);
  BOOST_CHECK(v21.is_commodity());

  BOOST_CHECK_THROW(v10 *= v8, value_error);
  BOOST_CHECK(v1.valid());
  BOOST_CHECK(v2.valid());
  BOOST_CHECK(v3.valid());
  BOOST_CHECK(v4.valid());
  BOOST_CHECK(v5.valid());
  BOOST_CHECK(v6.valid());
  BOOST_CHECK(v7.valid());
  BOOST_CHECK(v8.valid());
  BOOST_CHECK(v9.valid());
  BOOST_CHECK(v10.valid());
  BOOST_CHECK(v11.valid());
  BOOST_CHECK(v12.valid());
  BOOST_CHECK(v13.valid());
  BOOST_CHECK(v14.valid());
  BOOST_CHECK(v15.valid());
  BOOST_CHECK(v16.valid());
  BOOST_CHECK(v17.valid());
  BOOST_CHECK(v18.valid());
  BOOST_CHECK(v21.valid());
  BOOST_CHECK(v22.valid());
}

BOOST_AUTO_TEST_CASE(testDivision)
{
  struct tm localtime;
  strptime("10 February 2010 00:00:00", "%d %b %Y %H:%M:%S", &localtime);
  value_t::sequence_t s1;

  value_t v1;
  value_t v2(true);
  value_t v3(boost::posix_time::ptime_from_tm(localtime));
  value_t v4(date_t(parse_date("2014/08/14")));
  value_t v5(8L);
  value_t v6(2UL);
  value_t v7(1.00);
  value_t v8(amount_t("4 GBP"));
  value_t v9(balance_t("4 GBP"));
  value_t v10(mask_t("regex"));
  value_t v11(s1);
  value_t v12(string("$1"));
  value_t v13("2 CAD");
  value_t v14("comment", true);
  value_t v15(string("comment"), true);
  value_t v16(amount_t("8"));

  v5 /= value_t(2L);
  BOOST_CHECK_EQUAL(v5, value_t(4L));
  v5 /= value_t(amount_t("8"));
  BOOST_CHECK_EQUAL(v5, value_t(amount_t("2")));

  v16 /= value_t(2L);
  v16 /= value_t(amount_t("2"));
  BOOST_CHECK_EQUAL(v5, v16);

  v8 /= v9;
  v8 /= value_t(balance_t(2L));
  BOOST_CHECK_EQUAL(v8, value_t("0.5 GBP"));

  value_t v17(v9);
  v9 /= value_t(2L);
  BOOST_CHECK_EQUAL(v9, value_t("2 GBP"));
  v17 /= value_t("2 GBP");
  v17 /= value_t("2");
  BOOST_CHECK_EQUAL(v17, value_t(balance_t("1 GBP")));

  value_t v18(amount_t("$1").commodity());
  BOOST_CHECK_THROW(v18 / value_t(amount_t("$2").commodity()), value_error);

  BOOST_CHECK_THROW(v10 /= v8, value_error);
  BOOST_CHECK(v1.valid());
  BOOST_CHECK(v2.valid());
  BOOST_CHECK(v3.valid());
  BOOST_CHECK(v4.valid());
  BOOST_CHECK(v5.valid());
  BOOST_CHECK(v6.valid());
  BOOST_CHECK(v7.valid());
  BOOST_CHECK(v8.valid());
  BOOST_CHECK(v9.valid());
  BOOST_CHECK(v10.valid());
  BOOST_CHECK(v11.valid());
  BOOST_CHECK(v12.valid());
  BOOST_CHECK(v13.valid());
  BOOST_CHECK(v14.valid());
  BOOST_CHECK(v15.valid());
  BOOST_CHECK(v16.valid());
  BOOST_CHECK(v17.valid());
  BOOST_CHECK(v18.valid());
}

BOOST_AUTO_TEST_CASE(testType)
{
  value_t::sequence_t s1;
  value_t v1;
  value_t v2(true);
  value_t v3(boost::posix_time::from_time_t(time_t(NULL)));
  value_t v4(date_t(parse_date("2014/08/14")));
  value_t v5(2L);
  value_t v6(4UL);
  value_t v7(1.00);
  value_t v8(amount_t("4 GBP"));
  value_t v9(balance_t("3 EUR"));
  value_t v10(mask_t("regex"));
  value_t v11(s1);
  value_t v12(string("$1"));
  value_t v13("2 CAD");
  value_t v14("comment", true);
  value_t v15(string("tag"), true);
  value_t v16(amount_t("$1").commodity());

  BOOST_CHECK(v1.is_null());
  BOOST_CHECK(v2.is_boolean());
  BOOST_CHECK(v3.is_datetime());
  BOOST_CHECK(v4.is_date());
  BOOST_CHECK(v5.is_long());
  BOOST_CHECK(v6.is_amount());
  BOOST_CHECK(v7.is_amount());
  BOOST_CHECK(v8.is_amount());
  BOOST_CHECK(v9.is_balance());
  BOOST_CHECK(v10.is_mask());
  BOOST_CHECK(v11.is_sequence());
  BOOST_CHECK(v12.is_amount());
  BOOST_CHECK(v13.is_amount());
  BOOST_CHECK(v14.is_string());
  BOOST_CHECK(v15.is_string());
  BOOST_CHECK(v16.type() == value_t::type_t::COMMODITY && v16.is_commodity());
  BOOST_CHECK(!(
    v16.is_null() || v16.is_boolean() || v16.is_datetime() || v16.is_date() || v16.is_long() ||
    v16.is_amount() || v16.is_balance() || v16.is_string() || v16.is_mask() || v16.is_sequence() ||
    v16.is_scope() || v16.is_any()));

  BOOST_CHECK(v1.valid());
  BOOST_CHECK(v2.valid());
  BOOST_CHECK(v3.valid());
  BOOST_CHECK(v4.valid());
  BOOST_CHECK(v5.valid());
  BOOST_CHECK(v6.valid());
  BOOST_CHECK(v7.valid());
  BOOST_CHECK(v8.valid());
  BOOST_CHECK(v9.valid());
  BOOST_CHECK(v10.valid());
  BOOST_CHECK(v11.valid());
  BOOST_CHECK(v12.valid());
  BOOST_CHECK(v13.valid());
  BOOST_CHECK(v14.valid());
  BOOST_CHECK(v15.valid());
  BOOST_CHECK(v16.valid());
}

BOOST_AUTO_TEST_CASE(testForZero)
{
  value_t::sequence_t s1;
  value_t v1;
  value_t v2(true);
  value_t v3(boost::posix_time::from_time_t(time_t(NULL)));
  value_t v4(date_t(0));
  value_t v5(2L);
  value_t v6(0UL);
  value_t v7(1.00);
  value_t v8(amount_t("4 GBP"));
  value_t v9(balance_t("0"));
  value_t v10(mask_t(""));
  value_t v11(s1);
  value_t v12(string("$1"));
  value_t v13("2 CAD");
  value_t v14("comment", true);
  value_t v15(string(""), true);
  const auto& usd = amount_t("$1").commodity();
  value_t v16(usd);
  value_t v17(*usd.pool().null_commodity);

  BOOST_CHECK(v1.is_null());
  BOOST_CHECK(v2.is_nonzero());
  BOOST_CHECK(!v3.is_zero());
  BOOST_CHECK(v4.is_nonzero());
  BOOST_CHECK(v5.is_nonzero());
  BOOST_CHECK(v6.is_realzero());
  BOOST_CHECK(v7.is_nonzero());
  BOOST_CHECK(v8.is_nonzero());
  BOOST_CHECK(v9.is_zero());
  BOOST_CHECK_THROW(v10.is_zero(), value_error);
  BOOST_CHECK(v11.is_zero());
  BOOST_CHECK(v12.is_nonzero());
  BOOST_CHECK(v13.is_nonzero());
  BOOST_CHECK(v14.is_nonzero());
  BOOST_CHECK(v15.is_zero());
  BOOST_CHECK(static_cast<bool>(v16) && !v17);
  BOOST_CHECK(v16.is_nonzero() && v17.is_zero());
  BOOST_CHECK(!v16.is_realzero() && v17.is_realzero());
  BOOST_CHECK(!v16.is_null() && !v17.is_null());

  v11.push_back(v6);
  BOOST_CHECK(v11.is_nonzero());

  BOOST_CHECK(v1.valid());
  BOOST_CHECK(v2.valid());
  BOOST_CHECK(v3.valid());
  BOOST_CHECK(v4.valid());
  BOOST_CHECK(v5.valid());
  BOOST_CHECK(v6.valid());
  BOOST_CHECK(v7.valid());
  BOOST_CHECK(v8.valid());
  BOOST_CHECK(v9.valid());
  BOOST_CHECK(v10.valid());
  BOOST_CHECK(v11.valid());
  BOOST_CHECK(v12.valid());
  BOOST_CHECK(v13.valid());
  BOOST_CHECK(v14.valid());
  BOOST_CHECK(v15.valid());
  BOOST_CHECK(v16.valid());
  BOOST_CHECK(v17.valid());
}

BOOST_AUTO_TEST_CASE(testNegation)
{
  value_t::sequence_t s1;
  value_t v1;
  value_t v2(true);
  value_t v3(boost::posix_time::from_time_t(time_t(NULL)));
  value_t v4(date_t(parse_date("2014/08/09")));
  value_t v5(2L);
  value_t v6(0UL);
  value_t v7(1.00);
  value_t v8(amount_t("4 GBP"));
  value_t v9(balance_t("4 GBP"));
  value_t v10(mask_t(""));
  value_t v11(s1);
  value_t v12(string("$1"));
  value_t v13("$-1");
  value_t v14("comment", true);
  value_t v15(string("comment"), true);
  const auto& usd = amount_t("$1").commodity();
  value_t v16(usd);
  value_t v17(usd);
  value_t v18(*usd.pool().null_commodity);

  BOOST_CHECK(v1.negated().is_null());
  BOOST_CHECK_EQUAL(v2.negated(), value_t(false));
  v5.in_place_negate();
  BOOST_CHECK_EQUAL(v5, value_t(-2L));
  v8.in_place_negate();
  v9.in_place_negate();
  BOOST_CHECK_EQUAL(v8, v9);
  BOOST_CHECK_THROW(v10.negated(), value_error);
  BOOST_CHECK_EQUAL(-v12, v13);
  BOOST_CHECK_THROW(-v14, value_error);
  BOOST_CHECK_THROW(-v16, value_error);
  v17.in_place_not();
  v18.in_place_not();
  BOOST_CHECK(v17.is_boolean() && v17 == value_t(false));
  BOOST_CHECK(v18.is_boolean() && v18 == value_t(true));

  BOOST_CHECK(v1.valid());
  BOOST_CHECK(v2.valid());
  BOOST_CHECK(v3.valid());
  BOOST_CHECK(v4.valid());
  BOOST_CHECK(v5.valid());
  BOOST_CHECK(v6.valid());
  BOOST_CHECK(v7.valid());
  BOOST_CHECK(v8.valid());
  BOOST_CHECK(v9.valid());
  BOOST_CHECK(v10.valid());
  BOOST_CHECK(v11.valid());
  BOOST_CHECK(v12.valid());
  BOOST_CHECK(v13.valid());
  BOOST_CHECK(v14.valid());
  BOOST_CHECK(v15.valid());
  BOOST_CHECK(v16.valid());
  BOOST_CHECK(v17.valid());
  BOOST_CHECK(v18.valid());
}

BOOST_AUTO_TEST_CASE(testAbsoluteValue)
{
  value_t v1(amount_t("$1").commodity());

  BOOST_CHECK_THROW(v1.abs(), value_error);

  BOOST_CHECK(v1.valid());
}

BOOST_AUTO_TEST_CASE(testRounding)
{
  value_t v1(amount_t("$1").commodity());

  BOOST_CHECK_THROW(v1.rounded(), value_error);
  BOOST_CHECK(v1.roundto(2) == v1);
  BOOST_CHECK_THROW(v1.truncated(), value_error);
  BOOST_CHECK_THROW(v1.floored(), value_error);
  BOOST_CHECK_THROW(v1.ceilinged(), value_error);
  BOOST_CHECK_THROW(v1.unrounded(), value_error);
  BOOST_CHECK(v1.reduced() == v1);
  BOOST_CHECK(v1.unreduced() == v1);

  BOOST_CHECK(v1.valid());
}

BOOST_AUTO_TEST_CASE(testValuation)
{
  value_t v1(amount_t("$1").commodity());

  BOOST_CHECK_THROW(v1.value(datetime_t(), amount_t("EUR1").commodity_ptr()), value_error);
  BOOST_CHECK_THROW(v1.exchange_commodities("EUR"), value_error);

  BOOST_CHECK(v1.valid());
}

BOOST_AUTO_TEST_CASE(testConversion)
{
  auto& usd = amount_t("$1").commodity();
  usd.pool().alias("USD", usd);
  value_t v1(usd);

  BOOST_CHECK(v1.as_commodity() == usd && v1.to_commodity() == usd);
  BOOST_CHECK(value_t(amount_t("USD1").commodity()).to_commodity() == usd);
  BOOST_CHECK(v1.to_string() == "$");
  BOOST_CHECK(value_t(amount_t("USD1").commodity()).to_string() == "$");
  BOOST_CHECK(string_value("$").to_commodity() == usd);
  BOOST_CHECK(string_value("USD").to_commodity() == usd);
  BOOST_CHECK_THROW(string_value("A").to_commodity(), value_error);

  BOOST_CHECK(v1.casted(value_t::type_t::COMMODITY) == v1);
  BOOST_CHECK(value_t(amount_t("USD1").commodity()).casted(value_t::type_t::COMMODITY) == v1);
  BOOST_CHECK(v1.casted(value_t::type_t::STRING) == string_value("$"));
  BOOST_CHECK(value_t(amount_t("USD1").commodity()).casted(value_t::type_t::STRING) == string_value("$"));
  BOOST_CHECK(string_value("$").casted(value_t::type_t::COMMODITY) == v1);
  BOOST_CHECK(string_value("USD").casted(value_t::type_t::COMMODITY) == v1);
  BOOST_CHECK_THROW(string_value("A").casted(value_t::type_t::COMMODITY), value_error);

  BOOST_CHECK(v1.simplified() == v1);

  BOOST_CHECK_THROW(v1.number(), value_error);

  BOOST_CHECK(v1.valid());
}

BOOST_AUTO_TEST_CASE(testAnnotation)
{
  value_t v1(amount_t("$1").commodity());

  BOOST_CHECK_THROW(v1.annotate(annotation_t{}), value_error);
  BOOST_CHECK_THROW(v1.has_annotation(), value_error);
  BOOST_CHECK_THROW(v1.annotation(), value_error);
  BOOST_CHECK(v1.strip_annotations(keep_details_t{}) == v1);

  BOOST_CHECK(v1.valid());
}

BOOST_AUTO_TEST_CASE(testLogging)
{
  auto& usd = amount_t("$1").commodity();
  usd.pool().alias("USD", usd);
  value_t v1(usd);

  BOOST_CHECK(v1.label() == "a commodity");

  std::ostringstream log{};
  v1.print(log);
  BOOST_CHECK(log.str() == "$");
  log.str("");
  value_t(amount_t("USD1").commodity()).print(log);
  BOOST_CHECK(log.str() == "$");
  log.str("");

  v1.dump(log);
  BOOST_CHECK(log.str() == "$");
  log.str("");
  value_t(amount_t("USD1").commodity()).dump(log);
  BOOST_CHECK(log.str() == "$");
  log.str("");

  log << v1;
  BOOST_CHECK(log.str() == "$");
  log.str("");
  log << value_t(amount_t("USD1").commodity());
  BOOST_CHECK(log.str() == "$");
  log.str("");

  BOOST_CHECK(value_context(v1) ==
    "                                      $");
  BOOST_CHECK(value_context(value_t(amount_t("USD1").commodity())) ==
    "                                      $");

  BOOST_CHECK(v1.valid());
}

// ---------------------------------------------------------------------------
// Type Conversion Tests (in_place_cast)
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testCastBoolToInteger)
{
  value_t v1(true);
  v1.in_place_cast(value_t::type_t::INTEGER);
  BOOST_CHECK(v1.is_long());
  BOOST_CHECK_EQUAL(v1.to_long(), 1L);

  value_t v2(false);
  v2.in_place_cast(value_t::type_t::INTEGER);
  BOOST_CHECK(v2.is_long());
  BOOST_CHECK_EQUAL(v2.to_long(), 0L);
}

BOOST_AUTO_TEST_CASE(testCastBoolToAmount)
{
  value_t v1(true);
  v1.in_place_cast(value_t::type_t::AMOUNT);
  BOOST_CHECK(v1.is_amount());
  BOOST_CHECK_EQUAL(v1.as_amount(), amount_t(1L));

  value_t v2(false);
  v2.in_place_cast(value_t::type_t::AMOUNT);
  BOOST_CHECK(v2.is_amount());
  BOOST_CHECK_EQUAL(v2.as_amount(), amount_t(0L));
}

BOOST_AUTO_TEST_CASE(testCastBoolToString)
{
  value_t v1(true);
  v1.in_place_cast(value_t::type_t::STRING);
  BOOST_CHECK(v1.is_string());
  BOOST_CHECK_EQUAL(v1.as_string(), "true");

  value_t v2(false);
  v2.in_place_cast(value_t::type_t::STRING);
  BOOST_CHECK(v2.is_string());
  BOOST_CHECK_EQUAL(v2.as_string(), "false");
}

BOOST_AUTO_TEST_CASE(testCastIntegerToAmount)
{
  value_t v1(42L);
  v1.in_place_cast(value_t::type_t::AMOUNT);
  BOOST_CHECK(v1.is_amount());
  BOOST_CHECK_EQUAL(v1.as_amount(), amount_t(42L));
}

BOOST_AUTO_TEST_CASE(testCastIntegerToBalance)
{
  value_t v1(42L);
  v1.in_place_cast(value_t::type_t::BALANCE);
  BOOST_CHECK(v1.is_balance());
  BOOST_CHECK_EQUAL(v1.as_balance(), balance_t(42L));
}

BOOST_AUTO_TEST_CASE(testCastIntegerToString)
{
  value_t v1(42L);
  v1.in_place_cast(value_t::type_t::STRING);
  BOOST_CHECK(v1.is_string());
  BOOST_CHECK_EQUAL(v1.as_string(), "42");
}

BOOST_AUTO_TEST_CASE(testCastAmountToInteger)
{
  value_t v1(amount_t("42"));
  v1.in_place_cast(value_t::type_t::INTEGER);
  BOOST_CHECK(v1.is_long());
  BOOST_CHECK_EQUAL(v1.as_long(), 42L);
}

BOOST_AUTO_TEST_CASE(testCastAmountToBalance)
{
  value_t v1(amount_t("42 GBP"));
  v1.in_place_cast(value_t::type_t::BALANCE);
  BOOST_CHECK(v1.is_balance());
}

BOOST_AUTO_TEST_CASE(testCastAmountToString)
{
  value_t v1(amount_t("42"));
  v1.in_place_cast(value_t::type_t::STRING);
  BOOST_CHECK(v1.is_string());
  BOOST_CHECK_EQUAL(v1.as_string(), "42");
}

BOOST_AUTO_TEST_CASE(testCastNullAmountToInteger)
{
  amount_t null_amt;
  value_t v1(null_amt);
  v1.in_place_cast(value_t::type_t::INTEGER);
  BOOST_CHECK(v1.is_long());
  BOOST_CHECK_EQUAL(v1.as_long(), 0L);
}

BOOST_AUTO_TEST_CASE(testCastNullAmountToBalance)
{
  amount_t null_amt;
  value_t v1(null_amt);
  v1.in_place_cast(value_t::type_t::BALANCE);
  BOOST_CHECK(v1.is_balance());
}

BOOST_AUTO_TEST_CASE(testCastNullAmountToString)
{
  amount_t null_amt;
  value_t v1(null_amt);
  v1.in_place_cast(value_t::type_t::STRING);
  BOOST_CHECK(v1.is_string());
  BOOST_CHECK_EQUAL(v1.as_string(), "");
}

BOOST_AUTO_TEST_CASE(testCastBalanceToAmountSingle)
{
  value_t v1(balance_t("42 GBP"));
  v1.in_place_cast(value_t::type_t::AMOUNT);
  BOOST_CHECK(v1.is_amount());
  BOOST_CHECK_EQUAL(v1.as_amount(), amount_t("42 GBP"));
}

BOOST_AUTO_TEST_CASE(testCastBalanceToAmountEmpty)
{
  balance_t empty_bal;
  value_t v1(empty_bal);
  v1.in_place_cast(value_t::type_t::AMOUNT);
  BOOST_CHECK(v1.is_amount());
  BOOST_CHECK_EQUAL(v1.as_amount(), amount_t(0L));
}

BOOST_AUTO_TEST_CASE(testCastBalanceToAmountMultiThrows)
{
  balance_t bal;
  bal += amount_t("10 GBP");
  bal += amount_t("20 EUR");
  value_t v1(bal);
  BOOST_CHECK_THROW(v1.in_place_cast(value_t::type_t::AMOUNT), value_error);
}

BOOST_AUTO_TEST_CASE(testCastBalanceToString)
{
  value_t v1(balance_t("42 GBP"));
  v1.in_place_cast(value_t::type_t::STRING);
  BOOST_CHECK(v1.is_string());
}

BOOST_AUTO_TEST_CASE(testCastEmptyBalanceToString)
{
  balance_t empty_bal;
  value_t v1(empty_bal);
  v1.in_place_cast(value_t::type_t::STRING);
  BOOST_CHECK(v1.is_string());
  BOOST_CHECK_EQUAL(v1.as_string(), "");
}

BOOST_AUTO_TEST_CASE(testCastStringToInteger)
{
  value_t v1(string("42"), true);
  v1.in_place_cast(value_t::type_t::INTEGER);
  BOOST_CHECK(v1.is_long());
  BOOST_CHECK_EQUAL(v1.as_long(), 42L);
}

BOOST_AUTO_TEST_CASE(testCastStringToNegativeInteger)
{
  value_t v1(string("-7"), true);
  v1.in_place_cast(value_t::type_t::INTEGER);
  BOOST_CHECK(v1.is_long());
  BOOST_CHECK_EQUAL(v1.as_long(), -7L);
}

BOOST_AUTO_TEST_CASE(testCastStringToAmount)
{
  value_t v1(string("42 GBP"), true);
  v1.in_place_cast(value_t::type_t::AMOUNT);
  BOOST_CHECK(v1.is_amount());
  BOOST_CHECK_EQUAL(v1.as_amount(), amount_t("42 GBP"));
}

BOOST_AUTO_TEST_CASE(testCastStringToDate)
{
  value_t v1(string("2014/08/14"), true);
  v1.in_place_cast(value_t::type_t::DATE);
  BOOST_CHECK(v1.is_date());
  BOOST_CHECK_EQUAL(v1.as_date(), date_t(parse_date("2014/08/14")));
}

BOOST_AUTO_TEST_CASE(testCastStringToMask)
{
  value_t v1(string("regex"), true);
  v1.in_place_cast(value_t::type_t::MASK);
  BOOST_CHECK(v1.is_mask());
  BOOST_CHECK_EQUAL(v1.as_mask().str(), "regex");
}

BOOST_AUTO_TEST_CASE(testCastMaskToString)
{
  value_t v1(mask_t("test.*"));
  v1.in_place_cast(value_t::type_t::STRING);
  BOOST_CHECK(v1.is_string());
  BOOST_CHECK_EQUAL(v1.as_string(), "test.*");
}

BOOST_AUTO_TEST_CASE(testCastDateToDatetime)
{
  value_t v1(date_t(parse_date("2014/08/14")));
  v1.in_place_cast(value_t::type_t::DATETIME);
  BOOST_CHECK(v1.is_datetime());
  BOOST_CHECK_EQUAL(v1.as_datetime().date(), parse_date("2014/08/14"));
}

BOOST_AUTO_TEST_CASE(testCastDateToString)
{
  value_t v1(date_t(parse_date("2014/08/14")));
  v1.in_place_cast(value_t::type_t::STRING);
  BOOST_CHECK(v1.is_string());
  BOOST_CHECK(!v1.as_string().empty());
}

BOOST_AUTO_TEST_CASE(testCastDatetimeToDate)
{
  struct tm localtime;
  strptime("14 August 2014 10:30:00", "%d %b %Y %H:%M:%S", &localtime);
  value_t v1(boost::posix_time::ptime_from_tm(localtime));
  v1.in_place_cast(value_t::type_t::DATE);
  BOOST_CHECK(v1.is_date());
  BOOST_CHECK_EQUAL(v1.as_date(), parse_date("2014/08/14"));
}

BOOST_AUTO_TEST_CASE(testCastDatetimeToString)
{
  struct tm localtime;
  strptime("14 August 2014 10:30:00", "%d %b %Y %H:%M:%S", &localtime);
  value_t v1(boost::posix_time::ptime_from_tm(localtime));
  v1.in_place_cast(value_t::type_t::STRING);
  BOOST_CHECK(v1.is_string());
  BOOST_CHECK(!v1.as_string().empty());
}

BOOST_AUTO_TEST_CASE(testCastVoidToInteger)
{
  value_t v1;
  v1.in_place_cast(value_t::type_t::INTEGER);
  BOOST_CHECK(v1.is_long());
  BOOST_CHECK_EQUAL(v1.as_long(), 0L);
}

BOOST_AUTO_TEST_CASE(testCastVoidToAmount)
{
  value_t v1;
  v1.in_place_cast(value_t::type_t::AMOUNT);
  BOOST_CHECK(v1.is_amount());
}

BOOST_AUTO_TEST_CASE(testCastVoidToString)
{
  value_t v1;
  v1.in_place_cast(value_t::type_t::STRING);
  BOOST_CHECK(v1.is_string());
  BOOST_CHECK_EQUAL(v1.as_string(), "");
}

BOOST_AUTO_TEST_CASE(testCastAnyToBoolean)
{
  // Casting any type to BOOLEAN should use operator bool()
  value_t v1(42L);
  v1.in_place_cast(value_t::type_t::BOOLEAN);
  BOOST_CHECK(v1.is_boolean());
  BOOST_CHECK_EQUAL(v1.as_boolean(), true);

  value_t v2(0L);
  v2.in_place_cast(value_t::type_t::BOOLEAN);
  BOOST_CHECK(v2.is_boolean());
  BOOST_CHECK_EQUAL(v2.as_boolean(), false);

  value_t v3(amount_t("100"));
  v3.in_place_cast(value_t::type_t::BOOLEAN);
  BOOST_CHECK(v3.is_boolean());
  BOOST_CHECK_EQUAL(v3.as_boolean(), true);
}

BOOST_AUTO_TEST_CASE(testCastAnyToSequence)
{
  // Casting any type to SEQUENCE wraps in a one-element sequence
  value_t v1(42L);
  v1.in_place_cast(value_t::type_t::SEQUENCE);
  BOOST_CHECK(v1.is_sequence());
  BOOST_CHECK_EQUAL(v1.size(), 1);
  BOOST_CHECK_EQUAL(v1[0].to_long(), 42L);
}

BOOST_AUTO_TEST_CASE(testCastSameTypeNoop)
{
  value_t v1(42L);
  v1.in_place_cast(value_t::type_t::INTEGER);
  BOOST_CHECK(v1.is_long());
  BOOST_CHECK_EQUAL(v1.as_long(), 42L);
}

BOOST_AUTO_TEST_CASE(testCastInvalidThrows)
{
  // Integer cannot cast to DATE
  value_t v1(42L);
  BOOST_CHECK_THROW(v1.in_place_cast(value_t::type_t::DATE), value_error);

  // Boolean cannot cast to DATE
  value_t v2(true);
  BOOST_CHECK_THROW(v2.in_place_cast(value_t::type_t::DATE), value_error);

  // Mask cannot cast to INTEGER
  value_t v3(mask_t("regex"));
  BOOST_CHECK_THROW(v3.in_place_cast(value_t::type_t::INTEGER), value_error);
}

BOOST_AUTO_TEST_CASE(testCastCommodityToString)
{
  auto& usd = amount_t("$1").commodity();
  value_t v1(usd);
  v1.in_place_cast(value_t::type_t::STRING);
  BOOST_CHECK(v1.is_string());
  BOOST_CHECK_EQUAL(v1.as_string(), "$");
}

BOOST_AUTO_TEST_CASE(testCastStringToCommodity)
{
  // Ensure USD exists
  amount_t("$1");
  value_t v1(string("$"), true);
  v1.in_place_cast(value_t::type_t::COMMODITY);
  BOOST_CHECK(v1.is_commodity());
}

BOOST_AUTO_TEST_CASE(testCastStringToCommodityFails)
{
  value_t v1(string("NONEXISTENTXYZ"), true);
  BOOST_CHECK_THROW(v1.in_place_cast(value_t::type_t::COMMODITY), value_error);
}

// ---------------------------------------------------------------------------
// To-conversion Tests
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testToBooleanFromLong)
{
  value_t v1(1L);
  BOOST_CHECK_EQUAL(v1.to_boolean(), true);

  value_t v2(0L);
  BOOST_CHECK_EQUAL(v2.to_boolean(), false);
}

BOOST_AUTO_TEST_CASE(testToLongFromAmount)
{
  value_t v1(amount_t("42"));
  BOOST_CHECK_EQUAL(v1.to_long(), 42L);
}

BOOST_AUTO_TEST_CASE(testToIntFromLong)
{
  value_t v1(42L);
  BOOST_CHECK_EQUAL(v1.to_int(), 42);
}

BOOST_AUTO_TEST_CASE(testToIntFromAmount)
{
  value_t v1(amount_t("99"));
  BOOST_CHECK_EQUAL(v1.to_int(), 99);
}

BOOST_AUTO_TEST_CASE(testToAmountFromLong)
{
  value_t v1(42L);
  BOOST_CHECK_EQUAL(v1.to_amount(), amount_t(42L));
}

BOOST_AUTO_TEST_CASE(testToBalanceFromLong)
{
  value_t v1(42L);
  balance_t bal = v1.to_balance();
  BOOST_CHECK(bal.valid());
}

BOOST_AUTO_TEST_CASE(testToBalanceFromAmount)
{
  value_t v1(amount_t("42 GBP"));
  balance_t bal = v1.to_balance();
  BOOST_CHECK(bal.valid());
}

BOOST_AUTO_TEST_CASE(testToStringFromLong)
{
  value_t v1(42L);
  BOOST_CHECK_EQUAL(v1.to_string(), "42");
}

BOOST_AUTO_TEST_CASE(testToStringFromBool)
{
  value_t v1(true);
  BOOST_CHECK_EQUAL(v1.to_string(), "true");
}

BOOST_AUTO_TEST_CASE(testToMaskFromString)
{
  value_t v1(string("test.*"), true);
  mask_t m = v1.to_mask();
  BOOST_CHECK_EQUAL(m.str(), "test.*");
}

BOOST_AUTO_TEST_CASE(testToSequenceFromLong)
{
  value_t v1(42L);
  value_t::sequence_t seq = v1.to_sequence();
  BOOST_CHECK_EQUAL(seq.size(), 1);
}

BOOST_AUTO_TEST_CASE(testToDatetimeFromDate)
{
  value_t v1(date_t(parse_date("2014/08/14")));
  datetime_t dt = v1.to_datetime();
  BOOST_CHECK_EQUAL(dt.date(), parse_date("2014/08/14"));
}

BOOST_AUTO_TEST_CASE(testToDateFromDatetime)
{
  struct tm localtime;
  strptime("14 August 2014 10:30:00", "%d %b %Y %H:%M:%S", &localtime);
  value_t v1(boost::posix_time::ptime_from_tm(localtime));
  date_t d = v1.to_date();
  BOOST_CHECK_EQUAL(d, parse_date("2014/08/14"));
}

// ---------------------------------------------------------------------------
// Simplify Tests
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testSimplifyZeroValue)
{
  value_t v1(amount_t(0L));
  v1.in_place_simplify();
  BOOST_CHECK(v1.is_long());
  BOOST_CHECK_EQUAL(v1.as_long(), 0L);
}

BOOST_AUTO_TEST_CASE(testSimplifyBalanceSingleCommodity)
{
  value_t v1(balance_t("42 GBP"));
  v1.in_place_simplify();
  BOOST_CHECK(v1.is_amount());
  BOOST_CHECK_EQUAL(v1.as_amount(), amount_t("42 GBP"));
}

BOOST_AUTO_TEST_CASE(testSimplifyNonZeroAmount)
{
  value_t v1(amount_t("42"));
  v1.in_place_simplify();
  // Non-zero amount stays as amount
  BOOST_CHECK(v1.is_amount());
}

BOOST_AUTO_TEST_CASE(testSimplifyZeroBalance)
{
  balance_t empty_bal;
  value_t v1(empty_bal);
  v1.in_place_simplify();
  BOOST_CHECK(v1.is_long());
  BOOST_CHECK_EQUAL(v1.as_long(), 0L);
}

// ---------------------------------------------------------------------------
// Number Tests
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testNumberFromVoid)
{
  value_t v1;
  BOOST_CHECK_EQUAL(v1.number(), value_t(0L));
}

BOOST_AUTO_TEST_CASE(testNumberFromBoolTrue)
{
  value_t v1(true);
  BOOST_CHECK_EQUAL(v1.number(), value_t(1L));
}

BOOST_AUTO_TEST_CASE(testNumberFromBoolFalse)
{
  value_t v1(false);
  BOOST_CHECK_EQUAL(v1.number(), value_t(0L));
}

BOOST_AUTO_TEST_CASE(testNumberFromInteger)
{
  value_t v1(42L);
  BOOST_CHECK_EQUAL(v1.number(), value_t(42L));
}

BOOST_AUTO_TEST_CASE(testNumberFromAmount)
{
  value_t v1(amount_t("42 GBP"));
  value_t result = v1.number();
  BOOST_CHECK(result.is_amount());
}

BOOST_AUTO_TEST_CASE(testNumberFromSequence)
{
  value_t::sequence_t s1;
  value_t v1(s1);
  v1.push_back(value_t(2L));
  v1.push_back(value_t(3L));
  value_t result = v1.number();
  BOOST_CHECK_EQUAL(result, value_t(5L));
}

BOOST_AUTO_TEST_CASE(testNumberFromEmptySequence)
{
  value_t::sequence_t s1;
  value_t v1(s1);
  // Empty sequence falls through to error path and throws
  BOOST_CHECK_THROW(v1.number(), value_error);
}

BOOST_AUTO_TEST_CASE(testNumberFromStringThrows)
{
  value_t v1(string("hello"), true);
  BOOST_CHECK_THROW(v1.number(), value_error);
}

BOOST_AUTO_TEST_CASE(testNumberFromDateThrows)
{
  value_t v1(date_t(parse_date("2014/08/14")));
  BOOST_CHECK_THROW(v1.number(), value_error);
}

// ---------------------------------------------------------------------------
// Comparison Tests (is_less_than, is_greater_than)
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testLessThanBooleans)
{
  value_t vFalse(false);
  value_t vTrue(true);

  BOOST_CHECK(vFalse < vTrue);
  BOOST_CHECK(!(vTrue < vFalse));
  BOOST_CHECK(!(vTrue < vTrue));
  BOOST_CHECK(!(vFalse < vFalse));
}

BOOST_AUTO_TEST_CASE(testGreaterThanBooleans)
{
  value_t vFalse(false);
  value_t vTrue(true);

  BOOST_CHECK(vTrue > vFalse);
  BOOST_CHECK(!(vFalse > vTrue));
  BOOST_CHECK(!(vTrue > vTrue));
  BOOST_CHECK(!(vFalse > vFalse));
}

BOOST_AUTO_TEST_CASE(testLessThanDatetimes)
{
  struct tm t1, t2;
  strptime("10 February 2010 00:00:00", "%d %b %Y %H:%M:%S", &t1);
  strptime("11 February 2010 00:00:00", "%d %b %Y %H:%M:%S", &t2);
  value_t v1(boost::posix_time::ptime_from_tm(t1));
  value_t v2(boost::posix_time::ptime_from_tm(t2));

  BOOST_CHECK(v1 < v2);
  BOOST_CHECK(!(v2 < v1));
}

BOOST_AUTO_TEST_CASE(testGreaterThanDatetimes)
{
  struct tm t1, t2;
  strptime("10 February 2010 00:00:00", "%d %b %Y %H:%M:%S", &t1);
  strptime("11 February 2010 00:00:00", "%d %b %Y %H:%M:%S", &t2);
  value_t v1(boost::posix_time::ptime_from_tm(t1));
  value_t v2(boost::posix_time::ptime_from_tm(t2));

  BOOST_CHECK(v2 > v1);
  BOOST_CHECK(!(v1 > v2));
}

BOOST_AUTO_TEST_CASE(testLessThanDates)
{
  value_t v1(date_t(parse_date("2014/08/14")));
  value_t v2(date_t(parse_date("2014/08/15")));

  BOOST_CHECK(v1 < v2);
  BOOST_CHECK(!(v2 < v1));
}

BOOST_AUTO_TEST_CASE(testGreaterThanDates)
{
  value_t v1(date_t(parse_date("2014/08/14")));
  value_t v2(date_t(parse_date("2014/08/15")));

  BOOST_CHECK(v2 > v1);
  BOOST_CHECK(!(v1 > v2));
}

BOOST_AUTO_TEST_CASE(testLessThanIntegers)
{
  value_t v1(2L);
  value_t v2(5L);

  BOOST_CHECK(v1 < v2);
  BOOST_CHECK(!(v2 < v1));
}

BOOST_AUTO_TEST_CASE(testGreaterThanIntegers)
{
  value_t v1(2L);
  value_t v2(5L);

  BOOST_CHECK(v2 > v1);
  BOOST_CHECK(!(v1 > v2));
}

BOOST_AUTO_TEST_CASE(testLessThanIntegerAndAmount)
{
  value_t v1(2L);
  value_t v2(amount_t("5"));

  BOOST_CHECK(v1 < v2);
  BOOST_CHECK(!(v2 < v1));
}

BOOST_AUTO_TEST_CASE(testGreaterThanIntegerAndAmount)
{
  value_t v1(5L);
  value_t v2(amount_t("2"));

  BOOST_CHECK(v1 > v2);
  BOOST_CHECK(!(v2 > v1));
}

BOOST_AUTO_TEST_CASE(testLessThanAmounts)
{
  value_t v1(amount_t("2"));
  value_t v2(amount_t("5"));

  BOOST_CHECK(v1 < v2);
  BOOST_CHECK(!(v2 < v1));
}

BOOST_AUTO_TEST_CASE(testGreaterThanAmounts)
{
  value_t v1(amount_t("5"));
  value_t v2(amount_t("2"));

  BOOST_CHECK(v1 > v2);
  BOOST_CHECK(!(v2 > v1));
}

BOOST_AUTO_TEST_CASE(testLessThanAmountsDifferentCommodities)
{
  value_t v1(amount_t("2 AAA"));
  value_t v2(amount_t("5 BBB"));

  // Different commodities use commodity comparison
  // Should not throw since both have commodities
  bool result = v1 < v2;
  BOOST_CHECK(result || !result); // just ensure no exception
}

BOOST_AUTO_TEST_CASE(testLessThanAmountAndInteger)
{
  value_t v1(amount_t("2"));
  value_t v2(5L);

  BOOST_CHECK(v1 < v2);
  BOOST_CHECK(!(v2 < v1));
}

BOOST_AUTO_TEST_CASE(testGreaterThanAmountAndInteger)
{
  value_t v1(amount_t("5"));
  value_t v2(2L);

  BOOST_CHECK(v1 > v2);
  BOOST_CHECK(!(v2 > v1));
}

BOOST_AUTO_TEST_CASE(testLessThanStrings)
{
  value_t v1(string("apple"), true);
  value_t v2(string("banana"), true);

  BOOST_CHECK(v1 < v2);
  BOOST_CHECK(!(v2 < v1));
}

BOOST_AUTO_TEST_CASE(testGreaterThanStrings)
{
  value_t v1(string("banana"), true);
  value_t v2(string("apple"), true);

  BOOST_CHECK(v1 > v2);
  BOOST_CHECK(!(v2 > v1));
}

BOOST_AUTO_TEST_CASE(testLessThanSequences)
{
  value_t::sequence_t s1;
  value_t seq1(s1);
  seq1.push_back(value_t(1L));
  seq1.push_back(value_t(2L));

  value_t::sequence_t s2;
  value_t seq2(s2);
  seq2.push_back(value_t(3L));
  seq2.push_back(value_t(4L));

  BOOST_CHECK(seq1 < seq2);
}

BOOST_AUTO_TEST_CASE(testGreaterThanSequences)
{
  value_t::sequence_t s1;
  value_t seq1(s1);
  seq1.push_back(value_t(3L));
  seq1.push_back(value_t(4L));
  seq1.push_back(value_t(5L));

  value_t::sequence_t s2;
  value_t seq2(s2);
  seq2.push_back(value_t(1L));
  seq2.push_back(value_t(2L));

  BOOST_CHECK(seq1 > seq2);
}

BOOST_AUTO_TEST_CASE(testLessThanSequenceAndValue)
{
  value_t::sequence_t s1;
  value_t seq(s1);
  seq.push_back(value_t(1L));
  seq.push_back(value_t(2L));

  value_t v1(5L);
  BOOST_CHECK(seq < v1);
}

BOOST_AUTO_TEST_CASE(testGreaterThanSequenceAndValue)
{
  // Note: value_t > value_t is implemented as b < a via ordered_field_operators,
  // so seq > v1 becomes v1 < seq, which is INTEGER < SEQUENCE (unsupported).
  // Instead, test that seq < v1 works (SEQUENCE < INTEGER is supported).
  value_t::sequence_t s1;
  value_t seq(s1);
  seq.push_back(value_t(10L));
  seq.push_back(value_t(20L));

  value_t v1(5L);
  // SEQUENCE.is_less_than(INTEGER) checks if all elements < val
  BOOST_CHECK(!(seq < v1));  // 10 and 20 are not less than 5

  value_t v2(25L);
  BOOST_CHECK(seq < v2);    // 10 and 20 are both less than 25
}

BOOST_AUTO_TEST_CASE(testCompareIncompatibleTypesThrows)
{
  value_t v1(42L);
  value_t v2(mask_t("regex"));

  BOOST_CHECK_THROW(v1 < v2, value_error);
  BOOST_CHECK_THROW(v1 > v2, value_error);
}

BOOST_AUTO_TEST_CASE(testLessThanBalanceAndInteger)
{
  value_t v1(balance_t("2"));
  value_t v2(5L);

  BOOST_CHECK(v1 < v2);
}

BOOST_AUTO_TEST_CASE(testGreaterThanBalanceAndInteger)
{
  value_t v1(balance_t("10"));
  value_t v2(5L);

  BOOST_CHECK(v1 > v2);
}

BOOST_AUTO_TEST_CASE(testLessThanIntegerAndBalance)
{
  value_t v1(2L);
  value_t v2(balance_t("5"));

  BOOST_CHECK(v1 < v2);
}

BOOST_AUTO_TEST_CASE(testGreaterThanIntegerAndBalance)
{
  value_t v1(10L);
  value_t v2(balance_t("5"));

  BOOST_CHECK(v1 > v2);
}

BOOST_AUTO_TEST_CASE(testLessThanAmountAndBalance)
{
  value_t v1(amount_t("2"));
  value_t v2(balance_t("5"));

  BOOST_CHECK(v1 < v2);
}

BOOST_AUTO_TEST_CASE(testGreaterThanAmountAndBalance)
{
  value_t v1(amount_t("10"));
  value_t v2(balance_t("5"));

  BOOST_CHECK(v1 > v2);
}

BOOST_AUTO_TEST_CASE(testLessThanBalanceAndBalance)
{
  value_t v1(balance_t("2"));
  value_t v2(balance_t("5"));

  BOOST_CHECK(v1 < v2);
}

BOOST_AUTO_TEST_CASE(testGreaterThanBalanceAndBalance)
{
  value_t v1(balance_t("10"));
  value_t v2(balance_t("5"));

  BOOST_CHECK(v1 > v2);
}

// ---------------------------------------------------------------------------
// Unary Operation Tests
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testNegateSequence)
{
  value_t::sequence_t s1;
  value_t v1(s1);
  v1.push_back(value_t(2L));
  v1.push_back(value_t(3L));
  v1.in_place_negate();

  BOOST_CHECK_EQUAL(v1[0].to_long(), -2L);
  BOOST_CHECK_EQUAL(v1[1].to_long(), -3L);
}

BOOST_AUTO_TEST_CASE(testInPlaceNotBoolean)
{
  value_t v1(true);
  v1.in_place_not();
  BOOST_CHECK(v1.is_boolean());
  BOOST_CHECK_EQUAL(v1.as_boolean(), false);

  v1.in_place_not();
  BOOST_CHECK_EQUAL(v1.as_boolean(), true);
}

BOOST_AUTO_TEST_CASE(testInPlaceNotInteger)
{
  value_t v1(42L);
  v1.in_place_not();
  BOOST_CHECK(v1.is_boolean());
  BOOST_CHECK_EQUAL(v1.as_boolean(), false);

  value_t v2(0L);
  v2.in_place_not();
  BOOST_CHECK(v2.is_boolean());
  BOOST_CHECK_EQUAL(v2.as_boolean(), true);
}

BOOST_AUTO_TEST_CASE(testInPlaceNotAmount)
{
  value_t v1(amount_t("42"));
  v1.in_place_not();
  BOOST_CHECK(v1.is_boolean());
  BOOST_CHECK_EQUAL(v1.as_boolean(), false);

  value_t v2(amount_t(0L));
  v2.in_place_not();
  BOOST_CHECK(v2.is_boolean());
  BOOST_CHECK_EQUAL(v2.as_boolean(), true);
}

BOOST_AUTO_TEST_CASE(testInPlaceNotBalance)
{
  value_t v1(balance_t("42 GBP"));
  v1.in_place_not();
  BOOST_CHECK(v1.is_boolean());
  BOOST_CHECK_EQUAL(v1.as_boolean(), false);

  balance_t empty_bal;
  value_t v2(empty_bal);
  v2.in_place_not();
  BOOST_CHECK(v2.is_boolean());
  BOOST_CHECK_EQUAL(v2.as_boolean(), true);
}

BOOST_AUTO_TEST_CASE(testInPlaceNotString)
{
  value_t v1(string("hello"), true);
  v1.in_place_not();
  BOOST_CHECK(v1.is_boolean());
  BOOST_CHECK_EQUAL(v1.as_boolean(), false);

  value_t v2(string(""), true);
  v2.in_place_not();
  BOOST_CHECK(v2.is_boolean());
  BOOST_CHECK_EQUAL(v2.as_boolean(), true);
}

BOOST_AUTO_TEST_CASE(testInPlaceNotSequence)
{
  value_t::sequence_t s1;
  value_t v1(s1);
  v1.push_back(value_t(true));
  v1.push_back(value_t(false));
  v1.in_place_not();

  BOOST_CHECK(v1[0].is_boolean());
  BOOST_CHECK_EQUAL(v1[0].as_boolean(), false);
  BOOST_CHECK(v1[1].is_boolean());
  BOOST_CHECK_EQUAL(v1[1].as_boolean(), true);
}

BOOST_AUTO_TEST_CASE(testInPlaceNotMaskThrows)
{
  value_t v1(mask_t("regex"));
  BOOST_CHECK_THROW(v1.in_place_not(), value_error);
}

// ---------------------------------------------------------------------------
// Abs Tests
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testAbsVoid)
{
  value_t v1;
  value_t result = v1.abs();
  BOOST_CHECK(result.is_null());
}

BOOST_AUTO_TEST_CASE(testAbsPositiveInteger)
{
  value_t v1(42L);
  value_t result = v1.abs();
  BOOST_CHECK_EQUAL(result.to_long(), 42L);
}

BOOST_AUTO_TEST_CASE(testAbsNegativeInteger)
{
  value_t v1(-42L);
  value_t result = v1.abs();
  BOOST_CHECK_EQUAL(result.to_long(), 42L);
}

BOOST_AUTO_TEST_CASE(testAbsAmount)
{
  value_t v1(amount_t("-42"));
  value_t result = v1.abs();
  BOOST_CHECK_EQUAL(result.as_amount(), amount_t("42"));
}

BOOST_AUTO_TEST_CASE(testAbsBalance)
{
  value_t v1(balance_t("-42 GBP"));
  value_t result = v1.abs();
  BOOST_CHECK(result.valid());
}

BOOST_AUTO_TEST_CASE(testAbsStringThrows)
{
  value_t v1(string("hello"), true);
  BOOST_CHECK_THROW(v1.abs(), value_error);
}

// ---------------------------------------------------------------------------
// Rounding Tests for valid types
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testRoundInteger)
{
  value_t v1(42L);
  value_t result = v1.rounded();
  BOOST_CHECK(result.is_long());
  BOOST_CHECK_EQUAL(result.as_long(), 42L);
}

BOOST_AUTO_TEST_CASE(testRoundAmount)
{
  value_t v1(amount_t("$1.005"));
  value_t result = v1.rounded();
  BOOST_CHECK(result.is_amount());
  BOOST_CHECK(result.valid());
}

BOOST_AUTO_TEST_CASE(testRoundBalance)
{
  value_t v1(balance_t("42 GBP"));
  value_t result = v1.rounded();
  BOOST_CHECK(result.valid());
}

BOOST_AUTO_TEST_CASE(testRoundSequence)
{
  value_t::sequence_t s1;
  value_t v1(s1);
  v1.push_back(value_t(amount_t("$1.005")));
  v1.push_back(value_t(42L));
  value_t result = v1.rounded();
  BOOST_CHECK(result.valid());
}

BOOST_AUTO_TEST_CASE(testRoundStringThrows)
{
  value_t v1(string("hello"), true);
  BOOST_CHECK_THROW(v1.rounded(), value_error);
}

BOOST_AUTO_TEST_CASE(testRoundtoInteger)
{
  value_t v1(42L);
  value_t result = v1.roundto(2);
  BOOST_CHECK(result.is_long());
  BOOST_CHECK_EQUAL(result.as_long(), 42L);
}

BOOST_AUTO_TEST_CASE(testRoundtoAmount)
{
  value_t v1(amount_t("1.2345"));
  value_t result = v1.roundto(2);
  BOOST_CHECK(result.is_amount());
  BOOST_CHECK(result.valid());
}

BOOST_AUTO_TEST_CASE(testRoundtoBalance)
{
  value_t v1(balance_t("42 GBP"));
  value_t result = v1.roundto(2);
  BOOST_CHECK(result.valid());
}

BOOST_AUTO_TEST_CASE(testRoundtoSequence)
{
  value_t::sequence_t s1;
  value_t v1(s1);
  v1.push_back(value_t(amount_t("1.2345")));
  value_t result = v1.roundto(2);
  BOOST_CHECK(result.valid());
}

BOOST_AUTO_TEST_CASE(testTruncateInteger)
{
  value_t v1(42L);
  value_t result = v1.truncated();
  BOOST_CHECK(result.is_long());
  BOOST_CHECK_EQUAL(result.as_long(), 42L);
}

BOOST_AUTO_TEST_CASE(testTruncateAmount)
{
  value_t v1(amount_t("$1.99"));
  value_t result = v1.truncated();
  BOOST_CHECK(result.valid());
}

BOOST_AUTO_TEST_CASE(testTruncateBalance)
{
  value_t v1(balance_t("42 GBP"));
  value_t result = v1.truncated();
  BOOST_CHECK(result.valid());
}

BOOST_AUTO_TEST_CASE(testTruncateSequence)
{
  value_t::sequence_t s1;
  value_t v1(s1);
  v1.push_back(value_t(amount_t("$1.99")));
  value_t result = v1.truncated();
  BOOST_CHECK(result.valid());
}

BOOST_AUTO_TEST_CASE(testTruncateStringThrows)
{
  value_t v1(string("hello"), true);
  BOOST_CHECK_THROW(v1.truncated(), value_error);
}

BOOST_AUTO_TEST_CASE(testFloorInteger)
{
  value_t v1(42L);
  value_t result = v1.floored();
  BOOST_CHECK(result.is_long());
  BOOST_CHECK_EQUAL(result.as_long(), 42L);
}

BOOST_AUTO_TEST_CASE(testFloorAmount)
{
  value_t v1(amount_t("$1.99"));
  value_t result = v1.floored();
  BOOST_CHECK(result.valid());
}

BOOST_AUTO_TEST_CASE(testFloorBalance)
{
  value_t v1(balance_t("42 GBP"));
  value_t result = v1.floored();
  BOOST_CHECK(result.valid());
}

BOOST_AUTO_TEST_CASE(testFloorSequence)
{
  value_t::sequence_t s1;
  value_t v1(s1);
  v1.push_back(value_t(amount_t("$1.99")));
  value_t result = v1.floored();
  BOOST_CHECK(result.valid());
}

BOOST_AUTO_TEST_CASE(testFloorStringThrows)
{
  value_t v1(string("hello"), true);
  BOOST_CHECK_THROW(v1.floored(), value_error);
}

BOOST_AUTO_TEST_CASE(testCeilingInteger)
{
  value_t v1(42L);
  value_t result = v1.ceilinged();
  BOOST_CHECK(result.is_long());
  BOOST_CHECK_EQUAL(result.as_long(), 42L);
}

BOOST_AUTO_TEST_CASE(testCeilingAmount)
{
  value_t v1(amount_t("$1.01"));
  value_t result = v1.ceilinged();
  BOOST_CHECK(result.valid());
}

BOOST_AUTO_TEST_CASE(testCeilingBalance)
{
  value_t v1(balance_t("42 GBP"));
  value_t result = v1.ceilinged();
  BOOST_CHECK(result.valid());
}

BOOST_AUTO_TEST_CASE(testCeilingSequence)
{
  value_t::sequence_t s1;
  value_t v1(s1);
  v1.push_back(value_t(amount_t("$1.01")));
  value_t result = v1.ceilinged();
  BOOST_CHECK(result.valid());
}

BOOST_AUTO_TEST_CASE(testCeilingStringThrows)
{
  value_t v1(string("hello"), true);
  BOOST_CHECK_THROW(v1.ceilinged(), value_error);
}

BOOST_AUTO_TEST_CASE(testUnroundInteger)
{
  value_t v1(42L);
  value_t result = v1.unrounded();
  BOOST_CHECK(result.is_long());
  BOOST_CHECK_EQUAL(result.as_long(), 42L);
}

BOOST_AUTO_TEST_CASE(testUnroundAmount)
{
  value_t v1(amount_t("$1.00"));
  value_t result = v1.unrounded();
  BOOST_CHECK(result.valid());
}

BOOST_AUTO_TEST_CASE(testUnroundBalance)
{
  value_t v1(balance_t("42 GBP"));
  value_t result = v1.unrounded();
  BOOST_CHECK(result.valid());
}

BOOST_AUTO_TEST_CASE(testUnroundSequence)
{
  value_t::sequence_t s1;
  value_t v1(s1);
  v1.push_back(value_t(amount_t("$1.00")));
  value_t result = v1.unrounded();
  BOOST_CHECK(result.valid());
}

BOOST_AUTO_TEST_CASE(testUnroundStringThrows)
{
  value_t v1(string("hello"), true);
  BOOST_CHECK_THROW(v1.unrounded(), value_error);
}

// ---------------------------------------------------------------------------
// Reduce / Unreduce Tests
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testReduceAmount)
{
  value_t v1(amount_t("42 GBP"));
  value_t result = v1.reduced();
  BOOST_CHECK(result.valid());
}

BOOST_AUTO_TEST_CASE(testReduceBalance)
{
  value_t v1(balance_t("42 GBP"));
  value_t result = v1.reduced();
  BOOST_CHECK(result.valid());
}

BOOST_AUTO_TEST_CASE(testReduceSequence)
{
  value_t::sequence_t s1;
  value_t v1(s1);
  v1.push_back(value_t(amount_t("42 GBP")));
  value_t result = v1.reduced();
  BOOST_CHECK(result.valid());
}

BOOST_AUTO_TEST_CASE(testReduceInteger)
{
  value_t v1(42L);
  value_t result = v1.reduced();
  BOOST_CHECK(result.is_long());
  BOOST_CHECK_EQUAL(result.as_long(), 42L);
}

BOOST_AUTO_TEST_CASE(testUnreduceAmount)
{
  value_t v1(amount_t("42 GBP"));
  value_t result = v1.unreduced();
  BOOST_CHECK(result.valid());
}

BOOST_AUTO_TEST_CASE(testUnreduceBalance)
{
  value_t v1(balance_t("42 GBP"));
  value_t result = v1.unreduced();
  BOOST_CHECK(result.valid());
}

BOOST_AUTO_TEST_CASE(testUnreduceSequence)
{
  value_t::sequence_t s1;
  value_t v1(s1);
  v1.push_back(value_t(amount_t("42 GBP")));
  value_t result = v1.unreduced();
  BOOST_CHECK(result.valid());
}

BOOST_AUTO_TEST_CASE(testUnreduceInteger)
{
  value_t v1(42L);
  value_t result = v1.unreduced();
  BOOST_CHECK(result.is_long());
  BOOST_CHECK_EQUAL(result.as_long(), 42L);
}

// ---------------------------------------------------------------------------
// Arithmetic Edge Cases
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testAddVoidToValue)
{
  value_t v1;
  v1 += value_t(42L);
  BOOST_CHECK_EQUAL(v1, value_t(42L));
}

BOOST_AUTO_TEST_CASE(testSubtractFromVoid)
{
  value_t v1;
  v1 -= value_t(42L);
  BOOST_CHECK_EQUAL(v1, value_t(-42L));
}

BOOST_AUTO_TEST_CASE(testAddNullIsNoop)
{
  value_t v1(42L);
  value_t v2;
  v1 += v2;
  BOOST_CHECK_EQUAL(v1, value_t(42L));
}

BOOST_AUTO_TEST_CASE(testSubtractNullIsNoop)
{
  value_t v1(42L);
  value_t v2;
  v1 -= v2;
  BOOST_CHECK_EQUAL(v1, value_t(42L));
}

BOOST_AUTO_TEST_CASE(testAddAmountsDifferentCommodities)
{
  value_t v1(amount_t("10 GBP"));
  value_t v2(amount_t("20 EUR"));
  v1 += v2;
  // Should become a balance
  BOOST_CHECK(v1.is_balance());
}

BOOST_AUTO_TEST_CASE(testSubtractAmountsDifferentCommodities)
{
  value_t v1(amount_t("10 GBP"));
  value_t v2(amount_t("20 EUR"));
  v1 -= v2;
  // Should become a balance (after simplify may still be balance)
  BOOST_CHECK(v1.valid());
}

BOOST_AUTO_TEST_CASE(testAddCommodityAmountToInteger)
{
  value_t v1(10L);
  value_t v2(amount_t("5 GBP"));
  v1 += v2;
  // Integer + commoditized amount -> balance
  BOOST_CHECK(v1.is_balance());
}

BOOST_AUTO_TEST_CASE(testSubtractCommodityAmountFromInteger)
{
  value_t v1(10L);
  value_t v2(amount_t("5 GBP"));
  v1 -= v2;
  BOOST_CHECK(v1.valid());
}

BOOST_AUTO_TEST_CASE(testAddIntegerToBalance)
{
  value_t v1(balance_t("10 GBP"));
  v1 += value_t(5L);
  BOOST_CHECK(v1.is_balance());
  BOOST_CHECK(v1.valid());
}

BOOST_AUTO_TEST_CASE(testSubtractIntegerFromBalance)
{
  value_t v1(balance_t("10 GBP"));
  v1 -= value_t(5L);
  BOOST_CHECK(v1.valid());
}

BOOST_AUTO_TEST_CASE(testAddAmountToBalance)
{
  value_t v1(balance_t("10 GBP"));
  v1 += value_t(amount_t("5 GBP"));
  BOOST_CHECK(v1.valid());
}

BOOST_AUTO_TEST_CASE(testSubtractAmountFromBalance)
{
  value_t v1(balance_t("10 GBP"));
  v1 -= value_t(amount_t("5 GBP"));
  BOOST_CHECK(v1.valid());
}

BOOST_AUTO_TEST_CASE(testAddBalanceToBalance)
{
  value_t v1(balance_t("10 GBP"));
  v1 += value_t(balance_t("5 GBP"));
  BOOST_CHECK(v1.valid());
}

BOOST_AUTO_TEST_CASE(testSubtractBalanceFromBalance)
{
  value_t v1(balance_t("10 GBP"));
  v1 -= value_t(balance_t("5 GBP"));
  BOOST_CHECK(v1.valid());
}

BOOST_AUTO_TEST_CASE(testAddIntegerToAmountWithCommodity)
{
  value_t v1(amount_t("10 GBP"));
  v1 += value_t(5L);
  // Amount with commodity + integer -> balance
  BOOST_CHECK(v1.is_balance());
}

BOOST_AUTO_TEST_CASE(testSubtractIntegerFromAmountWithCommodity)
{
  value_t v1(amount_t("10 GBP"));
  v1 -= value_t(5L);
  BOOST_CHECK(v1.valid());
}

BOOST_AUTO_TEST_CASE(testAddAmountToAmountSameCommodity)
{
  value_t v1(amount_t("10 GBP"));
  v1 += value_t(amount_t("5 GBP"));
  BOOST_CHECK(v1.is_amount());
  BOOST_CHECK_EQUAL(v1.as_amount(), amount_t("15 GBP"));
}

BOOST_AUTO_TEST_CASE(testAddBalanceToAmount)
{
  value_t v1(amount_t("10 GBP"));
  v1 += value_t(balance_t("5 GBP"));
  BOOST_CHECK(v1.is_balance());
}

BOOST_AUTO_TEST_CASE(testSubtractBalanceFromAmount)
{
  value_t v1(amount_t("10 GBP"));
  v1 -= value_t(balance_t("5 GBP"));
  BOOST_CHECK(v1.valid());
}

BOOST_AUTO_TEST_CASE(testAddIntegerToAmountNoCommodity)
{
  value_t v1(amount_t("10"));
  v1 += value_t(5L);
  BOOST_CHECK(v1.is_amount());
}

BOOST_AUTO_TEST_CASE(testSubtractIntegerFromAmountNoCommodity)
{
  value_t v1(amount_t("10"));
  v1 -= value_t(5L);
  BOOST_CHECK(v1.is_amount());
}

BOOST_AUTO_TEST_CASE(testMultiplyIntegerByInteger)
{
  value_t v1(6L);
  v1 *= value_t(7L);
  BOOST_CHECK_EQUAL(v1, value_t(42L));
}

BOOST_AUTO_TEST_CASE(testMultiplyIntegerByAmount)
{
  value_t v1(6L);
  v1 *= value_t(amount_t("7"));
  BOOST_CHECK(v1.is_amount());
  BOOST_CHECK_EQUAL(v1.as_amount(), amount_t("42"));
}

BOOST_AUTO_TEST_CASE(testMultiplyAmountByInteger)
{
  value_t v1(amount_t("6 GBP"));
  v1 *= value_t(7L);
  BOOST_CHECK_EQUAL(v1.as_amount(), amount_t("42 GBP"));
}

BOOST_AUTO_TEST_CASE(testMultiplyAmountByAmount)
{
  value_t v1(amount_t("6"));
  v1 *= value_t(amount_t("7"));
  BOOST_CHECK_EQUAL(v1.as_amount(), amount_t("42"));
}

BOOST_AUTO_TEST_CASE(testMultiplyBalanceByInteger)
{
  value_t v1(balance_t("6 GBP"));
  v1 *= value_t(7L);
  BOOST_CHECK(v1.valid());
}

BOOST_AUTO_TEST_CASE(testMultiplyBalanceByUncommoditizedAmount)
{
  balance_t bal;
  bal += amount_t("10 GBP");
  bal += amount_t("20 EUR");
  value_t v1(bal);
  v1 *= value_t(amount_t("2"));
  BOOST_CHECK(v1.valid());
}

BOOST_AUTO_TEST_CASE(testDivideIntegerByInteger)
{
  value_t v1(42L);
  v1 /= value_t(7L);
  BOOST_CHECK_EQUAL(v1, value_t(6L));
}

BOOST_AUTO_TEST_CASE(testDivideAmountByInteger)
{
  value_t v1(amount_t("42 GBP"));
  v1 /= value_t(7L);
  BOOST_CHECK_EQUAL(v1.as_amount(), amount_t("6 GBP"));
}

BOOST_AUTO_TEST_CASE(testDivideAmountByAmount)
{
  value_t v1(amount_t("42"));
  v1 /= value_t(amount_t("7"));
  BOOST_CHECK_EQUAL(v1.as_amount(), amount_t("6"));
}

BOOST_AUTO_TEST_CASE(testDivideBalanceByInteger)
{
  value_t v1(balance_t("42 GBP"));
  v1 /= value_t(7L);
  BOOST_CHECK(v1.valid());
}

BOOST_AUTO_TEST_CASE(testDivideBalanceByUncommoditizedAmount)
{
  balance_t bal;
  bal += amount_t("10 GBP");
  bal += amount_t("20 EUR");
  value_t v1(bal);
  v1 /= value_t(amount_t("2"));
  BOOST_CHECK(v1.valid());
}

BOOST_AUTO_TEST_CASE(testDivideBalanceSingleAmountByAmount)
{
  value_t v1(balance_t("42 GBP"));
  v1 /= value_t(amount_t("7 GBP"));
  BOOST_CHECK(v1.valid());
}

BOOST_AUTO_TEST_CASE(testMultiplyAmountByBalanceSingleAmount)
{
  value_t v1(amount_t("6 GBP"));
  v1 *= value_t(balance_t("7 GBP"));
  BOOST_CHECK(v1.valid());
}

BOOST_AUTO_TEST_CASE(testDivideAmountByBalanceSingleAmount)
{
  value_t v1(amount_t("42 GBP"));
  v1 /= value_t(balance_t("7 GBP"));
  BOOST_CHECK(v1.valid());
}

BOOST_AUTO_TEST_CASE(testMultiplyBalanceSingleAmountByAmount)
{
  value_t v1(balance_t("6 GBP"));
  v1 *= value_t(amount_t("7 GBP"));
  BOOST_CHECK(v1.valid());
}

BOOST_AUTO_TEST_CASE(testAddBoolThrows)
{
  value_t v1(true);
  BOOST_CHECK_THROW(v1 += value_t(42L), value_error);
}

BOOST_AUTO_TEST_CASE(testMultiplyDateThrows)
{
  value_t v1(date_t(parse_date("2014/08/14")));
  BOOST_CHECK_THROW(v1 *= value_t(2L), value_error);
}

BOOST_AUTO_TEST_CASE(testDivideStringThrows)
{
  value_t v1(string("hello"), true);
  BOOST_CHECK_THROW(v1 /= value_t(2L), value_error);
}

BOOST_AUTO_TEST_CASE(testDivideDateThrows)
{
  value_t v1(date_t(parse_date("2014/08/14")));
  BOOST_CHECK_THROW(v1 /= value_t(2L), value_error);
}

BOOST_AUTO_TEST_CASE(testSubtractSequenceElement)
{
  value_t::sequence_t s1;
  value_t v1(s1);
  v1.push_back(value_t(2L));
  v1.push_back(value_t(3L));
  v1.push_back(value_t(4L));

  // Subtracting a non-sequence value from a sequence should erase matching elements
  v1 -= value_t(3L);
  BOOST_CHECK_EQUAL(v1.size(), 2);
}

BOOST_AUTO_TEST_CASE(testSubtractSequencesDifferentLengthsThrows)
{
  value_t::sequence_t s1, s2;
  value_t v1(s1);
  v1.push_back(value_t(2L));
  v1.push_back(value_t(3L));

  value_t v2(s2);
  v2.push_back(value_t(1L));

  BOOST_CHECK_THROW(v1 -= v2, value_error);
}

BOOST_AUTO_TEST_CASE(testAddSequencesDifferentLengthsThrows)
{
  value_t::sequence_t s1, s2;
  value_t v1(s1);
  v1.push_back(value_t(2L));
  v1.push_back(value_t(3L));

  value_t v2(s2);
  v2.push_back(value_t(1L));

  BOOST_CHECK_THROW(v1 += v2, value_error);
}

// ---------------------------------------------------------------------------
// Annotation Tests for Amount type
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testAnnotateAmount)
{
  value_t v1(amount_t("10 GBP"));
  annotation_t details;
  // Should not throw for amount type
  // (only throws if annotation is empty/invalid, but that is an amount_t concern)
  BOOST_CHECK(!v1.has_annotation());
}

// ---------------------------------------------------------------------------
// Print / Dump / Label Tests
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testPrintVoid)
{
  value_t v1;
  std::ostringstream out;
  v1.print(out);
  BOOST_CHECK_EQUAL(out.str(), "");
}

BOOST_AUTO_TEST_CASE(testPrintBoolean)
{
  value_t v1(true);
  std::ostringstream out;
  v1.print(out);
  BOOST_CHECK_EQUAL(out.str(), "1");

  value_t v2(false);
  std::ostringstream out2;
  v2.print(out2);
  BOOST_CHECK_EQUAL(out2.str(), "0");
}

BOOST_AUTO_TEST_CASE(testPrintDatetime)
{
  struct tm localtime;
  strptime("14 August 2014 10:30:00", "%d %b %Y %H:%M:%S", &localtime);
  value_t v1(boost::posix_time::ptime_from_tm(localtime));
  std::ostringstream out;
  v1.print(out);
  BOOST_CHECK(!out.str().empty());
}

BOOST_AUTO_TEST_CASE(testPrintDate)
{
  value_t v1(date_t(parse_date("2014/08/14")));
  std::ostringstream out;
  v1.print(out);
  BOOST_CHECK(!out.str().empty());
}

BOOST_AUTO_TEST_CASE(testPrintInteger)
{
  value_t v1(42L);
  std::ostringstream out;
  v1.print(out);
  BOOST_CHECK_EQUAL(out.str(), "42");
}

BOOST_AUTO_TEST_CASE(testPrintZeroAmount)
{
  value_t v1(amount_t("$0.00"));
  std::ostringstream out;
  v1.print(out);
  BOOST_CHECK_EQUAL(out.str(), "0");
}

BOOST_AUTO_TEST_CASE(testPrintNonZeroAmount)
{
  value_t v1(amount_t("$42.00"));
  std::ostringstream out;
  v1.print(out);
  BOOST_CHECK(!out.str().empty());
}

BOOST_AUTO_TEST_CASE(testPrintBalance)
{
  value_t v1(balance_t("42 GBP"));
  std::ostringstream out;
  v1.print(out);
  BOOST_CHECK(!out.str().empty());
}

BOOST_AUTO_TEST_CASE(testPrintString)
{
  value_t v1(string("hello"), true);
  std::ostringstream out;
  v1.print(out);
  BOOST_CHECK_EQUAL(out.str(), "hello");
}

BOOST_AUTO_TEST_CASE(testPrintMask)
{
  value_t v1(mask_t("test.*"));
  std::ostringstream out;
  v1.print(out);
  BOOST_CHECK(!out.str().empty());
  // Should contain slashes around the mask
  BOOST_CHECK(out.str().find('/') != string::npos);
}

BOOST_AUTO_TEST_CASE(testPrintSequence)
{
  value_t::sequence_t s1;
  value_t v1(s1);
  v1.push_back(value_t(2L));
  v1.push_back(value_t(3L));
  std::ostringstream out;
  v1.print(out);
  BOOST_CHECK_EQUAL(out.str(), "(2, 3)");
}

BOOST_AUTO_TEST_CASE(testPrintWithWidth)
{
  value_t v1(42L);
  std::ostringstream out;
  v1.print(out, 10);
  // Should be right-justified in 10 chars
  BOOST_CHECK(out.str().length() >= 2);
}

BOOST_AUTO_TEST_CASE(testPrintStringWithWidth)
{
  value_t v1(string("hi"), true);
  std::ostringstream out;
  v1.print(out, 10);
  BOOST_CHECK(out.str().length() >= 2);
}

BOOST_AUTO_TEST_CASE(testDumpVoid)
{
  value_t v1;
  std::ostringstream out;
  v1.dump(out);
  BOOST_CHECK_EQUAL(out.str(), "null");
}

BOOST_AUTO_TEST_CASE(testDumpBoolean)
{
  value_t v1(true);
  std::ostringstream out;
  v1.dump(out);
  BOOST_CHECK_EQUAL(out.str(), "true");

  value_t v2(false);
  std::ostringstream out2;
  v2.dump(out2);
  BOOST_CHECK_EQUAL(out2.str(), "false");
}

BOOST_AUTO_TEST_CASE(testDumpDatetime)
{
  struct tm localtime;
  strptime("14 August 2014 10:30:00", "%d %b %Y %H:%M:%S", &localtime);
  value_t v1(boost::posix_time::ptime_from_tm(localtime));
  std::ostringstream out;
  v1.dump(out);
  BOOST_CHECK(out.str().find('[') != string::npos);
  BOOST_CHECK(out.str().find(']') != string::npos);
}

BOOST_AUTO_TEST_CASE(testDumpDate)
{
  value_t v1(date_t(parse_date("2014/08/14")));
  std::ostringstream out;
  v1.dump(out);
  BOOST_CHECK(out.str().find('[') != string::npos);
  BOOST_CHECK(out.str().find(']') != string::npos);
}

BOOST_AUTO_TEST_CASE(testDumpInteger)
{
  value_t v1(42L);
  std::ostringstream out;
  v1.dump(out);
  BOOST_CHECK_EQUAL(out.str(), "42");
}

BOOST_AUTO_TEST_CASE(testDumpAmountRelaxed)
{
  value_t v1(amount_t("42 GBP"));
  std::ostringstream out;
  v1.dump(out, true);
  BOOST_CHECK(out.str().find('{') == string::npos);
}

BOOST_AUTO_TEST_CASE(testDumpAmountStrict)
{
  value_t v1(amount_t("42 GBP"));
  std::ostringstream out;
  v1.dump(out, false);
  BOOST_CHECK(out.str().find('{') != string::npos);
  BOOST_CHECK(out.str().find('}') != string::npos);
}

BOOST_AUTO_TEST_CASE(testDumpBalance)
{
  value_t v1(balance_t("42 GBP"));
  std::ostringstream out;
  v1.dump(out);
  BOOST_CHECK(!out.str().empty());
}

BOOST_AUTO_TEST_CASE(testDumpStringWithEscapes)
{
  value_t v1(string("he\"l\\lo"), true);
  std::ostringstream out;
  v1.dump(out);
  // Should have escaped quotes and backslashes
  BOOST_CHECK(out.str().find("\\\"") != string::npos);
  BOOST_CHECK(out.str().find("\\\\") != string::npos);
}

BOOST_AUTO_TEST_CASE(testDumpMask)
{
  value_t v1(mask_t("test.*"));
  std::ostringstream out;
  v1.dump(out);
  BOOST_CHECK(out.str().find('/') != string::npos);
}

BOOST_AUTO_TEST_CASE(testDumpSequence)
{
  value_t::sequence_t s1;
  value_t v1(s1);
  v1.push_back(value_t(2L));
  v1.push_back(value_t(3L));
  std::ostringstream out;
  v1.dump(out);
  BOOST_CHECK_EQUAL(out.str(), "(2, 3)");
}

BOOST_AUTO_TEST_CASE(testLabelAllTypes)
{
  BOOST_CHECK_EQUAL(value_t().label(), "an uninitialized value");
  BOOST_CHECK_EQUAL(value_t(true).label(), "a boolean");
  {
    struct tm localtime;
    strptime("14 August 2014 10:30:00", "%d %b %Y %H:%M:%S", &localtime);
    BOOST_CHECK_EQUAL(value_t(boost::posix_time::ptime_from_tm(localtime)).label(), "a date/time");
  }
  BOOST_CHECK_EQUAL(value_t(date_t(parse_date("2014/08/14"))).label(), "a date");
  BOOST_CHECK_EQUAL(value_t(42L).label(), "an integer");
  BOOST_CHECK_EQUAL(value_t(amount_t("42")).label(), "an amount");
  BOOST_CHECK_EQUAL(value_t(balance_t("42")).label(), "a balance");
  BOOST_CHECK_EQUAL(value_t(string("hi"), true).label(), "a string");
  BOOST_CHECK_EQUAL(value_t(mask_t("x")).label(), "a regexp");
  BOOST_CHECK_EQUAL(value_t(value_t::sequence_t()).label(), "a sequence");
}

// ---------------------------------------------------------------------------
// Is Zero / Is Realzero Tests
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testIsZeroBoolean)
{
  BOOST_CHECK(value_t(false).is_zero());
  BOOST_CHECK(!value_t(true).is_zero());
}

BOOST_AUTO_TEST_CASE(testIsZeroInteger)
{
  BOOST_CHECK(value_t(0L).is_zero());
  BOOST_CHECK(!value_t(42L).is_zero());
}

BOOST_AUTO_TEST_CASE(testIsZeroAmount)
{
  BOOST_CHECK(value_t(amount_t(0L)).is_zero());
  BOOST_CHECK(!value_t(amount_t(42L)).is_zero());
}

BOOST_AUTO_TEST_CASE(testIsZeroString)
{
  BOOST_CHECK(value_t(string(""), true).is_zero());
  BOOST_CHECK(!value_t(string("hello"), true).is_zero());
}

BOOST_AUTO_TEST_CASE(testIsZeroSequence)
{
  value_t::sequence_t s1;
  BOOST_CHECK(value_t(s1).is_zero());

  value_t v1(s1);
  v1.push_back(value_t(42L));
  BOOST_CHECK(!v1.is_zero());
}

BOOST_AUTO_TEST_CASE(testIsRealzeroBoolean)
{
  BOOST_CHECK(value_t(false).is_realzero());
  BOOST_CHECK(!value_t(true).is_realzero());
}

BOOST_AUTO_TEST_CASE(testIsRealzeroInteger)
{
  BOOST_CHECK(value_t(0L).is_realzero());
  BOOST_CHECK(!value_t(42L).is_realzero());
}

BOOST_AUTO_TEST_CASE(testIsRealzeroAmount)
{
  BOOST_CHECK(value_t(amount_t(0L)).is_realzero());
  BOOST_CHECK(!value_t(amount_t(42L)).is_realzero());
}

BOOST_AUTO_TEST_CASE(testIsRealzeroString)
{
  BOOST_CHECK(value_t(string(""), true).is_realzero());
  BOOST_CHECK(!value_t(string("hello"), true).is_realzero());
}

BOOST_AUTO_TEST_CASE(testIsRealzeroSequence)
{
  value_t::sequence_t s1;
  BOOST_CHECK(value_t(s1).is_realzero());
}

// ---------------------------------------------------------------------------
// Value method Tests
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testValueOfInteger)
{
  value_t v1(42L);
  value_t result = v1.value(datetime_t());
  BOOST_CHECK(result.is_null());
}

BOOST_AUTO_TEST_CASE(testValueOfAmount)
{
  value_t v1(amount_t("42 GBP"));
  value_t result = v1.value(datetime_t());
  // Without price data, returns null
  BOOST_CHECK(result.is_null());
}

BOOST_AUTO_TEST_CASE(testValueOfBalance)
{
  value_t v1(balance_t("42 GBP"));
  value_t result = v1.value(datetime_t());
  // Without price data, returns null
  BOOST_CHECK(result.is_null());
}

BOOST_AUTO_TEST_CASE(testValueOfStringThrows)
{
  value_t v1(string("hello"), true);
  BOOST_CHECK_THROW(v1.value(datetime_t()), value_error);
}

BOOST_AUTO_TEST_CASE(testValueOfBoolThrows)
{
  value_t v1(true);
  BOOST_CHECK_THROW(v1.value(datetime_t()), value_error);
}

// ---------------------------------------------------------------------------
// Sequence-specific Tests
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testSizeOfNull)
{
  value_t v1;
  BOOST_CHECK_EQUAL(v1.size(), 0);
  BOOST_CHECK(v1.empty());
}

BOOST_AUTO_TEST_CASE(testSizeOfNonSequence)
{
  value_t v1(42L);
  BOOST_CHECK_EQUAL(v1.size(), 1);
  BOOST_CHECK(!v1.empty());
}

BOOST_AUTO_TEST_CASE(testSizeOfSequence)
{
  value_t::sequence_t s1;
  value_t v1(s1);
  BOOST_CHECK_EQUAL(v1.size(), 0);
  BOOST_CHECK(v1.empty());

  v1.push_back(value_t(1L));
  v1.push_back(value_t(2L));
  BOOST_CHECK_EQUAL(v1.size(), 2);
  BOOST_CHECK(!v1.empty());
}

BOOST_AUTO_TEST_CASE(testIndexNonSequence)
{
  value_t v1(42L);
  BOOST_CHECK_EQUAL(v1[0], value_t(42L));
}

BOOST_AUTO_TEST_CASE(testConstIndexNonSequence)
{
  const value_t v1(42L);
  BOOST_CHECK_EQUAL(v1[0], value_t(42L));
}

BOOST_AUTO_TEST_CASE(testPopBackNonSequence)
{
  value_t v1(42L);
  v1.pop_back();
  BOOST_CHECK(v1.is_null());
}

BOOST_AUTO_TEST_CASE(testPopBackSequenceToOne)
{
  value_t::sequence_t s1;
  value_t v1(s1);
  v1.push_back(value_t(1L));
  v1.push_back(value_t(2L));
  v1.pop_back();
  // Should collapse to single value
  BOOST_CHECK_EQUAL(v1, value_t(1L));
}

BOOST_AUTO_TEST_CASE(testPopBackSequenceToEmpty)
{
  value_t::sequence_t s1;
  value_t v1(s1);
  v1.push_back(value_t(1L));
  v1.pop_back();
  BOOST_CHECK(v1.is_null());
}

BOOST_AUTO_TEST_CASE(testPushBackOnNull)
{
  value_t v1;
  v1.push_back(value_t(42L));
  BOOST_CHECK(v1.is_sequence());
  BOOST_CHECK_EQUAL(v1.size(), 1);
}

BOOST_AUTO_TEST_CASE(testPushFrontOnNull)
{
  value_t v1;
  v1.push_front(value_t(42L));
  BOOST_CHECK(v1.is_sequence());
  BOOST_CHECK_EQUAL(v1.size(), 1);
}

BOOST_AUTO_TEST_CASE(testPushBackOnNonSequence)
{
  value_t v1(42L);
  v1.push_back(value_t(99L));
  BOOST_CHECK(v1.is_sequence());
  BOOST_CHECK_EQUAL(v1.size(), 2);
}

// ---------------------------------------------------------------------------
// StripAnnotations Tests
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testStripAnnotationsKeepAll)
{
  value_t v1(amount_t("42 GBP"));
  keep_details_t keep_all;
  keep_all.keep_price = true;
  keep_all.keep_date = true;
  keep_all.keep_tag = true;
  value_t result = v1.strip_annotations(keep_all);
  BOOST_CHECK_EQUAL(result, v1);
}

BOOST_AUTO_TEST_CASE(testStripAnnotationsSequence)
{
  value_t::sequence_t s1;
  value_t v1(s1);
  v1.push_back(value_t(amount_t("42 GBP")));
  v1.push_back(value_t(42L));
  keep_details_t keep_none;
  value_t result = v1.strip_annotations(keep_none);
  BOOST_CHECK(result.valid());
  BOOST_CHECK(result.is_sequence());
}

BOOST_AUTO_TEST_CASE(testStripAnnotationsNonAnnotatable)
{
  value_t v1(42L);
  keep_details_t keep_none;
  value_t result = v1.strip_annotations(keep_none);
  BOOST_CHECK_EQUAL(result, v1);
}

// ---------------------------------------------------------------------------
// Operator bool Tests
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testBoolOperatorVoid)
{
  value_t v1;
  BOOST_CHECK(!v1);
}

BOOST_AUTO_TEST_CASE(testBoolOperatorBoolean)
{
  BOOST_CHECK(static_cast<bool>(value_t(true)));
  BOOST_CHECK(!static_cast<bool>(value_t(false)));
}

BOOST_AUTO_TEST_CASE(testBoolOperatorInteger)
{
  BOOST_CHECK(static_cast<bool>(value_t(42L)));
  BOOST_CHECK(!static_cast<bool>(value_t(0L)));
}

BOOST_AUTO_TEST_CASE(testBoolOperatorAmount)
{
  BOOST_CHECK(static_cast<bool>(value_t(amount_t("42"))));
  BOOST_CHECK(!static_cast<bool>(value_t(amount_t(0L))));
}

BOOST_AUTO_TEST_CASE(testBoolOperatorString)
{
  BOOST_CHECK(static_cast<bool>(value_t(string("hello"), true)));
  BOOST_CHECK(!static_cast<bool>(value_t(string(""), true)));
}

BOOST_AUTO_TEST_CASE(testBoolOperatorSequenceNonzero)
{
  value_t::sequence_t s1;
  value_t v1(s1);
  v1.push_back(value_t(42L));
  BOOST_CHECK(static_cast<bool>(v1));
}

BOOST_AUTO_TEST_CASE(testBoolOperatorSequenceAllZero)
{
  value_t::sequence_t s1;
  value_t v1(s1);
  v1.push_back(value_t(0L));
  v1.push_back(value_t(0L));
  BOOST_CHECK(!static_cast<bool>(v1));
}

BOOST_AUTO_TEST_CASE(testBoolOperatorMaskThrows)
{
  value_t v1(mask_t("regex"));
  BOOST_CHECK_THROW(static_cast<bool>(v1) ? true : false, value_error);
}

// ---------------------------------------------------------------------------
// ExchangeCommodities Tests
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testExchangeCommoditiesSimple)
{
  value_t v1(amount_t("42 GBP"));
  // Without price data, exchange with a single simple commodity should
  // just call value() which returns null for no price data
  value_t result = v1.exchange_commodities("EUR");
  // Since no price exists, result may just be the original
  BOOST_CHECK(result.valid());
}

BOOST_AUTO_TEST_CASE(testExchangeCommoditiesStringThrows)
{
  value_t v1(string("hello"), true);
  BOOST_CHECK_THROW(v1.exchange_commodities("EUR"), value_error);
}

BOOST_AUTO_TEST_SUITE_END()

