#define BOOST_TEST_DYN_LINK

#include <boost/test/unit_test.hpp>

#include <system.hh>

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

  BOOST_CHECK_THROW(v8 == v10, value_error);

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
  BOOST_CHECK(v19.valid());
  BOOST_CHECK(v20.valid());
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

  BOOST_CHECK_THROW(v1.negated(), value_error);
  BOOST_CHECK_EQUAL(v2.negated(), value_t(false));
  v5.in_place_negate();
  BOOST_CHECK_EQUAL(v5, value_t(-2L));
  v8.in_place_negate();
  v9.in_place_negate();
  BOOST_CHECK_EQUAL(v8, v9);
  BOOST_CHECK_THROW(v10.negated(), value_error);
  BOOST_CHECK_EQUAL(-v12, v13);
  BOOST_CHECK_THROW(-v14, value_error);

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
}

BOOST_AUTO_TEST_SUITE_END()

