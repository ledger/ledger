#define BOOST_TEST_DYN_LINK

#include <boost/test/unit_test.hpp>

#include <system.hh>

#include "format.h"

using namespace ledger;

struct format_fixture {
  format_fixture() {
    format_t::default_style = format_t::TRUNCATE_TRAILING;
    format_t::default_style_changed = false;
  }
  ~format_fixture() {
    format_t::default_style = format_t::TRUNCATE_TRAILING;
    format_t::default_style_changed = false;
  }
};

BOOST_FIXTURE_TEST_SUITE(format, format_fixture)

BOOST_AUTO_TEST_CASE(testTruncateTrailing)
{
  format_t::default_style = format_t::TRUNCATE_TRAILING;
  unistring str("abcd:1234:ABCD");
  BOOST_CHECK_EQUAL(format_t::truncate(str, 0, 0), "abcd:1234:ABCD");
  BOOST_CHECK_EQUAL(format_t::truncate(str, 99, 0), "abcd:1234:ABCD");

  BOOST_CHECK_EQUAL(format_t::truncate(str, 14, 0), "abcd:1234:ABCD");
  BOOST_CHECK_EQUAL(format_t::truncate(str, 13, 0), "abcd:1234:A..");
  BOOST_CHECK_EQUAL(format_t::truncate(str, 12, 0), "abcd:1234:..");
  BOOST_CHECK_EQUAL(format_t::truncate(str, 11, 0), "abcd:1234..");
  BOOST_CHECK_EQUAL(format_t::truncate(str, 10, 0), "abcd:123..");

  unistring ustr("中文:中文:中文");
  BOOST_CHECK_EQUAL(format_t::truncate(ustr, 14, 0), "中文:中文:中文");
  BOOST_CHECK_EQUAL(format_t::truncate(ustr, 13, 0), "中文:中文:...");
  BOOST_CHECK_EQUAL(format_t::truncate(ustr, 12, 0), "中文:中文:..");
  BOOST_CHECK_EQUAL(format_t::truncate(ustr, 11, 0), "中文:中文..");
  BOOST_CHECK_EQUAL(format_t::truncate(ustr, 10, 0), "中文:中...");
}

BOOST_AUTO_TEST_CASE(testTruncateMiddle)
{
  format_t::default_style = format_t::TRUNCATE_MIDDLE;
  unistring str("abcd:1234:ABCD");
  BOOST_CHECK_EQUAL(format_t::truncate(str, 0, 0), "abcd:1234:ABCD");
  BOOST_CHECK_EQUAL(format_t::truncate(str, 99, 0), "abcd:1234:ABCD");

  BOOST_CHECK_EQUAL(format_t::truncate(str, 14, 0), "abcd:1234:ABCD");
  BOOST_CHECK_EQUAL(format_t::truncate(str, 13, 0), "abcd:..4:ABCD");
  BOOST_CHECK_EQUAL(format_t::truncate(str, 12, 0), "abcd:..:ABCD");
  BOOST_CHECK_EQUAL(format_t::truncate(str, 11, 0), "abcd..:ABCD");
  BOOST_CHECK_EQUAL(format_t::truncate(str, 10, 0), "abcd..ABCD");

  unistring ustr("中文:中文:中文");
  BOOST_CHECK_EQUAL(format_t::truncate(ustr, 14, 0), "中文:中文:中文");
  BOOST_CHECK_EQUAL(format_t::truncate(ustr, 13, 0), "中文:...:中文");
  BOOST_CHECK_EQUAL(format_t::truncate(ustr, 12, 0), "中文:..:中文");
  BOOST_CHECK_EQUAL(format_t::truncate(ustr, 11, 0), "中文..:中文");
  BOOST_CHECK_EQUAL(format_t::truncate(ustr, 10, 0), "中文..中文");
}

BOOST_AUTO_TEST_CASE(testTruncateLeading)
{
  format_t::default_style = format_t::TRUNCATE_LEADING;
  unistring str("abcd:1234:ABCD");
  BOOST_CHECK_EQUAL(format_t::truncate(str, 0, 0), "abcd:1234:ABCD");
  BOOST_CHECK_EQUAL(format_t::truncate(str, 99, 0), "abcd:1234:ABCD");

  BOOST_CHECK_EQUAL(format_t::truncate(str, 14, 0), "abcd:1234:ABCD");
  BOOST_CHECK_EQUAL(format_t::truncate(str, 13, 0), "..d:1234:ABCD");
  BOOST_CHECK_EQUAL(format_t::truncate(str, 12, 0), "..:1234:ABCD");
  BOOST_CHECK_EQUAL(format_t::truncate(str, 11, 0), "..1234:ABCD");
  BOOST_CHECK_EQUAL(format_t::truncate(str, 10, 0), "..234:ABCD");

  unistring ustr("中文:中文:中文");
  BOOST_CHECK_EQUAL(format_t::truncate(ustr, 14, 0), "中文:中文:中文");
  BOOST_CHECK_EQUAL(format_t::truncate(ustr, 13, 0), "...:中文:中文");
  BOOST_CHECK_EQUAL(format_t::truncate(ustr, 12, 0), "..:中文:中文");
  BOOST_CHECK_EQUAL(format_t::truncate(ustr, 11, 0), "..中文:中文");
  BOOST_CHECK_EQUAL(format_t::truncate(ustr, 10, 0), "...文:中文");
}

BOOST_AUTO_TEST_CASE(testTruncateAbbreviate)
{
  format_t::default_style = format_t::ABBREVIATE;
  unistring str("abcd:1234:ABCD");
  BOOST_CHECK_EQUAL(format_t::truncate(str, 0, 2), "abcd:1234:ABCD");
  BOOST_CHECK_EQUAL(format_t::truncate(str, 99, 2), "abcd:1234:ABCD");
  BOOST_CHECK_EQUAL(format_t::truncate(str, 14, 2), "abcd:1234:ABCD");
  BOOST_CHECK_EQUAL(format_t::truncate(str, 13, 2), "abc:1234:ABCD");
  BOOST_CHECK_EQUAL(format_t::truncate(str, 12, 2), "ab:1234:ABCD");
  BOOST_CHECK_EQUAL(format_t::truncate(str, 11, 2), "ab:123:ABCD");
  BOOST_CHECK_EQUAL(format_t::truncate(str, 10, 2), "ab:12:ABCD");
  BOOST_CHECK_EQUAL(format_t::truncate(str, 9, 2), "..12:ABCD");
  BOOST_CHECK_EQUAL(format_t::truncate(str, 8, 2), "..2:ABCD");
  BOOST_CHECK_EQUAL(format_t::truncate(str, 7, 2), "..:ABCD");
  BOOST_CHECK_EQUAL(format_t::truncate(str, 6, 2), "..ABCD");
  BOOST_CHECK_EQUAL(format_t::truncate(str, 5, 2), "..BCD");

  unistring ustr("中文:中文:中文");
  BOOST_CHECK_EQUAL(format_t::truncate(ustr, 14, 2), "中文:中文:中文");
  BOOST_CHECK_EQUAL(format_t::truncate(ustr, 13, 2), "中.:中文:中文");
  BOOST_CHECK_EQUAL(format_t::truncate(ustr, 12, 2), "中:中文:中文");
  BOOST_CHECK_EQUAL(format_t::truncate(ustr, 11, 2), "中:中.:中文");
  BOOST_CHECK_EQUAL(format_t::truncate(ustr, 10, 2), "中:中:中文");
  BOOST_CHECK_EQUAL(format_t::truncate(ustr, 9, 2), "..中:中文");
  BOOST_CHECK_EQUAL(format_t::truncate(ustr, 8, 2), "...:中文");

  BOOST_CHECK_EQUAL(format_t::truncate(ustr, 14, 1), "中文:中文:中文");
  BOOST_CHECK_EQUAL(format_t::truncate(ustr, 13, 1), "中.:中文:中文");
  BOOST_CHECK_EQUAL(format_t::truncate(ustr, 12, 1), "中:中文:中文");
  BOOST_CHECK_EQUAL(format_t::truncate(ustr, 11, 1), ".:中文:中文");
  BOOST_CHECK_EQUAL(format_t::truncate(ustr, 10, 1), ".:中.:中文");
  BOOST_CHECK_EQUAL(format_t::truncate(ustr, 9, 1), ".:中:中文");
  BOOST_CHECK_EQUAL(format_t::truncate(ustr, 8, 1), ".:.:中文");
}

BOOST_AUTO_TEST_SUITE_END()
