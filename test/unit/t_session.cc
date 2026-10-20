#define BOOST_TEST_DYN_LINK
#define BOOST_TEST_MODULE session
#include <boost/test/unit_test.hpp>

#include <system.hh>

#include "session.h"
#include "report.h"
#include "journal.h"
#include "xact.h"
#include "post.h"
#include "format.h"
#include "mask.h"
#include "times.h"

using namespace ledger;

// A program that reads several journals in one process gives each its own
// session, and ends each with set_session_context(nullptr).  Options and
// directives also change process-wide defaults, which that teardown must
// restore, or one journal's settings change how the next is read and
// reported (#3292).  Each test reads a journal that changes one of those
// settings in a session of its own, then checks that the next session sees
// what a fresh process would.

namespace {

// Makes a session, with a report over it, current for the object's
// lifetime; the destructor tears the session down again.
struct current_session_t {
  session_t session;
  report_t report;
  scope_t* saved_default_scope;

  current_session_t() : report(session), saved_default_scope(scope_t::default_scope) {
    set_session_context(&session);
    scope_t::default_scope = &report;
  }
  ~current_session_t() {
    scope_t::default_scope = saved_default_scope;
    set_session_context(nullptr);
  }
};

// Read a journal in a session of its own, normalizing the report's options
// as a command would, and then tear that session down.
void read_in_own_session(const std::string& text) {
  current_session_t current;
  current.session.read_journal_from_string(text);
  current.report.normalize_options("balance");
}

} // namespace

BOOST_AUTO_TEST_SUITE(session_teardown)

BOOST_AUTO_TEST_CASE(testDecimalCommaIsReset)
{
  read_in_own_session("--decimal-comma\n");

  current_session_t current;
  BOOST_CHECK(!commodity_t::decimal_comma_by_default);

  journal_t* journal = current.session.read_journal_from_string(
    "2024/01/01 Payee\n"
    "    Expenses:Food    $1,000.00\n"
    "    Assets:Cash\n");
  BOOST_REQUIRE_EQUAL(journal->xacts.size(), 1u);
  const amount_t& amount = journal->xacts.front()->posts.front()->amount;
  BOOST_CHECK(amount.number() == amount_t(1000L));
  BOOST_CHECK_EQUAL(amount.to_string(), "$1,000.00");
}

BOOST_AUTO_TEST_CASE(testTimeColonIsReset)
{
  read_in_own_session("--time-colon\n");

  current_session_t current;
  BOOST_CHECK(!commodity_t::time_colon_by_default);
  // Parsing reduces hours to seconds; reports show them unreduced.
  BOOST_CHECK_EQUAL(amount_t("1.50h").unreduced().to_string(), "1.50h");
}

BOOST_AUTO_TEST_CASE(testInputDateFormatIsReset)
{
  read_in_own_session("--input-date-format %d.%m.%Y\n");

  current_session_t current;
  // With no custom input format, '.' and '-' separate fields like '/'.
  BOOST_CHECK_EQUAL(parse_date("2025.06.15"), date_t(2025, 6, 15));
  BOOST_CHECK_EQUAL(parse_date("2025-06"), date_t(2025, 6, 1));
}

BOOST_AUTO_TEST_CASE(testYearDirectiveIsReset)
{
  // With no --now in effect, a year directive also sets the epoch.
  read_in_own_session("year 2020\n");

  current_session_t current;
  BOOST_CHECK(!year_directive_year);
  BOOST_CHECK(!epoch);

  journal_t* journal = current.session.read_journal_from_string(
    "01/15 Payee\n"
    "    Expenses:Food    $10.00\n"
    "    Assets:Cash\n");
  BOOST_REQUIRE_EQUAL(journal->xacts.size(), 1u);
  BOOST_CHECK_EQUAL(static_cast<int>(journal->xacts.front()->date().year()),
                    static_cast<int>(gregorian::day_clock::local_day().year()));
}

BOOST_AUTO_TEST_CASE(testNowIsReset)
{
  read_in_own_session("--now 2020/06/15\n");

  current_session_t current;
  BOOST_CHECK(!epoch);
  BOOST_CHECK(CURRENT_DATE() != date_t(2020, 6, 15));
}

BOOST_AUTO_TEST_CASE(testStartOfWeekIsReset)
{
  read_in_own_session("--start-of-week monday\n");

  current_session_t current;
  BOOST_CHECK(start_of_week == gregorian::Sunday);
  // Wednesday, January 3, 2024 falls in the week of Sunday, December 31.
  BOOST_CHECK_EQUAL(date_duration_t::find_nearest(date_t(2024, 1, 3), date_duration_t::WEEKS),
                    date_t(2023, 12, 31));
}

BOOST_AUTO_TEST_CASE(testPeriodShiftIsReset)
{
  read_in_own_session("--period-shift 15\n");

  current_session_t current;
  BOOST_CHECK_EQUAL(day_of_period, 0);
  BOOST_CHECK_EQUAL(date_duration_t::find_nearest(date_t(2024, 3, 20), date_duration_t::MONTHS),
                    date_t(2024, 3, 1));
}

BOOST_AUTO_TEST_CASE(testTruncateIsReset)
{
  read_in_own_session("--truncate leading\n");

  current_session_t current;
  BOOST_CHECK(format_t::default_style == format_t::TRUNCATE_TRAILING);
  BOOST_CHECK(!format_t::default_style_changed);

  unistring name("abcd:1234:ABCD");
  BOOST_CHECK_EQUAL(format_t::truncate(name, 10), "abcd:123..");
  // Without an explicit --truncate, --abbrev-len abbreviates instead.
  BOOST_CHECK_EQUAL(format_t::truncate(name, 10, 2), "ab:12:ABCD");
}

BOOST_AUTO_TEST_CASE(testIgnoreDiacriticsIsReset)
{
  read_in_own_session("--ignore-diacritics\n");

  current_session_t current;
  BOOST_CHECK(!ignore_diacritics);
  BOOST_CHECK(!mask_t("cafe").match("café"));
}

BOOST_AUTO_TEST_CASE(testAuxDateIsReset)
{
  read_in_own_session("--aux-date\n");

  current_session_t current;
  BOOST_CHECK(!item_t::use_aux_date);

  journal_t* journal = current.session.read_journal_from_string(
    "2024/01/01=2024/02/02 Payee\n"
    "    Expenses:Food    $10.00\n"
    "    Assets:Cash\n");
  BOOST_REQUIRE_EQUAL(journal->xacts.size(), 1u);
  BOOST_CHECK_EQUAL(journal->xacts.front()->date(), date_t(2024, 1, 1));
}

BOOST_AUTO_TEST_SUITE_END()
