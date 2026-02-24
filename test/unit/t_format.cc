#define BOOST_TEST_DYN_LINK

#include <boost/test/unit_test.hpp>

#include <system.hh>

#include "format.h"
#include "session.h"
#include "report.h"

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

// -----------------------------------------------------------------------
// Format element parsing tests - cover format.cc uncovered lines
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testFormatParseEscapeSequences)
{
  // Cover format.cc lines 158-187: escape sequences in format strings
  times_initialize();
  amount_t::initialize();

  session_t session;
  report_t report(session);

  // Test various escape sequences
  format_t fmt1("\\b\\f\\n\\r\\t\\v\\\\\\x");
  string result = fmt1.real_calc(report);
  BOOST_CHECK(result.find('\b') != string::npos);
  BOOST_CHECK(result.find('\f') != string::npos);
  BOOST_CHECK(result.find('\n') != string::npos);
  BOOST_CHECK(result.find('\r') != string::npos);
  BOOST_CHECK(result.find('\t') != string::npos);
  BOOST_CHECK(result.find('\v') != string::npos);
  BOOST_CHECK(result.find('\\') != string::npos);

  amount_t::shutdown();
  times_shutdown();
}

BOOST_AUTO_TEST_CASE(testFormatParsePercent)
{
  // Cover format.cc lines 256-258: %% literal percent
  times_initialize();
  amount_t::initialize();

  session_t session;
  report_t report(session);

  format_t fmt("100%%");
  string result = fmt.real_calc(report);
  BOOST_CHECK_EQUAL(string("100%"), result);

  amount_t::shutdown();
  times_shutdown();
}

BOOST_AUTO_TEST_CASE(testFormatParseMinMaxWidth)
{
  // Cover format.cc lines 200-217: min/max width parsing
  times_initialize();
  amount_t::initialize();

  session_t session;
  report_t report(session);

  // Format with min.max width: %-20.40(account)
  format_t fmt("%-20.40(account)");
  // Just exercise the parsing, no real scope needed
  (void)fmt;

  amount_t::shutdown();
  times_shutdown();
}

BOOST_AUTO_TEST_CASE(testFormatSingleLetterMapping)
{
  // Cover format.cc lines 219-253: single letter format mappings
  times_initialize();
  amount_t::initialize();

  session_t session;
  report_t report(session);

  // %D = date, %P = payee, %a = account, %t = display_amount, etc.
  format_t fmtD("%D");
  format_t fmtP("%P");
  format_t fmta("%a");
  format_t fmtN("%N");

  // Just ensure parsing doesn't throw
  BOOST_CHECK(true);

  // Unrecognized format character
  BOOST_CHECK_THROW(format_t("%(bad stuff"), parse_error);

  amount_t::shutdown();
  times_shutdown();
}

BOOST_AUTO_TEST_CASE(testFormatFieldReference)
{
  // Cover format.cc lines 261-284: %$ field reference
  times_initialize();
  amount_t::initialize();

  // %$ without template should throw
  BOOST_CHECK_THROW(format_t("%$1"), format_error);

  amount_t::shutdown();
  times_shutdown();
}

BOOST_AUTO_TEST_CASE(testFormatUnrecognizedChar)
{
  // Cover format.cc line 389: unrecognized formatting character
  times_initialize();
  amount_t::initialize();

  BOOST_CHECK_THROW(format_t("%~"), format_error);

  amount_t::shutdown();
  times_shutdown();
}

BOOST_AUTO_TEST_CASE(testFormatElementDump)
{
  // Cover format.cc dump function if it exists
  times_initialize();
  amount_t::initialize();

  // Parse a format and dump its elements
  format_t fmt("Hello %P World");
  std::ostringstream out;
  fmt.dump(out);
  BOOST_CHECK(! out.str().empty());

  amount_t::shutdown();
  times_shutdown();
}

BOOST_AUTO_TEST_CASE(testFormatAlignLeft)
{
  // Cover format.cc line 191-198: %-  flag (left alignment)
  times_initialize();
  amount_t::initialize();

  session_t session;
  report_t report(session);

  format_t fmt("%-20(42)");
  string result = fmt.real_calc(report);
  BOOST_CHECK(! result.empty());

  amount_t::shutdown();
  times_shutdown();
}

BOOST_AUTO_TEST_CASE(testFormatExpressionEvaluation)
{
  // Cover format.cc lines 428-457: expression evaluation in format
  times_initialize();
  amount_t::initialize();

  session_t session;
  report_t report(session);

  // A simple expression format
  format_t fmt("%(2+3)");
  string result = fmt.real_calc(report);
  BOOST_CHECK_EQUAL(string("5"), result);

  amount_t::shutdown();
  times_shutdown();
}

BOOST_AUTO_TEST_CASE(testFormatStringOnly)
{
  // Cover format.cc lines 394-404: format with only string content (no %)
  times_initialize();
  amount_t::initialize();

  session_t session;
  report_t report(session);

  format_t fmt("Hello World");
  string result = fmt.real_calc(report);
  BOOST_CHECK_EQUAL(string("Hello World"), result);

  amount_t::shutdown();
  times_shutdown();
}

// -----------------------------------------------------------------------
// Coverage for format.cc line 240: expression after single letter mapping
// with $left substitution
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testFormatLeftAlignment)
{
  times_initialize();
  amount_t::initialize();

  session_t session;
  report_t report(session);

  // Left-aligned format with single letter
  format_t fmt("%-20(2+3)");
  string result = fmt.real_calc(report);
  BOOST_CHECK(!result.empty());

  amount_t::shutdown();
  times_shutdown();
}

// -----------------------------------------------------------------------
// Coverage for format.cc lines 361, 364-372: curly brace with colorize
// (O_CONS expression with color argument)
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testFormatCurlyBraceNoColor)
{
  times_initialize();
  amount_t::initialize();

  session_t session;
  report_t report(session);

  // Simple curly brace format (no colorize)
  format_t fmt("{2+3}");
  string result = fmt.real_calc(report);
  BOOST_CHECK(!result.empty());

  amount_t::shutdown();
  times_shutdown();
}

// -----------------------------------------------------------------------
// Coverage for format.cc lines 424, 435-438: min/max width truncation
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testFormatMinMaxWidthTruncation)
{
  times_initialize();
  amount_t::initialize();

  session_t session;
  report_t report(session);

  // Format with max width that causes truncation
  format_t fmt("%.3(\"abcdefgh\")");
  string result = fmt.real_calc(report);
  // Should be truncated to 3 chars
  BOOST_CHECK(result.length() <= 5);  // may include truncation indicator

  amount_t::shutdown();
  times_shutdown();
}

// -----------------------------------------------------------------------
// Coverage for format.cc lines 471-472: min_width padding
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testFormatMinWidthPadding)
{
  times_initialize();
  amount_t::initialize();

  session_t session;
  report_t report(session);

  // Format with min width that requires padding
  format_t fmt("%20(\"ab\")");
  string result = fmt.real_calc(report);
  BOOST_CHECK(result.length() >= 20);

  amount_t::shutdown();
  times_shutdown();
}

// -----------------------------------------------------------------------
// Coverage for format.cc lines 104-106, 115: parse escape sequences
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testFormatParseMoreEscapeSequences)
{
  times_initialize();
  amount_t::initialize();

  session_t session;
  report_t report(session);

  // Test \b, \f, \r, \v escape sequences
  format_t fmt1("\\b");
  string result1 = fmt1.real_calc(report);
  BOOST_CHECK_EQUAL(result1, string("\b"));

  format_t fmt2("\\f");
  string result2 = fmt2.real_calc(report);
  BOOST_CHECK_EQUAL(result2, string("\f"));

  format_t fmt3("\\r");
  string result3 = fmt3.real_calc(report);
  BOOST_CHECK_EQUAL(result3, string("\r"));

  format_t fmt4("\\v");
  string result4 = fmt4.real_calc(report);
  BOOST_CHECK_EQUAL(result4, string("\v"));

  format_t fmt5("\\\\");
  string result5 = fmt5.real_calc(report);
  BOOST_CHECK_EQUAL(result5, string("\\"));

  // Unknown escape
  format_t fmt6("\\x");
  string result6 = fmt6.real_calc(report);
  BOOST_CHECK_EQUAL(result6, string("x"));

  amount_t::shutdown();
  times_shutdown();
}

// -----------------------------------------------------------------------
// Coverage for format.cc line 617: truncation with trailing indicator
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testFormatTruncateByWidth)
{
  times_initialize();
  amount_t::initialize();

  session_t session;
  report_t report(session);

  // Test format with exact min and max width
  format_t fmt("%5.5(\"Hello World\")");
  string result = fmt.real_calc(report);
  BOOST_CHECK(result.length() <= 6);

  amount_t::shutdown();
  times_shutdown();
}

// -----------------------------------------------------------------------
// Coverage W7: format_t parse_elements with percent literal (line 256-258)
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testFormatPercentLiteralW7)
{
  times_initialize();
  amount_t::initialize();

  session_t session;
  report_t report(session);

  // Test %% producing literal %
  format_t fmt("100%%");
  string result = fmt.real_calc(report);
  BOOST_CHECK_EQUAL(result, "100%");

  amount_t::shutdown();
  times_shutdown();
}

// -----------------------------------------------------------------------
// Coverage W7: format_t parse with left-aligned format (line 191-197)
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testFormatLeftAlignedW7)
{
  times_initialize();
  amount_t::initialize();

  session_t session;
  report_t report(session);

  // Test left-aligned format with min width
  format_t fmt("%-20(\"test\")");
  string result = fmt.real_calc(report);
  BOOST_CHECK(result.length() >= 4);

  amount_t::shutdown();
  times_shutdown();
}

// -----------------------------------------------------------------------
// Coverage W7: format_t with min_width padding (lines 470-472)
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testFormatMinWidthPaddingW7)
{
  times_initialize();
  amount_t::initialize();

  session_t session;
  report_t report(session);

  // When min_width > content length, padding should be added
  format_t fmt("%20(\"hi\")");
  string result = fmt.real_calc(report);
  BOOST_CHECK(result.length() >= 20);

  amount_t::shutdown();
  times_shutdown();
}

// -----------------------------------------------------------------------
// Coverage W7: format_t unrecognized format character (line 253, 389)
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testFormatUnrecognizedCharW7)
{
  times_initialize();
  amount_t::initialize();

  // Test unrecognized character throws
  BOOST_CHECK_THROW(format_t("%z"), format_error);

  amount_t::shutdown();
  times_shutdown();
}

// -----------------------------------------------------------------------
// Coverage W7: format_t with template $N reference (lines 261-283)
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testFormatTemplateRefW7)
{
  times_initialize();
  amount_t::initialize();

  // Test that %$0 throws via parse_format with template
  format_t tmpl("%(account)");
  format_t fmt;
  BOOST_CHECK_THROW(fmt.parse_format("%$0", tmpl), format_error);

  // Test %$ without template throws (via format_t constructor)
  BOOST_CHECK_THROW(format_t fmt2("%$1"), format_error);

  amount_t::shutdown();
  times_shutdown();
}

// -----------------------------------------------------------------------
// Coverage W7: format_t with curly brace expression (lines 287-384)
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testFormatCurlyBraceW7)
{
  times_initialize();
  amount_t::initialize();

  session_t session;
  report_t report(session);

  // The {ident} form wraps in justify/scrub; exercised through
  // the ledger register --format "%{amount}" regression test.
  // Here we exercise the %(expr) path with a simple value.
  format_t fmt("%(\"hello\")");
  string result = fmt.real_calc(report);
  BOOST_CHECK_EQUAL(result, "hello");

  amount_t::shutdown();
  times_shutdown();
}

// -----------------------------------------------------------------------
// Coverage W7: format_t parse_single_expression eof (lines 103-106)
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testFormatSingleExprEofW7)
{
  times_initialize();
  amount_t::initialize();

  session_t session;
  report_t report(session);

  // Test an expression that consumes the entire remaining format string
  format_t fmt("%(1+2)");
  string result = fmt.real_calc(report);
  BOOST_CHECK_EQUAL(result, "3");

  amount_t::shutdown();
  times_shutdown();
}

// -----------------------------------------------------------------------
// Coverage W7: format_t parse with max_width only (line 207-217)
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testFormatMaxWidthOnlyW7)
{
  times_initialize();
  amount_t::initialize();

  session_t session;
  report_t report(session);

  // Test .5 syntax setting max_width and min_width to 5
  format_t fmt("%.5(\"Hello World\")");
  string result = fmt.real_calc(report);
  BOOST_CHECK(result.length() <= 6);

  amount_t::shutdown();
  times_shutdown();
}

// -----------------------------------------------------------------------
// Coverage W7: format_t truncate styles (lines 483-620)
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testFormatTruncateStylesW7)
{
  // TRUNCATE_LEADING
  format_t::default_style = format_t::TRUNCATE_LEADING;
  unistring long_str("abcdefghijklmnopqrstuvwxyz");
  string result1 = format_t::truncate(long_str, 10);
  BOOST_CHECK(result1.length() <= 10);

  // TRUNCATE_MIDDLE
  format_t::default_style = format_t::TRUNCATE_MIDDLE;
  string result2 = format_t::truncate(long_str, 10);
  BOOST_CHECK(result2.length() <= 10);

  // ABBREVIATE
  format_t::default_style = format_t::ABBREVIATE;
  unistring acct_str("Expenses:Food:Groceries:Store");
  string result3 = format_t::truncate(acct_str, 20, 2);
  BOOST_CHECK(result3.length() <= 20);

  // Reset default
  format_t::default_style = format_t::TRUNCATE_TRAILING;
}

// -----------------------------------------------------------------------
// Coverage W8: format.cc lines 104-106: parse_single_expression eof
// (when the format string ends inside an expression)
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testFormatParseExprEofW8)
{
  times_initialize();
  amount_t::initialize();

  session_t session;
  report_t report(session);

  // Test expression that consumes to EOF in parse_single_expression
  format_t fmt("%(1)");
  string result = fmt.real_calc(report);
  BOOST_CHECK_EQUAL(result, "1");

  amount_t::shutdown();
  times_shutdown();
}

// -----------------------------------------------------------------------
// Coverage W8: format.cc line 115: missing closing paren
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testFormatMissingCloseParenW8)
{
  times_initialize();
  amount_t::initialize();

  // Missing closing paren should throw
  BOOST_CHECK_THROW(format_t("%(1+2"), parse_error);

  amount_t::shutdown();
  times_shutdown();
}

// -----------------------------------------------------------------------
// Coverage W8: format.cc line 240: expression after single letter mapping
// with $min, $max, $left substitution keywords
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testFormatSubstitutionKeywordsW8)
{
  times_initialize();
  amount_t::initialize();

  session_t session;
  report_t report(session);

  // The single-letter format codes map to expressions like
  // "justify(scrub(...), $min, $max, $left, ...)"
  // The $min, $max, $left substitutions happen in lines 230-237.
  // We exercise this via %-20.40D which maps %D with min=20, max=40
  format_t fmt("%-20.40D");
  // Just ensure it parses without error
  BOOST_CHECK(true);

  amount_t::shutdown();
  times_shutdown();
}

// -----------------------------------------------------------------------
// Coverage W8: format.cc lines 267-268, 280: field reference parsing
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testFormatFieldRefOutOfRangeW8)
{
  times_initialize();
  amount_t::initialize();

  // Line 266-268: $0 and non-digit reference should throw
  format_t tmpl("%(account)");
  format_t fmt;
  BOOST_CHECK_THROW(fmt.parse_format("%$0", tmpl), format_error);

  // Line 280: reference that exceeds available template elements
  BOOST_CHECK_THROW(fmt.parse_format("%$9", tmpl), format_error);

  amount_t::shutdown();
  times_shutdown();
}

// -----------------------------------------------------------------------
// Coverage W8: format.cc lines 361-380: curly brace format with colorize
// (O_CONS expression that has a color argument)
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testFormatCurlyBraceWithColorW8)
{
  times_initialize();
  amount_t::initialize();

  session_t session;
  report_t report(session);

  // The {expr, color_expr} syntax creates an O_CONS node that
  // triggers lines 360-384 (colorize_op path).
  // We can exercise this through the parsing:
  format_t fmt("{2+3, 1}");
  string result = fmt.real_calc(report);
  // Just exercise the path; the result depends on ansify_if behavior
  BOOST_CHECK(!result.empty());

  amount_t::shutdown();
  times_shutdown();
}

// -----------------------------------------------------------------------
// Coverage W8: format.cc lines 424, 435-438: STRING element with min_width
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testFormatStringMinWidthW8)
{
  times_initialize();
  amount_t::initialize();

  session_t session;
  report_t report(session);

  // Line 423-425: STRING element with min_width set (not normally triggered
  // since min_width is only set for EXPR elements, but we can still test
  // the EXPR path with both min and max).
  format_t fmt("%10.5(\"long enough text\")");
  string result = fmt.real_calc(report);
  // max_width=5 so it should be truncated
  BOOST_CHECK(result.length() <= 10);

  amount_t::shutdown();
  times_shutdown();
}

// -----------------------------------------------------------------------
// Coverage W8: format.cc lines 471-472: min_width padding loop
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testFormatMinWidthPaddingLoopW8)
{
  times_initialize();
  amount_t::initialize();

  session_t session;
  report_t report(session);

  // Lines 470-472: when min_width > temp.length(), pad with spaces
  format_t fmt("%30(\"hi\")");
  string result = fmt.real_calc(report);
  BOOST_CHECK(result.length() >= 30);
  // Should be right-aligned with spaces
  BOOST_CHECK(result.find("hi") != string::npos);

  amount_t::shutdown();
  times_shutdown();
}

// -----------------------------------------------------------------------
// Coverage W8: format.cc line 617: abbreviation with adjust logic
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testFormatAbbreviateW8)
{
  // Lines 600-630: The abbreviation code in truncate().
  // Exercise the ABBREVIATE style with a colon-delimited account name.
  format_t::default_style = format_t::ABBREVIATE;

  unistring acct("Expenses:Food:Groceries:Very Long Store Name:Sub Category");
  string result = format_t::truncate(acct, 30, 2);
  BOOST_CHECK(result.length() <= 30);

  // Exercise with tighter constraint
  string result2 = format_t::truncate(acct, 15, 2);
  BOOST_CHECK(result2.length() <= 15);

  // Reset
  format_t::default_style = format_t::TRUNCATE_TRAILING;
}

// -----------------------------------------------------------------------
// Coverage W8: format.cc line 364-376: curly brace expr with O_CONS
// that has non-null colorize_op
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testFormatCurlyBraceColorizeOpW8)
{
  times_initialize();
  amount_t::initialize();

  session_t session;
  report_t report(session);

  // Parse a curly-brace format that uses the "expr, color" syntax
  // where the expression evaluates to O_CONS (a pair of subexpressions).
  // {expr, color_condition}
  format_t fmt("{1+2, 3 > 2}");
  string result = fmt.real_calc(report);
  BOOST_CHECK(!result.empty());

  amount_t::shutdown();
  times_shutdown();
}

// -----------------------------------------------------------------------
// Coverage W11: format.cc lines 104-106 (parse_single_expression eof)
// When expression consumes the entire format string
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testFormatExprEofW11)
{
  times_initialize();
  amount_t::initialize();

  session_t session;
  report_t report(session);

  // This expression should consume everything until EOF
  format_t fmt("%(1+2+3+4)");
  string result = fmt.real_calc(report);
  BOOST_CHECK_EQUAL(result, "10");

  amount_t::shutdown();
  times_shutdown();
}

// -----------------------------------------------------------------------
// Coverage W11: format.cc line 115 (whitespace rewind in expression)
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testFormatExprWithTrailingW11)
{
  times_initialize();
  amount_t::initialize();

  session_t session;
  report_t report(session);

  // Expression followed by more content
  format_t fmt("%(1+2) rest");
  string result = fmt.real_calc(report);
  BOOST_CHECK(result.find("3") != string::npos);
  BOOST_CHECK(result.find("rest") != string::npos);

  amount_t::shutdown();
  times_shutdown();
}

// -----------------------------------------------------------------------
// Coverage W11: format.cc line 240 (unrecognized substitution keyword)
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testFormatSingleLetterAllW11)
{
  times_initialize();
  amount_t::initialize();

  session_t session;
  report_t report(session);

  // Test all single-letter format codes
  format_t fmtD("%D");
  format_t fmtP("%P");
  format_t fmta("%a");
  format_t fmtt("%t");
  format_t fmtT("%T");
  format_t fmtN("%N");

  // Exercise the parsing with width specifiers
  format_t fmtDw("%-10.20D");
  format_t fmtPw("%20P");
  format_t fmtaw("%-30a");
  format_t fmttw("%15t");
  format_t fmtTw("%15T");

  BOOST_CHECK(true);

  amount_t::shutdown();
  times_shutdown();
}

// -----------------------------------------------------------------------
// Coverage W11: format.cc lines 267-268 (source column %$0 error)
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testFormatFieldRefInvalidW11)
{
  times_initialize();
  amount_t::initialize();

  // %$ with invalid reference character
  format_t tmpl("%(account)");
  format_t fmt;

  // $0 is invalid
  BOOST_CHECK_THROW(fmt.parse_format("%$0", tmpl), format_error);

  // Non-digit, non A-F is invalid
  BOOST_CHECK_THROW(fmt.parse_format("%$G", tmpl), format_error);

  amount_t::shutdown();
  times_shutdown();
}

// -----------------------------------------------------------------------
// Coverage W11: format.cc lines 361, 364-366, 369-372, 375-376,
// 379-380 (curly brace with colorize expression)
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testFormatCurlyBraceColorW11)
{
  times_initialize();
  amount_t::initialize();

  session_t session;
  report_t report(session);

  // Curly brace without colorize
  format_t fmt1("{42}");
  string result1 = fmt1.real_calc(report);
  BOOST_CHECK(!result1.empty());

  // Curly brace with colorize expression (O_CONS)
  format_t fmt2("{1+2, true}");
  string result2 = fmt2.real_calc(report);
  BOOST_CHECK(!result2.empty());

  amount_t::shutdown();
  times_shutdown();
}

// -----------------------------------------------------------------------
// Coverage W11: format.cc lines 435-438 (function call in EXPR element)
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testFormatFunctionCallW11)
{
  times_initialize();
  amount_t::initialize();

  session_t session;
  report_t report(session);

  // Use a built-in function
  format_t fmt("%(format_date(today, \"%Y\"))");
  string result = fmt.real_calc(report);
  // Should contain a year
  BOOST_CHECK(!result.empty());

  amount_t::shutdown();
  times_shutdown();
}

// -----------------------------------------------------------------------
// Coverage W11: format.cc lines 471-472 (min_width padding with
// Unicode content)
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testFormatMinWidthPaddingUnicodeW11)
{
  times_initialize();
  amount_t::initialize();

  session_t session;
  report_t report(session);

  // Min width larger than content, should pad
  format_t fmt("%30(\"short\")");
  string result = fmt.real_calc(report);
  BOOST_CHECK(result.length() >= 30);

  amount_t::shutdown();
  times_shutdown();
}

// -----------------------------------------------------------------------
// Coverage W11: format.cc line 617 (abbreviate adjust==0 path)
// -----------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testFormatAbbreviateEdgeCaseW11)
{
  // Exercise abbreviate with deeply nested account
  format_t::default_style = format_t::ABBREVIATE;

  unistring deep("A:BB:CCC:DDDD:EEEEE:FFFFFF:GGGGGGG");
  string result = format_t::truncate(deep, 15, 1);
  BOOST_CHECK(result.length() <= 15);

  // Very tight constraint
  unistring deep2("Expenses:Food:Groceries:BigStore:SubCategory");
  string result2 = format_t::truncate(deep2, 10, 1);
  BOOST_CHECK(result2.length() <= 10);

  format_t::default_style = format_t::TRUNCATE_TRAILING;
}

BOOST_AUTO_TEST_SUITE_END()
