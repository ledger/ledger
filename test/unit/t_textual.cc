#define BOOST_TEST_DYN_LINK
#define BOOST_TEST_MODULE textual
#include <boost/test/unit_test.hpp>

#include <system.hh>

#include "session.h"
#include "journal.h"
#include "context.h"
#include "account.h"
#include "xact.h"
#include "post.h"
#include "amount.h"
#include "error.h"
#include "pool.h"
#include "utils.h"

using namespace ledger;

struct textual_fixture {
  session_t session;
  scope_t*  saved_default_scope;

  textual_fixture() {
    set_session_context(&session);
    saved_default_scope = scope_t::default_scope;
    scope_t::default_scope = &session;
    amount_t::stream_fullstrings = true;
  }

  ~textual_fixture() {
    scope_t::default_scope = saved_default_scope;
    set_session_context(NULL);
  }

  // Parse a string as journal input and return the number of transactions
  // successfully parsed.  The journal is accessible via session.journal.
  std::size_t parse_text(const std::string& input) {
    session.journal.reset(new journal_t);

    shared_ptr<std::istream> stream(new std::istringstream(input));
    session.parsing_context.push(stream);

    parse_context_t& ctx = session.parsing_context.get_current();
    ctx.journal  = session.journal.get();
    ctx.master   = session.journal->master;

    std::size_t count = 0;
    try {
      count = session.journal->read(session.parsing_context, NO_HASHES);
    } catch (...) {
      session.parsing_context.pop();
      throw;
    }
    session.parsing_context.pop();
    return count;
  }
};

BOOST_FIXTURE_TEST_SUITE(textual_parser, textual_fixture)

// ---------------------------------------------------------------------------
// 1. Basic Parsing
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testParseEmptyString)
{
  // An empty journal should produce no transactions and no errors
  std::size_t count = parse_text("");
  BOOST_CHECK_EQUAL(0u, count);
  BOOST_CHECK(session.journal->xacts.empty());
}

BOOST_AUTO_TEST_CASE(testParseSimpleTransaction)
{
  // A simple two-posting transaction should parse successfully
  std::string input =
    "2024/01/15 Grocery Store\n"
    "    Expenses:Food                $50.00\n"
    "    Assets:Checking\n";

  std::size_t count = parse_text(input);
  BOOST_CHECK_EQUAL(1u, count);
  BOOST_CHECK_EQUAL(1u, session.journal->xacts.size());

  xact_t* xact = session.journal->xacts.front();
  BOOST_CHECK_EQUAL(string("Grocery Store"), xact->payee);
  BOOST_CHECK_EQUAL(2u, xact->posts.size());

  // First posting should be to Expenses:Food with $50.00
  post_t* first = xact->posts.front();
  BOOST_CHECK(first->account != NULL);
  BOOST_CHECK_EQUAL(string("Expenses:Food"), first->account->fullname());
  BOOST_CHECK(!first->amount.is_null());

  // Second posting should be to Assets:Checking with a calculated amount
  post_t* second = xact->posts.back();
  BOOST_CHECK(second->account != NULL);
  BOOST_CHECK_EQUAL(string("Assets:Checking"), second->account->fullname());
}

BOOST_AUTO_TEST_CASE(testParseClearedTransaction)
{
  // A transaction with a cleared marker (*) should have CLEARED state
  std::string input =
    "2024/01/15 * Payee Name\n"
    "    Expenses:Food                $25.00\n"
    "    Assets:Checking\n";

  std::size_t count = parse_text(input);
  BOOST_CHECK_EQUAL(1u, count);

  xact_t* xact = session.journal->xacts.front();
  BOOST_CHECK_EQUAL(item_t::CLEARED, xact->_state);
  BOOST_CHECK_EQUAL(string("Payee Name"), xact->payee);
}

BOOST_AUTO_TEST_CASE(testParsePendingTransaction)
{
  // A transaction with a pending marker (!) should have PENDING state
  std::string input =
    "2024/01/15 ! Pending Payee\n"
    "    Expenses:Food                $10.00\n"
    "    Assets:Checking\n";

  std::size_t count = parse_text(input);
  BOOST_CHECK_EQUAL(1u, count);

  xact_t* xact = session.journal->xacts.front();
  BOOST_CHECK_EQUAL(item_t::PENDING, xact->_state);
}

BOOST_AUTO_TEST_CASE(testParseTransactionWithNote)
{
  // A transaction with a note/comment on the transaction line
  std::string input =
    "2024/01/15 Grocery Store  ; This is a note\n"
    "    Expenses:Food                $50.00\n"
    "    Assets:Checking\n";

  std::size_t count = parse_text(input);
  BOOST_CHECK_EQUAL(1u, count);

  xact_t* xact = session.journal->xacts.front();
  BOOST_CHECK(xact->note);
}

// ---------------------------------------------------------------------------
// 2. Error Paths
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testParseInvalidDateThrows)
{
  // A line starting with a digit but having an invalid date format
  // should produce a parse error
  std::string input =
    "not-a-date Grocery Store\n"
    "    Expenses:Food                $50.00\n"
    "    Assets:Checking\n";

  // This does not start with a digit, so it goes through
  // general_directive.  Instead, use something that starts with a digit
  // but is an invalid date.
  std::string input2 =
    "9999/99/99 Grocery Store\n"
    "    Expenses:Food                $50.00\n"
    "    Assets:Checking\n";

  BOOST_CHECK_THROW(parse_text(input2), error_count);
}

BOOST_AUTO_TEST_CASE(testParseUnbalancedTransactionThrows)
{
  // A transaction where the amounts do not balance should throw
  std::string input =
    "2024/01/15 Grocery Store\n"
    "    Expenses:Food                $50.00\n"
    "    Assets:Checking              $-25.00\n";

  BOOST_CHECK_THROW(parse_text(input), error_count);
}

BOOST_AUTO_TEST_CASE(testParsePostingWithNoAccountThrows)
{
  // A posting line that has no account name (just whitespace then a comment)
  // should produce an error
  std::string input =
    "2024/01/15 Grocery Store\n"
    "    ;just a comment, no posting\n";

  // This is actually treated as a note on the transaction, not an error.
  // Instead, test a line that has whitespace but then invalid content.
  std::string input2 =
    "2024/01/15 Grocery Store\n"
    "    Expenses:Food                $50.00\n"
    "    \n";

  // A completely empty line terminates the transaction; the transaction
  // will have one posting but can still finalize (null posting auto-fills).
  // This is valid, so let us test something truly invalid.

  // A posting that starts with only a state flag but no account
  std::string input3 =
    "2024/01/15 Grocery Store\n"
    "    *\n";

  BOOST_CHECK_THROW(parse_text(input3), error_count);
}

BOOST_AUTO_TEST_CASE(testParseLeadingWhitespaceThrows)
{
  // A line that begins with whitespace outside of a transaction
  // should throw "Unexpected whitespace at beginning of line"
  std::string input =
    "    orphan posting line\n";

  BOOST_CHECK_THROW(parse_text(input), error_count);
}

BOOST_AUTO_TEST_CASE(testParseIncludeNonexistentFileThrows)
{
  // Including a file that does not exist should throw an error
  std::string input =
    "include /nonexistent/path/to/file.dat\n";

  BOOST_CHECK_THROW(parse_text(input), error_count);
}

BOOST_AUTO_TEST_CASE(testParseDirectiveWithoutArgument)
{
  // The "account" directive without an argument should throw
  std::string input = "account\n";

  BOOST_CHECK_THROW(parse_text(input), error_count);
}

BOOST_AUTO_TEST_CASE(testParseYearDirectiveInvalidArgument)
{
  // "Y" with a non-numeric argument should throw
  std::string input = "Y abc\n";

  BOOST_CHECK_THROW(parse_text(input), error_count);
}

BOOST_AUTO_TEST_CASE(testParseEndApplyWithoutApplyThrows)
{
  // "end apply" without a matching "apply" should throw
  std::string input = "end apply account\n";

  BOOST_CHECK_THROW(parse_text(input), error_count);
}

// ---------------------------------------------------------------------------
// 3. Directive Parsing
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testAccountDirective)
{
  // The "account" directive should register the account as ACCOUNT_KNOWN
  std::string input = "account Expenses:Food\n";

  std::size_t count = parse_text(input);
  BOOST_CHECK_EQUAL(0u, count);  // no transactions

  account_t* acct = session.journal->find_account("Expenses:Food", false);
  BOOST_CHECK(acct != NULL);
  BOOST_CHECK(acct->has_flags(ACCOUNT_KNOWN));
}

BOOST_AUTO_TEST_CASE(testAccountDirectiveWithNote)
{
  // The "account" directive with a "note" sub-directive should set the note
  std::string input =
    "account Expenses:Travel\n"
    "    note Business travel expenses\n";

  parse_text(input);

  account_t* acct = session.journal->find_account("Expenses:Travel", false);
  BOOST_CHECK(acct != NULL);
  BOOST_CHECK(acct->note);
  BOOST_CHECK_EQUAL(string("Business travel expenses"), *acct->note);
}

BOOST_AUTO_TEST_CASE(testCommodityDirective)
{
  // The "commodity" directive should create the commodity in the pool.
  // The COMMODITY_KNOWN flag is only set when checking_style >= CHECK_WARNING,
  // so we enable strict checking to verify it.
  std::string input = "commodity $\n";

  session.journal.reset(new journal_t);
  session.journal->checking_style = journal_t::CHECK_WARNING;

  shared_ptr<std::istream> stream(new std::istringstream(input));
  session.parsing_context.push(stream);
  parse_context_t& ctx = session.parsing_context.get_current();
  ctx.journal  = session.journal.get();
  ctx.master   = session.journal->master;
  try {
    session.journal->read(session.parsing_context, NO_HASHES);
  } catch (...) {
    session.parsing_context.pop();
    throw;
  }
  session.parsing_context.pop();

  commodity_t* comm = commodity_pool_t::current_pool->find("$");
  BOOST_CHECK(comm != NULL);
  BOOST_CHECK(comm->has_flags(COMMODITY_KNOWN));
}

BOOST_AUTO_TEST_CASE(testPayeeDirective)
{
  // The "payee" directive should register the payee.
  // Payees are tracked in known_payees only when check_payees is true
  // and checking_style >= CHECK_WARNING.  Enable strict mode to verify.
  std::string input =
    "payee Grocery Store\n"
    "    alias Groceries\n";

  session.journal.reset(new journal_t);
  session.journal->check_payees = true;
  session.journal->checking_style = journal_t::CHECK_WARNING;

  shared_ptr<std::istream> stream(new std::istringstream(input));
  session.parsing_context.push(stream);
  parse_context_t& ctx = session.parsing_context.get_current();
  ctx.journal  = session.journal.get();
  ctx.master   = session.journal->master;
  try {
    session.journal->read(session.parsing_context, NO_HASHES);
  } catch (...) {
    session.parsing_context.pop();
    throw;
  }
  session.parsing_context.pop();

  // The payee should be known
  BOOST_CHECK(session.journal->known_payees.find("Grocery Store") !=
              session.journal->known_payees.end());

  // The alias should map to the payee
  BOOST_CHECK(!session.journal->payee_alias_mappings.empty());
}

BOOST_AUTO_TEST_CASE(testAliasDirective)
{
  // The "alias" directive should create an account alias
  std::string input = "alias Food=Expenses:Food:Groceries\n";

  parse_text(input);

  // The alias mapping should exist
  BOOST_CHECK(!session.journal->account_aliases.empty());
  accounts_map::const_iterator it =
    session.journal->account_aliases.find("Food");
  BOOST_CHECK(it != session.journal->account_aliases.end());
  BOOST_CHECK_EQUAL(string("Expenses:Food:Groceries"),
                    it->second->fullname());
}

BOOST_AUTO_TEST_CASE(testTagDirective)
{
  // The "tag" directive should register the tag as known metadata.
  // Tags are only added to known_tags when checking_style >= CHECK_WARNING.
  std::string input = "tag ProjectCode\n";

  session.journal.reset(new journal_t);
  session.journal->checking_style = journal_t::CHECK_WARNING;

  shared_ptr<std::istream> stream(new std::istringstream(input));
  session.parsing_context.push(stream);
  parse_context_t& ctx = session.parsing_context.get_current();
  ctx.journal  = session.journal.get();
  ctx.master   = session.journal->master;
  try {
    session.journal->read(session.parsing_context, NO_HASHES);
  } catch (...) {
    session.parsing_context.pop();
    throw;
  }
  session.parsing_context.pop();

  BOOST_CHECK(session.journal->known_tags.find("ProjectCode") !=
              session.journal->known_tags.end());
}

BOOST_AUTO_TEST_CASE(testCommentDirective)
{
  // A "comment" block should be completely ignored
  std::string input =
    "comment\n"
    "This is a multi-line comment.\n"
    "It should be completely ignored by the parser.\n"
    "end comment\n"
    "2024/01/15 After Comment\n"
    "    Expenses:Food                $10.00\n"
    "    Assets:Checking\n";

  std::size_t count = parse_text(input);
  BOOST_CHECK_EQUAL(1u, count);

  xact_t* xact = session.journal->xacts.front();
  BOOST_CHECK_EQUAL(string("After Comment"), xact->payee);
}

BOOST_AUTO_TEST_CASE(testYearDirective)
{
  // The "year" or "Y" directive should allow partial dates to resolve
  // within the specified year
  std::string input =
    "Y 2023\n"
    "2023/06/15 Test Payee\n"
    "    Expenses:Food                $10.00\n"
    "    Assets:Checking\n";

  std::size_t count = parse_text(input);
  BOOST_CHECK_EQUAL(1u, count);

  xact_t* xact = session.journal->xacts.front();
  BOOST_CHECK_EQUAL(date_t(2023, 6, 15), xact->date());
}

// ---------------------------------------------------------------------------
// 4. Edge Cases
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testParseMultipleTransactions)
{
  // Multiple transactions should all be parsed
  std::string input =
    "2024/01/01 First\n"
    "    Expenses:A                   $10.00\n"
    "    Assets:Checking\n"
    "\n"
    "2024/01/02 Second\n"
    "    Expenses:B                   $20.00\n"
    "    Assets:Checking\n"
    "\n"
    "2024/01/03 Third\n"
    "    Expenses:C                   $30.00\n"
    "    Assets:Checking\n";

  std::size_t count = parse_text(input);
  BOOST_CHECK_EQUAL(3u, count);
  BOOST_CHECK_EQUAL(3u, session.journal->xacts.size());

  // Verify order
  xacts_list::iterator it = session.journal->xacts.begin();
  BOOST_CHECK_EQUAL(string("First"), (*it)->payee);
  ++it;
  BOOST_CHECK_EQUAL(string("Second"), (*it)->payee);
  ++it;
  BOOST_CHECK_EQUAL(string("Third"), (*it)->payee);
}

BOOST_AUTO_TEST_CASE(testParseVirtualPosting)
{
  // A posting with [Account] syntax should be virtual and must-balance
  std::string input =
    "2024/01/15 Budget Entry\n"
    "    [Budget:Food]                $100.00\n"
    "    [Budget:Savings]             $-100.00\n";

  std::size_t count = parse_text(input);
  BOOST_CHECK_EQUAL(1u, count);

  xact_t* xact = session.journal->xacts.front();
  for (post_t* post : xact->posts) {
    BOOST_CHECK(post->has_flags(POST_VIRTUAL));
    BOOST_CHECK(post->has_flags(POST_MUST_BALANCE));
  }
}

BOOST_AUTO_TEST_CASE(testParseUnbalancedVirtualPosting)
{
  // A posting with (Account) syntax is virtual and does not need to balance
  std::string input =
    "2024/01/15 Virtual Entry\n"
    "    (Budget:Food)                $100.00\n"
    "    Expenses:Food                $50.00\n"
    "    Assets:Checking\n";

  std::size_t count = parse_text(input);
  BOOST_CHECK_EQUAL(1u, count);

  xact_t* xact = session.journal->xacts.front();
  post_t* first = xact->posts.front();
  BOOST_CHECK(first->has_flags(POST_VIRTUAL));
  BOOST_CHECK(!first->has_flags(POST_MUST_BALANCE));
}

BOOST_AUTO_TEST_CASE(testParseTransactionWithCost)
{
  // A posting with @ price notation should parse correctly
  std::string input =
    "2024/01/15 Currency Exchange\n"
    "    Assets:EUR                   100 EUR @ $1.10\n"
    "    Assets:Checking\n";

  std::size_t count = parse_text(input);
  BOOST_CHECK_EQUAL(1u, count);

  xact_t* xact = session.journal->xacts.front();
  post_t* first = xact->posts.front();
  BOOST_CHECK(!first->amount.is_null());
  BOOST_CHECK(first->cost);
}

BOOST_AUTO_TEST_CASE(testParseTransactionWithCode)
{
  // A transaction with a code in parentheses should record the code
  std::string input =
    "2024/01/15 (1234) Check Payment\n"
    "    Expenses:Utilities           $75.00\n"
    "    Assets:Checking\n";

  std::size_t count = parse_text(input);
  BOOST_CHECK_EQUAL(1u, count);

  xact_t* xact = session.journal->xacts.front();
  BOOST_CHECK(xact->code);
  BOOST_CHECK_EQUAL(string("1234"), *xact->code);
  BOOST_CHECK_EQUAL(string("Check Payment"), xact->payee);
}

BOOST_AUTO_TEST_CASE(testParseTransactionWithAuxDate)
{
  // A transaction with an auxiliary date (date=aux_date) should record both
  std::string input =
    "2024/01/15=2024/01/20 Payment\n"
    "    Expenses:Food                $50.00\n"
    "    Assets:Checking\n";

  std::size_t count = parse_text(input);
  BOOST_CHECK_EQUAL(1u, count);

  xact_t* xact = session.journal->xacts.front();
  BOOST_CHECK_EQUAL(date_t(2024, 1, 15), xact->date());
  BOOST_CHECK(xact->_date_aux);
  BOOST_CHECK_EQUAL(date_t(2024, 1, 20), *xact->_date_aux);
}

BOOST_AUTO_TEST_CASE(testParseApplyAccount)
{
  // "apply account" should prefix all account names within its scope
  std::string input =
    "apply account Assets\n"
    "2024/01/15 Transfer\n"
    "    Checking                     $100.00\n"
    "    Savings                      $-100.00\n"
    "end apply\n";

  std::size_t count = parse_text(input);
  BOOST_CHECK_EQUAL(1u, count);

  xact_t* xact = session.journal->xacts.front();
  post_t* first = xact->posts.front();
  BOOST_CHECK_EQUAL(string("Assets:Checking"), first->account->fullname());

  post_t* second = xact->posts.back();
  BOOST_CHECK_EQUAL(string("Assets:Savings"), second->account->fullname());
}

BOOST_AUTO_TEST_CASE(testParseCommentsAndBlankLines)
{
  // Comments (;) and blank lines between transactions should be ignored
  std::string input =
    "; This is a top-level comment\n"
    "# This is also a comment\n"
    "\n"
    "2024/01/15 Payee\n"
    "    Expenses:Food                $10.00\n"
    "    Assets:Checking\n"
    "\n"
    "; Another comment\n"
    "\n"
    "2024/01/16 Another Payee\n"
    "    Expenses:Food                $20.00\n"
    "    Assets:Checking\n";

  std::size_t count = parse_text(input);
  BOOST_CHECK_EQUAL(2u, count);
}

BOOST_AUTO_TEST_CASE(testParsePriceDirective)
{
  // The P directive should register a price
  std::string input =
    "P 2024/01/15 EUR $1.10\n"
    "2024/01/15 Test\n"
    "    Expenses:Food                $10.00\n"
    "    Assets:Checking\n";

  std::size_t count = parse_text(input);
  BOOST_CHECK_EQUAL(1u, count);  // only the transaction counts
}

BOOST_AUTO_TEST_CASE(testParseSinglePostingAutoFill)
{
  // A transaction with only one explicit posting (no amount on the
  // second) should auto-fill the balancing amount
  std::string input =
    "2024/01/15 Single Posting Test\n"
    "    Expenses:Food                $42.50\n"
    "    Assets:Checking\n";

  std::size_t count = parse_text(input);
  BOOST_CHECK_EQUAL(1u, count);

  xact_t* xact = session.journal->xacts.front();
  BOOST_CHECK_EQUAL(2u, xact->posts.size());

  // After finalization the second posting should have an amount
  post_t* second = xact->posts.back();
  BOOST_CHECK(!second->amount.is_null());
  BOOST_CHECK(second->has_flags(POST_CALCULATED));
}

BOOST_AUTO_TEST_CASE(testParseTransactionWithPostingNote)
{
  // A posting followed by a semicolon note should capture the note
  std::string input =
    "2024/01/15 Test\n"
    "    Expenses:Food                $10.00 ; posting note\n"
    "    Assets:Checking\n";

  std::size_t count = parse_text(input);
  BOOST_CHECK_EQUAL(1u, count);

  xact_t* xact = session.journal->xacts.front();
  post_t* first = xact->posts.front();
  BOOST_CHECK(first->note);
}

BOOST_AUTO_TEST_CASE(testParseAutomatedTransaction)
{
  // An automated transaction (= predicate) should be registered
  std::string input =
    "= Expenses:Food\n"
    "    (Budget:Food)                 1\n"
    "\n"
    "2024/01/15 Grocery Store\n"
    "    Expenses:Food                $50.00\n"
    "    Assets:Checking\n";

  std::size_t count = parse_text(input);
  BOOST_CHECK_EQUAL(1u, count);
  BOOST_CHECK(!session.journal->auto_xacts.empty());
}

BOOST_AUTO_TEST_CASE(testParsePeriodTransaction)
{
  // A periodic transaction (~ period) should be registered
  std::string input =
    "~ Monthly\n"
    "    Expenses:Rent                $1000.00\n"
    "    Assets:Checking\n";

  parse_text(input);
  BOOST_CHECK(!session.journal->period_xacts.empty());
}

BOOST_AUTO_TEST_SUITE_END()
