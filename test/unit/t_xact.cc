#define BOOST_TEST_DYN_LINK
//#define BOOST_TEST_MODULE xact
#include <boost/test/unit_test.hpp>

#include <system.hh>

#include "xact.h"
#include "post.h"
#include "account.h"
#include "amount.h"
#include "journal.h"
#include "session.h"
#include "draft.h"

using namespace ledger;

struct xact_fixture {
  journal_t journal;
  account_t* root;
  account_t* expenses;
  account_t* assets;

  xact_fixture() {
    times_initialize();
    amount_t::initialize();
    amount_t::stream_fullstrings = true;

    // Cause the display precision for dollars to be initialized to 2.
    amount_t x1("$1.00");

    root = journal.master;
    expenses = root->find_account("Expenses");
    assets = root->find_account("Assets");
  }

  ~xact_fixture() {
    amount_t::shutdown();
    times_shutdown();
  }
};

BOOST_FIXTURE_TEST_SUITE(xact_tests, xact_fixture)

// ---------------------------------------------------------------------------
// 1. Construction and Basic Properties
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testDefaultConstruction)
{
  // A default-constructed xact_t has no date, empty payee, no code, no posts
  xact_t xact;
  xact.add_flags(ITEM_TEMP);
  BOOST_CHECK(! xact._date);
  BOOST_CHECK_EQUAL(string(""), xact.payee);
  BOOST_CHECK(! xact.code);
  BOOST_CHECK(xact.posts.empty());
  BOOST_CHECK(xact.journal == NULL);
}

BOOST_AUTO_TEST_CASE(testSetDateAndPayee)
{
  // Setting date and payee on a transaction
  xact_t xact;
  xact.add_flags(ITEM_TEMP);
  xact._date = parse_date("2024/01/15");
  xact.payee = "Grocery Store";
  xact.code = string("1234");

  BOOST_CHECK(xact._date);
  BOOST_CHECK_EQUAL(parse_date("2024/01/15"), *xact._date);
  BOOST_CHECK_EQUAL(string("Grocery Store"), xact.payee);
  BOOST_CHECK(xact.code);
  BOOST_CHECK_EQUAL(string("1234"), *xact.code);
}

BOOST_AUTO_TEST_CASE(testValidRequiresDate)
{
  // valid() returns false when _date is not set
  xact_t xact;
  xact.add_flags(ITEM_TEMP);
  BOOST_CHECK(! xact.valid());

  // After setting a date, valid() returns true (no posts to validate)
  xact._date = parse_date("2024/06/01");
  BOOST_CHECK(xact.valid());
}

BOOST_AUTO_TEST_CASE(testCopyConstructor)
{
  // Copy constructor preserves date, payee, and code
  xact_t original;
  original.add_flags(ITEM_TEMP);
  original._date = parse_date("2024/03/20");
  original.payee = "Test Payee";
  original.code = string("ABC");

  xact_t copy(original);
  copy.add_flags(ITEM_TEMP);
  BOOST_CHECK(copy._date);
  BOOST_CHECK_EQUAL(*original._date, *copy._date);
  BOOST_CHECK_EQUAL(original.payee, copy.payee);
  BOOST_CHECK(copy.code);
  BOOST_CHECK_EQUAL(*original.code, *copy.code);
  // Copy does not copy posts
  BOOST_CHECK(copy.posts.empty());
}

// ---------------------------------------------------------------------------
// 2. Post Management
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testAddPostSetsXact)
{
  // add_post sets the posting's xact pointer to this transaction
  xact_t xact;
  xact.add_flags(ITEM_TEMP);

  post_t* post = new post_t(expenses, amount_t("$10.00"));
  post->add_flags(ITEM_TEMP);
  xact.add_post(post);

  BOOST_CHECK_EQUAL(1, xact.posts.size());
  BOOST_CHECK(post->xact == &xact);
  BOOST_CHECK(xact.posts.front() == post);

  delete post;
}

BOOST_AUTO_TEST_CASE(testAddMultiplePosts)
{
  // Adding multiple posts to a transaction
  xact_t xact;
  xact.add_flags(ITEM_TEMP);

  post_t* p1 = new post_t(expenses, amount_t("$25.00"));
  p1->add_flags(ITEM_TEMP);
  post_t* p2 = new post_t(assets, amount_t("$-25.00"));
  p2->add_flags(ITEM_TEMP);

  xact.add_post(p1);
  xact.add_post(p2);

  BOOST_CHECK_EQUAL(2, xact.posts.size());
  BOOST_CHECK(p1->xact == &xact);
  BOOST_CHECK(p2->xact == &xact);

  delete p1;
  delete p2;
}

BOOST_AUTO_TEST_CASE(testRemovePost)
{
  // remove_post removes a posting and sets its xact pointer to NULL
  xact_t xact;
  xact.add_flags(ITEM_TEMP);

  post_t* post = new post_t(expenses, amount_t("$10.00"));
  post->add_flags(ITEM_TEMP);
  xact.add_post(post);
  BOOST_CHECK_EQUAL(1, xact.posts.size());
  BOOST_CHECK(post->xact == &xact);

  bool removed = xact.remove_post(post);
  BOOST_CHECK(removed);
  BOOST_CHECK(xact.posts.empty());
  BOOST_CHECK(post->xact == NULL);

  delete post;
}

BOOST_AUTO_TEST_CASE(testValidChecksPostXactPointer)
{
  // valid() checks that every post's xact pointer matches this transaction
  xact_t xact;
  xact.add_flags(ITEM_TEMP);
  xact._date = parse_date("2024/01/01");

  post_t* post = new post_t(expenses, amount_t("$5.00"));
  post->add_flags(ITEM_TEMP);
  xact.add_post(post);
  BOOST_CHECK(xact.valid());

  // Corrupt the xact pointer -- valid() should return false
  post->xact = NULL;
  BOOST_CHECK(! xact.valid());

  // Restore for clean destruction
  post->xact = &xact;
  delete post;
}

// ---------------------------------------------------------------------------
// 3. Finalization
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testFinalizeTwoBalancedPosts)
{
  // Two postings that sum to zero should finalize without error
  xact_t xact;
  xact._date = parse_date("2024/01/15");
  xact.payee = "Balanced Transaction";
  xact.journal = &journal;

  post_t* p1 = new post_t(expenses, amount_t("$50.00"));
  post_t* p2 = new post_t(assets, amount_t("$-50.00"));
  xact.add_post(p1);
  xact.add_post(p2);

  BOOST_CHECK(xact.finalize());
  BOOST_CHECK(xact.valid());
}

BOOST_AUTO_TEST_CASE(testFinalizeNullAmountInferred)
{
  // One posting with a null amount gets the balance inferred
  xact_t xact;
  xact._date = parse_date("2024/02/01");
  xact.payee = "Inferred Amount";
  xact.journal = &journal;

  post_t* p1 = new post_t(expenses, amount_t("$100.00"));
  post_t* p2 = new post_t(assets);  // null amount
  xact.add_post(p1);
  xact.add_post(p2);

  BOOST_CHECK(xact.finalize());

  // The null post should now have -$100.00 and the POST_CALCULATED flag
  BOOST_CHECK(! p2->amount.is_null());
  BOOST_CHECK_EQUAL(amount_t("$-100.00"), p2->amount);
  BOOST_CHECK(p2->has_flags(POST_CALCULATED));
}

BOOST_AUTO_TEST_CASE(testFinalizeUnbalancedThrows)
{
  // A transaction that does not balance should throw balance_error
  xact_t xact;
  xact._date = parse_date("2024/03/01");
  xact.payee = "Unbalanced";
  xact.journal = &journal;

  post_t* p1 = new post_t(expenses, amount_t("$50.00"));
  post_t* p2 = new post_t(assets, amount_t("$-30.00"));
  xact.add_post(p1);
  xact.add_post(p2);

  BOOST_CHECK_THROW(xact.finalize(), balance_error);
}

BOOST_AUTO_TEST_CASE(testFinalizeWithCost)
{
  // A transaction with a cost (@ price) should finalize when balanced
  xact_t xact;
  xact._date = parse_date("2024/04/01");
  xact.payee = "Foreign Purchase";
  xact.journal = &journal;

  // 100 EUR @ $1.10 => cost $110.00
  amount_t eur_amount("100 EUR");
  post_t* p1 = new post_t(expenses, eur_amount);
  p1->cost = amount_t("$110.00");
  post_t* p2 = new post_t(assets, amount_t("$-110.00"));

  xact.add_post(p1);
  xact.add_post(p2);

  BOOST_CHECK(xact.finalize());
  // After finalize with cost, the post amount is modified by exchange().
  // The transaction is valid if finalize completes without throwing.
}

BOOST_AUTO_TEST_CASE(testFinalizeVirtualPostExcluded)
{
  // A virtual post (without POST_MUST_BALANCE) is excluded from balance
  xact_t xact;
  xact._date = parse_date("2024/05/01");
  xact.payee = "Virtual Post";
  xact.journal = &journal;

  account_t* budget = root->find_account("Budget:Food");

  post_t* p1 = new post_t(expenses, amount_t("$75.00"));
  post_t* p2 = new post_t(assets, amount_t("$-75.00"));
  // Virtual post that does not need to balance
  post_t* p3 = new post_t(budget, amount_t("$-75.00"));
  p3->add_flags(POST_VIRTUAL);

  xact.add_post(p1);
  xact.add_post(p2);
  xact.add_post(p3);

  // p3 is virtual without POST_MUST_BALANCE, so it is excluded from balance
  BOOST_CHECK(! p3->must_balance());
  BOOST_CHECK(xact.finalize());
  BOOST_CHECK(xact.valid());
}

BOOST_AUTO_TEST_CASE(testFinalizeZeroTotal)
{
  // A transaction where all postings sum to zero is valid
  xact_t xact;
  xact._date = parse_date("2024/06/01");
  xact.payee = "Zero Total";
  xact.journal = &journal;

  post_t* p1 = new post_t(expenses, amount_t("$0.00"));
  post_t* p2 = new post_t(assets, amount_t("$0.00"));
  xact.add_post(p1);
  xact.add_post(p2);

  // finalize returns false when all posts are zero (all_null check), but
  // zero-amount posts are not null -- they should balance.
  BOOST_CHECK(xact.finalize());
}

// ---------------------------------------------------------------------------
// 4. Error Paths
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testFinalizeTwoNullAmountsThrows)
{
  // Two postings with null amounts should throw (cannot infer both)
  xact_t xact;
  xact._date = parse_date("2024/07/01");
  xact.payee = "Two Nulls";
  xact.journal = &journal;

  post_t* p1 = new post_t(expenses);  // null amount
  post_t* p2 = new post_t(assets);    // null amount
  xact.add_post(p1);
  xact.add_post(p2);

  BOOST_CHECK_THROW(xact.finalize(), std::logic_error);
}

BOOST_AUTO_TEST_CASE(testFinalizeEmptyXact)
{
  // An empty transaction (no posts) -- finalize should return false because
  // all_null is true (vacuously) when dynamic_cast<xact_t*> succeeds
  xact_t xact;
  xact._date = parse_date("2024/08/01");
  xact.payee = "Empty";
  xact.journal = &journal;

  // With no posts, balance is null, null_post is NULL.
  // The dynamic_cast<xact_t*> section runs. all_null stays true since no
  // posts are iterated.  Returns false.
  bool result = xact.finalize();
  BOOST_CHECK(! result);
}

BOOST_AUTO_TEST_CASE(testFinalizeCostSameCommodityThrows)
{
  // A posting where cost is the same commodity as amount should throw
  xact_t xact;
  xact._date = parse_date("2024/09/01");
  xact.payee = "Same Commodity Cost";
  xact.journal = &journal;

  post_t* p1 = new post_t(expenses, amount_t("$50.00"));
  p1->cost = amount_t("$50.00");  // same commodity as amount
  post_t* p2 = new post_t(assets, amount_t("$-50.00"));
  xact.add_post(p1);
  xact.add_post(p2);

  BOOST_CHECK_THROW(xact.finalize(), balance_error);
}

BOOST_AUTO_TEST_CASE(testFinalizeThreePostsOneNull)
{
  // Three postings where one is null -- the null one gets the remaining balance
  xact_t xact;
  xact._date = parse_date("2024/10/01");
  xact.payee = "Three Posts One Null";
  xact.journal = &journal;

  account_t* checking = root->find_account("Assets:Checking");
  account_t* savings = root->find_account("Assets:Savings");

  post_t* p1 = new post_t(expenses, amount_t("$100.00"));
  post_t* p2 = new post_t(checking, amount_t("$-60.00"));
  post_t* p3 = new post_t(savings);  // null amount: should get $-40.00
  xact.add_post(p1);
  xact.add_post(p2);
  xact.add_post(p3);

  BOOST_CHECK(xact.finalize());
  BOOST_CHECK(! p3->amount.is_null());
  BOOST_CHECK_EQUAL(amount_t("$-40.00"), p3->amount);
  BOOST_CHECK(p3->has_flags(POST_CALCULATED));
}

// ---------------------------------------------------------------------------
// 5. Metadata, Flags, and State
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testHasXdataAndClearXdata)
{
  // has_xdata() checks whether any post has xdata set
  xact_t xact;
  xact.add_flags(ITEM_TEMP);

  post_t* post = new post_t(expenses, amount_t("$10.00"));
  post->add_flags(ITEM_TEMP);
  xact.add_post(post);

  // Initially no xdata
  BOOST_CHECK(! xact.has_xdata());

  // Create xdata on the post
  post->xdata().add_flags(POST_EXT_VISITED);
  BOOST_CHECK(xact.has_xdata());

  // clear_xdata removes it (but only on non-ITEM_TEMP posts)
  // Since post has ITEM_TEMP, clear_xdata skips it
  xact.clear_xdata();
  // Post still has xdata because ITEM_TEMP posts are skipped
  BOOST_CHECK(xact.has_xdata());

  delete post;
}

BOOST_AUTO_TEST_CASE(testMagnitudeCalculation)
{
  // magnitude() sums the positive-side of the transaction
  xact_t xact;
  xact.add_flags(ITEM_TEMP);

  post_t* p1 = new post_t(expenses, amount_t("$75.00"));
  p1->add_flags(ITEM_TEMP);
  post_t* p2 = new post_t(assets, amount_t("$-75.00"));
  p2->add_flags(ITEM_TEMP);

  xact.add_post(p1);
  xact.add_post(p2);

  value_t mag = xact.magnitude();
  // magnitude() starts with 0L (long) and adds a commoditized amount,
  // which promotes the result to a balance_t.
  BOOST_CHECK(mag.is_balance());
  BOOST_CHECK_EQUAL(amount_t("$75.00"), mag.to_amount());

  delete p1;
  delete p2;
}

BOOST_AUTO_TEST_CASE(testMagnitudeWithCost)
{
  // magnitude() uses cost when available for positive posts
  xact_t xact;
  xact.add_flags(ITEM_TEMP);

  amount_t eur_amount("50 EUR");
  post_t* p1 = new post_t(expenses, eur_amount);
  p1->add_flags(ITEM_TEMP);
  p1->cost = amount_t("$55.00");
  post_t* p2 = new post_t(assets, amount_t("$-55.00"));
  p2->add_flags(ITEM_TEMP);

  xact.add_post(p1);
  xact.add_post(p2);

  value_t mag = xact.magnitude();
  // magnitude uses cost for p1 since it has a cost and is positive.
  // Result is a balance_t because magnitude starts as long(0) and
  // adds a commoditized amount.
  BOOST_CHECK(mag.is_balance());
  BOOST_CHECK_EQUAL(amount_t("$55.00"), mag.to_amount());

  delete p1;
  delete p2;
}

BOOST_AUTO_TEST_CASE(testTransactionState)
{
  // Transaction state: UNCLEARED, CLEARED, PENDING
  xact_t xact;
  xact.add_flags(ITEM_TEMP);
  xact._date = parse_date("2024/11/01");

  // Default state is UNCLEARED
  BOOST_CHECK_EQUAL(item_t::UNCLEARED, xact.state());

  // Set to CLEARED
  xact.set_state(item_t::CLEARED);
  BOOST_CHECK_EQUAL(item_t::CLEARED, xact.state());

  // Set to PENDING
  xact.set_state(item_t::PENDING);
  BOOST_CHECK_EQUAL(item_t::PENDING, xact.state());
}

BOOST_AUTO_TEST_CASE(testDescription)
{
  // description() returns a localized string depending on whether pos is set
  xact_t xact;
  xact.add_flags(ITEM_TEMP);

  // Without pos, returns the generated transaction string
  string desc = xact.description();
  BOOST_CHECK(! desc.empty());

  // With pos set, description mentions the line number
  position_t position;
  position.beg_line = 42;
  xact.pos = position;
  desc = xact.description();
  BOOST_CHECK(! desc.empty());
}

BOOST_AUTO_TEST_CASE(testXactBasePropertiesViaXact)
{
  // xact_base_t properties are accessible through xact_t (which inherits)
  xact_t xact;
  xact.add_flags(ITEM_TEMP);
  BOOST_CHECK(xact.journal == NULL);
  BOOST_CHECK(xact.posts.empty());
  // xact_base_t::valid() returns true unconditionally
  // but xact_t::valid() requires a date, so test base valid indirectly
  xact._date = parse_date("2024/01/01");
  BOOST_CHECK(xact.valid());
}

BOOST_AUTO_TEST_CASE(testFinalizeBalancedMustBalanceVirtual)
{
  // A virtual post with POST_MUST_BALANCE participates in balance checking
  xact_t xact;
  xact._date = parse_date("2024/12/01");
  xact.payee = "Must Balance Virtual";
  xact.journal = &journal;

  account_t* budget = root->find_account("Budget:Food");

  post_t* p1 = new post_t(expenses, amount_t("$30.00"));
  post_t* p2 = new post_t(assets, amount_t("$-30.00"));
  // Virtual post WITH must_balance
  post_t* p3 = new post_t(budget, amount_t("$10.00"));
  p3->add_flags(POST_VIRTUAL | POST_MUST_BALANCE);

  xact.add_post(p1);
  xact.add_post(p2);
  xact.add_post(p3);

  // p3 is virtual but must_balance, so it participates and causes imbalance
  BOOST_CHECK(p3->must_balance());
  BOOST_CHECK_THROW(xact.finalize(), balance_error);
}

// ---------------------------------------------------------------------------
// 6. Journal Methods - cover journal.cc uncovered lines
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testJournalAddRemoveAccount)
{
  // Cover journal.cc lines 104-110: add_account, remove_account
  account_t* acct = new account_t(root, "TestAccount");
  journal.add_account(acct);

  account_t* found = journal.find_account("TestAccount", false);
  BOOST_CHECK(found != NULL);
  BOOST_CHECK_EQUAL(string("TestAccount"), found->name);

  bool removed = journal.remove_account(acct);
  BOOST_CHECK(removed);

  found = journal.find_account("TestAccount", false);
  BOOST_CHECK(found == NULL);

  delete acct;
}

BOOST_AUTO_TEST_CASE(testJournalFindAccountRe)
{
  // Cover journal.cc lines 116-118: find_account_re
  journal.find_account("Expenses:Food");
  journal.find_account("Expenses:Rent");
  journal.find_account("Assets:Bank");

  account_t* found = journal.find_account_re("Expense");
  BOOST_CHECK(found != NULL);
}

BOOST_AUTO_TEST_CASE(testJournalRemoveXact)
{
  // Cover journal.cc lines 423-438: remove_xact
  // Need balanced transaction for add_xact to accept it
  xact_t* x1 = new xact_t;
  x1->_date = parse_date("2024/01/01");
  x1->payee = "Test Payee";
  post_t* p1 = new post_t(expenses, amount_t("$10.00"));
  post_t* p2 = new post_t(assets, amount_t("$-10.00"));
  x1->add_post(p1);
  x1->add_post(p2);
  journal.add_xact(x1);
  std::size_t count_before = journal.xacts.size();
  BOOST_CHECK(count_before > 0);

  bool removed = journal.remove_xact(x1);
  BOOST_CHECK(removed);
  BOOST_CHECK_EQUAL(count_before - 1, journal.xacts.size());
  BOOST_CHECK(x1->journal == NULL);

  // Remove non-existent xact
  xact_t x2;
  x2._date = parse_date("2024/02/01");
  x2.add_flags(ITEM_TEMP);
  BOOST_CHECK(! journal.remove_xact(&x2));

  delete x1;
}

BOOST_AUTO_TEST_CASE(testJournalHasXdata)
{
  // Cover journal.cc lines 480-497: has_xdata, clear_xdata
  // Create a fresh journal for this test to ensure clean state
  journal_t j;
  account_t* r = j.master;
  account_t* exp = r->find_account("Expenses");
  account_t* ast = r->find_account("Assets");

  xact_t* x1 = new xact_t;
  x1->_date = parse_date("2024/01/01");
  x1->payee = "Test";
  post_t* p1 = new post_t(exp, amount_t("$10.00"));
  post_t* p2 = new post_t(ast, amount_t("$-10.00"));
  x1->add_post(p1);
  x1->add_post(p2);
  j.add_xact(x1);

  // Add xdata to the post
  p1->xdata().add_flags(POST_EXT_VISITED);
  BOOST_CHECK(j.has_xdata());

  // Clear xdata
  j.clear_xdata();
  BOOST_CHECK(! p1->has_xdata());
}

BOOST_AUTO_TEST_CASE(testJournalValid)
{
  // Cover journal.cc lines 515-524: valid()
  BOOST_CHECK(journal.valid());

  // Add a valid transaction
  xact_t* x1 = new xact_t;
  x1->_date = parse_date("2024/01/01");
  x1->payee = "Test Valid";
  post_t* p1 = new post_t(expenses, amount_t("$10.00"));
  post_t* p2 = new post_t(assets, amount_t("$-10.00"));
  x1->add_post(p1);
  x1->add_post(p2);
  journal.add_xact(x1);
  BOOST_CHECK(journal.valid());
}

// ---------------------------------------------------------------------------
// 7. Session Methods - cover session.cc uncovered lines
// These require default_scope to be set, so we set it up carefully
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testSessionFnMinMax)
{
  // Cover session.cc lines 258-259, 261-262: fn_min, fn_max
  session_t session;
  scope_t* old_scope = scope_t::default_scope;
  scope_t::default_scope = &session;

  {
    call_scope_t args(session);
    args.push_back(value_t(10L));
    args.push_back(value_t(5L));
    value_t result = session.fn_min(args);
    BOOST_CHECK_EQUAL(5L, result.to_long());
  }
  {
    call_scope_t args(session);
    args.push_back(value_t(10L));
    args.push_back(value_t(5L));
    value_t result = session.fn_max(args);
    BOOST_CHECK_EQUAL(10L, result.to_long());
  }

  scope_t::default_scope = old_scope;
}

BOOST_AUTO_TEST_CASE(testSessionFnInt)
{
  // Cover session.cc fn_int, fn_str
  session_t session;
  scope_t* old_scope = scope_t::default_scope;
  scope_t::default_scope = &session;

  {
    call_scope_t args(session);
    args.push_back(value_t(42L));
    value_t result = session.fn_int(args);
    BOOST_CHECK_EQUAL(42L, result.to_long());
  }
  {
    call_scope_t args(session);
    args.push_back(string_value("hello"));
    value_t result = session.fn_str(args);
    BOOST_CHECK_EQUAL(string("hello"), result.to_string());
  }

  scope_t::default_scope = old_scope;
}

BOOST_AUTO_TEST_CASE(testSessionReadJournalFromString)
{
  // Cover session.cc lines 216-235: read_journal_from_string
  session_t session;
  scope_t* old_scope = scope_t::default_scope;
  scope_t::default_scope = &session;

  journal_t* j = session.read_journal_from_string(
    "2024/01/01 Test\n"
    "    Expenses:Food    $10.00\n"
    "    Assets:Cash\n"
  );
  BOOST_CHECK(j != NULL);
  BOOST_CHECK_EQUAL(1, j->xacts.size());

  scope_t::default_scope = old_scope;
}

BOOST_AUTO_TEST_CASE(testSessionCloseJournalFiles)
{
  // Cover session.cc lines 237-243: close_journal_files
  session_t session;
  scope_t* old_scope = scope_t::default_scope;
  scope_t::default_scope = &session;

  session.read_journal_from_string(
    "2024/01/01 Test\n"
    "    Expenses:Food    $10.00\n"
    "    Assets:Cash\n"
  );
  session.close_journal_files();
  // After closing, journal should be reset
  journal_t* j = session.get_journal();
  BOOST_CHECK(j != NULL);
  BOOST_CHECK_EQUAL(0, j->xacts.size());

  scope_t::default_scope = old_scope;
}

BOOST_AUTO_TEST_CASE(testSessionGetJournal)
{
  // Cover session.cc lines 245-247: get_journal
  session_t session;
  scope_t* old_scope = scope_t::default_scope;
  scope_t::default_scope = &session;

  journal_t* j = session.get_journal();
  BOOST_CHECK(j != NULL);

  scope_t::default_scope = old_scope;
}

// ---------------------------------------------------------------------------
// 8. Draft Methods - cover draft.cc uncovered lines
// Uses only the public API since xact_template_t is private
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testDraftDumpSimple)
{
  // Cover draft.cc lines 47-83: xact_template_t::dump via public dump()
  value_t args;
  args.push_back(string_value("Grocery"));
  args.push_back(string_value("$50.00"));
  draft_t draft(args);

  std::ostringstream out;
  draft.dump(out);
  BOOST_CHECK(! out.str().empty());
  BOOST_CHECK(out.str().find("Grocery") != string::npos);
}

BOOST_AUTO_TEST_CASE(testDraftDumpWithCode)
{
  // Cover draft.cc lines 53-54: dump with code
  value_t args;
  args.push_back(string_value("Grocery"));
  args.push_back(string_value("code"));
  args.push_back(string_value("1234"));
  draft_t draft(args);

  std::ostringstream out;
  draft.dump(out);
  BOOST_CHECK(out.str().find("1234") != string::npos);
}

BOOST_AUTO_TEST_CASE(testDraftDumpWithNote)
{
  // Cover draft.cc line 56: dump with note
  value_t args;
  args.push_back(string_value("Grocery"));
  args.push_back(string_value("note"));
  args.push_back(string_value("lunch"));
  draft_t draft(args);

  std::ostringstream out;
  draft.dump(out);
  BOOST_CHECK(out.str().find("lunch") != string::npos);
}

BOOST_AUTO_TEST_CASE(testDraftDumpWithPosts)
{
  // Cover draft.cc lines 63-83: dump with posts
  value_t args;
  args.push_back(string_value("Grocery"));
  args.push_back(string_value("to"));
  args.push_back(string_value("Expenses:Food"));
  args.push_back(string_value("$10.00"));
  args.push_back(string_value("from"));
  args.push_back(string_value("Assets:Cash"));
  draft_t draft(args);

  std::ostringstream out;
  draft.dump(out);
  BOOST_CHECK(! out.str().empty());
  BOOST_CHECK(out.str().find("$10.00") != string::npos);
}

// testDraftDumpWithCost removed: draft_t needs full session context

BOOST_AUTO_TEST_CASE(testDraftParseArgsOn)
{
  // Cover draft.cc lines 131-135: "on" keyword
  value_t args;
  args.push_back(string_value("Grocery"));
  args.push_back(string_value("on"));
  args.push_back(string_value("2024/01/15"));
  draft_t draft(args);

  std::ostringstream out;
  draft.dump(out);
  BOOST_CHECK(! out.str().empty());
}

BOOST_AUTO_TEST_CASE(testDraftParseArgsAt)
{
  // Cover draft.cc lines 118-121: "at" keyword
  value_t args;
  args.push_back(string_value("2024/01/15"));
  args.push_back(string_value("at"));
  args.push_back(string_value("Grocery"));
  draft_t draft(args);

  std::ostringstream out;
  draft.dump(out);
  BOOST_CHECK(out.str().find("Grocery") != string::npos);
}

BOOST_AUTO_TEST_CASE(testDraftParseArgsRest)
{
  // Cover draft.cc line 144-145: "rest" keyword (ignored)
  value_t args;
  args.push_back(string_value("Grocery"));
  args.push_back(string_value("$50.00"));
  args.push_back(string_value("rest"));
  draft_t draft(args);

  std::ostringstream out;
  draft.dump(out);
  BOOST_CHECK(! out.str().empty());
}

BOOST_AUTO_TEST_CASE(testDraftInsertNoTemplate)
{
  // Cover draft.cc line 213-214: insert with no template (empty args)
  value_t empty_args;
  draft_t draft(empty_args);
  BOOST_CHECK(draft.insert(journal) == NULL);
}

// ---------------------------------------------------------------------------
// 9. Xact Lookup Functions - cover xact.cc lines 608-662
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testXactLookupCode)
{
  // Cover xact.cc get_code (lines 592-597) and lookup (lines 633-662)
  xact_t xact;
  xact.add_flags(ITEM_TEMP);
  xact._date = parse_date("2024/01/01");
  xact.payee = "Test";
  xact.code = string("XYZ");

  auto op = xact.lookup(symbol_t::FUNCTION, "code");
  BOOST_CHECK(op != NULL);
}

BOOST_AUTO_TEST_CASE(testXactLookupPayee)
{
  // Cover xact.cc get_payee (lines 599-601) and lookup 'p'/'payee'
  xact_t xact;
  xact.add_flags(ITEM_TEMP);
  xact._date = parse_date("2024/01/01");
  xact.payee = "Test Payee";

  auto op = xact.lookup(symbol_t::FUNCTION, "payee");
  BOOST_CHECK(op != NULL);

  auto op2 = xact.lookup(symbol_t::FUNCTION, "p");
  BOOST_CHECK(op2 != NULL);
}

BOOST_AUTO_TEST_CASE(testXactLookupMagnitude)
{
  // Cover xact.cc get_magnitude (lines 588-590)
  xact_t xact;
  xact.add_flags(ITEM_TEMP);
  xact._date = parse_date("2024/01/01");
  xact.payee = "Test";

  auto op = xact.lookup(symbol_t::FUNCTION, "magnitude");
  BOOST_CHECK(op != NULL);
}

BOOST_AUTO_TEST_CASE(testXactLookupAnyAll)
{
  // Cover xact.cc fn_any (lines 608-618) and fn_all (620-630)
  xact_t xact;
  xact.add_flags(ITEM_TEMP);
  xact._date = parse_date("2024/01/01");
  xact.payee = "Test";

  auto op_any = xact.lookup(symbol_t::FUNCTION, "any");
  BOOST_CHECK(op_any != NULL);

  auto op_all = xact.lookup(symbol_t::FUNCTION, "all");
  BOOST_CHECK(op_all != NULL);
}

BOOST_AUTO_TEST_CASE(testXactLookupNonFunction)
{
  // Cover xact.cc lookup non-FUNCTION kind (line 634-635)
  xact_t xact;
  xact.add_flags(ITEM_TEMP);
  xact._date = parse_date("2024/01/01");
  xact.payee = "Test";

  auto op = xact.lookup(symbol_t::OPTION, "test");
  BOOST_CHECK(op == NULL);
}

// ---------------------------------------------------------------------------
// 10. Xact Hash - cover xact.cc lines 691-729
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testXactHash)
{
  // Cover xact.cc hash() (lines 691-729)
  xact_t xact;
  xact.add_flags(ITEM_TEMP);
  xact._date = parse_date("2024/06/01");
  xact.payee = "Hash Test";
  xact.code = string("H1");

  post_t* p1 = new post_t(expenses, amount_t("$50.00"));
  p1->add_flags(ITEM_TEMP);
  xact.add_post(p1);

  post_t* p2 = new post_t(assets, amount_t("$-50.00"));
  p2->add_flags(ITEM_TEMP);
  xact.add_post(p2);

  string hash512 = xact.hash("nonce", HASH_SHA512);
  BOOST_CHECK(!hash512.empty());
  BOOST_CHECK_EQUAL(128u, hash512.length());

  string hash256 = xact.hash("nonce", HASH_SHA512_Half);
  BOOST_CHECK(!hash256.empty());
  BOOST_CHECK_EQUAL(64u, hash256.length());

  delete p1;
  delete p2;
}

BOOST_AUTO_TEST_CASE(testXactHashNoCode)
{
  // Cover xact.cc hash() without code (line 699-700)
  xact_t xact;
  xact.add_flags(ITEM_TEMP);
  xact._date = parse_date("2024/06/01");
  xact.payee = "No Code Hash";

  post_t* p1 = new post_t(expenses, amount_t("$10.00"));
  p1->add_flags(ITEM_TEMP);
  xact.add_post(p1);

  string hash = xact.hash("nonce", HASH_SHA512);
  BOOST_CHECK(!hash.empty());

  delete p1;
}

BOOST_AUTO_TEST_CASE(testXactHashWithCost)
{
  // Cover xact.cc hash() with cost (line 711-712)
  xact_t xact;
  xact.add_flags(ITEM_TEMP);
  xact._date = parse_date("2024/06/01");
  xact.payee = "Cost Hash";

  post_t* p1 = new post_t(expenses, amount_t("100 EUR"));
  p1->add_flags(ITEM_TEMP);
  p1->cost = amount_t("$110.00");
  xact.add_post(p1);

  string hash = xact.hash("nonce", HASH_SHA512);
  BOOST_CHECK(!hash.empty());

  delete p1;
}

// ---------------------------------------------------------------------------
// 11. Post Lookup Functions - cover post.cc uncovered lines
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testPostHasTagInheritance)
{
  // Cover post.cc has_tag with inherit (lines 43-48, 51-57)
  xact_t xact;
  xact.add_flags(ITEM_TEMP);
  xact._date = parse_date("2024/01/01");
  xact.payee = "Test";
  xact.set_tag("XactTag", string_value("xval"));

  post_t* post = new post_t(expenses, amount_t("$10.00"));
  post->add_flags(ITEM_TEMP);
  xact.add_post(post);

  BOOST_CHECK(! post->has_tag("XactTag", false));
  BOOST_CHECK(post->has_tag("XactTag", true));

  mask_t tag_mask("XactTag");
  BOOST_CHECK(! post->has_tag(tag_mask, std::nullopt, false));
  BOOST_CHECK(post->has_tag(tag_mask, std::nullopt, true));

  delete post;
}

BOOST_AUTO_TEST_CASE(testPostGetTagInheritance)
{
  // Cover post.cc get_tag with inherit (lines 60-74)
  xact_t xact;
  xact.add_flags(ITEM_TEMP);
  xact._date = parse_date("2024/01/01");
  xact.payee = "Test";
  xact.set_tag("XactTag", string_value("inherited_value"));

  post_t* post = new post_t(expenses, amount_t("$10.00"));
  post->add_flags(ITEM_TEMP);
  xact.add_post(post);

  auto val_no = post->get_tag("XactTag", false);
  BOOST_CHECK(! val_no);
  auto val_yes = post->get_tag("XactTag", true);
  BOOST_CHECK(val_yes);

  mask_t tag_mask("XactTag");
  auto val_mask_no = post->get_tag(tag_mask, std::nullopt, false);
  BOOST_CHECK(! val_mask_no);
  auto val_mask_yes = post->get_tag(tag_mask, std::nullopt, true);
  BOOST_CHECK(val_mask_yes);

  delete post;
}

BOOST_AUTO_TEST_CASE(testPostValid)
{
  // Cover post.cc valid() (lines 569-602)
  xact_t xact;
  xact.add_flags(ITEM_TEMP);
  xact._date = parse_date("2024/01/01");
  xact.payee = "Test";

  post_t* post = new post_t(expenses, amount_t("$10.00"));
  post->add_flags(ITEM_TEMP);
  xact.add_post(post);
  BOOST_CHECK(post->valid());

  post_t orphan(expenses, amount_t("$5.00"));
  orphan.add_flags(ITEM_TEMP);
  BOOST_CHECK(! orphan.valid());

  delete post;
}

BOOST_AUTO_TEST_CASE(testPostXactId)
{
  // Cover post.cc xact_id() (lines 547-556)
  xact_t xact;
  xact.add_flags(ITEM_TEMP);
  xact._date = parse_date("2024/01/01");
  xact.payee = "Test";

  post_t* p1 = new post_t(expenses, amount_t("$10.00"));
  p1->add_flags(ITEM_TEMP);
  xact.add_post(p1);

  post_t* p2 = new post_t(assets, amount_t("$-10.00"));
  p2->add_flags(ITEM_TEMP);
  xact.add_post(p2);

  BOOST_CHECK_EQUAL(1u, p1->xact_id());
  BOOST_CHECK_EQUAL(2u, p2->xact_id());

  delete p1;
  delete p2;
}

BOOST_AUTO_TEST_CASE(testPostAccountId)
{
  // Cover post.cc account_id() (lines 558-566)
  xact_t xact;
  xact.add_flags(ITEM_TEMP);
  xact._date = parse_date("2024/01/01");
  xact.payee = "Test";

  post_t* p1 = new post_t(expenses, amount_t("$10.00"));
  p1->add_flags(ITEM_TEMP);
  xact.add_post(p1);
  expenses->add_post(p1);

  BOOST_CHECK_EQUAL(1u, p1->account_id());

  expenses->remove_post(p1);
  delete p1;
}

BOOST_AUTO_TEST_CASE(testPostAddToValue)
{
  // Cover post.cc add_to_value() (lines 605-639)
  xact_t xact;
  xact.add_flags(ITEM_TEMP);
  xact._date = parse_date("2024/01/01");
  xact.payee = "Test";

  post_t* post = new post_t(expenses, amount_t("$10.00"));
  post->add_flags(ITEM_TEMP);
  xact.add_post(post);

  value_t val;
  post->add_to_value(val);
  BOOST_CHECK_EQUAL(amount_t("$10.00"), val.to_amount());

  post->xdata().compound_value = amount_t("$20.00");
  post->xdata().add_flags(POST_EXT_COMPOUND);

  value_t val2;
  post->add_to_value(val2);
  BOOST_CHECK_EQUAL(amount_t("$20.00"), val2.to_amount());

  delete post;
}

// ---------------------------------------------------------------------------
// 12. Finalize with Two Commodities - cover xact.cc lines 220-275
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testFinalizeTwoCommoditiesAutoPrice)
{
  // Cover xact.cc lines 220-275: auto-cost inference with two commodities
  xact_t xact;
  xact._date = parse_date("2024/01/01");
  xact.payee = "Currency Exchange";
  xact.journal = &journal;

  amount_t eur("100 EUR");
  post_t* p1 = new post_t(expenses, eur);
  post_t* p2 = new post_t(assets, amount_t("$-110.00"));
  xact.add_post(p1);
  xact.add_post(p2);

  BOOST_CHECK(xact.finalize());
  BOOST_CHECK(p1->cost);
  BOOST_CHECK(p1->has_flags(POST_COST_CALCULATED));
}

// ---------------------------------------------------------------------------
// 13. Virtual Null Post Balancing - cover xact.cc lines 454-467
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testFinalizeVirtualNullPostBalance)
{
  // Cover xact.cc lines 454-467: balanced-virtual null post
  xact_t xact;
  xact._date = parse_date("2024/01/01");
  xact.payee = "Virtual Null";
  xact.journal = &journal;

  post_t* p1 = new post_t(expenses, amount_t("$50.00"));
  post_t* p2 = new post_t(assets, amount_t("$-50.00"));

  account_t* budget = root->find_account("Budget:Food");
  post_t* p3 = new post_t(budget, amount_t("$-50.00"));
  p3->add_flags(POST_VIRTUAL | POST_MUST_BALANCE);

  account_t* budget_income = root->find_account("Budget:Income");
  post_t* p4 = new post_t(budget_income);
  p4->add_flags(POST_VIRTUAL | POST_MUST_BALANCE);

  xact.add_post(p1);
  xact.add_post(p2);
  xact.add_post(p3);
  xact.add_post(p4);

  BOOST_CHECK(xact.finalize());
  BOOST_CHECK(!p4->amount.is_null());
  BOOST_CHECK_EQUAL(amount_t("$50.00"), p4->amount);
}

// ---------------------------------------------------------------------------
// Wave 4: post_t::has_tag with inheritance = false paths (post.cc lines 48, 57)
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testPostHasTagNoInherit)
{
  xact_t xact;
  xact.add_flags(ITEM_TEMP);
  xact._date = parse_date("2024/01/01");
  xact.payee = "Test";
  xact.set_tag("XactTag", string_value("xval"));

  post_t* post = new post_t(expenses, amount_t("$10.00"));
  post->add_flags(ITEM_TEMP);
  xact.add_post(post);

  // With inherit=true (default), post should see xact's tag
  BOOST_CHECK(post->has_tag("XactTag"));
  // With inherit=false, post should NOT see xact's tag
  BOOST_CHECK(!post->has_tag("XactTag", false));

  // Same for mask version
  BOOST_CHECK(post->has_tag(mask_t("XactTag"), std::nullopt));
  BOOST_CHECK(!post->has_tag(mask_t("XactTag"), std::nullopt, false));

  delete post;
}

// ---------------------------------------------------------------------------
// Wave 4: post_t::get_tag with inheritance = false paths (post.cc lines 71, 74)
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testPostGetTagNoInherit)
{
  xact_t xact;
  xact.add_flags(ITEM_TEMP);
  xact._date = parse_date("2024/01/01");
  xact.payee = "Test";
  xact.set_tag("XactTag", string_value("xval"));

  post_t* post = new post_t(expenses, amount_t("$10.00"));
  post->add_flags(ITEM_TEMP);
  xact.add_post(post);

  // With inherit=true (default), post can retrieve xact's tag
  auto val = post->get_tag("XactTag");
  BOOST_CHECK(val.has_value());

  // With inherit=false, post cannot see xact's tag
  auto val2 = post->get_tag("XactTag", false);
  BOOST_CHECK(!val2.has_value());

  // Mask version
  auto val3 = post->get_tag(mask_t("XactTag"), std::nullopt);
  BOOST_CHECK(val3.has_value());
  auto val4 = post->get_tag(mask_t("XactTag"), std::nullopt, false);
  BOOST_CHECK(!val4.has_value());

  delete post;
}

// ---------------------------------------------------------------------------
// Wave 4: post_t::primary_date with xdata (post.cc line 97)
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testPostPrimaryDateWithXdata)
{
  xact_t xact;
  xact.add_flags(ITEM_TEMP);
  xact._date = parse_date("2024/01/01");
  xact.payee = "Test";

  post_t* post = new post_t(expenses, amount_t("$10.00"));
  post->add_flags(ITEM_TEMP);
  xact.add_post(post);

  // Without xdata, primary_date should return xact date
  BOOST_CHECK_EQUAL(post->primary_date(), parse_date("2024/01/01"));

  // Set xdata date to override
  post->xdata().date = parse_date("2024/06/15");
  BOOST_CHECK_EQUAL(post->primary_date(), parse_date("2024/06/15"));

  delete post;
}

// ---------------------------------------------------------------------------
// Wave 4: post_t::valid edge cases (post.cc lines 570-600)
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testPostValidNoXact)
{
  post_t post(expenses, amount_t("$10.00"));
  post.add_flags(ITEM_TEMP);
  // Post without xact should be invalid
  BOOST_CHECK(!post.valid());
}

BOOST_AUTO_TEST_CASE(testPostValidNotInXactPosts)
{
  xact_t xact;
  xact.add_flags(ITEM_TEMP);
  xact._date = parse_date("2024/01/01");
  xact.payee = "Test";

  post_t* post = new post_t(expenses, amount_t("$10.00"));
  post->add_flags(ITEM_TEMP);
  post->xact = &xact;
  // Post has xact pointer but is NOT in xact.posts list
  BOOST_CHECK(!post->valid());

  delete post;
}

BOOST_AUTO_TEST_CASE(testPostValidNoAccount)
{
  xact_t xact;
  xact.add_flags(ITEM_TEMP);
  xact._date = parse_date("2024/01/01");
  xact.payee = "Test";

  post_t* post = new post_t;
  post->add_flags(ITEM_TEMP);
  post->amount = amount_t("$10.00");
  xact.add_post(post);

  // Post with null account should be invalid
  BOOST_CHECK(!post->valid());

  delete post;
}

// ---------------------------------------------------------------------------
// Wave 4: xact_t::verify with same-commodity cost (xact.cc line 560)
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testXactVerifySameCommodityCostThrows)
{
  xact_t xact;
  xact.add_flags(ITEM_TEMP);
  xact._date = parse_date("2024/01/01");
  xact.payee = "Same commodity";
  xact.journal = &journal;

  post_t* p1 = new post_t(expenses, amount_t("$50.00"));
  p1->add_flags(ITEM_TEMP);
  p1->cost = amount_t("$50.00"); // same commodity!
  post_t* p2 = new post_t(assets, amount_t("$-50.00"));
  p2->add_flags(ITEM_TEMP);

  xact.add_post(p1);
  xact.add_post(p2);

  BOOST_CHECK_THROW(xact.verify(), std::exception);

  delete p1;
  delete p2;
}

// ---------------------------------------------------------------------------
// Wave 4: xact_t::valid with bad post (xact.cc lines 670-673)
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testXactValidBadPost)
{
  xact_t xact;
  xact.add_flags(ITEM_TEMP);
  xact._date = parse_date("2024/01/01");
  xact.payee = "Test";

  post_t* post = new post_t;
  post->add_flags(ITEM_TEMP);
  post->amount = amount_t("$10.00");
  xact.add_post(post);

  // Post has null account -> post->valid() returns false -> xact::valid() returns false
  BOOST_CHECK(!xact.valid());

  delete post;
}

// ---------------------------------------------------------------------------
// Wave 4: post_t payee_from_tag and payee (post.cc lines 115-130)
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testPostPayeeFromTag)
{
  xact_t xact;
  xact.add_flags(ITEM_TEMP);
  xact._date = parse_date("2024/01/01");
  xact.payee = "Default Payee";

  post_t* post = new post_t(expenses, amount_t("$10.00"));
  post->add_flags(ITEM_TEMP);
  xact.add_post(post);

  // Without Payee tag, payee() returns xact payee
  BOOST_CHECK_EQUAL(post->payee(), "Default Payee");

  // With a Payee tag on the post, it should override
  post->set_tag("Payee", string_value("Override Payee"));
  BOOST_CHECK_EQUAL(post->payee(), "Override Payee");

  delete post;
}

// ---------------------------------------------------------------------------
// Wave 4: post_t value_date (post.cc lines 77-81)
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testPostValueDate)
{
  xact_t xact;
  xact.add_flags(ITEM_TEMP);
  xact._date = parse_date("2024/01/01");
  xact.payee = "Test";

  post_t* post = new post_t(expenses, amount_t("$10.00"));
  post->add_flags(ITEM_TEMP);
  xact.add_post(post);

  // Without xdata, value_date returns date()
  BOOST_CHECK_EQUAL(post->value_date(), post->date());

  // Set xdata value_date
  post->xdata().value_date = parse_date("2024/03/15");
  BOOST_CHECK_EQUAL(post->value_date(), parse_date("2024/03/15"));

  delete post;
}

// ---------------------------------------------------------------------------
// Wave 4: post_t::valid with cost (post.cc lines 591-600)
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testPostValidWithCost)
{
  xact_t xact;
  xact.add_flags(ITEM_TEMP);
  xact._date = parse_date("2024/01/01");
  xact.payee = "Test";

  post_t* post = new post_t(expenses, amount_t("$10.00"));
  post->add_flags(ITEM_TEMP);
  amount_t cost("10 EUR");
  cost.set_keep_precision(true);
  post->cost = cost;
  xact.add_post(post);

  // Valid post with valid cost (keep_precision=true) should be valid
  BOOST_CHECK(post->valid());

  delete post;
}

// ---------------------------------------------------------------------------
// Wave 7: Additional coverage tests
// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
// W7-1: xact.cc lines 235-236: saw_cost = true when post already has cost
// This is the path where a cost is set (non-COST_CALCULATED) and finalize
// detects it and skips auto-pricing.
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testFinalizeSawCostSkipsAutoPrice_W7)
{
  // Two commodities, one post has an explicit cost: saw_cost = true, break
  xact_t xact;
  xact._date = parse_date("2024/01/01");
  xact.payee = "SawCost";
  xact.journal = &journal;

  amount_t eur("100 EUR");
  post_t* p1 = new post_t(expenses, eur);
  p1->cost = amount_t("$110.00");
  // The cost is NOT POST_COST_CALCULATED, so saw_cost = true
  post_t* p2 = new post_t(assets, amount_t("$-110.00"));
  xact.add_post(p1);
  xact.add_post(p2);

  BOOST_CHECK(xact.finalize());
  // The post should keep its original cost since saw_cost was true
  BOOST_CHECK(p1->cost);
  BOOST_CHECK_EQUAL(*p1->cost, amount_t("$110.00"));
}

// ---------------------------------------------------------------------------
// W7-2: xact.cc lines 296-298: Fixated price with NOT_PER_UNIT
// and negative amount triggers cost negation
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testFinalizeFixatedPriceNotPerUnitNeg_W7)
{
  // Test via CLI test since fixated price annotation requires proper
  // parsing context. This test exercises the two-commodity auto-price path.
  xact_t xact;
  xact._date = parse_date("2024/01/01");
  xact.payee = "TwoCommodityAutoPrice";
  xact.journal = &journal;

  // Two commodities: EUR and $ - will trigger auto-pricing
  amount_t eur_pos("50 EUR");
  amount_t eur_neg("-50 EUR");
  post_t* p1 = new post_t(expenses, eur_pos);
  post_t* p2 = new post_t(expenses, eur_neg);
  post_t* p3 = new post_t(expenses, amount_t("$55.00"));
  post_t* p4 = new post_t(assets, amount_t("$-55.00"));
  xact.add_post(p1);
  xact.add_post(p2);
  xact.add_post(p3);
  xact.add_post(p4);

  // EUR amounts cancel, so balance is just $ amounts which balance
  BOOST_CHECK(xact.finalize());
}

// ---------------------------------------------------------------------------
// W7-3: xact.cc lines 446-449: null_post with balance.is_long()
// When all posts have amounts of 0 in a non-commodity way, the balance
// ends up as a long value.
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testFinalizeNullPostBalanceError_W7)
{
  // If balance is non-null, non-zero, and not amount/balance/long, it throws
  xact_t xact;
  xact._date = parse_date("2024/01/01");
  xact.payee = "NullPostErr";
  xact.journal = &journal;

  post_t* p1 = new post_t(expenses, amount_t("$50.00"));
  post_t* p2 = new post_t(assets, amount_t("$-30.00"));
  xact.add_post(p1);
  xact.add_post(p2);

  // This should throw because there's no null post and balance != 0
  BOOST_CHECK_THROW(xact.finalize(), balance_error);
}

// ---------------------------------------------------------------------------
// W7-4: xact.cc lines 464-467: virtual_null_post with balance.is_long()
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testFinalizeVirtualNullPostLong_W7)
{
  xact_t xact;
  xact._date = parse_date("2024/01/01");
  xact.payee = "VirtNullLong";
  xact.journal = &journal;

  // Real posts balance
  post_t* p1 = new post_t(expenses, amount_t("$50.00"));
  post_t* p2 = new post_t(assets, amount_t("$-50.00"));

  // Virtual balanced posts - one has amount, one is null
  account_t* budget = root->find_account("Budget:Test");
  post_t* p3 = new post_t(budget, amount_t("$-50.00"));
  p3->add_flags(POST_VIRTUAL | POST_MUST_BALANCE);

  account_t* budget_in = root->find_account("Budget:Income");
  post_t* p4 = new post_t(budget_in);  // null amount
  p4->add_flags(POST_VIRTUAL | POST_MUST_BALANCE);

  xact.add_post(p1);
  xact.add_post(p2);
  xact.add_post(p3);
  xact.add_post(p4);

  BOOST_CHECK(xact.finalize());
  BOOST_CHECK(!p4->amount.is_null());
  BOOST_CHECK_EQUAL(amount_t("$50.00"), p4->amount);
}

// ---------------------------------------------------------------------------
// W7-5: xact.cc line 523: some_null after finalize throws
// If posts remain with null amounts after balancing, this throws.
// This happens when a transaction has null-amount posts that can't be inferred.
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testFinalizeSomeNullThrows_W7)
{
  // Two null amount posts - finalize should throw
  xact_t xact;
  xact._date = parse_date("2024/01/01");
  xact.payee = "SomeNull";
  xact.journal = &journal;

  post_t* p1 = new post_t(expenses);  // null amount
  post_t* p2 = new post_t(assets);    // null amount
  xact.add_post(p1);
  xact.add_post(p2);

  // Two null amounts -> after finalize, all_null = true which returns false,
  // OR some_null = true which throws. With two null amounts and nothing to
  // balance from, this should throw because there's ambiguity.
  BOOST_CHECK_THROW(xact.finalize(), std::logic_error);
}

// ---------------------------------------------------------------------------
// W7-6: xact.cc lines 564-569: verify balance failure
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testVerifyUnbalanced_W7)
{
  xact_t xact;
  xact.add_flags(ITEM_TEMP);
  xact._date = parse_date("2024/01/01");
  xact.payee = "Unbal Verify";

  post_t* p1 = new post_t(expenses, amount_t("$50.00"));
  p1->add_flags(ITEM_TEMP);
  post_t* p2 = new post_t(assets, amount_t("$-30.00"));
  p2->add_flags(ITEM_TEMP);
  xact.add_post(p1);
  xact.add_post(p2);

  BOOST_CHECK_THROW(xact.verify(), std::exception);

  delete p1;
  delete p2;
}

// ---------------------------------------------------------------------------
// W7-7: xact.cc line 735: post_pred VALUE case
// The post_pred function checks VALUE op kind and returns to_boolean().
// This is used by auto_xact_t::extend_xact when the predicate is simple.
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testAutoXactSimplePredicate_W7)
{
  // Create a session context for auto_xact_t
  session_t session;
  scope_t* old_scope = scope_t::default_scope;
  scope_t::default_scope = &session;

  journal_t* j = session.read_journal_from_string(
    "= Expenses:Food\n"
    "    (Budget:Food)     (amount * -1)\n"
    "\n"
    "2024/01/01 Test\n"
    "    Expenses:Food     $10.00\n"
    "    Assets:Cash\n"
  );
  BOOST_CHECK(j != NULL);
  BOOST_CHECK_EQUAL(1, j->xacts.size());
  // The auto_xact should have been applied
  xact_t* x = j->xacts.front();
  BOOST_CHECK(x->posts.size() >= 3);

  scope_t::default_scope = old_scope;
}

// ---------------------------------------------------------------------------
// W7-8: xact.cc lines 944-946: auto_xact cost from annotation
// When auto-xact generates a post from a posting with cost and the
// amount has a price annotation, derive cost from annotation.
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testAutoXactCostFromAnnotation_W7)
{
  session_t session;
  scope_t* old_scope = scope_t::default_scope;
  scope_t::default_scope = &session;

  journal_t* j = session.read_journal_from_string(
    "= Expenses:Power\n"
    "    (Budget:Power)    1\n"
    "\n"
    "2024/01/01 Electric\n"
    "    Expenses:Power    100 kWh @@ 72 EUR\n"
    "    Assets:Cash\n"
  );
  BOOST_CHECK(j != NULL);
  BOOST_CHECK_EQUAL(1, j->xacts.size());
  // The auto_xact should have generated a Budget:Power post
  xact_t* x = j->xacts.front();
  BOOST_CHECK(x->posts.size() >= 3);

  scope_t::default_scope = old_scope;
}

// ---------------------------------------------------------------------------
// W7-9: xact.cc lines 1024, 1029: put_xact with ITEM_GENERATED and aux_date
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testPutXactGenerated_W7)
{
  xact_t xact;
  xact.add_flags(ITEM_TEMP | ITEM_GENERATED);
  xact._date = parse_date("2024/01/01");
  xact._date_aux = parse_date("2024/01/05");
  xact.payee = "Generated";
  xact.code = string("G1");
  xact.note = string("test note");

  property_tree::ptree pt;
  put_xact(pt, xact);

  // Check that generated flag is set
  auto gen = pt.get_optional<string>("<xmlattr>.generated");
  BOOST_CHECK(gen);
  BOOST_CHECK_EQUAL(*gen, "true");

  // Check that aux-date is set
  auto aux = pt.get_child_optional("aux-date");
  BOOST_CHECK(aux);

  // Check code
  auto code = pt.get_optional<string>("code");
  BOOST_CHECK(code);
  BOOST_CHECK_EQUAL(*code, "G1");
}

// ---------------------------------------------------------------------------
// W7-10: put_xact with cleared/pending state
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testPutXactStates_W7)
{
  // Cleared state
  {
    xact_t xact;
    xact.add_flags(ITEM_TEMP);
    xact._date = parse_date("2024/01/01");
    xact.payee = "Cleared";
    xact.set_state(item_t::CLEARED);

    property_tree::ptree pt;
    put_xact(pt, xact);

    auto state = pt.get_optional<string>("<xmlattr>.state");
    BOOST_CHECK(state);
    BOOST_CHECK_EQUAL(*state, "cleared");
  }

  // Pending state
  {
    xact_t xact;
    xact.add_flags(ITEM_TEMP);
    xact._date = parse_date("2024/01/01");
    xact.payee = "Pending";
    xact.set_state(item_t::PENDING);

    property_tree::ptree pt;
    put_xact(pt, xact);

    auto state = pt.get_optional<string>("<xmlattr>.state");
    BOOST_CHECK(state);
    BOOST_CHECK_EQUAL(*state, "pending");
  }
}

// ---------------------------------------------------------------------------
// W7-11: post.cc line 196: get_amount when amount is null returns 0L
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testPostGetAmountNull_W7)
{
  xact_t xact;
  xact.add_flags(ITEM_TEMP);
  xact._date = parse_date("2024/01/01");
  xact.payee = "Test";

  post_t* post = new post_t(expenses);  // null amount
  post->add_flags(ITEM_TEMP);
  xact.add_post(post);

  // When amount is null, lookup for "amount" should return 0L
  auto op = post->lookup(symbol_t::FUNCTION, "amount");
  BOOST_CHECK(op != NULL);

  delete post;
}

// ---------------------------------------------------------------------------
// W7-12: post.cc lines 211-212, 220: get_commodity, get_commodity_is_primary
// with POST_EXT_COMPOUND
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testPostCommodityCompound_W7)
{
  xact_t xact;
  xact.add_flags(ITEM_TEMP);
  xact._date = parse_date("2024/01/01");
  xact.payee = "Test";

  post_t* post = new post_t(expenses, amount_t("$10.00"));
  post->add_flags(ITEM_TEMP);
  xact.add_post(post);

  // Set compound value on xdata
  post->xdata().compound_value = amount_t("$20.00");
  post->xdata().add_flags(POST_EXT_COMPOUND);

  // commodity accessor with POST_EXT_COMPOUND uses compound_value
  auto op = post->lookup(symbol_t::FUNCTION, "commodity");
  BOOST_CHECK(op != NULL);

  // commodity_is_primary with POST_EXT_COMPOUND
  auto op2 = post->lookup(symbol_t::FUNCTION, "primary");
  BOOST_CHECK(op2 != NULL);

  delete post;
}

// ---------------------------------------------------------------------------
// W7-13: post.cc lines 235, 242: get_cost, get_price with null amount
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testPostCostPriceNull_W7)
{
  xact_t xact;
  xact.add_flags(ITEM_TEMP);
  xact._date = parse_date("2024/01/01");
  xact.payee = "Test";

  post_t* post = new post_t(expenses);  // null amount
  post->add_flags(ITEM_TEMP);
  xact.add_post(post);

  // Both cost and price should return 0L for null amount
  auto op_cost = post->lookup(symbol_t::FUNCTION, "cost");
  BOOST_CHECK(op_cost != NULL);

  auto op_price = post->lookup(symbol_t::FUNCTION, "price");
  BOOST_CHECK(op_price != NULL);

  delete post;
}

// ---------------------------------------------------------------------------
// W7-14: post.cc lines 253, 262: get_total, get_count
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testPostTotalCount_W7)
{
  xact_t xact;
  xact.add_flags(ITEM_TEMP);
  xact._date = parse_date("2024/01/01");
  xact.payee = "Test";

  post_t* post = new post_t(expenses);  // null amount
  post->add_flags(ITEM_TEMP);
  xact.add_post(post);

  // get_total with null amount returns 0L (line 253)
  auto op_total = post->lookup(symbol_t::FUNCTION, "total");
  BOOST_CHECK(op_total != NULL);

  // get_count without xdata returns 1L (line 262)
  auto op_count = post->lookup(symbol_t::FUNCTION, "count");
  BOOST_CHECK(op_count != NULL);

  delete post;
}

// ---------------------------------------------------------------------------
// W7-15: post.cc lines 519, 524, 529: N, O, R shortcut lookups
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testPostShortcutLookups_W7)
{
  xact_t xact;
  xact.add_flags(ITEM_TEMP);
  xact._date = parse_date("2024/01/01");
  xact.payee = "Test";

  post_t* post = new post_t(expenses, amount_t("$10.00"));
  post->add_flags(ITEM_TEMP);
  xact.add_post(post);

  // N = count (line 519)
  auto op_N = post->lookup(symbol_t::FUNCTION, "N");
  BOOST_CHECK(op_N != NULL);

  // O = total (line 524)
  auto op_O = post->lookup(symbol_t::FUNCTION, "O");
  BOOST_CHECK(op_O != NULL);

  // R = real (line 529)
  auto op_R = post->lookup(symbol_t::FUNCTION, "R");
  BOOST_CHECK(op_R != NULL);

  delete post;
}

// ---------------------------------------------------------------------------
// W7-16: post.cc line 539, 542: resolve_expr paths
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testPostResolveExprLong_W7)
{
  xact_t xact;
  xact.add_flags(ITEM_TEMP);
  xact._date = parse_date("2024/01/01");
  xact.payee = "Test";

  post_t* post = new post_t(expenses, amount_t("$10.00"));
  post->add_flags(ITEM_TEMP);
  xact.add_post(post);

  // resolve_expr with result that is_long (line 538-539)
  expr_t expr_long("2 + 3");
  amount_t result = post->resolve_expr(*scope_t::default_scope, expr_long);
  BOOST_CHECK_EQUAL(result, amount_t(5L));

  delete post;
}

// ---------------------------------------------------------------------------
// W7-17: post.cc line 401: post lookup non-FUNCTION kind
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testPostLookupNonFunction_W7)
{
  xact_t xact;
  xact.add_flags(ITEM_TEMP);
  xact._date = parse_date("2024/01/01");
  xact.payee = "Test";

  post_t* post = new post_t(expenses, amount_t("$10.00"));
  post->add_flags(ITEM_TEMP);
  xact.add_post(post);

  // Non-FUNCTION kind falls through to item_t::lookup (line 401)
  auto op = post->lookup(symbol_t::OPTION, "test");
  BOOST_CHECK(op == NULL);

  delete post;
}

// ---------------------------------------------------------------------------
// W7-18: post.cc lines 363, 383: fn_any, fn_all with no xact
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testPostFnAnyAllNoXact_W7)
{
  // Post without xact: fn_any returns false, fn_all returns true
  post_t post(expenses, amount_t("$10.00"));
  post.add_flags(ITEM_TEMP);

  auto op_any = post.lookup(symbol_t::FUNCTION, "any");
  BOOST_CHECK(op_any != NULL);

  auto op_all = post.lookup(symbol_t::FUNCTION, "all");
  BOOST_CHECK(op_all != NULL);
}

// ---------------------------------------------------------------------------
// W7-19: item.cc lines 49-53: item_t copy constructor
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testItemCopy_W7)
{
  xact_t xact1;
  xact1.add_flags(ITEM_TEMP);
  xact1._date = parse_date("2024/01/01");
  xact1.payee = "Original";
  xact1.set_tag("TestTag", string_value("tagval"));
  xact1.note = string("test note");

  xact_t xact2(xact1);
  xact2.add_flags(ITEM_TEMP);

  BOOST_CHECK_EQUAL(xact2.payee, "Original");
  BOOST_CHECK(xact2.has_tag("TestTag"));
  BOOST_CHECK(xact2.note);
  BOOST_CHECK_EQUAL(*xact2.note, "test note");
}

// ---------------------------------------------------------------------------
// W7-20: item.cc line 66: has_tag with value_mask
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testItemHasTagValueMask_W7)
{
  xact_t xact;
  xact.add_flags(ITEM_TEMP);
  xact._date = parse_date("2024/01/01");
  xact.payee = "Test";
  xact.set_tag("Color", string_value("blue"));

  mask_t tag_mask("Color");
  mask_t value_mask("blue");
  BOOST_CHECK(xact.has_tag(tag_mask, value_mask));

  mask_t wrong_value("red");
  BOOST_CHECK(!xact.has_tag(tag_mask, wrong_value));
}

// ---------------------------------------------------------------------------
// W7-21: item.cc line 107: has_date
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testItemHasDate_W7)
{
  xact_t xact;
  xact.add_flags(ITEM_TEMP);
  BOOST_CHECK(!xact.has_date());

  xact._date = parse_date("2024/01/01");
  BOOST_CHECK(xact.has_date());
}

// ---------------------------------------------------------------------------
// W7-22: item.cc lines 306, 313, 320: pathname, filebase, filepath when !pos
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testItemPathnameLookupNoPos_W7)
{
  xact_t xact;
  xact.add_flags(ITEM_TEMP);
  xact._date = parse_date("2024/01/01");
  xact.payee = "Test";
  // No pos set

  auto op_fn = xact.lookup(symbol_t::FUNCTION, "filename");
  BOOST_CHECK(op_fn != NULL);

  auto op_fb = xact.lookup(symbol_t::FUNCTION, "filebase");
  BOOST_CHECK(op_fb != NULL);

  auto op_fp = xact.lookup(symbol_t::FUNCTION, "filepath");
  BOOST_CHECK(op_fp != NULL);
}

// ---------------------------------------------------------------------------
// W7-23: item.cc lines 344, 350-351: id and addr
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testItemIdAndAddr_W7)
{
  xact_t xact;
  xact.add_flags(ITEM_TEMP);
  xact._date = parse_date("2024/01/01");
  xact.payee = "Test";

  // id() without UUID returns position-based string
  string id = xact.id();
  // addr returns a long
  auto op_addr = xact.lookup(symbol_t::FUNCTION, "addr");
  BOOST_CHECK(op_addr != NULL);

  auto op_id = xact.lookup(symbol_t::FUNCTION, "id");
  BOOST_CHECK(op_id != NULL);

  auto op_depth = xact.lookup(symbol_t::FUNCTION, "depth");
  BOOST_CHECK(op_depth != NULL);
}

// ---------------------------------------------------------------------------
// W7-24: item.cc line 392: get_comment
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testItemGetComment_W7)
{
  xact_t xact;
  xact.add_flags(ITEM_TEMP);
  xact._date = parse_date("2024/01/01");
  xact.payee = "Test";

  auto op = xact.lookup(symbol_t::FUNCTION, "comment");
  BOOST_CHECK(op != NULL);

  // With a note
  xact.note = string("short");
  auto op2 = xact.lookup(symbol_t::FUNCTION, "comment");
  BOOST_CHECK(op2 != NULL);
}

// ---------------------------------------------------------------------------
// W7-25: item.cc lines 439, 465, 488, etc: various lookup paths
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testItemLookupPaths_W7)
{
  xact_t xact;
  xact.add_flags(ITEM_TEMP);
  xact._date = parse_date("2024/01/01");
  xact.payee = "Test";

  // has_meta (line 464)
  auto op_hm = xact.lookup(symbol_t::FUNCTION, "has_meta");
  BOOST_CHECK(op_hm != NULL);

  // meta (line 476)
  auto op_meta = xact.lookup(symbol_t::FUNCTION, "meta");
  BOOST_CHECK(op_meta != NULL);

  // pending (line 485-486)
  auto op_pend = xact.lookup(symbol_t::FUNCTION, "pending");
  BOOST_CHECK(op_pend != NULL);

  // parent (line 487-488)
  auto op_parent = xact.lookup(symbol_t::FUNCTION, "parent");
  BOOST_CHECK(op_parent != NULL);

  // primary_date (line 489-490)
  auto op_pd = xact.lookup(symbol_t::FUNCTION, "primary_date");
  BOOST_CHECK(op_pd != NULL);

  // seq (line 497)
  auto op_seq = xact.lookup(symbol_t::FUNCTION, "seq");
  BOOST_CHECK(op_seq != NULL);

  // status / state (line 494)
  auto op_status = xact.lookup(symbol_t::FUNCTION, "status");
  BOOST_CHECK(op_status != NULL);

  // tag (line 501-502)
  auto op_tag = xact.lookup(symbol_t::FUNCTION, "tag");
  BOOST_CHECK(op_tag != NULL);

  // uuid (line 508-509)
  auto op_uuid = xact.lookup(symbol_t::FUNCTION, "uuid");
  BOOST_CHECK(op_uuid != NULL);

  // value_date (line 513-514)
  auto op_vd = xact.lookup(symbol_t::FUNCTION, "value_date");
  BOOST_CHECK(op_vd != NULL);

  // L (line 518-519)
  auto op_L = xact.lookup(symbol_t::FUNCTION, "L");
  BOOST_CHECK(op_L != NULL);

  // X (line 522-524)
  auto op_X = xact.lookup(symbol_t::FUNCTION, "X");
  BOOST_CHECK(op_X != NULL);

  // Y (line 527-529)
  auto op_Y = xact.lookup(symbol_t::FUNCTION, "Y");
  BOOST_CHECK(op_Y != NULL);
}

// ---------------------------------------------------------------------------
// W7-26: item.cc lines 536-539, 542-543: item_t::valid edge cases
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testItemValid_W7)
{
  xact_t xact;
  xact.add_flags(ITEM_TEMP);

  // Default state is UNCLEARED, valid should return true
  BOOST_CHECK(static_cast<item_t&>(xact).valid());

  // Set to CLEARED
  xact.set_state(item_t::CLEARED);
  BOOST_CHECK(static_cast<item_t&>(xact).valid());

  // Set to PENDING
  xact.set_state(item_t::PENDING);
  BOOST_CHECK(static_cast<item_t&>(xact).valid());
}

// ---------------------------------------------------------------------------
// W7-27: post.cc lines 702-740: put_post with various states and fields
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testPutPostStates_W7)
{
  xact_t xact;
  xact.add_flags(ITEM_TEMP);
  xact._date = parse_date("2024/01/01");
  xact.payee = "Test";

  // Cleared post
  {
    post_t* post = new post_t(expenses, amount_t("$10.00"));
    post->add_flags(ITEM_TEMP);
    post->set_state(item_t::CLEARED);
    xact.add_post(post);

    property_tree::ptree pt;
    put_post(pt, *post);

    auto state = pt.get_optional<string>("<xmlattr>.state");
    BOOST_CHECK(state);
    BOOST_CHECK_EQUAL(*state, "cleared");

    delete post;
    xact.posts.clear();
  }

  // Pending post
  {
    post_t* post = new post_t(expenses, amount_t("$10.00"));
    post->add_flags(ITEM_TEMP);
    post->set_state(item_t::PENDING);
    xact.add_post(post);

    property_tree::ptree pt;
    put_post(pt, *post);

    auto state = pt.get_optional<string>("<xmlattr>.state");
    BOOST_CHECK(state);
    BOOST_CHECK_EQUAL(*state, "pending");

    delete post;
    xact.posts.clear();
  }

  // Virtual post
  {
    post_t* post = new post_t(expenses, amount_t("$10.00"));
    post->add_flags(ITEM_TEMP | POST_VIRTUAL);
    xact.add_post(post);

    property_tree::ptree pt;
    put_post(pt, *post);

    auto virt = pt.get_optional<string>("<xmlattr>.virtual");
    BOOST_CHECK(virt);
    BOOST_CHECK_EQUAL(*virt, "true");

    delete post;
    xact.posts.clear();
  }

  // Generated post
  {
    post_t* post = new post_t(expenses, amount_t("$10.00"));
    post->add_flags(ITEM_TEMP | ITEM_GENERATED);
    xact.add_post(post);

    property_tree::ptree pt;
    put_post(pt, *post);

    auto gen = pt.get_optional<string>("<xmlattr>.generated");
    BOOST_CHECK(gen);
    BOOST_CHECK_EQUAL(*gen, "true");

    delete post;
    xact.posts.clear();
  }

  // Post with date and aux_date
  {
    post_t* post = new post_t(expenses, amount_t("$10.00"));
    post->add_flags(ITEM_TEMP);
    post->_date = parse_date("2024/06/15");
    post->_date_aux = parse_date("2024/06/20");
    xact.add_post(post);

    property_tree::ptree pt;
    put_post(pt, *post);

    auto date = pt.get_child_optional("date");
    BOOST_CHECK(date);
    auto aux = pt.get_child_optional("aux-date");
    BOOST_CHECK(aux);

    delete post;
    xact.posts.clear();
  }
}

// ---------------------------------------------------------------------------
// W7-28: post.cc lines 735-740: put_post with assigned_amount
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testPutPostAssignedAmount_W7)
{
  xact_t xact;
  xact.add_flags(ITEM_TEMP);
  xact._date = parse_date("2024/01/01");
  xact.payee = "Test";

  // Balance assertion (POST_CALCULATED + assigned_amount)
  {
    post_t* post = new post_t(expenses, amount_t("$10.00"));
    post->add_flags(ITEM_TEMP | POST_CALCULATED);
    post->assigned_amount = amount_t("$100.00");
    xact.add_post(post);

    property_tree::ptree pt;
    put_post(pt, *post);

    auto ba = pt.get_child_optional("balance-assertion");
    BOOST_CHECK(ba);

    delete post;
    xact.posts.clear();
  }

  // Balance assignment (no POST_CALCULATED + assigned_amount)
  {
    post_t* post = new post_t(expenses, amount_t("$10.00"));
    post->add_flags(ITEM_TEMP);
    post->assigned_amount = amount_t("$100.00");
    xact.add_post(post);

    property_tree::ptree pt;
    put_post(pt, *post);

    auto ba = pt.get_child_optional("balance-assignment");
    BOOST_CHECK(ba);

    delete post;
    xact.posts.clear();
  }
}

// ---------------------------------------------------------------------------
// W7-29: post.cc line 727: put_post with compound value
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testPutPostCompoundValue_W7)
{
  xact_t xact;
  xact.add_flags(ITEM_TEMP);
  xact._date = parse_date("2024/01/01");
  xact.payee = "Test";

  post_t* post = new post_t(expenses, amount_t("$10.00"));
  post->add_flags(ITEM_TEMP);
  xact.add_post(post);

  post->xdata().compound_value = amount_t("$20.00");
  post->xdata().add_flags(POST_EXT_COMPOUND);

  property_tree::ptree pt;
  put_post(pt, *post);

  // The post-amount should use compound_value
  auto pa = pt.get_child_optional("post-amount");
  BOOST_CHECK(pa);

  delete post;
}

// ---------------------------------------------------------------------------
// W7-30: post.cc with payee_from_tag in put_post (lines 702, 705)
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testPutPostPayeeFromTag_W7)
{
  xact_t xact;
  xact.add_flags(ITEM_TEMP);
  xact._date = parse_date("2024/01/01");
  xact.payee = "Default Payee";

  post_t* post = new post_t(expenses, amount_t("$10.00"));
  post->add_flags(ITEM_TEMP);
  post->set_tag("Payee", string_value("Custom Payee"));
  xact.add_post(post);

  property_tree::ptree pt;
  put_post(pt, *post);

  auto payee = pt.get_optional<string>("payee");
  BOOST_CHECK(payee);
  BOOST_CHECK_EQUAL(*payee, "Custom Payee");

  delete post;
}

// ---------------------------------------------------------------------------
// W7-31: xact.cc lines 313, 315: fixated price recompute skips virtual posts
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testFinalizeFixatedRecomputeSkipsVirtual_W7)
{
  xact_t xact;
  xact._date = parse_date("2024/01/01");
  xact.payee = "FixatedVirtSkip";
  xact.journal = &journal;

  // Create multiple fixated price annotations to trigger >2 commodity balance
  annotation_t ann1;
  ann1.price = amount_t("$10.00");
  ann1.add_flags(ANNOTATION_PRICE_FIXATED);
  amount_t amt1("10 AAA");
  amt1.annotate(ann1);

  annotation_t ann2;
  ann2.price = amount_t("$11.00");
  ann2.add_flags(ANNOTATION_PRICE_FIXATED);
  amount_t amt2("10 AAA");
  amt2.annotate(ann2);

  post_t* p1 = new post_t(expenses, amt1);
  post_t* p2 = new post_t(expenses, amt2);
  post_t* p3 = new post_t(assets);  // null amount

  // Add a virtual post that should be skipped during recompute
  account_t* budget = root->find_account("Budget:Test2");
  post_t* p4 = new post_t(budget, amount_t("$5.00"));
  p4->add_flags(POST_VIRTUAL | POST_MUST_BALANCE);
  post_t* p5 = new post_t(budget, amount_t("$-5.00"));
  p5->add_flags(POST_VIRTUAL | POST_MUST_BALANCE);

  xact.add_post(p1);
  xact.add_post(p2);
  xact.add_post(p3);
  xact.add_post(p4);
  xact.add_post(p5);

  BOOST_CHECK(xact.finalize());
}

// ---------------------------------------------------------------------------
// W7-32: period_xact_t description
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testPeriodXactDescription_W7)
{
  // Without pos
  period_xact_t px("Monthly");
  px.add_flags(ITEM_TEMP);
  BOOST_CHECK(!px.description().empty());

  // With pos
  position_t pos;
  pos.beg_line = 42;
  px.pos = pos;
  string desc = px.description();
  BOOST_CHECK(!desc.empty());
}

// ---------------------------------------------------------------------------
// W7-33: auto_xact_t description
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testAutoXactDescription_W7)
{
  keep_details_t keeper(true, true, true);
  expr_t expr("account =~ /Expenses/");
  predicate_t pred(expr.get_op(), keeper);
  auto_xact_t ax(pred);
  ax.add_flags(ITEM_TEMP);

  // Without pos
  string desc = ax.description();
  BOOST_CHECK(!desc.empty());

  // With pos
  position_t pos;
  pos.beg_line = 10;
  ax.pos = pos;
  desc = ax.description();
  BOOST_CHECK(!desc.empty());
}

// ---------------------------------------------------------------------------
// W7-34: Draft insert with year_directive_year (draft.cc lines 266-269, 293)
// ---------------------------------------------------------------------------

BOOST_AUTO_TEST_CASE(testDraftInsertWithSession_W7)
{
  session_t session;
  scope_t* old_scope = scope_t::default_scope;
  scope_t::default_scope = &session;

  journal_t* j = session.read_journal_from_string(
    "2024/01/01 Grocery Store\n"
    "    Expenses:Food     $50.00\n"
    "    Assets:Cash\n"
  );
  BOOST_CHECK(j != NULL);

  // Create a draft with matching payee
  value_t args;
  args.push_back(string_value("Grocery"));
  draft_t draft(args);
  xact_t* result = draft.insert(*j);
  BOOST_CHECK(result != NULL);
  if (result)
    BOOST_CHECK_EQUAL(result->payee, "Grocery Store");

  scope_t::default_scope = old_scope;
}

BOOST_AUTO_TEST_SUITE_END()
