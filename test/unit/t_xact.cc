#define BOOST_TEST_DYN_LINK
//#define BOOST_TEST_MODULE xact
#include <boost/test/unit_test.hpp>

#include <system.hh>

#include "xact.h"
#include "post.h"
#include "account.h"
#include "amount.h"
#include "journal.h"

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

BOOST_AUTO_TEST_SUITE_END()
