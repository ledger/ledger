#define BOOST_TEST_DYN_LINK
#define BOOST_TEST_MODULE filters
#include <boost/test/unit_test.hpp>

#include <system.hh>

#include "filters.h"
#include "journal.h"
#include "xact.h"
#include "post.h"
#include "account.h"
#include "amount.h"
#include "report.h"
#include "session.h"

using namespace ledger;

struct filters_fixture {
  session_t session;
  report_t report;
  journal_t* journal;
  account_t* root;

  // Xacts created by make_xact are owned by the journal (via add_xact).
  // In rare cases where add_xact is bypassed, we track xacts here for cleanup.
  std::list<xact_t*> owned_xacts;

  filters_fixture() : report(session) {
    set_session_context(&session);
    journal = session.journal.get();
    root = journal->master;
  }

  ~filters_fixture() {
    for (xact_t* xact : owned_xacts)
      delete xact;
    set_session_context(NULL);
  }

  // Create a test account under the journal root
  account_t* make_account(const std::string& name) {
    return root->find_account(name);
  }

  // Create a balanced transaction with two postings, owned by the journal.
  // Returns pointers to the xact and both postings.
  struct xact_with_posts {
    xact_t* xact;
    post_t* debit;
    post_t* credit;
  };

  xact_with_posts make_balanced_xact(const std::string& payee,
                                     const date_t& date,
                                     account_t* debit_acct,
                                     account_t* credit_acct,
                                     const amount_t& amt) {
    xact_t* xact = new xact_t;
    xact->payee = payee;
    xact->_date = date;

    post_t* debit = new post_t(debit_acct, amt);
    xact->add_post(debit);

    amount_t neg_amt(amt);
    neg_amt.in_place_negate();
    post_t* credit = new post_t(credit_acct, neg_amt);
    xact->add_post(credit);

    if (!journal->add_xact(xact)) {
      delete xact;
      BOOST_FAIL("add_xact failed for balanced transaction");
    }
    xact_with_posts result = { xact, debit, credit };
    return result;
  }

  // Create a transaction with a single posting.  The xact is registered
  // with the journal so that xact->journal is set, but will not balance.
  // We bypass add_xact's finalize by setting the journal pointer directly.
  xact_t* make_xact(const std::string& payee, const date_t& date) {
    xact_t* xact = new xact_t;
    xact->payee = payee;
    xact->_date = date;
    xact->journal = journal;
    journal->xacts.push_back(xact);
    return xact;
  }

  // Create a posting on a transaction for a given account and amount
  post_t* make_post(xact_t* xact, account_t* account, const amount_t& amt) {
    post_t* post = new post_t(account, amt);
    xact->add_post(post);
    return post;
  }
};

BOOST_FIXTURE_TEST_SUITE(filters, filters_fixture)

//////////////////////////////////////////////////////////////////////
//
// collect_posts tests
//

BOOST_AUTO_TEST_CASE(testCollectPostsEmpty)
{
  // A freshly constructed collect_posts has zero posts
  collect_posts collector;
  BOOST_CHECK_EQUAL(0u, collector.length());
  BOOST_CHECK(collector.begin() == collector.end());
}

BOOST_AUTO_TEST_CASE(testCollectPostsSingle)
{
  // Pushing a single post through operator() adds it to the collection
  collect_posts collector;

  account_t* acct = make_account("Expenses:Food");
  amount_t amt(10L);
  xact_t* xact = make_xact("Grocery", date_t(2024, 1, 15));
  post_t* post = make_post(xact, acct, amt);

  collector(*post);

  BOOST_CHECK_EQUAL(1u, collector.length());
  BOOST_CHECK(collector.posts[0] == post);
}

BOOST_AUTO_TEST_CASE(testCollectPostsMultiplePreservesOrder)
{
  // Multiple posts are collected in the order they were pushed
  collect_posts collector;

  account_t* acct = make_account("Expenses:Food");
  xact_t* xact = make_xact("Store", date_t(2024, 1, 15));

  post_t* p1 = make_post(xact, acct, amount_t(10L));
  post_t* p2 = make_post(xact, acct, amount_t(20L));
  post_t* p3 = make_post(xact, acct, amount_t(30L));

  collector(*p1);
  collector(*p2);
  collector(*p3);

  BOOST_CHECK_EQUAL(3u, collector.length());
  BOOST_CHECK(collector.posts[0] == p1);
  BOOST_CHECK(collector.posts[1] == p2);
  BOOST_CHECK(collector.posts[2] == p3);
}

BOOST_AUTO_TEST_CASE(testCollectPostsClear)
{
  // clear() empties the collected posts
  collect_posts collector;

  account_t* acct = make_account("Expenses");
  xact_t* xact = make_xact("Test", date_t(2024, 1, 1));
  post_t* post = make_post(xact, acct, amount_t(10L));

  collector(*post);
  BOOST_CHECK_EQUAL(1u, collector.length());

  collector.clear();
  BOOST_CHECK_EQUAL(0u, collector.length());
}

//////////////////////////////////////////////////////////////////////
//
// ignore_posts tests
//

BOOST_AUTO_TEST_CASE(testIgnorePostsDiscardsAll)
{
  // ignore_posts does not forward anything; a downstream collector
  // should receive nothing when chained after ignore_posts
  shared_ptr<collect_posts> collector(new collect_posts);
  ignore_posts ignorer;

  account_t* acct = make_account("Assets:Cash");
  xact_t* xact = make_xact("ATM", date_t(2024, 2, 1));
  post_t* post = make_post(xact, acct, amount_t(100L));

  // Calling operator() on ignore_posts does nothing
  ignorer(*post);

  // The collector was never invoked, so it remains empty
  BOOST_CHECK_EQUAL(0u, collector->length());
}

BOOST_AUTO_TEST_CASE(testIgnorePostsDoesNotThrow)
{
  // Pushing multiple posts through ignore_posts discards all of them
  // without any errors
  ignore_posts ignorer;

  account_t* acct = make_account("Assets:Cash");
  xact_t* xact = make_xact("ATM", date_t(2024, 2, 1));
  post_t* p1 = make_post(xact, acct, amount_t(100L));
  post_t* p2 = make_post(xact, acct, amount_t(200L));

  BOOST_CHECK_NO_THROW(ignorer(*p1));
  BOOST_CHECK_NO_THROW(ignorer(*p2));
}

//////////////////////////////////////////////////////////////////////
//
// filter_posts tests
//

BOOST_AUTO_TEST_CASE(testFilterPostsAlwaysTrue)
{
  // A predicate that always evaluates to true passes all posts through
  shared_ptr<collect_posts> collector(new collect_posts);
  predicate_t pred("1", keep_details_t());
  filter_posts filter(collector, pred, report);

  account_t* acct = make_account("Expenses:Food");
  xact_t* xact = make_xact("Store", date_t(2024, 3, 1));
  post_t* p1 = make_post(xact, acct, amount_t(10L));
  post_t* p2 = make_post(xact, acct, amount_t(20L));

  filter(*p1);
  filter(*p2);
  filter.flush();

  BOOST_CHECK_EQUAL(2u, collector->length());
  BOOST_CHECK(collector->posts[0] == p1);
  BOOST_CHECK(collector->posts[1] == p2);
}

BOOST_AUTO_TEST_CASE(testFilterPostsAlwaysFalse)
{
  // A predicate that always evaluates to false discards all posts
  shared_ptr<collect_posts> collector(new collect_posts);
  predicate_t pred("0", keep_details_t());
  filter_posts filter(collector, pred, report);

  account_t* acct = make_account("Expenses:Food");
  xact_t* xact = make_xact("Store", date_t(2024, 3, 1));
  post_t* p1 = make_post(xact, acct, amount_t(10L));
  post_t* p2 = make_post(xact, acct, amount_t(20L));

  filter(*p1);
  filter(*p2);
  filter.flush();

  BOOST_CHECK_EQUAL(0u, collector->length());
}

BOOST_AUTO_TEST_CASE(testFilterPostsByAccount)
{
  // A predicate filtering by account name only passes matching posts
  shared_ptr<collect_posts> collector(new collect_posts);
  predicate_t pred("account =~ /Expenses/", keep_details_t());
  filter_posts filter(collector, pred, report);

  account_t* expenses = make_account("Expenses:Food");
  account_t* assets = make_account("Assets:Cash");
  xact_t* xact = make_xact("Store", date_t(2024, 3, 1));
  post_t* p_exp = make_post(xact, expenses, amount_t(50L));
  post_t* p_ast = make_post(xact, assets, amount_t(-50L));

  filter(*p_exp);
  filter(*p_ast);
  filter.flush();

  BOOST_CHECK_EQUAL(1u, collector->length());
  BOOST_CHECK(collector->posts[0] == p_exp);
}

BOOST_AUTO_TEST_CASE(testFilterPostsSetsMatchFlag)
{
  // Posts that match the filter predicate should have POST_EXT_MATCHES set
  shared_ptr<collect_posts> collector(new collect_posts);
  predicate_t pred("1", keep_details_t());
  filter_posts filter(collector, pred, report);

  account_t* acct = make_account("Expenses");
  xact_t* xact = make_xact("Test", date_t(2024, 3, 1));
  post_t* post = make_post(xact, acct, amount_t(10L));

  filter(*post);

  BOOST_CHECK(post->has_xdata());
  BOOST_CHECK(post->xdata().has_flags(POST_EXT_MATCHES));
}

//////////////////////////////////////////////////////////////////////
//
// sort_posts tests
//

BOOST_AUTO_TEST_CASE(testSortPostsEmpty)
{
  // Sorting an empty set produces an empty output
  shared_ptr<collect_posts> collector(new collect_posts);
  sort_posts sorter(collector, expr_t("date"), report);

  sorter.flush();

  BOOST_CHECK_EQUAL(0u, collector->length());
}

BOOST_AUTO_TEST_CASE(testSortPostsSingle)
{
  // A single post passes through unchanged
  shared_ptr<collect_posts> collector(new collect_posts);
  sort_posts sorter(collector, expr_t("date"), report);

  account_t* acct = make_account("Expenses");
  xact_t* xact = make_xact("Test", date_t(2024, 4, 1));
  post_t* post = make_post(xact, acct, amount_t(10L));

  sorter(*post);
  sorter.flush();

  BOOST_CHECK_EQUAL(1u, collector->length());
  BOOST_CHECK(collector->posts[0] == post);
}

BOOST_AUTO_TEST_CASE(testSortPostsByDate)
{
  // Posts from different transactions should be sorted by date
  shared_ptr<collect_posts> collector(new collect_posts);
  sort_posts sorter(collector, expr_t("date"), report);

  account_t* acct = make_account("Expenses");

  // Create posts with dates out of order
  xact_t* xact3 = make_xact("Third", date_t(2024, 4, 30));
  post_t* p3 = make_post(xact3, acct, amount_t(30L));

  xact_t* xact1 = make_xact("First", date_t(2024, 4, 1));
  post_t* p1 = make_post(xact1, acct, amount_t(10L));

  xact_t* xact2 = make_xact("Second", date_t(2024, 4, 15));
  post_t* p2 = make_post(xact2, acct, amount_t(20L));

  // Feed in order: p3, p1, p2
  sorter(*p3);
  sorter(*p1);
  sorter(*p2);
  sorter.flush();

  BOOST_CHECK_EQUAL(3u, collector->length());
  BOOST_CHECK(collector->posts[0] == p1);
  BOOST_CHECK(collector->posts[1] == p2);
  BOOST_CHECK(collector->posts[2] == p3);
}

BOOST_AUTO_TEST_CASE(testSortPostsClear)
{
  // After clear(), accumulated posts are discarded
  shared_ptr<collect_posts> collector(new collect_posts);
  sort_posts sorter(collector, expr_t("date"), report);

  account_t* acct = make_account("Expenses");
  xact_t* xact = make_xact("Test", date_t(2024, 4, 1));
  post_t* post = make_post(xact, acct, amount_t(10L));

  sorter(*post);

  // Clear before flushing
  sorter.clear();
  sorter.flush();

  // Nothing should have been forwarded
  BOOST_CHECK_EQUAL(0u, collector->length());
}

//////////////////////////////////////////////////////////////////////
//
// truncate_xacts tests
//

BOOST_AUTO_TEST_CASE(testTruncateXactsHeadOne)
{
  // head_count=1 returns only the first transaction's posts
  shared_ptr<collect_posts> collector(new collect_posts);
  truncate_xacts truncator(collector, /*head_count=*/1, /*tail_count=*/0);

  account_t* acct = make_account("Expenses");

  xact_t* xact1 = make_xact("First", date_t(2024, 5, 1));
  post_t* p1a = make_post(xact1, acct, amount_t(10L));
  post_t* p1b = make_post(xact1, acct, amount_t(15L));

  xact_t* xact2 = make_xact("Second", date_t(2024, 5, 2));
  post_t* p2 = make_post(xact2, acct, amount_t(20L));

  xact_t* xact3 = make_xact("Third", date_t(2024, 5, 3));
  post_t* p3 = make_post(xact3, acct, amount_t(30L));

  truncator(*p1a);
  truncator(*p1b);
  truncator(*p2);
  truncator(*p3);
  truncator.flush();

  // Only the first transaction's posts (p1a, p1b) should appear
  BOOST_CHECK_EQUAL(2u, collector->length());
  BOOST_CHECK(collector->posts[0] == p1a);
  BOOST_CHECK(collector->posts[1] == p1b);
}

BOOST_AUTO_TEST_CASE(testTruncateXactsTailOne)
{
  // tail_count=1 returns only the last transaction's posts
  shared_ptr<collect_posts> collector(new collect_posts);
  truncate_xacts truncator(collector, /*head_count=*/0, /*tail_count=*/1);

  account_t* acct = make_account("Expenses");

  xact_t* xact1 = make_xact("First", date_t(2024, 5, 1));
  post_t* p1 = make_post(xact1, acct, amount_t(10L));

  xact_t* xact2 = make_xact("Second", date_t(2024, 5, 2));
  post_t* p2 = make_post(xact2, acct, amount_t(20L));

  xact_t* xact3 = make_xact("Third", date_t(2024, 5, 3));
  post_t* p3a = make_post(xact3, acct, amount_t(30L));
  post_t* p3b = make_post(xact3, acct, amount_t(35L));

  truncator(*p1);
  truncator(*p2);
  truncator(*p3a);
  truncator(*p3b);
  truncator.flush();

  // Only the last transaction's posts (p3a, p3b) should appear
  BOOST_CHECK_EQUAL(2u, collector->length());
  BOOST_CHECK(collector->posts[0] == p3a);
  BOOST_CHECK(collector->posts[1] == p3b);
}

BOOST_AUTO_TEST_CASE(testTruncateXactsHeadLargerThanInput)
{
  // head_count larger than total xacts returns all posts
  shared_ptr<collect_posts> collector(new collect_posts);
  truncate_xacts truncator(collector, /*head_count=*/10, /*tail_count=*/0);

  account_t* acct = make_account("Expenses");

  xact_t* xact1 = make_xact("First", date_t(2024, 5, 1));
  post_t* p1 = make_post(xact1, acct, amount_t(10L));

  xact_t* xact2 = make_xact("Second", date_t(2024, 5, 2));
  post_t* p2 = make_post(xact2, acct, amount_t(20L));

  truncator(*p1);
  truncator(*p2);
  truncator.flush();

  BOOST_CHECK_EQUAL(2u, collector->length());
  BOOST_CHECK(collector->posts[0] == p1);
  BOOST_CHECK(collector->posts[1] == p2);
}

BOOST_AUTO_TEST_CASE(testTruncateXactsHeadAndTailZero)
{
  // head_count=0 and tail_count=0 produces no output
  shared_ptr<collect_posts> collector(new collect_posts);
  truncate_xacts truncator(collector, /*head_count=*/0, /*tail_count=*/0);

  account_t* acct = make_account("Expenses");

  xact_t* xact1 = make_xact("First", date_t(2024, 5, 1));
  post_t* p1 = make_post(xact1, acct, amount_t(10L));

  truncator(*p1);
  truncator.flush();

  BOOST_CHECK_EQUAL(0u, collector->length());
}

//////////////////////////////////////////////////////////////////////
//
// anonymize_posts tests
//

BOOST_AUTO_TEST_CASE(testAnonymizePostsAccountChanged)
{
  // After anonymization, the post's account name should differ from
  // the original
  shared_ptr<collect_posts> collector(new collect_posts);
  anonymize_posts anonymizer(collector);

  account_t* expenses = make_account("Expenses:Food:Groceries");
  account_t* assets = make_account("Assets:Cash");
  xact_with_posts xwp = make_balanced_xact(
      "Whole Foods Market", date_t(2024, 6, 1),
      expenses, assets, amount_t(42L));

  anonymizer(*xwp.debit);
  anonymizer.flush();

  BOOST_CHECK_EQUAL(1u, collector->length());

  // The anonymized post should have a different account name
  post_t* anon_post = collector->posts[0];
  string original_name = expenses->fullname();
  string anon_name = anon_post->account->fullname();

  BOOST_CHECK(anon_name != original_name);
  // The anonymized name should not be empty
  BOOST_CHECK(!anon_name.empty());
}

BOOST_AUTO_TEST_CASE(testAnonymizePostsPayeeChanged)
{
  // After anonymization, the transaction payee should differ from
  // the original
  shared_ptr<collect_posts> collector(new collect_posts);
  anonymize_posts anonymizer(collector);

  account_t* expenses = make_account("Expenses:Dining");
  account_t* assets = make_account("Assets:Cash");
  xact_with_posts xwp = make_balanced_xact(
      "Fancy Restaurant", date_t(2024, 6, 15),
      expenses, assets, amount_t(85L));

  anonymizer(*xwp.debit);
  anonymizer.flush();

  BOOST_CHECK_EQUAL(1u, collector->length());

  post_t* anon_post = collector->posts[0];
  string anon_payee = anon_post->xact->payee;

  BOOST_CHECK(anon_payee != "Fancy Restaurant");
  BOOST_CHECK(!anon_payee.empty());
}

BOOST_AUTO_TEST_CASE(testAnonymizePostsFlagSet)
{
  // Anonymized posts should have the POST_ANONYMIZED flag set
  shared_ptr<collect_posts> collector(new collect_posts);
  anonymize_posts anonymizer(collector);

  account_t* expenses = make_account("Expenses:Transport");
  account_t* assets = make_account("Assets:Cash");
  xact_with_posts xwp = make_balanced_xact(
      "ATM", date_t(2024, 6, 1),
      expenses, assets, amount_t(200L));

  anonymizer(*xwp.debit);
  anonymizer.flush();

  BOOST_CHECK_EQUAL(1u, collector->length());
  BOOST_CHECK(collector->posts[0]->has_flags(POST_ANONYMIZED));
}

//////////////////////////////////////////////////////////////////////
//
// push_to_posts_list tests
//

BOOST_AUTO_TEST_CASE(testPushToPostsList)
{
  // push_to_posts_list should add posts to an external posts_list
  posts_list external_list;
  push_to_posts_list pusher(external_list);

  account_t* acct = make_account("Expenses");
  xact_t* xact = make_xact("Test", date_t(2024, 7, 1));
  post_t* p1 = make_post(xact, acct, amount_t(10L));
  post_t* p2 = make_post(xact, acct, amount_t(20L));

  pusher(*p1);
  pusher(*p2);

  BOOST_CHECK_EQUAL(2u, external_list.size());
  BOOST_CHECK(external_list.front() == p1);
  BOOST_CHECK(external_list.back() == p2);
}

//////////////////////////////////////////////////////////////////////
//
// Handler chain integration tests
//

BOOST_AUTO_TEST_CASE(testFilterThenCollect)
{
  // Verify chaining: filter_posts -> collect_posts
  shared_ptr<collect_posts> collector(new collect_posts);
  predicate_t pred("1", keep_details_t());
  shared_ptr<filter_posts> filter(new filter_posts(collector, pred, report));

  account_t* acct = make_account("Expenses");
  xact_t* xact = make_xact("Test", date_t(2024, 8, 1));
  post_t* post = make_post(xact, acct, amount_t(10L));

  (*filter)(*post);
  filter->flush();

  BOOST_CHECK_EQUAL(1u, collector->length());
}

BOOST_AUTO_TEST_CASE(testSortThenFilterThenCollect)
{
  // Verify multi-stage chaining: sort_posts -> filter_posts -> collect_posts
  shared_ptr<collect_posts> collector(new collect_posts);
  predicate_t pred("1", keep_details_t());
  shared_ptr<filter_posts> filter(new filter_posts(collector, pred, report));
  sort_posts sorter(filter, expr_t("date"), report);

  account_t* acct = make_account("Expenses");

  xact_t* xact2 = make_xact("Later", date_t(2024, 8, 15));
  post_t* p2 = make_post(xact2, acct, amount_t(20L));

  xact_t* xact1 = make_xact("Earlier", date_t(2024, 8, 1));
  post_t* p1 = make_post(xact1, acct, amount_t(10L));

  sorter(*p2);
  sorter(*p1);
  sorter.flush();

  // Should be sorted by date and all pass through the true-predicate filter
  BOOST_CHECK_EQUAL(2u, collector->length());
  BOOST_CHECK(collector->posts[0] == p1);
  BOOST_CHECK(collector->posts[1] == p2);
}

BOOST_AUTO_TEST_SUITE_END()
