#define BOOST_TEST_DYN_LINK
//#define BOOST_TEST_MODULE account
#include <boost/test/unit_test.hpp>

#include <system.hh>

#include "account.h"
#include "post.h"
#include "xact.h"
#include "amount.h"

using namespace ledger;

struct account_fixture {
  account_fixture() {
  times_initialize();
  amount_t::initialize();
  amount_t::stream_fullstrings = true;
  }

  ~account_fixture() {
  amount_t::shutdown();
  times_shutdown();
  }
};

BOOST_FIXTURE_TEST_SUITE(account, account_fixture)

BOOST_AUTO_TEST_CASE(testConstructRoot)
{
  // A root account has no parent, an empty name, and depth 0
  account_t root;
  BOOST_CHECK(root.parent == NULL);
  BOOST_CHECK_EQUAL(string(""), root.name);
  BOOST_CHECK(! root.note);
  BOOST_CHECK_EQUAL(0, root.depth);
  BOOST_CHECK(root.valid());
}

BOOST_AUTO_TEST_CASE(testConstructChild)
{
  // A child account has a parent pointer and depth = parent->depth + 1
  account_t root;
  account_t child(&root, "Assets");
  BOOST_CHECK(child.parent == &root);
  BOOST_CHECK_EQUAL(string("Assets"), child.name);
  BOOST_CHECK_EQUAL(1, child.depth);
  BOOST_CHECK(child.valid());

  // A grandchild has depth 2
  account_t grandchild(&child, "Bank");
  BOOST_CHECK(grandchild.parent == &child);
  BOOST_CHECK_EQUAL(string("Bank"), grandchild.name);
  BOOST_CHECK_EQUAL(2, grandchild.depth);
  BOOST_CHECK(grandchild.valid());
}

BOOST_AUTO_TEST_CASE(testConstructWithNote)
{
  account_t root;
  account_t child(&root, "Expenses", string("General expenses"));
  BOOST_CHECK(child.note);
  BOOST_CHECK_EQUAL(string("General expenses"), *child.note);
  BOOST_CHECK(child.valid());
}

BOOST_AUTO_TEST_CASE(testFullname)
{
  // Build a hierarchy: root -> Assets -> Bank -> Checking
  account_t root;
  account_t assets(&root, "Assets");
  account_t bank(&assets, "Bank");
  account_t checking(&bank, "Checking");

  // Root has an empty fullname
  BOOST_CHECK_EQUAL(string(""), root.fullname());

  // First level: just the name itself
  BOOST_CHECK_EQUAL(string("Assets"), assets.fullname());

  // Second level: parent:child
  BOOST_CHECK_EQUAL(string("Assets:Bank"), bank.fullname());

  // Third level: grandparent:parent:child
  BOOST_CHECK_EQUAL(string("Assets:Bank:Checking"), checking.fullname());
}

BOOST_AUTO_TEST_CASE(testDepthCalculation)
{
  account_t root;
  BOOST_CHECK_EQUAL(0, root.depth);

  account_t level1(&root, "A");
  BOOST_CHECK_EQUAL(1, level1.depth);

  account_t level2(&level1, "B");
  BOOST_CHECK_EQUAL(2, level2.depth);

  account_t level3(&level2, "C");
  BOOST_CHECK_EQUAL(3, level3.depth);

  account_t level4(&level3, "D");
  BOOST_CHECK_EQUAL(4, level4.depth);
}

BOOST_AUTO_TEST_CASE(testFindAccountAutoCreate)
{
  // find_account with auto_create=true (the default) should create
  // missing accounts along the colon-separated path
  account_t root;
  account_t* found = root.find_account("Assets:Bank:Checking");
  BOOST_CHECK(found != NULL);
  BOOST_CHECK_EQUAL(string("Checking"), found->name);
  BOOST_CHECK_EQUAL(string("Assets:Bank:Checking"), found->fullname());
  BOOST_CHECK_EQUAL(3, found->depth);

  // The intermediate accounts should also have been created
  account_t* assets = root.find_account("Assets");
  BOOST_CHECK(assets != NULL);
  BOOST_CHECK_EQUAL(string("Assets"), assets->name);
  BOOST_CHECK_EQUAL(1, assets->depth);

  account_t* bank = root.find_account("Assets:Bank");
  BOOST_CHECK(bank != NULL);
  BOOST_CHECK_EQUAL(string("Bank"), bank->name);
  BOOST_CHECK_EQUAL(2, bank->depth);

  // Finding the same path again should return the same pointer
  account_t* found_again = root.find_account("Assets:Bank:Checking");
  BOOST_CHECK(found == found_again);

  BOOST_CHECK(root.valid());
}

BOOST_AUTO_TEST_CASE(testFindAccountNoAutoCreate)
{
  // find_account with auto_create=false should return NULL for missing accounts
  account_t root;
  account_t* found = root.find_account("Expenses:Food", false);
  BOOST_CHECK(found == NULL);

  // Create the account, then find it with auto_create=false
  account_t* created = root.find_account("Expenses:Food", true);
  BOOST_CHECK(created != NULL);

  account_t* found_again = root.find_account("Expenses:Food", false);
  BOOST_CHECK(found_again == created);

  // A partial path that does not exist returns NULL when auto_create is false
  // First, only "Expenses" and "Expenses:Food" exist. "Expenses:Rent" does not.
  account_t* missing = root.find_account("Expenses:Rent", false);
  BOOST_CHECK(missing == NULL);

  BOOST_CHECK(root.valid());
}

BOOST_AUTO_TEST_CASE(testAddAndRemoveAccount)
{
  account_t root;

  // Manually add a child account
  account_t* child = new account_t(&root, "Liabilities");
  root.add_account(child);
  BOOST_CHECK_EQUAL(1, root.accounts.size());

  // Find the added account
  accounts_map::const_iterator it = root.accounts.find("Liabilities");
  BOOST_CHECK(it != root.accounts.end());
  BOOST_CHECK(it->second == child);

  // Remove the account
  bool removed = root.remove_account(child);
  BOOST_CHECK(removed);
  BOOST_CHECK_EQUAL(0, root.accounts.size());

  // Removing a non-existent account returns false
  account_t other(NULL, "Phantom");
  bool removed_again = root.remove_account(&other);
  BOOST_CHECK(! removed_again);

  // Clean up the orphaned child since root's destructor will not delete it
  // after it was removed from the accounts map
  delete child;

  BOOST_CHECK(root.valid());
}

BOOST_AUTO_TEST_CASE(testAddAndRemovePost)
{
  account_t root;
  account_t* expenses = root.find_account("Expenses");

  // Initially no posts
  BOOST_CHECK(expenses->posts.empty());

  // Create a simple post and add it
  post_t post1(expenses);
  expenses->add_post(&post1);
  BOOST_CHECK_EQUAL(1, expenses->posts.size());
  BOOST_CHECK(expenses->posts.front() == &post1);

  // Add a second post
  post_t post2(expenses);
  expenses->add_post(&post2);
  BOOST_CHECK_EQUAL(2, expenses->posts.size());

  // Remove the first post
  bool removed = expenses->remove_post(&post1);
  BOOST_CHECK(removed);
  BOOST_CHECK_EQUAL(1, expenses->posts.size());
  BOOST_CHECK(expenses->posts.front() == &post2);

  // After remove_post, the post's account pointer is set to NULL
  BOOST_CHECK(post1.account == NULL);

  // Remove the second post
  removed = expenses->remove_post(&post2);
  BOOST_CHECK(removed);
  BOOST_CHECK(expenses->posts.empty());

  BOOST_CHECK(root.valid());
}

BOOST_AUTO_TEST_CASE(testXdata)
{
  account_t root;
  account_t* acct = root.find_account("Assets");

  // Initially has_xdata() is false
  BOOST_CHECK(! acct->has_xdata());

  // Calling xdata() creates the xdata
  account_t::xdata_t& xd = acct->xdata();
  BOOST_CHECK(acct->has_xdata());

  // The xdata should have default values
  BOOST_CHECK(! xd.has_flags(ACCOUNT_EXT_VISITED));
  BOOST_CHECK(! xd.has_flags(ACCOUNT_EXT_MATCHING));

  // Set and check a flag
  xd.add_flags(ACCOUNT_EXT_VISITED);
  BOOST_CHECK(xd.has_flags(ACCOUNT_EXT_VISITED));
  BOOST_CHECK(acct->has_xflags(ACCOUNT_EXT_VISITED));

  // clear_xdata() should reset it
  acct->clear_xdata();
  BOOST_CHECK(! acct->has_xdata());

  BOOST_CHECK(root.valid());
}

BOOST_AUTO_TEST_CASE(testValid)
{
  account_t root;

  // A fresh root account is valid
  BOOST_CHECK(root.valid());

  // Create a hierarchy via find_account and verify the whole tree is valid
  root.find_account("Income:Salary");
  root.find_account("Expenses:Food:Groceries");
  root.find_account("Assets:Bank:Checking");
  root.find_account("Assets:Bank:Savings");
  BOOST_CHECK(root.valid());
}

BOOST_AUTO_TEST_CASE(testAccountFlags)
{
  account_t root;

  // Default flags are ACCOUNT_NORMAL (0)
  BOOST_CHECK(! root.has_flags(ACCOUNT_KNOWN));
  BOOST_CHECK(! root.has_flags(ACCOUNT_TEMP));
  BOOST_CHECK(! root.has_flags(ACCOUNT_GENERATED));

  // Set ACCOUNT_KNOWN
  root.add_flags(ACCOUNT_KNOWN);
  BOOST_CHECK(root.has_flags(ACCOUNT_KNOWN));
  BOOST_CHECK(! root.has_flags(ACCOUNT_TEMP));

  // Set ACCOUNT_TEMP
  root.add_flags(ACCOUNT_TEMP);
  BOOST_CHECK(root.has_flags(ACCOUNT_KNOWN));
  BOOST_CHECK(root.has_flags(ACCOUNT_TEMP));

  // Drop ACCOUNT_KNOWN
  root.drop_flags(ACCOUNT_KNOWN);
  BOOST_CHECK(! root.has_flags(ACCOUNT_KNOWN));
  BOOST_CHECK(root.has_flags(ACCOUNT_TEMP));

  // Test that ACCOUNT_TEMP propagates to children created via find_account
  account_t temp_root;
  temp_root.add_flags(ACCOUNT_TEMP);
  account_t* child = temp_root.find_account("Temp:Child");
  BOOST_CHECK(child != NULL);
  BOOST_CHECK(child->has_flags(ACCOUNT_TEMP));

  // The intermediate account should also have ACCOUNT_TEMP
  account_t* intermediate = temp_root.find_account("Temp");
  BOOST_CHECK(intermediate != NULL);
  BOOST_CHECK(intermediate->has_flags(ACCOUNT_TEMP));

  // ACCOUNT_GENERATED also propagates
  account_t gen_root;
  gen_root.add_flags(ACCOUNT_GENERATED);
  account_t* gen_child = gen_root.find_account("Generated:Sub");
  BOOST_CHECK(gen_child != NULL);
  BOOST_CHECK(gen_child->has_flags(ACCOUNT_GENERATED));

  BOOST_CHECK(root.valid());
  BOOST_CHECK(temp_root.valid());
  BOOST_CHECK(gen_root.valid());
}

BOOST_AUTO_TEST_CASE(testOperatorString)
{
  // The operator string() returns fullname()
  account_t root;
  account_t assets(&root, "Assets");
  account_t bank(&assets, "Bank");

  string name = static_cast<string>(bank);
  BOOST_CHECK_EQUAL(string("Assets:Bank"), name);
}

BOOST_AUTO_TEST_CASE(testOutputStream)
{
  // operator<< prints fullname()
  account_t root;
  account_t assets(&root, "Assets");
  account_t bank(&assets, "Checking");

  std::ostringstream out;
  out << bank;
  BOOST_CHECK_EQUAL(string("Assets:Checking"), out.str());
}

BOOST_AUTO_TEST_SUITE_END()
