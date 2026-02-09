#!/usr/bin/env python3

import unittest
import operator

from ledger import *
from datetime import *

class PostingTestCase(unittest.TestCase):
    def setUp(self):
        self.journal = read_journal_from_string("""
2012-03-01 * KFC
    ; Payee: KFC
    Expenses:Food      $21.34
    ; Food: true
    Assets:Cash

2012-03-02 MJT
    Expenses:Museum      $45.67
    Assets:Cash

2012-03-03 (1234) Gas Station
    Expenses:Auto:Gas      20 GAL @ $3.50
    Assets:Cash

2012-03-04 ! Pending Transaction
    Expenses:Supplies      $10.00
    Assets:Cash

2012-03-05 Virtual Transaction
    Expenses:Real          $100.00
    [Budget:Savings]       $50.00
    [Budget:Emergency]    $-50.00
    Assets:Cash
""")

    def tearDown(self):
        close_journal_files()

    def test_basic_posting_access(self):
        """Test basic properties: amount, account, date."""
        posts = self.journal.query("food")
        self.assertEqual(len(posts), 1)

        post = posts[0]
        self.assertEqual(post.amount, Amount("$21.34"))
        self.assertEqual(str(post.account), "Expenses:Food")
        self.assertEqual(post.date, date(2012, 3, 1))

    def test_posting_amount_value(self):
        """Test that posting amounts have correct values and commodities."""
        posts = self.journal.query("museum")
        self.assertEqual(len(posts), 1)
        self.assertEqual(posts[0].amount, Amount("$45.67"))

    def test_posting_amount_with_cost(self):
        """Test that a posting with @ cost has an annotated amount."""
        posts = self.journal.query("gas")
        self.assertEqual(len(posts), 1)
        # The amount is 20 GAL but annotated with the price
        post = posts[0]
        self.assertEqual(post.amount.number(), Amount("20").number())
        self.assertEqual(post.amount.commodity.symbol, "GAL")

    def test_posting_account_name(self):
        """Test account name via str() for various accounts."""
        posts = self.journal.query("food")
        self.assertEqual(str(posts[0].account), "Expenses:Food")

        posts = self.journal.query("museum")
        self.assertEqual(str(posts[0].account), "Expenses:Museum")

    def test_posting_account_hierarchy(self):
        """Test that hierarchical accounts are parsed correctly."""
        posts = self.journal.query("gas")
        self.assertEqual(str(posts[0].account), "Expenses:Auto:Gas")

    def test_posting_date(self):
        """Test that posting dates match their parent transaction dates."""
        posts = self.journal.query("food")
        self.assertEqual(posts[0].date, date(2012, 3, 1))

        posts = self.journal.query("museum")
        self.assertEqual(posts[0].date, date(2012, 3, 2))

        posts = self.journal.query("gas")
        self.assertEqual(posts[0].date, date(2012, 3, 3))

    def test_posting_cost(self):
        """Test cost property for postings with @ price notation."""
        posts = self.journal.query("gas")
        self.assertEqual(len(posts), 1)
        post = posts[0]
        self.assertIsNotNone(post.cost)
        self.assertEqual(post.cost, Amount("$70.00"))

    def test_posting_no_cost(self):
        """Test that cost is None for postings without price notation."""
        posts = self.journal.query("food")
        self.assertEqual(len(posts), 1)
        self.assertIsNone(posts[0].cost)

    def test_posting_given_cost(self):
        """Test given_cost property for postings with @ price notation."""
        posts = self.journal.query("gas")
        self.assertEqual(len(posts), 1)
        post = posts[0]
        # given_cost reflects the per-unit cost as written
        self.assertIsNotNone(post.given_cost)

    def test_posting_given_cost_none(self):
        """Test given_cost is None when no cost is specified."""
        posts = self.journal.query("food")
        self.assertIsNone(posts[0].given_cost)

    def test_posting_has_tag_string(self):
        """Test has_tag with a string argument on posting-level tag."""
        posts = self.journal.query("food")
        self.assertEqual(len(posts), 1)
        post = posts[0]
        self.assertTrue(post.has_tag("Food"))
        self.assertFalse(post.has_tag("NonExistent"))

    def test_posting_get_tag_string(self):
        """Test get_tag with a string argument returns tag value."""
        posts = self.journal.query("food")
        self.assertEqual(len(posts), 1)
        post = posts[0]
        tag_val = post.get_tag("Food")
        self.assertIsNotNone(tag_val)

    def test_posting_get_tag_missing(self):
        """Test get_tag returns None for a tag that does not exist."""
        posts = self.journal.query("food")
        post = posts[0]
        tag_val = post.get_tag("NoSuchTag")
        self.assertIsNone(tag_val)

    def test_posting_tag_inheritance(self):
        """Test that posting inherits tags from its parent transaction."""
        posts = self.journal.query("food")
        post = posts[0]
        # The 'Payee' tag is on the transaction, should be inherited
        self.assertTrue(post.has_tag("Payee"))
        payee_val = post.get_tag("Payee")
        self.assertIsNotNone(payee_val)

    def test_reported_account(self):
        """Test that reported_account() returns the posting's account."""
        posts = self.journal.query("food")
        post = posts[0]
        reported = post.reported_account()
        self.assertEqual(str(reported), "Expenses:Food")

    def test_reported_account_default(self):
        """Test reported_account equals account when no override is set."""
        posts = self.journal.query("museum")
        post = posts[0]
        self.assertEqual(str(post.reported_account()), str(post.account))

    def test_set_reported_account(self):
        """Test set_reported_account changes reported_account result."""
        posts = self.journal.query("food")
        food_post = posts[0]
        posts_museum = self.journal.query("museum")
        museum_post = posts_museum[0]

        # Set food posting's reported account to museum's account
        food_post.set_reported_account(museum_post.account)
        self.assertEqual(str(food_post.reported_account()), "Expenses:Museum")

    def test_must_balance_regular(self):
        """Test must_balance() returns True for regular postings."""
        posts = self.journal.query("food")
        self.assertTrue(posts[0].must_balance())

    def test_must_balance_virtual_balanced(self):
        """Test must_balance() for virtual balanced postings [brackets]."""
        posts = self.journal.query("budget and savings")
        self.assertEqual(len(posts), 1)
        # [Budget:Savings] is a balanced virtual posting: must_balance is True
        self.assertTrue(posts[0].must_balance())

    def test_must_balance_virtual_unbalanced(self):
        """Test must_balance() is False for unbalanced virtual postings."""
        journal = read_journal_from_string("""
2012-06-01 Test
    Expenses:Food      $10.00
    (Virtual:Unbal)    $5.00
    Assets:Cash
""")
        posts = journal.query("unbal")
        self.assertEqual(len(posts), 1)
        # (Virtual:Unbal) is an unbalanced virtual posting: must_balance is False
        self.assertFalse(posts[0].must_balance())

    def test_posting_note(self):
        """Test the note property inherited from item_t."""
        posts = self.journal.query("food")
        post = posts[0]
        # The posting has a note/tag line "; Food: true"
        self.assertIsNotNone(post.note)

    def test_posting_note_absent(self):
        """Test note is None when no note is present on the posting."""
        posts = self.journal.query("museum")
        post = posts[0]
        # Museum posting has no note
        self.assertIsNone(post.note)

    def test_posting_state_cleared(self):
        """Test state property for a cleared (*) transaction's postings."""
        posts = self.journal.query("food")
        post = posts[0]
        # Parent transaction is "2012-03-01 * KFC" (cleared)
        self.assertEqual(post.state, State.Cleared)

    def test_posting_state_uncleared(self):
        """Test state property for an uncleared transaction's postings."""
        posts = self.journal.query("museum")
        post = posts[0]
        self.assertEqual(post.state, State.Uncleared)

    def test_posting_state_pending(self):
        """Test state property for a pending (!) transaction's postings."""
        posts = self.journal.query("supplies")
        post = posts[0]
        self.assertEqual(post.state, State.Pending)

    def test_posting_valid(self):
        """Test that valid() returns True for parsed postings."""
        posts = self.journal.query("food")
        self.assertTrue(posts[0].valid())

        posts = self.journal.query("gas")
        self.assertTrue(posts[0].valid())

    def test_posting_xact_reference(self):
        """Test accessing the parent transaction via the xact property."""
        posts = self.journal.query("food")
        post = posts[0]
        xact = post.xact
        self.assertIsNotNone(xact)
        self.assertEqual(xact.payee, "KFC")

    def test_posting_xact_payee_different_transactions(self):
        """Test xact reference across different transactions."""
        posts = self.journal.query("museum")
        self.assertEqual(posts[0].xact.payee, "MJT")

        posts = self.journal.query("supplies")
        self.assertEqual(posts[0].xact.payee, "Pending Transaction")

    def test_posting_xdata_lifecycle(self):
        """Test xdata creation, has_xdata, and clear_xdata."""
        posts = self.journal.query("food")
        post = posts[0]

        # Force creation by calling xdata()
        xd = post.xdata()
        self.assertTrue(post.has_xdata())

        post.clear_xdata()
        self.assertFalse(post.has_xdata())

    def test_posting_xdata_create_after_clear(self):
        """Test that xdata can be recreated after clearing."""
        posts = self.journal.query("food")
        post = posts[0]

        post.xdata()
        self.assertTrue(post.has_xdata())

        post.clear_xdata()
        self.assertFalse(post.has_xdata())

        # Recreate
        xd = post.xdata()
        self.assertTrue(post.has_xdata())
        self.assertIsNotNone(xd.count)

    def test_posting_xdata_count(self):
        """Test xdata count property."""
        posts = self.journal.query("food")
        post = posts[0]
        xd = post.xdata()
        # count starts at 0 before any reporting pipeline runs
        self.assertEqual(xd.count, 0)

    def test_posting_xdata_flags(self):
        """Test xdata flags operations."""
        posts = self.journal.query("food")
        post = posts[0]
        xd = post.xdata()

        # Initially no extended flags
        self.assertFalse(xd.has_flags(POST_EXT_RECEIVED))

        xd.add_flags(POST_EXT_RECEIVED)
        self.assertTrue(xd.has_flags(POST_EXT_RECEIVED))

        xd.drop_flags(POST_EXT_RECEIVED)
        self.assertFalse(xd.has_flags(POST_EXT_RECEIVED))

    def test_posting_iterator_on_xact(self):
        """Test iterating postings within a transaction."""
        xacts = [x for x in self.journal]
        # First transaction (KFC) has 2 postings
        kfc_posts = [p for p in xacts[0]]
        self.assertEqual(len(kfc_posts), 2)
        amounts = [str(p.amount) for p in kfc_posts]
        self.assertIn("$21.34", amounts)
        self.assertIn("$-21.34", amounts)

    def test_posting_iterator_counts(self):
        """Test posting counts across different transactions."""
        xacts = [x for x in self.journal]
        # KFC: 2 postings, MJT: 2, Gas Station: 2, Pending: 2, Virtual: 4
        self.assertEqual(len([p for p in xacts[0]]), 2)
        self.assertEqual(len([p for p in xacts[1]]), 2)
        self.assertEqual(len([p for p in xacts[2]]), 2)
        self.assertEqual(len([p for p in xacts[3]]), 2)
        self.assertEqual(len([p for p in xacts[4]]), 4)

    def test_posting_balancing_amount(self):
        """Test that the auto-calculated balancing posting has correct amount."""
        posts = self.journal.query("cash and expr 'date == [2012-03-01]'")
        self.assertEqual(len(posts), 1)
        self.assertEqual(posts[0].amount, Amount("$-21.34"))

    def test_multiple_queries_xdata_stability(self):
        """Test that repeated queries produce consistent results."""
        posts1 = self.journal.query("food")
        amount1 = posts1[0].amount
        account1 = str(posts1[0].account)

        posts2 = self.journal.query("food")
        amount2 = posts2[0].amount
        account2 = str(posts2[0].account)

        self.assertEqual(amount1, amount2)
        self.assertEqual(account1, account2)

    def test_query_then_iterate_stability(self):
        """Test that query results remain stable after iterating transactions."""
        posts = self.journal.query("food")
        food_amount = posts[0].amount

        # Now iterate all transactions
        for xact in self.journal:
            for post in xact:
                _ = post.amount

        # Original query results should still be valid
        self.assertEqual(posts[0].amount, food_amount)
        self.assertEqual(str(posts[0].account), "Expenses:Food")

    def test_sequential_queries_different_accounts(self):
        """Test that sequential queries for different accounts work correctly."""
        posts_food = self.journal.query("food")
        posts_museum = self.journal.query("museum")
        posts_gas = self.journal.query("gas")

        self.assertEqual(str(posts_food[0].account), "Expenses:Food")
        self.assertEqual(str(posts_museum[0].account), "Expenses:Museum")
        self.assertEqual(str(posts_gas[0].account), "Expenses:Auto:Gas")

        # Verify amounts are still correct after all queries
        self.assertEqual(posts_food[0].amount, Amount("$21.34"))
        self.assertEqual(posts_museum[0].amount, Amount("$45.67"))

    def test_multiple_postings_same_query(self):
        """Test querying for multiple postings at once."""
        posts = self.journal.query("expenses")
        # There are 5 expense postings across all transactions:
        # Food, Museum, Gas, Supplies, Real
        self.assertEqual(len(posts), 5)

    def test_posting_assigned_amount_none(self):
        """Test that assigned_amount is None when not set."""
        posts = self.journal.query("food")
        self.assertIsNone(posts[0].assigned_amount)

    def test_posting_id_and_seq(self):
        """Test that id() and seq() return identifiers."""
        posts = self.journal.query("food")
        post = posts[0]
        # id returns a string representation, seq returns an int
        self.assertIsNotNone(post.id())
        self.assertIsInstance(post.seq(), int)

    def test_posting_account_identity(self):
        """Test that the account object is shared between posting and xact iteration."""
        posts = self.journal.query("food")
        queried_account = str(posts[0].account)

        xacts = [x for x in self.journal]
        kfc_posts = [p for p in xacts[0]]
        food_post = [p for p in kfc_posts if str(p.account) == "Expenses:Food"][0]

        self.assertEqual(queried_account, str(food_post.account))


class PostingWithAuxDateTestCase(unittest.TestCase):
    """Separate test case for auxiliary date features."""

    def setUp(self):
        self.journal = read_journal_from_string("""
2012-04-01=2012-04-15 Dated Transaction
    Expenses:Rent      $500.00
    Assets:Cash
""")

    def tearDown(self):
        close_journal_files()

    def test_posting_aux_date(self):
        """Test that aux_date is accessible when set on the transaction."""
        posts = self.journal.query("rent")
        self.assertEqual(len(posts), 1)
        post = posts[0]
        self.assertEqual(post.date, date(2012, 4, 1))
        self.assertIsNotNone(post.aux_date)
        self.assertEqual(post.aux_date, date(2012, 4, 15))

    def test_posting_no_aux_date(self):
        """Test aux_date for a posting without an auxiliary date."""
        posts = self.journal.query("cash")
        post = posts[0]
        # Cash posting inherits from same xact, so it also has aux_date
        self.assertEqual(post.aux_date, date(2012, 4, 15))


class PostingWithAssignedAmountTestCase(unittest.TestCase):
    """Test case for balance assignment features."""

    def setUp(self):
        self.journal = read_journal_from_string("""
2012-05-01 Opening
    Assets:Checking      $1000.00
    Equity:Opening

2012-05-02 Adjustment
    Assets:Checking       = $900.00
    Expenses:Adjustment
""")

    def tearDown(self):
        close_journal_files()

    def test_posting_assigned_amount(self):
        """Test assigned_amount for balance assignment postings."""
        posts = self.journal.query("checking and expr 'date == [2012-05-02]'")
        self.assertEqual(len(posts), 1)
        post = posts[0]
        self.assertIsNotNone(post.assigned_amount)
        self.assertEqual(post.assigned_amount, Amount("$900.00"))

    def test_posting_no_assigned_amount(self):
        """Test assigned_amount is None for normal (non-assignment) postings."""
        posts = self.journal.query("checking and expr 'date == [2012-05-01]'")
        self.assertEqual(len(posts), 1)
        self.assertIsNone(posts[0].assigned_amount)


class PostingTagsTestCase(unittest.TestCase):
    """Dedicated test case for comprehensive tag operations."""

    def setUp(self):
        self.journal = read_journal_from_string("""
2012-06-01 Store
    ; TxnTag: transaction-level
    Expenses:Food      $30.00
    ; PostTag: post-level
    ; Category: groceries
    Assets:Cash
""")

    def tearDown(self):
        close_journal_files()

    def test_posting_level_tag(self):
        """Test accessing a tag defined directly on the posting."""
        posts = self.journal.query("food")
        post = posts[0]
        self.assertTrue(post.has_tag("PostTag"))
        val = post.get_tag("PostTag")
        self.assertIsNotNone(val)

    def test_transaction_level_tag_inherited(self):
        """Test that posting inherits tags from its parent transaction."""
        posts = self.journal.query("food")
        post = posts[0]
        self.assertTrue(post.has_tag("TxnTag"))
        val = post.get_tag("TxnTag")
        self.assertIsNotNone(val)

    def test_multiple_tags_on_posting(self):
        """Test accessing multiple tags on the same posting."""
        posts = self.journal.query("food")
        post = posts[0]
        self.assertTrue(post.has_tag("PostTag"))
        self.assertTrue(post.has_tag("Category"))

        cat_val = post.get_tag("Category")
        self.assertIsNotNone(cat_val)

    def test_tag_not_present(self):
        """Test that has_tag returns False for absent tags."""
        posts = self.journal.query("food")
        post = posts[0]
        self.assertFalse(post.has_tag("MissingTag"))
        self.assertIsNone(post.get_tag("MissingTag"))

    def test_tag_on_cash_posting(self):
        """Test that the cash posting does not have the food posting's tags."""
        posts = self.journal.query("cash")
        post = posts[0]
        # Cash posting should not have PostTag (that is on the Food posting)
        self.assertFalse(post.has_tag("PostTag"))
        # But it should inherit the transaction-level tag
        self.assertTrue(post.has_tag("TxnTag"))

    def test_get_tag_value_content(self):
        """Test that get_tag returns the correct tag value."""
        posts = self.journal.query("food")
        post = posts[0]
        val = post.get_tag("Category")
        self.assertIsNotNone(val)
        self.assertEqual(str(val), "groceries")

    def test_get_tag_txn_value_content(self):
        """Test that inherited transaction tag value is correct."""
        posts = self.journal.query("food")
        post = posts[0]
        val = post.get_tag("TxnTag")
        self.assertIsNotNone(val)
        self.assertEqual(str(val), "transaction-level")


def suite():
    loader = unittest.TestLoader()
    suite = unittest.TestSuite()
    suite.addTests(loader.loadTestsFromTestCase(PostingTestCase))
    suite.addTests(loader.loadTestsFromTestCase(PostingWithAuxDateTestCase))
    suite.addTests(loader.loadTestsFromTestCase(PostingWithAssignedAmountTestCase))
    suite.addTests(loader.loadTestsFromTestCase(PostingTagsTestCase))
    return suite

if __name__ == '__main__':
    unittest.main()
