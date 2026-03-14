#!/usr/bin/env python3

import unittest

from lpy.core import *

class JournalTestCase(unittest.TestCase):
    def tearDown(self):
        close_journal_files()

    def testBasicRead(self):
        journal = read_journal_from_string("""
2012-03-01 KFC
    Expenses:Food      $21.34
    Assets:Cash
""")
        self.assertEqual(type(journal), Journal)

        for xact in journal:
            self.assertEqual(xact.payee, "KFC")

        for post in journal.query("food"):
            self.assertEqual(str(post.account), "Expenses:Food")
            self.assertEqual(post.amount, Amount("$21.34"))

    def testXactsProperty(self):
        """Test that journal.xacts is iterable as a property (GitHub issue #682)."""
        journal = read_journal_from_string("""
2012-03-01 KFC
    Expenses:Food      $21.34
    Assets:Cash
""")
        # xacts should be iterable directly without calling it as a method
        xacts = [xact for xact in journal.xacts]
        self.assertEqual(len(xacts), 1)
        self.assertEqual(xacts[0].payee, "KFC")

        # posts should be iterable directly from xact.posts (no parentheses)
        posts = [post for post in xacts[0].posts]
        self.assertEqual(len(posts), 2)

        # post.xact.posts should also be iterable directly (the original bug)
        for post in journal.query("food"):
            sibling_posts = [p for p in post.xact.posts]
            self.assertEqual(len(sibling_posts), 2)

    def testParseError(self):
        # TODO: ledger spits out parse errors to standard out.
        # This should not happen, especially when the error
        # has already been captured by a Python exception.
        def fun():
            read_journal_from_string("""
2012-03-01 KFC
    Expenses:Food  rsnetnirsnti
    Assets:Cash
""")
        self.assertRaises(RuntimeError, fun)
        try:
            fun()
        except RuntimeError as e:
            self.assertEqual(str(e).splitlines()[-1],
                              "No quantity specified for amount")

    def testRereadJournalFromString(self):
        """Test that reading a journal a second time does not cause an assertion failure.
        Regression test for GitHub issues #2320 and #514."""
        journal = read_journal_from_string("""
2012-03-01 KFC
    Expenses:Food      $21.34
    Assets:Cash
""")
        for xact in journal:
            self.assertEqual(xact.payee, "KFC")

        # Reading a second journal should reset state and work correctly
        journal = read_journal_from_string("""
2012-03-02 Starbucks
    Expenses:Coffee    $5.00
    Assets:Cash
""")
        xacts = [xact for xact in journal]
        self.assertEqual(len(xacts), 1)
        self.assertEqual(xacts[0].payee, "Starbucks")

        for post in journal.query("coffee"):
            self.assertEqual(str(post.account), "Expenses:Coffee")
            self.assertEqual(post.amount, Amount("$5.00"))

    def testCloseAndRereadJournal(self):
        """Test that close_journal_files followed by read works correctly.
        Regression test for GitHub issues #2320 and #514."""
        journal = read_journal_from_string("""
2012-03-01 KFC
    Expenses:Food      $21.34
    Assets:Cash
""")
        for xact in journal:
            self.assertEqual(xact.payee, "KFC")

        close_journal_files()

        journal = read_journal_from_string("""
2012-03-02 Starbucks
    Expenses:Coffee    $5.00
    Assets:Cash
""")
        xacts = [xact for xact in journal]
        self.assertEqual(len(xacts), 1)
        self.assertEqual(xacts[0].payee, "Starbucks")

    def testFilterByCommodity(self):
        journal = read_journal_from_string("""
commodity Comm
    alias Abc

2025/01/01 Shop
    Expenses:Books         Comm100.00
    Assets:Cash

2025/01/02 Shop
    Expenses:Books         Abc100.00
    Assets:Cash

2025/01/03 Shop
    Expenses:Books         B100.00
    Assets:Cash
""")
 
        comm_control = Amount("Comm1").commodity
        b_control = Amount("B1").commodity

        posts = journal.query('books -l "commodity == \\"Comm\\""')
        self.assertTrue(
            len(posts) == 2
            and posts[0].amount.commodity == comm_control
            and posts[1].amount.commodity == comm_control)

        posts = journal.query('books -l "commodity == \\"Abc\\""')
        self.assertTrue(
            len(posts) == 2
            and posts[0].amount.commodity == comm_control
            and posts[1].amount.commodity == comm_control)

        posts = journal.query('books -l "commodity < \\"C\\""')
        self.assertTrue(len(posts) == 1 and posts[0].amount.commodity == b_control)

        posts = journal.query('books -l "commodity  + \\"A\\" == \\"BA\\""')
        self.assertTrue(len(posts) == 1 and posts[0].amount.commodity == b_control)

        posts = journal.query('books -l "commodity  * 2 == \\"BB\\""')
        self.assertTrue(len(posts) == 1 and posts[0].amount.commodity == b_control)

    def testQueryDisplayAmounts_exchange(self):
        """Test that display_amounts reflects -X commodity exchange (issue #2158).

        journal.query("-X EUR ...") should expose the EUR-converted amounts
        via PostCollectorWrapper.display_amounts, even though posting.amount
        still returns the original commodity (USD) for backward compatibility.
        """
        journal = read_journal_from_string("""
commodity USD
commodity EUR

D 1000.00 EUR

P 2023/01/01 EUR 1.07 USD

2023/01/01 * Initial balance
    Assets:Account  1000.00 USD
    Equity:Initial

2023/01/01 * Test expense
    Expenses:Category  10.00 USD
    Assets:Account
""")
        posts = journal.query("-X EUR --no-rounding --no-revalued Assets:Account")
        self.assertEqual(len(posts), 2)

        # Raw posting amounts are still in USD (backward compat)
        self.assertEqual(posts[0].amount, Amount("1000.00 USD"))
        self.assertEqual(posts[1].amount, Amount("-10.00 USD"))

        # display_amounts should contain EUR-converted values
        display_amts = posts.display_amounts
        self.assertEqual(len(display_amts), 2)

        # 1000.00 USD / 1.07 USD per EUR ≈ 934.58 EUR
        amt0 = display_amts[0].to_amount()
        self.assertEqual(amt0.commodity.symbol, "EUR")
        self.assertAlmostEqual(float(str(amt0.number())), 934.58, places=0)

        # -10.00 USD / 1.07 USD per EUR ≈ -9.35 EUR
        amt1 = display_amts[1].to_amount()
        self.assertEqual(amt1.commodity.symbol, "EUR")
        self.assertAlmostEqual(float(str(amt1.number())), -9.35, places=0)

    def testQueryDisplayAmounts_no_exchange(self):
        """Test that display_amounts without -X returns the raw amounts."""
        journal = read_journal_from_string("""
2023/01/01 * Test
    Expenses:Food    $10.00
    Assets:Cash
""")
        posts = journal.query("food")
        self.assertEqual(len(posts), 1)

        # Without -X, display_amounts should equal posting.amount
        display_amts = posts.display_amounts
        self.assertEqual(len(display_amts), 1)
        self.assertEqual(display_amts[0].to_amount(), Amount("$10.00"))

    def testQueryDisplayAmounts_market(self):
        """Test that display_amounts reflects -X EUR market valuation on USD amounts."""
        journal = read_journal_from_string("""
commodity USD
commodity EUR

P 2023/01/01 EUR 1.07 USD

2023/01/01 * Test
    Assets:USDAccount   107.00 USD
    Equity:Initial
""")
        posts = journal.query("-X EUR --no-rounding --no-revalued Assets")
        self.assertEqual(len(posts), 1)

        # Raw amount is in USD
        self.assertEqual(posts[0].amount.commodity.symbol, "USD")

        # display_amounts with -X EUR should give the EUR value
        display_amts = posts.display_amounts
        self.assertEqual(len(display_amts), 1)
        amt = display_amts[0].to_amount()
        self.assertEqual(amt.commodity.symbol, "EUR")
        # 107.00 USD / 1.07 USD per EUR = 100.00 EUR
        self.assertAlmostEqual(float(str(amt.number())), 100.00, places=0)


def suite():
    return unittest.TestLoader().loadTestsFromTestCase(JournalTestCase)

if __name__ == '__main__':
    unittest.main()
