#!/usr/bin/env python3

import unittest

from ledger import *

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

def suite():
    return unittest.TestLoader().loadTestsFromTestCase(JournalTestCase)

if __name__ == '__main__':
    unittest.main()
