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

 
def suite():
    return unittest.TestLoader().loadTestsFromTestCase(JournalTestCase)

if __name__ == '__main__':
    unittest.main()
