# -*- coding: utf-8 -*-

import unittest

from ledger import *

class JournalTestCase(unittest.TestCase):
    def tearDown(self):
        session.close_journal_files()

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
 
def suite():
    return unittest.TestLoader().loadTestsFromTestCase(JournalTestCase)

if __name__ == '__main__':
    unittest.main()
