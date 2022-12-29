# -*- coding: utf-8 -*-

import unittest
import operator

from ledger import *
from datetime import *

class TransactionTestCase(unittest.TestCase):
    def setUp(self):
        self.journal = read_journal_from_string("""
2012-03-01 KFC
    Expenses:Food      $21.34
    Assets:Cash
2012-03-02 MJT
    Expenses:Museum      $45.67
    Assets:Cash
""")
        
    def tearDown(self):
        close_journal_files()

    def testAddRemovePosts(self):
        xacts = [xact for xact in self.journal]
        x1_posts = [post for post in xacts[1]]
        for post in x1_posts:
            xacts[0].add_post(post)
            xacts[1].remove_post(post)
        x0_posts = [post for post in xacts[0]]
        x1_posts = [post for post in xacts[1]]
        self.assertEqual(len(x0_posts), 4)
        self.assertEqual(len(x1_posts), 0)

def suite():
    return unittest.TestLoader().loadTestsFromTestCase(TransactionTestCase)

if __name__ == '__main__':
    unittest.main()
