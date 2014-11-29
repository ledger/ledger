# -*- coding: utf-8 -*-

import unittest
import exceptions
import operator

from ledger import *
from StringIO import *
from datetime import *

class PostingTestCase(unittest.TestCase):
    def setUp(self):
        journal = read_journal_from_string("""
2012-03-01 * (2) KFC
    Expenses:Food      $21.34
    Assets:Cash
""")
        self.posting = journal.query("food")[0]

    def tearDown(self):
        pass

    def testType(self):
        self.assertEqual(type(self.posting), Posting)

    def testDate(self):
        self.assertEqual(self.posting.date, date(2012, 3, 1))

    def testAccountName(self):
        self.assertEqual(self.posting.account.name, 'Food')
        self.assertEqual(str(self.posting.account), 'Expenses:Food')
 
    def testAccountFullame(self):
        self.assertEqual(self.posting.account.fullname(), 'Expenses:Food')
 
    def testAccountParent(self):
        self.assertEqual(self.posting.account.parent.name, 'Expenses')
        self.assertEqual(str(self.posting.account.parent), 'Expenses')
 
    def testAmount(self):
        self.assertEqual(self.posting.amount, '$21.34')
 
def suite():
    return unittest.TestLoader().loadTestsFromTestCase(PostingTestCase)

if __name__ == '__main__':
    unittest.main()
