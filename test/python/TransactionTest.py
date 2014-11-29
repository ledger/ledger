# -*- coding: utf-8 -*-

import unittest
import exceptions
import operator
import sys
sys.path.append(".")

from ledger import *
from StringIO import *
from datetime import *

class TransactionTestCase(unittest.TestCase):
    def setUp(self):
        # TODO: There is a bug when reading multiple journal data
        #       results in the journal data to be mixed up.
        #       Closing the journal may help, but that currently
        #       results in a segmentation fault
        journal = read_journal_from_string("""
;2012-03-01 * (2) KFC
;    Expenses:Food      $21.34
;    Assets:Cash
""")
        self.xact = journal.xacts().next()

    def tearDown(self):
        pass

    def testType(self):
        self.assertEqual(type(self.xact), Transaction)

    def testDate(self):
        self.assertEqual(self.xact.date, date(2012, 3, 1))

    def testPayee(self):
        self.assertEqual(self.xact.payee, "KFC")

    def testCode(self):
        self.assertEqual(self.xact.code, "2")

    def testToString(self):
        # NOTE: There is a TODO in src/py_xact.cc to change
        # the string representation of a Transaction to something
        # more meaningful than an empty string. What should it be?
        self.assertEqual(str(self.xact), "")

def suite():
    return unittest.TestLoader().loadTestsFromTestCase(TransactionTestCase)

if __name__ == '__main__':
    unittest.main()
