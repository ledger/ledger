# -*- coding: utf-8 -*-

import unittest
import sys
sys.path.append(".")

from ledger import *

class JournalTestCase(unittest.TestCase):
    # NOTE: tearDown is disabled since it crashes with a segmentation fault
    def DISABLED_tearDown(self):
        session.close_journal_files()

    # NOTE: This test is disabled since it crashes with a segmentation fault
    def DISABLED_testCloseJournal(self):
        journal = read_journal_from_string("""
2012-03-01 KFC
    Expenses:Food      $21.34
    Assets:Cash
""")
        # This line currently causes a segmentation fault
        session.close_journal_files()
        # if we get to here we can safely assume the bug is fixed
        self.assertEqual(True, True)

    def testBasicRead(self):
        # TODO: There is a bug when reading multiple journal data
        #       results in the journal data to be mixed up.
        #       Closing the journal may help, but that currently
        #       results in a segmentation fault
        journal = read_journal_from_string("""
2012-03-01 * (2) KFC
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
