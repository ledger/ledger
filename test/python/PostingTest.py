#!/usr/bin/env python3

import unittest
import operator

from ledger import *
from datetime import *

class PostingTestCase(unittest.TestCase):
    def setUp(self):
        pass

    def tearDown(self):
        close_journal_files()

    def test_(self):
        pass
 
def suite():
    return unittest.TestLoader().loadTestsFromTestCase(PostingTestCase)

if __name__ == '__main__':
    unittest.main()
