from unittest import TextTestRunner, TestSuite

import JournalTest
import TransactionTest
import PostingTest
import sys
sys.path.append(".")

suites = [
    JournalTest.suite(),
    TransactionTest.suite(),
    PostingTest.suite()
]
TextTestRunner().run(TestSuite(suites))
