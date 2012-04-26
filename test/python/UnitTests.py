from unittest import TextTestRunner, TestSuite

import JournalTest
import TransactionTest
import PostingTest

suites = [
    JournalTest.suite(),
    TransactionTest.suite(),
    PostingTest.suite()
]
TextTestRunner().run(TestSuite(suites))
