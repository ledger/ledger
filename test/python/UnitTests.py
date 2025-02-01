from unittest import TextTestRunner, TestSuite

import JournalTest
import TransactionTest
import PostingTest
import ValueTest

suites = [
    JournalTest.suite(),
    TransactionTest.suite(),
    PostingTest.suite(),
    ValueTest.suite()
]
TextTestRunner().run(TestSuite(suites))
