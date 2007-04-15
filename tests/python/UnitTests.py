from unittest import TextTestRunner, TestSuite

import tests.python.corelib.numerics.BasicAmount as BasicAmount

suites = [
    BasicAmount.suite(),
]

TextTestRunner().run(TestSuite(suites))
