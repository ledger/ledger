from unittest import TextTestRunner, TestSuite

import test.python.math.t_amount as t_amount

suites = [
    t_amount.suite(),
]

TextTestRunner().run(TestSuite(suites))
