from unittest import TextTestRunner, TestSuite

import tests.python.numerics.t_amount as t_amount

suites = [
    t_amount.suite(),
]

TextTestRunner().run(TestSuite(suites))
