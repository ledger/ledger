from unittest import TextTestRunner, TestSuite

import tests.python.numerics.BasicAmount as BasicAmount
import tests.python.numerics.CommodityAmount as CommodityAmount

suites = [
    BasicAmount.suite(),
    CommodityAmount.suite(),
]

TextTestRunner().run(TestSuite(suites))
