from unittest import TextTestRunner, TestSuite

import tests.python.corelib.numerics.BasicAmount as BasicAmount
import tests.python.corelib.numerics.CommodityAmount as CommodityAmount

suites = [
    BasicAmount.suite(),
    CommodityAmount.suite(),
]

TextTestRunner().run(TestSuite(suites))
