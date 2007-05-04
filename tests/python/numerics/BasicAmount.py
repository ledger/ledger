import sys

import unittest
import exceptions

from ledger import amount

class BasicAmountTestCase(unittest.TestCase):
    def testConstructors(self):
        x0 = amount()
        x1 = amount(123456)
        x2 = amount(123456L)
        x3 = amount(123.456)
        x5 = amount("123456")
        x6 = amount("123.456")
        x9 = amount(x3)
        x10 = amount(x6)

        self.assertEqual(amount(0), x0)
        self.assertEqual(x2, x1)
        self.assertEqual(x5, x1)
        self.assertEqual(x6, x3)
        self.assertEqual(x10, x3)
        self.assertEqual(x10, x9)

        self.assertTrue(x0.valid())
        self.assertTrue(x1.valid())
        self.assertTrue(x2.valid())
        self.assertTrue(x3.valid())
        self.assertTrue(x5.valid())
        self.assertTrue(x6.valid())
        self.assertTrue(x9.valid())
        self.assertTrue(x10.valid())

    def testNegation(self):
        x0 = amount()
        x1 = amount(-123456)
        x3 = amount(-123.456)
        x5 = amount("-123456")
        x6 = amount("-123.456")
        x9 = amount(- x3)

        self.assertEqual(amount(0), x0)
        self.assertEqual(x5, x1)
        self.assertEqual(x6, x3)
        self.assertEqual(- x6, x9)
        self.assertEqual(x3.negate(), x9)

        x10 = amount(x9.negate())

        self.assertEqual(x3, x10)

        self.assertTrue(x0.valid())
        self.assertTrue(x1.valid())
        self.assertTrue(x3.valid())
        self.assertTrue(x5.valid())
        self.assertTrue(x6.valid())
        self.assertTrue(x9.valid())
        self.assertTrue(x10.valid())

    def testAssignment(self):
        x0 = amount()
        x1  = amount(123456)
        x2  = amount(123456L)
        x3  = amount(123.456)
        x5  = amount("123456")
        x6  = amount("123.456")
        x9  = x3
        x10 = amount(x6)

        self.assertEqual(amount(0), x0)
        self.assertEqual(x2, x1)
        self.assertEqual(x5, x1)
        self.assertEqual(x6, x3)
        self.assertEqual(x10, x3)
        self.assertEqual(x10, x9)

        x0  = amount()
        x1  = amount(123456)
        x2  = amount(123456L)
        x3  = amount(123.456)
        x5  = amount("123456")
        x6  = amount("123.456")
        x9  = x3
        x10 = amount(x6)

        self.assertEqual(amount(0), x0)
        self.assertEqual(x2, x1)
        self.assertEqual(x5, x1)
        self.assertEqual(x6, x3)
        self.assertEqual(x10, x3)
        self.assertEqual(x10, x9)

        self.assertTrue(x0.valid())
        self.assertTrue(x1.valid())
        self.assertTrue(x2.valid())
        self.assertTrue(x3.valid())
        self.assertTrue(x5.valid())
        self.assertTrue(x6.valid())
        self.assertTrue(x9.valid())
        self.assertTrue(x10.valid())

    def testEquality(self):
        x1 = amount(123456)
        x2 = amount(456789)
        x3 = amount(333333)
        x4 = amount(123456.0)
        x5 = amount("123456.0")

        self.assertTrue(x1 == 123456)
        self.assertTrue(x1 != x2)
        self.assertTrue(x1 == (x2 - x3))
        self.assertTrue(x1 == x4)
        self.assertTrue(x4 == x5)

        self.assertTrue(x1.valid())
        self.assertTrue(x2.valid())
        self.assertTrue(x3.valid())
        self.assertTrue(x4.valid())
        self.assertTrue(x5.valid())

    def testIntegerAddition(self):
        x1 = amount(123)
        y1 = amount(456)

        self.assertEqual(amount(579), x1 + y1)
        self.assertEqual(amount(579), x1 + 456)
        self.assertEqual(amount(579), 456 + x1)

        x1 += amount(456)
        self.assertEqual(amount(579), x1)
        x1 += 456
        self.assertEqual(amount(1035), x1)

        x4 = amount("123456789123456789123456789")

        self.assertEqual(amount("246913578246913578246913578"), x4 + x4)

        self.assertTrue(x1.valid())
        self.assertTrue(y1.valid())
        self.assertTrue(x4.valid())

    def testFractionalAddition(self):
        x1 = amount(123.123)
        y1 = amount(456.456)

        self.assertEqual(amount(579.579), x1 + y1)
        self.assertEqual(amount(579.579), x1 + 456.456)
        self.assertEqual(amount(579.579), 456.456 + x1)

        x1 += amount(456.456)
        self.assertEqual(amount(579.579), x1)
        x1 += 456.456
        self.assertEqual(amount(1036.035), x1)
        x1 += 456
        self.assertEqual(amount(1492.035), x1)

        x2 = amount("123456789123456789.123456789123456789")

        self.assertEqual(amount("246913578246913578.246913578246913578"), x2 + x2)

        self.assertTrue(x1.valid())
        self.assertTrue(y1.valid())
        self.assertTrue(x2.valid())

    def testIntegerSubtraction(self):
        x1 = amount(123)
        y1 = amount(456)

        self.assertEqual(amount(333), y1 - x1)
        self.assertEqual(amount(-333), x1 - y1)
        self.assertEqual(amount(23), x1 - 100)
        self.assertEqual(amount(-23), 100 - x1)

        x1 -= amount(456)
        self.assertEqual(amount(-333), x1)
        x1 -= 456
        self.assertEqual(amount(-789), x1)

        x4 = amount("123456789123456789123456789")
        y4 = amount("8238725986235986")

        self.assertEqual(amount("123456789115218063137220803"), x4 - y4)
        self.assertEqual(amount("-123456789115218063137220803"), y4 - x4)

        self.assertTrue(x1.valid())
        self.assertTrue(y1.valid())
        self.assertTrue(x4.valid())
        self.assertTrue(y4.valid())

    def testFractionalSubtraction(self):
        x1 = amount(123.123)
        y1 = amount(456.456)

        self.assertEqual(amount(-333.333), x1 - y1)
        self.assertEqual(amount(333.333), y1 - x1)

        x1 -= amount(456.456)
        self.assertEqual(amount(-333.333), x1)
        x1 -= 456.456
        self.assertEqual(amount(-789.789), x1)
        x1 -= 456
        self.assertEqual(amount(-1245.789), x1)

        x2 = amount("123456789123456789.123456789123456789")
        y2 = amount("9872345982459.248974239578")

        self.assertEqual(amount("123446916777474329.874482549545456789"), x2 - y2)
        self.assertEqual(amount("-123446916777474329.874482549545456789"), y2 - x2)

        self.assertTrue(x1.valid())
        self.assertTrue(y1.valid())
        self.assertTrue(x2.valid())
        self.assertTrue(y2.valid())

    def testIntegerMultiplication(self):
        x1 = amount(123)
        y1 = amount(456)

        self.assertEqual(amount(0), x1 * 0)
        self.assertEqual(amount(0), amount(0) * x1)
        self.assertEqual(amount(0), 0 * x1)
        self.assertEqual(x1, x1 * 1)
        self.assertEqual(x1, amount(1) * x1)
        self.assertEqual(x1, 1 * x1)
        self.assertEqual(- x1, x1 * -1)
        self.assertEqual(- x1, amount(-1) * x1)
        self.assertEqual(- x1, -1 * x1)
        self.assertEqual(amount(56088), x1 * y1)
        self.assertEqual(amount(56088), y1 * x1)
        self.assertEqual(amount(56088), x1 * 456)
        self.assertEqual(amount(56088), amount(456) * x1)
        self.assertEqual(amount(56088), 456 * x1)

        x1 *= amount(123)
        self.assertEqual(amount(15129), x1)
        x1 *= 123
        self.assertEqual(amount(1860867), x1)

        x4 = amount("123456789123456789123456789")

        self.assertEqual(amount("15241578780673678546105778281054720515622620750190521"),
                    x4 * x4)

        self.assertTrue(x1.valid())
        self.assertTrue(y1.valid())
        self.assertTrue(x4.valid())

    def testFractionalMultiplication(self):
        x1 = amount(123.123)
        y1 = amount(456.456)

        self.assertEqual(amount(0), x1 * 0)
        self.assertEqual(amount(0), amount(0) * x1)
        self.assertEqual(amount(0), 0 * x1)
        self.assertEqual(x1, x1 * 1)
        self.assertEqual(x1, amount(1) * x1)
        self.assertEqual(x1, 1 * x1)
        self.assertEqual(- x1, x1 * -1)
        self.assertEqual(- x1, amount(-1) * x1)
        self.assertEqual(- x1, -1 * x1)
        self.assertEqual(amount("56200.232088"), x1 * y1)
        self.assertEqual(amount("56200.232088"), y1 * x1)
        self.assertEqual(amount("56200.232088"), x1 * 456.456)
        self.assertEqual(amount("56200.232088"), amount(456.456) * x1)
        self.assertEqual(amount("56200.232088"), 456.456 * x1)

        x1 *= amount(123.123)
        self.assertEqual(amount("15159.273129"), x1)
        x1 *= 123.123
        self.assertEqual(amount("1866455.185461867"), x1)
        x1 *= 123
        self.assertEqual(amount("229573987.811809641"), x1)

        x2 = amount("123456789123456789.123456789123456789")

        self.assertEqual(amount("15241578780673678546105778311537878.046486820281054720515622620750190521"),
                    x2 * x2)

        self.assertTrue(x1.valid())
        self.assertTrue(y1.valid())
        self.assertTrue(x2.valid())

    def divideByZero(self, amt):
        return amt / 0

    def testIntegerDivision(self):
        x1 = amount(123)
        y1 = amount(456)

        self.assertRaises(exceptions.ArithmeticError, self.divideByZero, x1)
        self.assertEqual(amount(0), amount(0) / x1)
        self.assertEqual(amount(0), 0 / x1)
        self.assertEqual(x1, x1 / 1)
        self.assertEqual(amount("0.008130"), amount(1) / x1)
        self.assertEqual(amount("0.008130"), 1 / x1)
        self.assertEqual(- x1, x1 / -1)
        self.assertEqual(- amount("0.008130"), amount(-1) / x1)
        self.assertEqual(- amount("0.008130"), -1 / x1)
        self.assertEqual(amount("0.269737"), x1 / y1)
        self.assertEqual(amount("3.707317"), y1 / x1)
        self.assertEqual(amount("0.269737"), x1 / 456)
        self.assertEqual(amount("3.707317"), amount(456) / x1)
        self.assertEqual(amount("3.707317"), 456 / x1)

        x1 /= amount(456)
        self.assertEqual(amount("0.269737"), x1)
        x1 /= 456
        self.assertEqual(amount("0.00059152850877193"), x1)

        x4 = amount("123456789123456789123456789")
        y4 = amount("56")

        self.assertEqual(amount(1), x4 / x4)
        self.assertEqual(amount("2204585520061728377204585.517857"), x4 / y4)

        self.assertTrue(x1.valid())
        self.assertTrue(y1.valid())
        self.assertTrue(x4.valid())
        self.assertTrue(y4.valid())

    def testFractionalDivision(self):
        x1 = amount(123.123)
        y1 = amount(456.456)

        self.assertRaises(exceptions.ArithmeticError, self.divideByZero, x1)
        self.assertEqual(amount("0.008121959"), amount(1.0) / x1)
        self.assertEqual(amount("0.008121959"), 1.0 / x1)
        self.assertEqual(x1, x1 / 1.0)
        self.assertEqual(amount("0.008121959"), amount(1.0) / x1)
        self.assertEqual(amount("0.008121959"), 1.0 / x1)
        self.assertEqual(- x1, x1 / -1.0)
        self.assertEqual(- amount("0.008121959"), amount(-1.0) / x1)
        self.assertEqual(- amount("0.008121959"), -1.0 / x1)
        self.assertEqual(amount("0.269736842105263"), x1 / y1)
        self.assertEqual(amount("3.707317073170732"), y1 / x1)
        self.assertEqual(amount("0.269736842105263"), x1 / 456.456)
        self.assertEqual(amount("3.707317073170732"), amount(456.456) / x1)
        self.assertEqual(amount("3.707317073170732"), 456.456 / x1)

        x1 /= amount(456.456)
        self.assertEqual(amount("0.269736842105263"), x1)
        x1 /= 456.456
        self.assertEqual(amount("0.000590937225286255411255411255411255411"), x1)
        x1 /= 456
        self.assertEqual(amount("0.000001295914967733016252753094858358016252192982456140350877192982456140350877192982"), x1)

        x4 = amount("1234567891234567.89123456789")
        y4 = amount("56.789")

        self.assertEqual(amount(1.0), x4 / x4)
        self.assertEqual(amount("21739560323910.7554497273748437197344556164046"),
                         x4 / y4)

        self.assertTrue(x1.valid())
        self.assertTrue(y1.valid())
        self.assertTrue(x4.valid())
        self.assertTrue(y4.valid())

    def testIntegerConversion(self):
        x1 = amount(123456)

        self.assertTrue(x1)
        self.assertEqual(123456, int(x1))
        self.assertEqual(123456.0, float(x1))
        self.assertEqual("123456", x1.to_string())
        self.assertEqual("123456", x1.quantity_string())

        self.assertTrue(x1.valid())

    def testFractionalConversion(self):
        x1 = amount(1234.56)

        self.assertTrue(x1)
        self.assertEqual(1234, int(x1))
        self.assertEqual(1234.56, float(x1))
        self.assertEqual("1234.56", x1.to_string())
        self.assertEqual("1234.56", x1.quantity_string())

        self.assertTrue(x1.valid())

    def testFractionalRound(self):
        x1 = amount("1234.567890")

        self.assertEqual(amount("1234.56789"), x1.round(6))
        self.assertEqual(amount("1234.56789"), x1.round(5))
        self.assertEqual(amount("1234.5679"), x1.round(4))
        self.assertEqual(amount("1234.568"), x1.round(3))
        self.assertEqual(amount("1234.57"), x1.round(2))
        self.assertEqual(amount("1234.6"), x1.round(1))
        self.assertEqual(amount("1235"), x1.round(0))

        x2 = amount("9876.543210")

        self.assertEqual(amount("9876.543210"), x2.round(6))
        self.assertEqual(amount("9876.54321"), x2.round(5))
        self.assertEqual(amount("9876.5432"), x2.round(4))
        self.assertEqual(amount("9876.543"), x2.round(3))
        self.assertEqual(amount("9876.54"), x2.round(2))
        self.assertEqual(amount("9876.5"), x2.round(1))
        self.assertEqual(amount("9877"), x2.round(0))

        x3 = amount("-1234.567890")

        self.assertEqual(amount("-1234.56789"), x3.round(6))
        self.assertEqual(amount("-1234.56789"), x3.round(5))
        self.assertEqual(amount("-1234.5679"), x3.round(4))
        self.assertEqual(amount("-1234.568"), x3.round(3))
        self.assertEqual(amount("-1234.57"), x3.round(2))
        self.assertEqual(amount("-1234.6"), x3.round(1))
        self.assertEqual(amount("-1235"), x3.round(0))

        x4 = amount("-9876.543210")

        self.assertEqual(amount("-9876.543210"), x4.round(6))
        self.assertEqual(amount("-9876.54321"), x4.round(5))
        self.assertEqual(amount("-9876.5432"), x4.round(4))
        self.assertEqual(amount("-9876.543"), x4.round(3))
        self.assertEqual(amount("-9876.54"), x4.round(2))
        self.assertEqual(amount("-9876.5"), x4.round(1))
        self.assertEqual(amount("-9877"), x4.round(0))

        self.assertTrue(x1.valid())
        self.assertTrue(x2.valid())
        self.assertTrue(x3.valid())
        self.assertTrue(x4.valid())

    def testTruth(self):
        x0 = amount()
        x1 = amount("1234")
        x2 = amount("1234.56")

        self.assertFalse(x0)
        self.assertTrue(x1)
        self.assertTrue(x2)

        self.assertTrue(x0.valid())
        self.assertTrue(x1.valid())
        self.assertTrue(x2.valid())

    def testForZero(self):
        x0 = amount()
        x1 = amount("0.000000000000000000001")

        self.assertFalse(x0)
        self.assertTrue(x1)
        self.assertTrue(x0.zero())
        self.assertTrue(x0.realzero())
        self.assertFalse(x1.zero())
        self.assertFalse(x1.realzero())

        self.assertTrue(x0.valid())
        self.assertTrue(x1.valid())

    def testComparisons(self):
        x0 = amount()
        x1 = amount(-123)
        x2 = amount(123)
        x3 = amount(-123.45)
        x4 = amount(123.45)
        x5 = amount("-123.45")
        x6 = amount("123.45")

        self.assertTrue(x0 > x1)
        self.assertTrue(x0 < x2)
        self.assertTrue(x0 > x3)
        self.assertTrue(x0 < x4)
        self.assertTrue(x0 > x5)
        self.assertTrue(x0 < x6)

        self.assertTrue(x1 > x3)
        self.assertTrue(x3 <= x5)
        self.assertTrue(x3 >= x5)
        self.assertTrue(x3 < x1)
        self.assertTrue(x3 < x4)

        self.assertTrue(x0.valid())
        self.assertTrue(x1.valid())
        self.assertTrue(x2.valid())
        self.assertTrue(x3.valid())
        self.assertTrue(x4.valid())
        self.assertTrue(x5.valid())
        self.assertTrue(x6.valid())

    def testSign(self):
        x0 = amount()
        x1 = amount("0.0000000000000000000000000000000000001")
        x2 = amount("-0.0000000000000000000000000000000000001")
        x3 = amount("1")
        x4 = amount("-1")

        self.assertEqual(x0.sign(), 0)
        self.assertTrue(x1.sign() > 0)
        self.assertTrue(x2.sign() < 0)
        self.assertTrue(x3.sign() > 0)
        self.assertTrue(x4.sign() < 0)

        self.assertTrue(x0.valid())
        self.assertTrue(x1.valid())
        self.assertTrue(x2.valid())
        self.assertTrue(x3.valid())
        self.assertTrue(x4.valid())

    def testAbs(self):
        x0 = amount()
        x1 = amount(-1234)
        x2 = amount(1234)

        self.assertEqual(amount(), abs(x0))
        self.assertEqual(amount(1234), abs(x1))
        self.assertEqual(amount(1234), abs(x2))

        self.assertTrue(x0.valid())
        self.assertTrue(x1.valid())
        self.assertTrue(x2.valid())

    def testPrinting(self):
        pass


def suite():
    return unittest.TestLoader().loadTestsFromTestCase(BasicAmountTestCase)

if __name__ == '__main__':
    unittest.main()
