import unittest
import exceptions

from ledger import amount

class BasicAmountTestCase(unittest.TestCase):
    def testConstructors(self):
        x0 = amount()
        x1 = amount(123456L)
        x2 = amount(123456)
        x4 = amount(True)
        x5 = amount("123456")
        x6 = amount("123.456")
        x10 = amount(x6)

        self.assertEqual(amount(0L), x0)
        self.assertEqual(x2, x1)
        self.assertEqual(x5, x1)
        self.assertEqual(x10, x6)
        self.assertEqual(amount(1L), x4)

    def testNegation(self):
        x0 = amount()
        x1 = amount(-123456L)
        x5 = amount("-123456")
        x6 = amount("-123.456")
        x9 = amount(- x6)

        self.assertEqual(amount(0L), x0)
        self.assertEqual(x5, x1)
        self.assertEqual(- x6, x9)
        self.assertEqual(x6.negated(), x9)

        x10 = amount(x9)
        x10.negate()

        self.assertEqual(x6, x10)

    def testAssignment(self):
        x0  = amount()
        x1  = amount(123456L)
        x2  = amount(123456)
        x4  = amount(True)
        x5  = amount("123456")
        x6  = amount("123.456")
        x9  = x6
        x10 = amount(x6)

        self.assertEqual(amount(0L), x0)
        self.assertEqual(x2, x1)
        self.assertEqual(x5, x1)
        self.assertEqual(x10, x6)
        self.assertEqual(amount(1L), x4)
        self.assertEqual(x10, x9)

        x0  = amount()
        x1  = amount(123456L)
        x2  = amount(123456)
        x4  = amount(True)
        x5  = amount("123456")
        x6  = amount("123.456")
        x9  = x6
        x10 = amount(x6)

        self.assertEqual(amount(0L), x0)
        self.assertEqual(x2, x1)
        self.assertEqual(x5, x1)
        self.assertEqual(x10, x6)
        self.assertEqual(amount(1L), x4)
        self.assertEqual(x10, x9)

    def testEquality(self):
        x1 = amount(123456L)
        x2 = amount(456789L)
        x3 = amount(333333L)
        x5 = amount("123456.0")

        self.assertTrue(x1 == 123456L)
        self.assertTrue(x1 != x2)
        self.assertTrue(x1 == (x2 - x3))
        self.assertTrue(x1 == x5)

    def testIntegerAddition(self):
        x1 = amount(123L)
        y1 = amount(456L)

        self.assertEqual(amount(579L), x1 + y1)
        self.assertEqual(amount(579L), x1 + 456L)
        self.assertEqual(amount(579L), 456L + x1)

        x1 += amount(456L)
        self.assertEqual(amount(579L), x1)
        x1 += 456L
        self.assertEqual(amount(1035L), x1)

        x3 = amount(True)
        y3 = amount(True)

        self.assertEqual(amount(2L), x3 + y3)
        self.assertEqual(amount(2L), x3 + True)

        x4 = amount("123456789123456789123456789")

        self.assertEqual(amount("246913578246913578246913578"), x4 + x4)

    def testFractionalAddition(self):
        x1 = amount("123.123")
        y1 = amount("456.456")

        self.assertEqual(amount("579.579"), x1 + y1)
        self.assertEqual(amount("579.579"), x1 + amount("456.456"))
        self.assertEqual(amount("579.579"), amount("456.456") + x1)

        x1 += amount("456.456")
        self.assertEqual(amount("579.579"), x1)
        x1 += amount("456.456")
        self.assertEqual(amount("1036.035"), x1)
        x1 += 456L
        self.assertEqual(amount("1492.035"), x1)

        x2 = amount("123456789123456789.123456789123456789")

        self.assertEqual(amount("246913578246913578.246913578246913578"), x2 + x2)

    def testIntegerSubtraction(self):
        x1 = amount(123L)
        y1 = amount(456L)

        self.assertEqual(amount(333L), y1 - x1)
        self.assertEqual(amount(-333L), x1 - y1)
        self.assertEqual(amount(23L), x1 - 100L)
        self.assertEqual(amount(-23L), 100L - x1)

        x1 -= amount(456L)
        self.assertEqual(amount(-333L), x1)
        x1 -= 456L
        self.assertEqual(amount(-789L), x1)

        x3 = amount(True)
        y3 = amount(True)

        self.assertEqual(amount(False), x3 - y3)

        x4 = amount("123456789123456789123456789")
        y4 = amount("8238725986235986")

        self.assertEqual(amount("123456789115218063137220803"), x4 - y4)
        self.assertEqual(amount("-123456789115218063137220803"), y4 - x4)

    def testFractionalSubtraction(self):
        x1 = amount("123.123")
        y1 = amount("456.456")

        self.assertEqual(amount("-333.333"), x1 - y1)
        self.assertEqual(amount("333.333"), y1 - x1)

        x1 -= amount("456.456")
        self.assertEqual(amount("-333.333"), x1)
        x1 -= amount("456.456")
        self.assertEqual(amount("-789.789"), x1)
        x1 -= 456L
        self.assertEqual(amount("-1245.789"), x1)

        x2 = amount("123456789123456789.123456789123456789")
        y2 = amount("9872345982459.248974239578")

        self.assertEqual(amount("123446916777474329.874482549545456789"), x2 - y2)
        self.assertEqual(amount("-123446916777474329.874482549545456789"), y2 - x2)

    def testIntegerMultiplication(self):
        x1 = amount(123L)
        y1 = amount(456L)

        self.assertEqual(amount(0L), x1 * 0L)
        self.assertEqual(amount(0L), amount(0L) * x1)
        self.assertEqual(amount(0L), 0L * x1)
        self.assertEqual(x1, x1 * 1L)
        self.assertEqual(x1, amount(1L) * x1)
        self.assertEqual(x1, 1L * x1)
        self.assertEqual(- x1, x1 * -1L)
        self.assertEqual(- x1, amount(-1L) * x1)
        self.assertEqual(- x1, -1L * x1)
        self.assertEqual(amount(56088L), x1 * y1)
        self.assertEqual(amount(56088L), y1 * x1)
        self.assertEqual(amount(56088L), x1 * 456L)
        self.assertEqual(amount(56088L), amount(456L) * x1)
        self.assertEqual(amount(56088L), 456L * x1)

        x1 *= amount(123L)
        self.assertEqual(amount(15129L), x1)
        x1 *= 123L
        self.assertEqual(amount(1860867L), x1)

        x3 = amount(True)
        y3 = amount(True)

        self.assertEqual(amount(True), x3 * y3)

        x4 = amount("123456789123456789123456789")

        self.assertEqual(amount("15241578780673678546105778281054720515622620750190521"),
                         x4 * x4)

    def testFractionalMultiplication(self):
        x1 = amount("123.123")
        y1 = amount("456.456")

        self.assertEqual(amount(0L), x1 * 0L)
        self.assertEqual(amount(0L), amount(0L) * x1)
        self.assertEqual(amount(0L), 0L * x1)
        self.assertEqual(x1, x1 * 1L)
        self.assertEqual(x1, amount(1L) * x1)
        self.assertEqual(x1, 1L * x1)
        self.assertEqual(- x1, x1 * -1L)
        self.assertEqual(- x1, amount(-1L) * x1)
        self.assertEqual(- x1, -1L * x1)
        self.assertEqual(amount("56200.232088"), x1 * y1)
        self.assertEqual(amount("56200.232088"), y1 * x1)
        self.assertEqual(amount("56200.232088"), x1 * amount("456.456"))
        self.assertEqual(amount("56200.232088"), amount("456.456") * x1)

        x1 *= amount("123.123")
        self.assertEqual(amount("15159.273129"), x1)
        x1 *= amount("123.123")
        self.assertEqual(amount("1866455.185461867"), x1)
        x1 *= 123L
        self.assertEqual(amount("229573987.811809641"), x1)

        x2 = amount("123456789123456789.123456789123456789")

        self.assertEqual(amount("15241578780673678546105778311537878.046486820281054720515622620750190521"),
                         x2 * x2)

    def divideByZero(self, amt):
        return amt / 0

    def testIntegerDivision(self):
        x1 = amount(123L)
        y1 = amount(456L)

        self.assertRaises(exceptions.ArithmeticError, self.divideByZero, x1)
        self.assertEqual(amount(0L), amount(0L) / x1)
        self.assertEqual(amount(0L), 0L / x1)
        self.assertEqual(x1, x1 / 1L)
        self.assertEqual(amount("0.008130"), amount(1L) / x1)
        self.assertEqual(amount("0.008130"), 1L / x1)
        self.assertEqual(- x1, x1 / -1L)
        self.assertEqual(- amount("0.008130"), amount(-1L) / x1)
        self.assertEqual(- amount("0.008130"), -1L / x1)
        self.assertEqual(amount("0.269736"), x1 / y1)
        self.assertEqual(amount("3.707317"), y1 / x1)
        self.assertEqual(amount("0.269736"), x1 / 456L)
        self.assertEqual(amount("3.707317"), amount(456L) / x1)
        self.assertEqual(amount("3.707317"), 456L / x1)

        x1 /= amount(456L)
        self.assertEqual(amount("0.269736"), x1)
        x1 /= 456L
        self.assertEqual(amount("0.000591526315789473"), x1)

        x4 = amount("123456789123456789123456789")
        y4 = amount("56")

        self.assertEqual(amount(1L), x4 / x4)
        self.assertEqual(amount("2204585520061728377204585.517857"), x4 / y4)

    def testFractionalDivision(self):
        x1 = amount("123.123")
        y1 = amount("456.456")

        self.assertRaises(exceptions.ArithmeticError, self.divideByZero, x1)
        self.assertEqual(amount("0.00812195"), amount("1.0") / x1)
        self.assertEqual(x1, x1 / amount("1.0"))
        self.assertEqual(amount("0.00812195"), amount("1.0") / x1)
        self.assertEqual(- x1, x1 / amount("-1.0"))
        self.assertEqual(- amount("0.00812195"), amount("-1.0") / x1)
        self.assertEqual(amount("0.269736842105"), x1 / y1)
        self.assertEqual(amount("3.707317073170"), y1 / x1)
        self.assertEqual(amount("0.269736842105"), x1 / amount("456.456"))
        self.assertEqual(amount("3.707317073170"), amount("456.456") / x1)

        x1 /= amount("456.456")
        self.assertEqual(amount("0.269736842105"), x1)
        x1 /= amount("456.456")
        self.assertEqual(amount("0.0005909372252856792330476541"), x1)
        x1 /= 456L
        self.assertEqual(amount("0.00000129591496773175270405187302631578947368421052631578947368421"), x1)

        x4 = amount("1234567891234567.89123456789")
        y4 = amount("56.789")

        self.assertEqual(amount("1.0"), x4 / x4)
        self.assertEqual(amount("21739560323910.7554497273748437197344556164"),
                         x4 / y4)

def suite():
    return unittest.TestLoader().loadTestsFromTestCase(BasicAmountTestCase)

if __name__ == '__main__':
    unittest.main()
