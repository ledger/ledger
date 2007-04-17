# -*- coding: utf-8 -*-

import unittest
import exceptions
import operator

from ledger import amount

internalAmount = amount.exact


class CommodityAmountTestCase(unittest.TestCase):
    def setUp(self):
        # Cause the display precision for dollars to be initialized to 2.
        x1 = amount("$1.00")
        self.assertTrue(x1)
        amount.full_strings = True # makes error reports from UnitTests accurate

    def tearDown(self):
        amount.full_strings = False

    def assertValid(self, amt):
        self.assertTrue(amt.valid())

    def testConstructors(self):
        x1  = amount("$123.45")
        x2  = amount("-$123.45")
        x3  = amount("$-123.45")
        x4  = amount("DM 123.45")
        x5  = amount("-DM 123.45")
        x6  = amount("DM -123.45")
        x7  = amount("123.45 euro")
        x8  = amount("-123.45 euro")
        x9  = amount("123.45€")
        x10 = amount("-123.45€")

        self.assertEqual(amount("$123.45"), x1)
        self.assertEqual(amount("-$123.45"), x2)
        self.assertEqual(amount("$-123.45"), x3)
        self.assertEqual(amount("DM 123.45"), x4)
        self.assertEqual(amount("-DM 123.45"), x5)
        self.assertEqual(amount("DM -123.45"), x6)
        self.assertEqual(amount("123.45 euro"), x7)
        self.assertEqual(amount("-123.45 euro"), x8)
        self.assertEqual(amount("123.45€"), x9)
        self.assertEqual(amount("-123.45€"), x10)

        self.assertEqual("$123.45", x1.to_string())
        self.assertEqual("$-123.45", x2.to_string())
        self.assertEqual("$-123.45", x3.to_string())
        self.assertEqual("DM 123.45", x4.to_string())
        self.assertEqual("DM -123.45", x5.to_string())
        self.assertEqual("DM -123.45", x6.to_string())
        self.assertEqual("123.45 euro", x7.to_string())
        self.assertEqual("-123.45 euro", x8.to_string())
        self.assertEqual("123.45€", x9.to_string())
        self.assertEqual("-123.45€", x10.to_string())

        self.assertValid(x1)
        self.assertValid(x2)
        self.assertValid(x3)
        self.assertValid(x4)
        self.assertValid(x5)
        self.assertValid(x6)
        self.assertValid(x7)
        self.assertValid(x8)
        self.assertValid(x9)
        self.assertValid(x10)

    def testNegation(self):
        x1 = amount("$123.45")
        x2 = amount("-$123.45")
        x3 = amount("$-123.45")
        x4 = amount("DM 123.45")
        x5 = amount("-DM 123.45")
        x6 = amount("DM -123.45")
        x7 = amount("123.45 euro")
        x8 = amount("-123.45 euro")
        x9 = amount("123.45€")
        x10 = amount("-123.45€")

        self.assertEqual(amount("$-123.45"), - x1)
        self.assertEqual(amount("$123.45"), - x2)
        self.assertEqual(amount("$123.45"), - x3)
        self.assertEqual(amount("DM -123.45"), - x4)
        self.assertEqual(amount("DM 123.45"), - x5)
        self.assertEqual(amount("DM 123.45"), - x6)
        self.assertEqual(amount("-123.45 euro"), - x7)
        self.assertEqual(amount("123.45 euro"), - x8)
        self.assertEqual(amount("-123.45€"), - x9)
        self.assertEqual(amount("123.45€"), - x10)

        self.assertEqual(amount("$-123.45"), x1.negated())
        self.assertEqual(amount("$123.45"), x2.negated())
        self.assertEqual(amount("$123.45"), x3.negated())

        self.assertEqual("$-123.45", (- x1).to_string())
        self.assertEqual("$123.45", (- x2).to_string())
        self.assertEqual("$123.45", (- x3).to_string())
        self.assertEqual("DM -123.45", (- x4).to_string())
        self.assertEqual("DM 123.45", (- x5).to_string())
        self.assertEqual("DM 123.45", (- x6).to_string())
        self.assertEqual("-123.45 euro", (- x7).to_string())
        self.assertEqual("123.45 euro", (- x8).to_string())
        self.assertEqual("-123.45€", (- x9).to_string())
        self.assertEqual("123.45€", (- x10).to_string())

        x1.negate()
        x2.negate()
        x3.negate()

        self.assertEqual(amount("$-123.45"), x1)
        self.assertEqual(amount("$123.45"), x2)
        self.assertEqual(amount("$123.45"), x3)

        self.assertValid(x1)
        self.assertValid(x2)
        self.assertValid(x3)
        self.assertValid(x4)
        self.assertValid(x5)
        self.assertValid(x6)
        self.assertValid(x7)
        self.assertValid(x8)
        self.assertValid(x9)
        self.assertValid(x10)

    def testAssignment(self):
        x1 = amount("$123.45")
        x2 = amount("-$123.45")
        x3 = amount("$-123.45")
        x4 = amount("DM 123.45")
        x5 = amount("-DM 123.45")
        x6 = amount("DM -123.45")
        x7 = amount("123.45 euro")
        x8 = amount("-123.45 euro")
        x9 = amount("123.45€")
        x10 = amount("-123.45€")

        self.assertEqual(amount("$123.45"), x1)
        self.assertEqual(amount("-$123.45"), x2)
        self.assertEqual(amount("$-123.45"), x3)
        self.assertEqual(amount("DM 123.45"), x4)
        self.assertEqual(amount("-DM 123.45"), x5)
        self.assertEqual(amount("DM -123.45"), x6)
        self.assertEqual(amount("123.45 euro"), x7)
        self.assertEqual(amount("-123.45 euro"), x8)
        self.assertEqual(amount("123.45€"), x9)
        self.assertEqual(amount("-123.45€"), x10)

        self.assertEqual("$123.45", x1.to_string())
        self.assertEqual("$-123.45", x2.to_string())
        self.assertEqual("$-123.45", x3.to_string())
        self.assertEqual("DM 123.45", x4.to_string())
        self.assertEqual("DM -123.45", x5.to_string())
        self.assertEqual("DM -123.45", x6.to_string())
        self.assertEqual("123.45 euro", x7.to_string())
        self.assertEqual("-123.45 euro", x8.to_string())
        self.assertEqual("123.45€", x9.to_string())
        self.assertEqual("-123.45€", x10.to_string())

        self.assertValid(x1)
        self.assertValid(x2)
        self.assertValid(x3)
        self.assertValid(x4)
        self.assertValid(x5)
        self.assertValid(x6)
        self.assertValid(x7)
        self.assertValid(x8)
        self.assertValid(x9)
        self.assertValid(x10)

    def testEquality(self):
        x0 = amount()
        x1 = amount("$123.45")
        x2 = amount("-$123.45")
        x3 = amount("$-123.45")
        x4 = amount("DM 123.45")
        x5 = amount("-DM 123.45")
        x6 = amount("DM -123.45")
        x7 = amount("123.45 euro")
        x8 = amount("-123.45 euro")
        x9 = amount("123.45€")
        x10 = amount("-123.45€")

        self.assertTrue(x0.null())
        self.assertTrue(x0.zero())
        self.assertTrue(x0.realzero())
        self.assertTrue(x0.sign() == 0)
        self.assertTrue(x0.compare(x1) < 0)
        self.assertTrue(x0.compare(x2) > 0)
        self.assertTrue(x0.compare(x0) == 0)

        self.assertTrue(x1 != x2)
        self.assertTrue(x1 != x4)
        self.assertTrue(x1 != x7)
        self.assertTrue(x1 != x9)
        self.assertTrue(x2 == x3)
        self.assertTrue(x4 != x5)
        self.assertTrue(x5 == x6)
        self.assertTrue(x7 == - x8)
        self.assertTrue(x9 == - x10)

        self.assertValid(x0)
        self.assertValid(x1)
        self.assertValid(x2)
        self.assertValid(x3)
        self.assertValid(x4)
        self.assertValid(x5)
        self.assertValid(x6)
        self.assertValid(x7)
        self.assertValid(x8)
        self.assertValid(x9)
        self.assertValid(x10)

    def testAddition(self):
        x0 = amount()
        x1 = amount("$123.45")
        x2 = amount(internalAmount("$123.456789"))
        x3 = amount("DM 123.45")
        x4 = amount("123.45 euro")
        x5 = amount("123.45€")
        x6 = amount("123.45")

        self.assertEqual(amount("$246.90"), x1 + x1)
        self.assertNotEqual(amount("$246.91"), x1 + x2)
        self.assertEqual(internalAmount("$246.906789"), x1 + x2)

        # Converting to string drops internal precision
        self.assertEqual("$246.90", (x1 + x1).to_string())
        self.assertEqual("$246.91", (x1 + x2).to_string())

        self.assertRaises(exceptions.ArithmeticError, operator.add, x1, x0)
        self.assertRaises(exceptions.ArithmeticError, operator.add, x1, x3)
        self.assertRaises(exceptions.ArithmeticError, operator.add, x1, x4)
        self.assertRaises(exceptions.ArithmeticError, operator.add, x1, x5)
        self.assertRaises(exceptions.ArithmeticError, operator.add, x1, x6)
        self.assertRaises(exceptions.ArithmeticError, operator.add, x1, 123.45)
        self.assertRaises(exceptions.ArithmeticError, operator.add, x1, 123)

        self.assertEqual(amount("DM 246.90"), x3 + x3)
        self.assertEqual(amount("246.90 euro"), x4 + x4)
        self.assertEqual(amount("246.90€"), x5 + x5)

        self.assertEqual("DM 246.90", (x3 + x3).to_string())
        self.assertEqual("246.90 euro", (x4 + x4).to_string())
        self.assertEqual("246.90€", (x5 + x5).to_string())

        x1 += amount("$456.45")
        self.assertEqual(amount("$579.90"), x1)
        x1 += amount("$456.45")
        self.assertEqual(amount("$1036.35"), x1)
        x1 += amount("$456")
        self.assertEqual(amount("$1492.35"), x1)

        x7 = amount(internalAmount("$123456789123456789.123456789123456789"))

        self.assertEqual(internalAmount("$246913578246913578.246913578246913578"), x7 + x7)

        self.assertValid(x1)
        self.assertValid(x2)
        self.assertValid(x3)
        self.assertValid(x4)
        self.assertValid(x5)
        self.assertValid(x6)
        self.assertValid(x7)

    def testSubtraction(self):
        x0 = amount()
        x1 = amount("$123.45")
        x2 = amount(internalAmount("$123.456789"))
        x3 = amount("DM 123.45")
        x4 = amount("123.45 euro")
        x5 = amount("123.45€")
        x6 = amount("123.45")

        self.assertNotEqual(amount(), x1 - x1)
        self.assertEqual(amount("$0"), x1 - x1)
        self.assertEqual(amount("$23.45"), x1 - amount("$100.00"))
        self.assertEqual(amount("$-23.45"), amount("$100.00") - x1)
        self.assertNotEqual(amount("$-0.01"), x1 - x2)
        self.assertEqual(internalAmount("$-0.006789"), x1 - x2)

        # Converting to string drops internal precision.  If an amount is
        # zero, it drops the commodity as well.
        self.assertEqual("$0.00", (x1 - x1).to_string())
        self.assertEqual("$-0.01", (x1 - x2).to_string())

        self.assertRaises(exceptions.ArithmeticError, operator.sub, x1, x0)
        self.assertRaises(exceptions.ArithmeticError, operator.sub, x1, x3)
        self.assertRaises(exceptions.ArithmeticError, operator.sub, x1, x4)
        self.assertRaises(exceptions.ArithmeticError, operator.sub, x1, x5)
        self.assertRaises(exceptions.ArithmeticError, operator.sub, x1, x6)
        self.assertRaises(exceptions.ArithmeticError, operator.sub, x1, 123.45)
        self.assertRaises(exceptions.ArithmeticError, operator.sub, x1, 123)

        self.assertEqual(amount("DM 0.00"), x3 - x3)
        self.assertEqual(amount("DM 23.45"), x3 - amount("DM 100.00"))
        self.assertEqual(amount("DM -23.45"), amount("DM 100.00") - x3)
        self.assertEqual(amount("0.00 euro"), x4 - x4)
        self.assertEqual(amount("23.45 euro"), x4 - amount("100.00 euro"))
        self.assertEqual(amount("-23.45 euro"), amount("100.00 euro") - x4)
        self.assertEqual(amount("0.00€"), x5 - x5)
        self.assertEqual(amount("23.45€"), x5 - amount("100.00€"))
        self.assertEqual(amount("-23.45€"), amount("100.00€") - x5)

        self.assertEqual("DM 0.00", (x3 - x3).to_string())
        self.assertEqual("DM 23.45", (x3 - amount("DM 100.00")).to_string())
        self.assertEqual("DM -23.45", (amount("DM 100.00") - x3).to_string())
        self.assertEqual("0.00 euro", (x4 - x4).to_string())
        self.assertEqual("23.45 euro", (x4 - amount("100.00 euro")).to_string())
        self.assertEqual("-23.45 euro", (amount("100.00 euro") - x4).to_string())
        self.assertEqual("0.00€", (x5 - x5).to_string())
        self.assertEqual("23.45€", (x5 - amount("100.00€")).to_string())
        self.assertEqual("-23.45€", (amount("100.00€") - x5).to_string())

        x1 -= amount("$456.45")
        self.assertEqual(amount("$-333.00"), x1)
        x1 -= amount("$456.45")
        self.assertEqual(amount("$-789.45"), x1)
        x1 -= amount("$456")
        self.assertEqual(amount("$-1245.45"), x1)

        x7 = amount(internalAmount("$123456789123456789.123456789123456789"))
        x8 = amount(internalAmount("$2354974984698.98459845984598"))

        self.assertEqual(internalAmount("$123454434148472090.138858329277476789"), x7 - x8)
        self.assertEqual("$123454434148472090.138858329277476789", (x7 - x8).to_string())
        self.assertEqual("$123454434148472090.14",
	      (amount("$1.00") * (x7 - x8)).to_string())
        self.assertEqual(internalAmount("$-123454434148472090.138858329277476789"), x8 - x7)
        self.assertEqual("$-123454434148472090.138858329277476789", (x8 - x7).to_string())
        self.assertEqual("$-123454434148472090.14",
	      (amount("$1.00") * (x8 - x7)).to_string())

        self.assertValid(x1)
        self.assertValid(x2)
        self.assertValid(x3)
        self.assertValid(x4)
        self.assertValid(x5)
        self.assertValid(x6)
        self.assertValid(x7)
        self.assertValid(x8)

    def testMultiplication(self):
        x1 = amount("$123.12")
        y1 = amount("$456.45")
        x2 = amount(internalAmount("$123.456789"))
        x3 = amount("DM 123.45")
        x4 = amount("123.45 euro")
        x5 = amount("123.45€")

        self.assertEqual(amount("$0.00"), x1 * 0)
        self.assertEqual(amount("$0.00"), 0 * x1)
        self.assertEqual(x1, x1 * 1)
        self.assertEqual(x1, 1 * x1)
        self.assertEqual(- x1, x1 * -1)
        self.assertEqual(- x1, -1 * x1)
        self.assertEqual(internalAmount("$56198.124"), x1 * y1)
        self.assertEqual("$56198.12", (x1 * y1).to_string())
        self.assertEqual(internalAmount("$56198.124"), y1 * x1)
        self.assertEqual("$56198.12", (y1 * x1).to_string())

        # Internal amounts retain their precision, even when being
        # converted to strings
        self.assertEqual(internalAmount("$15199.99986168"), x1 * x2)
        self.assertEqual(internalAmount("$15199.99986168"), x2 * x1)
        self.assertEqual("$15200.00", (x1 * x2).to_string())
        self.assertEqual("$15199.99986168", (x2 * x1).to_string())

        self.assertRaises(exceptions.ArithmeticError, operator.mul, x1, x3)
        self.assertRaises(exceptions.ArithmeticError, operator.mul, x1, x4)
        self.assertRaises(exceptions.ArithmeticError, operator.mul, x1, x5)

        x1 *= amount("123.12")
        self.assertEqual(internalAmount("$15158.5344"), x1)
        self.assertEqual("$15158.53", x1.to_string())
        x1 *= 123.12
        self.assertEqual(internalAmount("$1866318.755328"), x1)
        self.assertEqual("$1866318.76", x1.to_string())
        x1 *= 123
        self.assertEqual(internalAmount("$229557206.905344"), x1)
        self.assertEqual("$229557206.91", x1.to_string())

        x7 = amount(internalAmount("$123456789123456789.123456789123456789"))

        self.assertEqual(internalAmount("$15241578780673678546105778311537878.046486820281054720515622620750190521"),
	      x7 * x7)

        self.assertValid(x1)
        self.assertValid(x2)
        self.assertValid(x3)
        self.assertValid(x4)
        self.assertValid(x5)
        self.assertValid(x7)

    def testDivision(self):
        x1 = amount("$123.12")
        y1 = amount("$456.45")
        x2 = amount(internalAmount("$123.456789"))
        x3 = amount("DM 123.45")
        x4 = amount("123.45 euro")
        x5 = amount("123.45€")

        self.assertRaises(exceptions.ArithmeticError, operator.div, x1, 0)
        self.assertEqual(amount("$0.00"), 0 / x1)
        self.assertEqual(x1, x1 / 1)
        self.assertEqual(internalAmount("$0.00812216"), 1 / x1)
        self.assertEqual(- x1, x1 / -1)
        self.assertEqual(internalAmount("$-0.00812216"), -1 / x1)
        self.assertEqual(internalAmount("$0.26973382"), x1 / y1)
        self.assertEqual("$0.27", (x1 / y1).to_string())
        self.assertEqual(internalAmount("$3.70735867"), y1 / x1)
        self.assertEqual("$3.71", (y1 / x1).to_string())

        # Internal amounts retain their precision, even when being
        # converted to strings
        self.assertEqual(internalAmount("$0.99727201"), x1 / x2)
        self.assertEqual(internalAmount("$1.00273545321637426901"), x2 / x1)
        self.assertEqual("$1.00", (x1 / x2).to_string())
        self.assertEqual("$1.00273545321637426901", (x2 / x1).to_string())

        self.assertRaises(exceptions.ArithmeticError, operator.div, x1, x3)
        self.assertRaises(exceptions.ArithmeticError, operator.div, x1, x4)
        self.assertRaises(exceptions.ArithmeticError, operator.div, x1, x5)

        x1 /= amount("123.12")
        self.assertEqual(internalAmount("$1.00"), x1)
        self.assertEqual("$1.00", x1.to_string())
        x1 /= 123.12
        self.assertEqual(internalAmount("$0.00812216"), x1)
        self.assertEqual("$0.01", x1.to_string())
        x1 /= 123
        self.assertEqual(internalAmount("$0.00006603"), x1)
        self.assertEqual("$0.00", x1.to_string())

        x6 = amount(internalAmount("$237235987235987.98723987235978"))
        x7 = amount(internalAmount("$123456789123456789.123456789123456789"))

        self.assertEqual(amount("$1"), x7 / x7)
        self.assertEqual(internalAmount("$0.0019216115121765559608381226612019501046413574469262"),
	      x6 / x7)
        self.assertEqual(internalAmount("$520.39654928343335571379527154924040947271699678158689736256"),
	      x7 / x6)

        self.assertValid(x1)
        self.assertValid(x2)
        self.assertValid(x3)
        self.assertValid(x4)
        self.assertValid(x5)
        self.assertValid(x6)
        self.assertValid(x7)

    def testConversion(self):
        x1 = amount("$1234.56")

        self.assertEqual(True, bool(x1))
        self.assertEqual(1234, int(x1))
        self.assertEqual(1234.56, float(x1))
        self.assertEqual("$1234.56", x1.to_string())
        self.assertEqual("1234.56", x1.quantity_string())

        self.assertValid(x1)

    def testRound(self):
        x1 = amount(internalAmount("$1234.567890"))

        self.assertEqual(internalAmount("$1234.56789"), x1.round(6))
        self.assertEqual(internalAmount("$1234.56789"), x1.round(5))
        self.assertEqual(internalAmount("$1234.5679"), x1.round(4))
        self.assertEqual(internalAmount("$1234.568"), x1.round(3))
        self.assertEqual(amount("$1234.57"), x1.round(2))
        self.assertEqual(amount("$1234.6"), x1.round(1))
        self.assertEqual(amount("$1235"), x1.round(0))

        x2 = amount(internalAmount("$9876.543210"))

        self.assertEqual(internalAmount("$9876.543210"), x2.round(6))
        self.assertEqual(internalAmount("$9876.54321"), x2.round(5))
        self.assertEqual(internalAmount("$9876.5432"), x2.round(4))
        self.assertEqual(internalAmount("$9876.543"), x2.round(3))
        self.assertEqual(amount("$9876.54"), x2.round(2))
        self.assertEqual(amount("$9876.5"), x2.round(1))
        self.assertEqual(amount("$9877"), x2.round(0))

        x3 = amount(internalAmount("$-1234.567890"))

        self.assertEqual(internalAmount("$-1234.56789"), x3.round(6))
        self.assertEqual(internalAmount("$-1234.56789"), x3.round(5))
        self.assertEqual(internalAmount("$-1234.5679"), x3.round(4))
        self.assertEqual(internalAmount("$-1234.568"), x3.round(3))
        self.assertEqual(amount("$-1234.57"), x3.round(2))
        self.assertEqual(amount("$-1234.6"), x3.round(1))
        self.assertEqual(amount("$-1235"), x3.round(0))

        x4 = amount(internalAmount("$-9876.543210"))

        self.assertEqual(internalAmount("$-9876.543210"), x4.round(6))
        self.assertEqual(internalAmount("$-9876.54321"), x4.round(5))
        self.assertEqual(internalAmount("$-9876.5432"), x4.round(4))
        self.assertEqual(internalAmount("$-9876.543"), x4.round(3))
        self.assertEqual(amount("$-9876.54"), x4.round(2))
        self.assertEqual(amount("$-9876.5"), x4.round(1))
        self.assertEqual(amount("$-9877"), x4.round(0))

        self.assertValid(x1)
        self.assertValid(x2)
        self.assertValid(x3)
        self.assertValid(x4)

    def testDisplayRound(self):
        x1 = amount("$0.85")
        x2 = amount("$0.1")

        x1 *= 0.19

        self.assertNotEqual(amount("$0.16"), x1)
        self.assertEqual(internalAmount("$0.1615"), x1)
        self.assertEqual("$0.16", x1.to_string())

        self.assertEqual(amount("$0.10"), x2)
        self.assertNotEqual(internalAmount("$0.101"), x2)
        self.assertEqual("$0.10", x2.to_string())

        x1 *= 7

        self.assertNotEqual(amount("$1.13"), x1)
        self.assertEqual(internalAmount("$1.1305"), x1)
        self.assertEqual("$1.13", x1.to_string())

    def testTruth(self):
        x1 = amount("$1234")
        x2 = amount("$1234.56")

        if x1:
            self.assertTrue(True)
        else:
            self.assertTrue(False)

        if x2:
            self.assertTrue(True)
        else:
            self.assertTrue(False)

        self.assertValid(x1)
        self.assertValid(x2)

    def testForZero(self):
        x1 = amount(internalAmount("$0.000000000000000000001"))

        self.assertFalse(x1)
        self.assertTrue(x1.zero())
        self.assertFalse(x1.realzero())

        self.assertValid(x1)

    def testComparisons(self):
        x0 = amount()
        x1 = amount("$-123")
        x2 = amount("$123.00")
        x3 = amount(internalAmount("$-123.4544"))
        x4 = amount(internalAmount("$123.4544"))
        x5 = amount("$-123.45")
        x6 = amount("$123.45")

        self.assertTrue(x0 > x1)
        self.assertTrue(x0 < x2)
        self.assertTrue(x0 > x3)
        self.assertTrue(x0 < x4)
        self.assertTrue(x0 > x5)
        self.assertTrue(x0 < x6)

        self.assertTrue(x1 > x3)
        self.assertTrue(x3 <= x5)
        self.assertTrue(x3 < x5)
        self.assertTrue(x3 <= x5)
        self.assertFalse(x3 == x5)
        self.assertTrue(x3 < x1)
        self.assertTrue(x3 < x4)

        self.assertValid(x0)
        self.assertValid(x1)
        self.assertValid(x2)
        self.assertValid(x3)
        self.assertValid(x4)
        self.assertValid(x5)
        self.assertValid(x6)

    def testSign(self):
        x0 = amount()
        x1 = amount(internalAmount("$0.0000000000000000000000000000000000001"))
        x2 = amount(internalAmount("$-0.0000000000000000000000000000000000001"))
        x3 = amount("$1")
        x4 = amount("$-1")

        self.assertFalse(x0.sign())
        self.assertTrue(x1.sign() != 0)
        self.assertTrue(x2.sign() != 0)
        self.assertTrue(x3.sign() > 0)
        self.assertTrue(x4.sign() < 0)

        self.assertValid(x0)
        self.assertValid(x1)
        self.assertValid(x2)
        self.assertValid(x3)
        self.assertValid(x4)

    def testAbs(self):
        x0 = amount()
        x1 = amount("$-1234.56")
        x2 = amount("$1234.56")

        self.assertEqual(amount(), abs(x0))
        self.assertEqual(amount("$1234.56"), abs(x1))
        self.assertEqual(amount("$1234.56"), abs(x2))

        x0.abs()
        x1.abs()
        x2.abs()

        self.assertEqual(amount(), x0)
        self.assertEqual(amount("$1234.56"), x1)
        self.assertEqual(amount("$1234.56"), x2)

        self.assertValid(x0)
        self.assertValid(x1)
        self.assertValid(x2)

    def testPrinting(self):
        pass


def suite():
    return unittest.TestLoader().loadTestsFromTestCase(CommodityAmountTestCase)

if __name__ == '__main__':
    unittest.main()
