# -*- coding: utf-8 -*-

import unittest
import exceptions
import operator

from ledger import *
from StringIO import *

internalAmount = amount.exact

class t_amountTestCase(unittest.TestCase):
    testSession = None

    def assertValid(self, amt):
        self.assertTrue(amt.valid())

    def setUp(self):
        #self.testSession = session()
        #set_session_context(self.testSession)

        # Cause the display precision for dollars to be initialized to 2.
        x1 = amount("$1.00")
        self.assertTrue(x1)

        amount.stream_fullstrings = True # make reports from UnitTests accurate

    def tearDown(self):
        amount.stream_fullstrings = False

        #set_session_context()
        #self.testSession = None

    def testParser(self):
        x0 = amount()
        x1 = amount()
        x2 = amount()
        x3 = amount()
        x4 = amount("123.456")
        x5 = amount(x4)
        x6 = amount(x4)
        x7 = amount(x4)
        x8 = amount("$123.45")
        x9 = amount(x8)
        x10 = amount(x8)
        x11 = amount(x8)
        x12 = amount("$100")

        self.assertEqual(2, x12.commodity.precision)

        #buf = "$100..."
        #input = StringIO(buf)
        #x13 = amount()
        #x13.parse(input)
        #self.assertEqual(x12, x13)

        x14 = amount()
        self.assertRaises(exceptions.ArithmeticError, lambda: x14.parse("DM"))

        x15 = amount("$1.000.000,00")

        x16 = amount("$2000")
        self.assertEqual("$2.000,00", x16.to_string())
        x16.parse("$2000,00")
        self.assertEqual("$2.000,00", x16.to_string())

        # Since European-ness is an additive quality, we must switch back
        # to American-ness manually
        x15.commodity.drop_flags(COMMODITY_STYLE_EUROPEAN)

        x17 = amount("$1,000,000.00")

        x18 = amount("$2000")
        self.assertEqual("$2,000.00", x18.to_string())
        x18.parse("$2,000")
        self.assertEqual("$2,000.00", x18.to_string())

        self.assertEqual(x15, x17)

        x19 = amount("EUR 1000")
        x20 = amount("EUR 1000")

        self.assertEqual("EUR 1000", x19.to_string())
        self.assertEqual("EUR 1000", x20.to_string())

        x1.parse("$100.0000", AMOUNT_PARSE_NO_MIGRATE)
        self.assertEqual(2, x12.commodity.precision)
        self.assertEqual(x1.commodity, x12.commodity)
        self.assertEqual(x1, x12)

        x0.parse("$100.0000")
        self.assertEqual(4, x12.commodity.precision)
        self.assertEqual(x0.commodity, x12.commodity)
        self.assertEqual(x0, x12)

        x2.parse("$100.00", AMOUNT_PARSE_NO_REDUCE)
        self.assertEqual(x2, x12)
        x3.parse("$100.00", AMOUNT_PARSE_NO_MIGRATE | AMOUNT_PARSE_NO_REDUCE)
        self.assertEqual(x3, x12)

        x4.parse("$100.00")
        self.assertEqual(x4, x12)
        x5.parse("$100.00", AMOUNT_PARSE_NO_MIGRATE)
        self.assertEqual(x5, x12)
        x6.parse("$100.00", AMOUNT_PARSE_NO_REDUCE)
        self.assertEqual(x6, x12)
        x7.parse("$100.00", AMOUNT_PARSE_NO_MIGRATE | AMOUNT_PARSE_NO_REDUCE)
        self.assertEqual(x7, x12)

        x8.parse("$100.00")
        self.assertEqual(x8, x12)
        x9.parse("$100.00", AMOUNT_PARSE_NO_MIGRATE)
        self.assertEqual(x9, x12)
        x10.parse("$100.00", AMOUNT_PARSE_NO_REDUCE)
        self.assertEqual(x10, x12)
        x11.parse("$100.00", AMOUNT_PARSE_NO_MIGRATE | AMOUNT_PARSE_NO_REDUCE)
        self.assertEqual(x11, x12)

        self.assertValid(x0)
        self.assertValid(x1)
        self.assertValid(x2)
        self.assertValid(x3)
        self.assertValid(x5)
        self.assertValid(x6)
        self.assertValid(x7)
        self.assertValid(x8)
        self.assertValid(x9)
        self.assertValid(x10)
        self.assertValid(x11)
        self.assertValid(x12)

    def testConstructors(self):
        x0 = amount()
        x1 = amount(123456)
        x2 = amount(123456L)
        x3 = amount("123.456")
        x5 = amount("123456")
        x6 = amount("123.456")
        x7 = amount("123456")
        x8 = amount("123.456")
        x9 = amount(x3)
        x10 = amount(x6)
        x11 = amount(x8)

        self.assertRaises(exceptions.ArithmeticError, lambda: amount(0) == x0)
        self.assertRaises(exceptions.ArithmeticError, lambda: amount() == x0)
        self.assertRaises(exceptions.ArithmeticError, lambda: amount("0") == x0)
        self.assertRaises(exceptions.ArithmeticError, lambda: amount("0.0") == x0)
        self.assertEqual(x2, x1)
        self.assertEqual(x5, x1)
        self.assertEqual(x7, x1)
        self.assertEqual(x6, x3)
        self.assertEqual(x8, x3)
        self.assertEqual(x10, x3)
        self.assertEqual(x10, x9)

        self.assertValid(x0)
        self.assertValid(x1)
        self.assertValid(x2)
        self.assertValid(x3)
        self.assertValid(x5)
        self.assertValid(x6)
        self.assertValid(x7)
        self.assertValid(x8)
        self.assertValid(x9)
        self.assertValid(x10)
        self.assertValid(x11)

    def testCommodityConstructors(self):
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

    def testAssignment(self):
        x0 = amount()
        x1 = amount(123456)
        x2 = amount(123456L)
        x3 = amount("123.456")
        x5 = amount("123456")
        x6 = amount("123.456")
        x7 = "123456"
        x8 = "123.456"
        x9 = amount(x3)
        x10 = amount(x6)

        self.assertEqual(x2, x1)
        self.assertEqual(x5, x1)
        self.assertEqual(x7, x1)
        self.assertEqual(x6, x3)
        self.assertEqual(x8, x3)
        self.assertEqual(x10, x3)
        self.assertEqual(x10, x9)

        x1  = amount(123456)
        x2  = amount(123456L)
        x3  = amount("123.456")
        x5  = amount("123456")
        x6  = amount("123.456")
        x7  = amount("123456")
        x8  = amount("123.456")
        x9  = x3
        x10 = amount(x6)

        self.assertEqual(x2, x1)
        self.assertEqual(x5, x1)
        self.assertEqual(x7, x1)
        self.assertEqual(x6, x3)
        self.assertEqual(x8, x3)
        self.assertEqual(x10, x3)
        self.assertEqual(x10, x9)

        self.assertFalse(x1.is_null())
        x1 = x0			# sets x1 back to uninitialized state
        self.assertTrue(x0.is_null())
        self.assertTrue(x1.is_null())

        self.assertValid(x0)
        self.assertValid(x1)
        self.assertValid(x2)
        self.assertValid(x3)
        self.assertValid(x5)
        self.assertValid(x6)
        self.assertValid(x7)
        self.assertValid(x8)
        self.assertValid(x9)
        self.assertValid(x10)

    def testCommodityAssignment(self):
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
        x1 = amount(123456)
        x2 = amount(456789)
        x3 = amount(333333)
        x4 = amount("123456.0")
        x5 = amount("123456.0")
        x6 = amount("123456.0")

        self.assertTrue(x1 == 123456)
        self.assertTrue(x1 != x2)
        self.assertTrue(x1 == (x2 - x3))
        self.assertTrue(x1 == x4)
        self.assertTrue(x4 == x5)
        self.assertTrue(x4 == x6)

        self.assertTrue(x1 == 123456)
        self.assertTrue(123456 == x1)
        self.assertTrue(x1 == 123456L)
        self.assertTrue(123456L == x1)
        self.assertTrue(x1 == amount("123456.0"))
        self.assertTrue(amount("123456.0") == x1)

        self.assertValid(x1)
        self.assertValid(x2)
        self.assertValid(x3)
        self.assertValid(x4)
        self.assertValid(x5)
        self.assertValid(x6)

    def testCommodityEquality(self):
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

        self.assertTrue(x0.is_null())
        self.assertRaises(exceptions.ArithmeticError, lambda: x0.is_zero())
        self.assertRaises(exceptions.ArithmeticError, lambda: x0.is_realzero())
        self.assertRaises(exceptions.ArithmeticError, lambda: x0.sign() == 0)
        self.assertRaises(exceptions.ArithmeticError, lambda: x0.compare(x1) < 0)
        self.assertRaises(exceptions.ArithmeticError, lambda: x0.compare(x2) > 0)
        self.assertRaises(exceptions.ArithmeticError, lambda: x0.compare(x0) == 0)

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

    def testComparisons(self):
        x0 = amount()
        x1 = amount(-123)
        x2 = amount(123)
        x3 = amount("-123.45")
        x4 = amount("123.45")
        x5 = amount("-123.45")
        x6 = amount("123.45")

        self.assertRaises(exceptions.ArithmeticError, lambda: x0 > x1)
        self.assertRaises(exceptions.ArithmeticError, lambda: x0 < x2)
        self.assertRaises(exceptions.ArithmeticError, lambda: x0 > x3)
        self.assertRaises(exceptions.ArithmeticError, lambda: x0 < x4)
        self.assertRaises(exceptions.ArithmeticError, lambda: x0 > x5)
        self.assertRaises(exceptions.ArithmeticError, lambda: x0 < x6)

        self.assertTrue(x1 > x3)
        self.assertTrue(x3 <= x5)
        self.assertTrue(x3 >= x5)
        self.assertTrue(x3 < x1)
        self.assertTrue(x3 < x4)

        self.assertTrue(x1 < 100)
        self.assertTrue(100 > x1)
        self.assertTrue(x1 < 100L)
        self.assertTrue(100L > x1)
        self.assertTrue(x1 < amount("100.0"))
        self.assertTrue(amount("100.0") > x1)

        self.assertValid(x0)
        self.assertValid(x1)
        self.assertValid(x2)
        self.assertValid(x3)
        self.assertValid(x4)
        self.assertValid(x5)
        self.assertValid(x6)

    def testCommodityComparisons(self):
        x1 = amount("$-123")
        x2 = amount("$123.00")
        x3 = amount(internalAmount("$-123.4544"))
        x4 = amount(internalAmount("$123.4544"))
        x5 = amount("$-123.45")
        x6 = amount("$123.45")
        x7 = amount("DM 123.45")

        self.assertTrue(x1 > x3)
        self.assertTrue(x3 <= x5)
        self.assertTrue(x3 < x5)
        self.assertTrue(x3 <= x5)
        self.assertFalse(x3 == x5)
        self.assertTrue(x3 < x1)
        self.assertTrue(x3 < x4)
        self.assertFalse(x6 == x7)
        self.assertRaises(exceptions.ArithmeticError, lambda: x6 < x7)

        self.assertValid(x1)
        self.assertValid(x2)
        self.assertValid(x3)
        self.assertValid(x4)
        self.assertValid(x5)
        self.assertValid(x6)

    def testIntegerAddition(self):
        x0 = amount()
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

        self.assertValid(x0)
        self.assertValid(x1)
        self.assertValid(y1)
        self.assertValid(x4)

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
        x1 += 456
        self.assertEqual(amount("1492.035"), x1)

        x2 = amount("123456789123456789.123456789123456789")

        self.assertEqual(amount("246913578246913578.246913578246913578"), x2 + x2)

        self.assertValid(x1)
        self.assertValid(y1)
        self.assertValid(x2)

    def testCommodityAddition(self):
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

        self.assertRaises(exceptions.ArithmeticError, lambda: x1 + x0)
        self.assertRaises(exceptions.ArithmeticError, lambda: x0 + x1)
        self.assertRaises(exceptions.ArithmeticError, lambda: x0 + x0)
        self.assertRaises(exceptions.ArithmeticError, lambda: x1 + x3)
        self.assertRaises(exceptions.ArithmeticError, lambda: x1 + x4)
        self.assertRaises(exceptions.ArithmeticError, lambda: x1 + x5)
        self.assertRaises(exceptions.ArithmeticError, lambda: x1 + x6)
        self.assertRaises(exceptions.ArithmeticError, lambda: x1 + amount("123.45"))
        self.assertRaises(exceptions.ArithmeticError, lambda: x1 + 123)

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

        self.assertValid(x1)
        self.assertValid(y1)
        self.assertValid(x4)
        self.assertValid(y4)

    def testFractionalSubtraction(self):
        x1 = amount("123.123")
        y1 = amount("456.456")

        self.assertEqual(amount("-333.333"), x1 - y1)
        self.assertEqual(amount("333.333"), y1 - x1)

        x1 -= amount("456.456")
        self.assertEqual(amount("-333.333"), x1)
        x1 -= amount("456.456")
        self.assertEqual(amount("-789.789"), x1)
        x1 -= 456
        self.assertEqual(amount("-1245.789"), x1)

        x2 = amount("123456789123456789.123456789123456789")
        y2 = amount("9872345982459.248974239578")

        self.assertEqual(amount("123446916777474329.874482549545456789"), x2 - y2)
        self.assertEqual(amount("-123446916777474329.874482549545456789"), y2 - x2)

        self.assertValid(x1)
        self.assertValid(y1)
        self.assertValid(x2)
        self.assertValid(y2)

    def testCommoditySubtraction(self):
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

        self.assertRaises(exceptions.ArithmeticError, lambda: x1 - x0)
        self.assertRaises(exceptions.ArithmeticError, lambda: x0 - x1)
        self.assertRaises(exceptions.ArithmeticError, lambda: x0 - x0)
        self.assertRaises(exceptions.ArithmeticError, lambda: x1 - x3)
        self.assertRaises(exceptions.ArithmeticError, lambda: x1 - x4)
        self.assertRaises(exceptions.ArithmeticError, lambda: x1 - x5)
        self.assertRaises(exceptions.ArithmeticError, lambda: x1 - x6)
        self.assertRaises(exceptions.ArithmeticError, lambda: x1 - amount("123.45"))
        self.assertRaises(exceptions.ArithmeticError, lambda: x1 - 123)

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

        self.assertValid(x1)
        self.assertValid(y1)
        self.assertValid(x4)

    def testFractionalMultiplication(self):
        x1 = amount("123.123")
        y1 = amount("456.456")

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
        self.assertEqual(amount("56200.232088"), x1 * amount("456.456"))
        self.assertEqual(amount("56200.232088"), amount("456.456") * x1)
        self.assertEqual(amount("56200.232088"), amount("456.456") * x1)

        x1 *= amount("123.123")
        self.assertEqual(amount("15159.273129"), x1)
        x1 *= amount("123.123")
        self.assertEqual(amount("1866455.185461867"), x1)
        x1 *= 123
        self.assertEqual(amount("229573987.811809641"), x1)

        x2 = amount("123456789123456789.123456789123456789")

        self.assertEqual(amount("15241578780673678546105778311537878.046486820281054720515622620750190521"),
                         x2 * x2)

        self.assertValid(x1)
        self.assertValid(y1)
        self.assertValid(x2)

    def testCommodityMultiplication(self):
        x0 = amount()
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

        self.assertRaises(exceptions.ArithmeticError, lambda: x1 * x0)
        self.assertRaises(exceptions.ArithmeticError, lambda: x0 * x1)
        self.assertRaises(exceptions.ArithmeticError, lambda: x0 * x0)
        self.assertRaises(exceptions.ArithmeticError, lambda: x0 * x3)
        self.assertRaises(exceptions.ArithmeticError, lambda: x0 * x4)
        self.assertRaises(exceptions.ArithmeticError, lambda: x0 * x5)

        x1 *= amount("123.12")
        self.assertEqual(internalAmount("$15158.5344"), x1)
        self.assertEqual("$15158.53", x1.to_string())
        x1 *= amount("123.12")
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

    def testIntegerDivision(self):
        x1 = amount(123)
        y1 = amount(456)

        self.assertRaises(exceptions.ArithmeticError, lambda: x1 / 0)
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

        self.assertValid(x1)
        self.assertValid(y1)
        self.assertValid(x4)
        self.assertValid(y4)

    def testFractionalDivision(self):
        x1 = amount("123.123")
        y1 = amount("456.456")

        self.assertRaises(exceptions.ArithmeticError, lambda: x1 / 0)
        self.assertEqual(amount("0.00812195934"), amount("1.0") / x1)
        self.assertEqual(amount("0.00812195934"), amount("1.0") / x1)
        self.assertEqual(x1, x1 / amount("1.0"))
        self.assertEqual(amount("0.00812195934"), amount("1.0") / x1)
        self.assertEqual(amount("0.00812195934"), amount("1.0") / x1)
        self.assertEqual(- x1, x1 / amount("-1.0"))
        self.assertEqual(- amount("0.00812195934"), amount("-1.0") / x1)
        self.assertEqual(- amount("0.00812195934"), amount("-1.0") / x1)
        self.assertEqual(amount("0.269736842105263"), x1 / y1)
        self.assertEqual(amount("3.707317073170732"), y1 / x1)
        self.assertEqual(amount("0.269736842105263"), x1 / amount("456.456"))
        self.assertEqual(amount("3.707317073170732"), amount("456.456") / x1)
        self.assertEqual(amount("3.707317073170732"), amount("456.456") / x1)

        x1 /= amount("456.456")
        self.assertEqual(amount("0.269736842105263"), x1)
        x1 /= amount("456.456")
        self.assertEqual(amount("0.000590937225286255411255411255411255411"), x1)
        x1 /= 456
        self.assertEqual(amount("0.000001295914967733016252753094858358016252192982456140350877192982456140350877192982"), x1)

        x4 = amount("1234567891234567.89123456789")
        y4 = amount("56.789")

        self.assertEqual(amount("1.0"), x4 / x4)
        self.assertEqual(amount("21739560323910.7554497273748437197344556164046"), x4 / y4)

        self.assertValid(x1)
        self.assertValid(y1)
        self.assertValid(x4)
        self.assertValid(y4)

    def testCommodityDivision(self):
        x0 = amount()
        x1 = amount("$123.12")
        y1 = amount("$456.45")
        x2 = amount(internalAmount("$123.456789"))
        x3 = amount("DM 123.45")
        x4 = amount("123.45 euro")
        x5 = amount("123.45€")

        self.assertRaises(exceptions.ArithmeticError, lambda: x1 / 0)
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

        self.assertRaises(exceptions.ArithmeticError, lambda: x1 / x0)
        self.assertRaises(exceptions.ArithmeticError, lambda: x0 / x1)
        self.assertRaises(exceptions.ArithmeticError, lambda: x0 / x0)
        self.assertRaises(exceptions.ArithmeticError, lambda: x0 / x3)
        self.assertRaises(exceptions.ArithmeticError, lambda: x0 / x4)
        self.assertRaises(exceptions.ArithmeticError, lambda: x0 / x5)

        x1 /= amount("123.12")
        self.assertEqual(internalAmount("$1.00"), x1)
        self.assertEqual("$1.00", x1.to_string())
        x1 /= amount("123.12")
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

    def testNegation(self):
        x0 = amount()
        x1 = amount(-123456)
        x3 = amount("-123.456")
        x5 = amount("-123456")
        x6 = amount("-123.456")
        x7 = amount("-123456")
        x8 = amount("-123.456")
        x9 = amount(- x3)

        self.assertRaises(exceptions.ArithmeticError, lambda: x0.negate())
        self.assertEqual(x5, x1)
        self.assertEqual(x7, x1)
        self.assertEqual(x6, x3)
        self.assertEqual(x8, x3)
        self.assertEqual(- x6, x9)
        self.assertEqual(x3.negate(), x9)

        x10 = amount(x9.negate())

        self.assertEqual(x3, x10)

        self.assertValid(x1)
        self.assertValid(x3)
        self.assertValid(x5)
        self.assertValid(x6)
        self.assertValid(x7)
        self.assertValid(x8)
        self.assertValid(x9)
        self.assertValid(x10)

    def testCommodityNegation(self):
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

        self.assertEqual(amount("$-123.45"), x1.negate())
        self.assertEqual(amount("$123.45"), x2.negate())
        self.assertEqual(amount("$123.45"), x3.negate())

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

        self.assertEqual(amount("$-123.45"), x1.negate())
        self.assertEqual(amount("$123.45"), x2.negate())
        self.assertEqual(amount("$123.45"), x3.negate())

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

    def testAbs(self):
        x0 = amount()
        x1 = amount(-1234)
        x2 = amount(1234)

        self.assertRaises(exceptions.ArithmeticError, lambda: x0.abs())
        self.assertEqual(amount(1234), x1.abs())
        self.assertEqual(amount(1234), x2.abs())

        self.assertValid(x0)
        self.assertValid(x1)
        self.assertValid(x2)

    def testCommodityAbs(self):
        x1 = amount("$-1234.56")
        x2 = amount("$1234.56")

        self.assertEqual(amount("$1234.56"), x1.abs())
        self.assertEqual(amount("$1234.56"), x2.abs())

        self.assertValid(x1)
        self.assertValid(x2)

    def testFractionalRound(self):
        x0 = amount()
        x1 = amount("1234.567890")

        self.assertRaises(exceptions.ArithmeticError, lambda: x0.precision)
        self.assertRaises(exceptions.ArithmeticError, lambda: x0.round())
        self.assertRaises(exceptions.ArithmeticError, lambda: x0.round(2))
        self.assertRaises(exceptions.ArithmeticError, lambda: x0.unround())
        self.assertEqual(6, x1.precision)

        x1b = amount(x1.unround())

        self.assertEqual(x1b.precision, x1b.unround().precision)

        y7 = amount(x1.round(7))
        y6 = amount(x1.round(6))
        y5 = amount(x1.round(5))
        y4 = amount(x1.round(4))
        y3 = amount(x1.round(3))
        y2 = amount(x1.round(2))
        y1 = amount(x1.round(1))
        y0 = amount(x1.round(0))

        self.assertEqual(6, y7.precision)
        self.assertEqual(6, y6.precision)
        self.assertEqual(5, y5.precision)
        self.assertEqual(4, y4.precision)
        self.assertEqual(3, y3.precision)
        self.assertEqual(2, y2.precision)
        self.assertEqual(1, y1.precision)
        self.assertEqual(0, y0.precision)

        self.assertEqual(amount("1234.56789"), y7)
        self.assertEqual(amount("1234.56789"), y6)
        self.assertEqual(amount("1234.56789"), y5)
        self.assertEqual(amount("1234.5679"), y4)
        self.assertEqual(amount("1234.568"), y3)
        self.assertEqual(amount("1234.57"), y2)
        self.assertEqual(amount("1234.6"), y1)
        self.assertEqual(amount("1235"), y0)

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

        x5 = amount("0.0000000000000000000000000000000000001")

        self.assertEqual(amount("0.0000000000000000000000000000000000001"),
	      x5.round(37))
        self.assertEqual(amount(0), x5.round(36))

        self.assertValid(x1)
        self.assertValid(x2)
        self.assertValid(x3)
        self.assertValid(x4)
        self.assertValid(x5)

    def testCommodityRound(self):
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

        x5 = amount("$123.45")

        x5 *= amount("100.12")

        self.assertEqual(internalAmount("$12359.814"), x5)
        self.assertEqual("$12359.81", x5.to_string())
        self.assertEqual("$12359.814", x5.to_fullstring())
        self.assertEqual("$12359.814", x5.unround().to_string())

        self.assertValid(x1)
        self.assertValid(x2)
        self.assertValid(x3)
        self.assertValid(x4)
        self.assertValid(x5)

    def testCommodityDisplayRound(self):
        x1 = amount("$0.85")
        x2 = amount("$0.1")

        x1 *= amount("0.19")

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

    def testReduction(self):
        x0 = amount()
        x1 = amount("60s")
        x2 = amount("600s")
        x3 = amount("6000s")
        x4 = amount("360000s")
        x5 = amount("10m")
        x6 = amount("100m")
        x7 = amount("1000m")
        x8 = amount("10000m")
        x9 = amount("10h")
        x10 = amount("100h")
        x11 = amount("1000h")
        x12 = amount("10000h")

        self.assertRaises(exceptions.ArithmeticError, lambda: x0.reduce())
        self.assertRaises(exceptions.ArithmeticError, lambda: x0.unreduce())
        self.assertEqual(x2, x5)
        self.assertEqual(x3, x6)
        self.assertEqual(x4, x10)
        self.assertEqual("100.0h", x4.unreduce().to_string())

    def testSign(self):
        x0 = amount()
        x1 = amount("0.0000000000000000000000000000000000001")
        x2 = amount("-0.0000000000000000000000000000000000001")
        x3 = amount("1")
        x4 = amount("-1")

        self.assertRaises(exceptions.ArithmeticError, lambda: x0.sign())
        self.assertTrue(x1.sign() > 0)
        self.assertTrue(x2.sign() < 0)
        self.assertTrue(x3.sign() > 0)
        self.assertTrue(x4.sign() < 0)

        self.assertValid(x0)
        self.assertValid(x1)
        self.assertValid(x2)
        self.assertValid(x3)
        self.assertValid(x4)

    def testCommoditySign(self):
        x1 = amount(internalAmount("$0.0000000000000000000000000000000000001"))
        x2 = amount(internalAmount("$-0.0000000000000000000000000000000000001"))
        x3 = amount("$1")
        x4 = amount("$-1")

        self.assertTrue(x1.sign() != 0)
        self.assertTrue(x2.sign() != 0)
        self.assertTrue(x3.sign() > 0)
        self.assertTrue(x4.sign() < 0)

        self.assertValid(x1)
        self.assertValid(x2)
        self.assertValid(x3)
        self.assertValid(x4)

    def testTruth(self):
        x0 = amount()
        x1 = amount("1234")
        x2 = amount("1234.56")

        self.assertRaises(exceptions.ArithmeticError, lambda: 1 if x0 else 0)

        self.assertTrue(x1)
        self.assertTrue(x2)

        self.assertValid(x0)
        self.assertValid(x1)
        self.assertValid(x2)

    def testCommodityTruth(self):
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
        x0 = amount()
        x1 = amount("0.000000000000000000001")

        self.assertTrue(x1)
        self.assertRaises(exceptions.ArithmeticError, lambda: x0.is_zero())
        self.assertRaises(exceptions.ArithmeticError, lambda: x0.is_realzero())
        self.assertFalse(x1.is_zero())
        self.assertFalse(x1.is_realzero())

        self.assertValid(x0)
        self.assertValid(x1)

    def testCommodityForZero(self):
        x1 = amount(internalAmount("$0.000000000000000000001"))

        self.assertTrue(x1)
        self.assertFalse(x1.is_zero())
        self.assertFalse(x1.is_realzero())

        self.assertValid(x1)

    def testIntegerConversion(self):
        x0 = amount()
        x1 = amount(123456)
        x2 = amount("12345682348723487324")

        self.assertRaises(exceptions.ArithmeticError, lambda: x0.to_long())
        #self.assertRaises(exceptions.ArithmeticError, lambda: x0.to_double())
        self.assertFalse(x2.fits_in_long())
        self.assertEqual(123456, x1.to_long())
        #self.assertEqual(123456.0, x1.to_double())
        self.assertEqual("123456", x1.to_string())
        self.assertEqual("123456", x1.quantity_string)

        self.assertValid(x1)

    def testFractionalConversion(self):
        x1 = amount("1234.56")
        x2 = amount("1234.5683787634678348734")

        self.assertRaises(exceptions.ArithmeticError, lambda: x1.to_long()) # loses precision
        #self.assertRaises(exceptions.ArithmeticError, lambda: x2.to_double()) # loses precision
        #self.assertFalse(x2.fits_in_double())
        self.assertEqual(1234, x1.to_long(True))
        #self.assertEqual(1234.56, x1.to_double())
        self.assertEqual("1234.56", x1.to_string())
        self.assertEqual("1234.56", x1.quantity_string)

        self.assertValid(x1)

    def testCommodityConversion(self):
        x1 = amount("$1234.56")

        self.assertRaises(exceptions.ArithmeticError, lambda: x1.to_long()) # loses precision
        self.assertEqual(1234, x1.to_long(True))
        #self.assertEqual(1234.56, x1.to_double())
        self.assertEqual("$1234.56", x1.to_string())
        self.assertEqual("1234.56", x1.quantity_string)

        self.assertValid(x1)

    def testPrinting(self):
        pass
        #x0 = amount()
        #x1 = amount("982340823.380238098235098235098235098")
        #
        #bufstr = StringIO()
        #self.assertRaises(exceptions.ArithmeticError, lambda: bufstr.write(x0))
        #
        #bufstr = StringIO()
        #bufstr.write(x1)
        #
        #self.assertEqual("982340823.380238098235098235098235098",
        #                 bufstr.getvalue())
        #
        #self.assertValid(x0)
        #self.assertValid(x1)

    def testCommodityPrinting(self):
        pass
        #x1 = amount(internalAmount("$982340823.386238098235098235098235098"))
        #x2 = amount("$982340823.38")
        #
        #bufstr = StringIO()
        #bufstr.write(x1)
        #
        #self.assertEqual("$982340823.386238098235098235098235098",
        #                 bufstr.getvalue())
        #
        #bufstr = StringIO()
        #bufstr.write((x1 * x2).to_string())
        #
        #self.assertEqual("$964993493285024293.18099172508158508135413499124",
        #                 bufstr.getvalue())
        #
        #bufstr = StringIO()
        #bufstr.write((x2 * x1).to_string())
        #
        #self.assertEqual("$964993493285024293.18", bufstr.getvalue())
        #
        #self.assertValid(x1)
        #self.assertValid(x2)

    def testSerialization(self):
        pass
        #x0 = amount()
        #x1 = amount("$8,192.34")
        #x2 = amount("8192.34")
        #x3 = amount("8192.34")
        #x4 = amount("-8192.34")
        #x5 = amount(x4)
        #
        ## Force x3's pointer to actually be set to null_commodity
        ##x3.set_commodity(*x3.current_pool.null_commodity)
        #
        #buf = ""
        #storage = StringIO()
        #self.assertRaises(exceptions.ArithmeticError, lambda: x0.write(storage))
        #x1.write(storage)
        #x2.write(storage)
        #x3.write(storage)
        #x4.write(storage)
        #x5.write(storage)
        #buf = storage.getvalue()
        #
        #x1b = amount()
        #x2b = amount()
        #x3b = amount()
        #x4b = amount()
        #x5b = amount()
        #
        #storage = StringIO(buf)
        #x1b.read(storage)
        #x2b.read(storage)
        #x3b.read(storage)
        #x4b.read(storage)
        #x5b.read(storage)
        #
        #self.assertEqual(x1, x1b)
        #self.assertEqual(x2, x2b)
        #self.assertEqual(x3, x3b)
        #self.assertEqual(x4, x4b)
        #
        #ptr = buf.c_str()
        #
        #x1c = amount()
        #x2c = amount()
        #x3c = amount()
        #x4c = amount()
        #x5c = amount()
        #
        #x1c.read(ptr)
        #x2c.read(ptr)
        #x3c.read(ptr)
        #x4c.read(ptr)
        #x5c.read(ptr)
        #
        #self.assertEqual(x1, x1b)
        #self.assertEqual(x2, x2b)
        #self.assertEqual(x3, x3b)
        #self.assertEqual(x4, x4b)
        #
        #self.assertValid(x1)
        #self.assertValid(x2)
        #self.assertValid(x3)
        #self.assertValid(x4)
        #self.assertValid(x1b)
        #self.assertValid(x2b)
        #self.assertValid(x3b)
        #self.assertValid(x4b)
        #self.assertValid(x1c)
        #self.assertValid(x2c)
        #self.assertValid(x3c)
        #self.assertValid(x4c)

 
def suite():
    return unittest.TestLoader().loadTestsFromTestCase(t_amountTestCase)

if __name__ == '__main__':
    unittest.main()
