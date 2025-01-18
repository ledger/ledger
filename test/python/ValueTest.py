#!/usr/bin/env python3

import unittest

from ledger import *

class ValueTestCase(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.usd = Amount("$1").commodity
        cls.usd.pool().alias("USD", cls.usd)

    def testConstructor(self):
        v1 = Value(self.usd)
        v2 = Value(v1)
        v3 = v1

        self.assertTrue(v1.valid())
        self.assertTrue(v2.valid())
        self.assertTrue(v3.valid())
 
    def testEquality(self):
        v1 = Value(self.usd)

        self.assertTrue(v1 == Value(Amount("$2").commodity))
        self.assertTrue(v1 == Value(Amount("USD2").commodity))
        self.assertTrue(v1 != Value(Amount("EUR2").commodity))
        self.assertTrue(v1 == Amount("$2").commodity)
        self.assertTrue(v1 == Amount("USD2").commodity)
        self.assertTrue(Amount("USD2").commodity == v1)
        self.assertTrue(v1 != Amount("EUR2").commodity)
        self.assertTrue(Amount("EUR2").commodity != v1)
        self.assertTrue(v1 == string_value("$"))
        self.assertTrue(v1 == string_value("USD"))
        self.assertTrue(string_value("$") == v1)
        self.assertTrue(v1 != string_value("EUR"))
        self.assertTrue(string_value("EUR") != v1)
        with self.assertRaises(ArithmeticError):
            v1 == Value(Amount("USD1"))
        with self.assertRaises(ArithmeticError):
            Value(Amount("USD1")) == v1

        self.assertTrue(v1.valid())

    def testOrder(self):
        commodity = Amount("B1").commodity
        commodity.pool().alias("Cbis", commodity)
        commodity.pool().alias("Abis", commodity)
        v1 = Value(commodity)

        self.assertTrue(v1 < Value(Amount("C2").commodity))
        self.assertFalse(v1 < Value(Amount("Cbis2").commodity))
        self.assertTrue(v1 < Amount("C2").commodity)
        self.assertFalse(v1 < Amount("Cbis2").commodity)
        self.assertTrue(Amount("A2").commodity < v1)
        self.assertTrue(v1 < string_value("C"))
        self.assertFalse(v1 < string_value("Cbis"))
        self.assertTrue(string_value("A") < v1)
        with self.assertRaises(ArithmeticError):
            v1 < Value(Amount("C1"))
        with self.assertRaises(ArithmeticError):
            Value(Amount("A1")) < v1

        self.assertTrue(v1 <= Value(Amount("C2").commodity))
        self.assertTrue(v1 <= Value(Amount("Cbis2").commodity))
        self.assertTrue(v1 <= Amount("C2").commodity)
        self.assertTrue(v1 <= Amount("Cbis2").commodity)
        self.assertTrue(Amount("A2").commodity <= v1)
        self.assertTrue(v1 <= string_value("C"))
        self.assertTrue(v1 <= string_value("Cbis"))
        self.assertTrue(string_value("A") <= v1)
        with self.assertRaises(ArithmeticError):
            v1 <= Value(Amount("C1"))
        with self.assertRaises(ArithmeticError):
            Value(Amount("A1")) <= v1

        self.assertTrue(v1 > Value(Amount("A2").commodity))
        self.assertFalse(v1 > Value(Amount("Abis2").commodity))
        self.assertTrue(v1 > Amount("A2").commodity)
        self.assertFalse(v1 > Amount("Abis2").commodity)
        self.assertTrue(Amount("C2").commodity > v1)
        self.assertTrue(v1 > string_value("A"))
        self.assertFalse(v1 > string_value("Abis"))
        self.assertTrue(string_value("C") > v1)
        with self.assertRaises(ArithmeticError):
            v1 > Value(Amount("A1"))
        with self.assertRaises(ArithmeticError):
            Value(Amount("C1")) > v1

        self.assertTrue(v1 >= Value(Amount("A2").commodity))
        self.assertTrue(v1 >= Value(Amount("Abis2").commodity))
        self.assertTrue(v1 >= Amount("A2").commodity)
        self.assertTrue(v1 >= Amount("Abis2").commodity)
        self.assertTrue(Amount("C2").commodity >= v1)
        self.assertTrue(v1 >= string_value("A"))
        self.assertTrue(v1 >= string_value("Abis"))
        self.assertTrue(string_value("C") >= v1)
        with self.assertRaises(ArithmeticError):
            v1 >= Value(Amount("A1"))
        with self.assertRaises(ArithmeticError):
            Value(Amount("C1")) >= v1

        self.assertTrue(v1.valid())

    def testAddition(self):
        v1 = Value(self.usd)
        v2 = Value(self.usd)
        v2 += string_value("A")

        self.assertTrue(v2.is_string() and v2 == string_value("$A"))
        self.assertTrue(v1 + Value(Amount("$2").commodity) == string_value("$$"))
        self.assertTrue(v1 + Value(Amount("USD2").commodity) == string_value("$$"))
        self.assertTrue(v1 + Amount("$2").commodity == string_value("$$"))
        self.assertTrue(v1 + Amount("USD2").commodity == string_value("$$"))
        self.assertTrue(Amount("$2").commodity + v1 == string_value("$$"))
        self.assertTrue(v1 + string_value("A") == string_value("$A"))
        self.assertTrue(v1 + string_value("USD") == string_value("$USD"))
        self.assertTrue(string_value("A") + v1 == string_value("A$"))
        self.assertTrue(v1 + Value(Amount("USD1")) == string_value("$$1"))
        with self.assertRaises(ArithmeticError):
            Value(Amount("USD1")) + v1
        self.assertTrue(v1.is_commodity())

        self.assertTrue(v1.valid())
        self.assertTrue(v2.valid())

    def testSubtraction(self):
        v1 = Value(self.usd)

        with self.assertRaises(ArithmeticError):
            v1 - Value(Amount("$2").commodity)

        self.assertTrue(v1.valid())

    def testMultiplication(self):
        v1 = Value(self.usd)
        v2 = Value(self.usd)
        two = Value()
        two.set_long(2)
        v2 *= two

        self.assertTrue(v2.is_string() and v2 == string_value("$$"))
        with self.assertRaises(ArithmeticError):
            v1 * Value(Amount("$2").commodity)
        self.assertTrue(v1 * two == string_value("$$"))
        self.assertTrue(Value(Amount("USD1").commodity) * two == string_value("$$"))
        with self.assertRaises(ArithmeticError):
            two * v1
        self.assertTrue(v1 * Value(Amount("$2.5")) == string_value("$$"))
        with self.assertRaises(ArithmeticError):
            Value(Amount("$2.5")) * v1
        self.assertTrue(v1.is_commodity())

        self.assertTrue(v1.valid())
        self.assertTrue(v2.valid())

def suite():
    return unittest.TestLoader().loadTestsFromTestCase(ValueTestCase)

if __name__ == '__main__':
    unittest.main()
