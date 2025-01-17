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

def suite():
    return unittest.TestLoader().loadTestsFromTestCase(ValueTestCase)

if __name__ == '__main__':
    unittest.main()
