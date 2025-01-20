#!/usr/bin/env python3

from datetime import datetime
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

    def testDivision(self):
        v1 = Value(self.usd)

        with self.assertRaises(ArithmeticError):
            v1 / Value(Amount("$2").commodity)

        self.assertTrue(v1.valid())

    def testType(self):
        v1 = Value(self.usd)

        self.assertTrue(v1.type() == ValueType.Commodity and v1.is_commodity())
        self.assertFalse(
            v1.is_null() or v1.is_boolean() or v1.is_datetime() or v1.is_date() or v1.is_long() or
            v1.is_amount() or v1.is_balance() or v1.is_string() or v1.is_mask() or
            v1.is_sequence() or v1.is_scope())

        self.assertTrue(v1.valid())

    def testForZero(self):
        v1 = Value(self.usd)
        v2 = Value(self.usd.pool().null_commodity)

        self.assertTrue(bool(v1) and not v2)
        self.assertTrue(v1.is_nonzero() and v2.is_zero())
        self.assertTrue(not v1.is_realzero() and v2.is_realzero())
        self.assertTrue(not v1.is_null() and not v2.is_null())

        self.assertTrue(v1.valid())
        self.assertTrue(v2.valid())

    def testNegation(self):
        v1 = Value(self.usd)
        v2 = Value(self.usd)
        v3 = Value(self.usd.pool().null_commodity)

        with self.assertRaises(ArithmeticError):
            -v1
        v2.in_place_not()
        v3.in_place_not()
        self.assertTrue(v2.is_boolean() and v2 == Value(False))
        self.assertTrue(v3.is_boolean() and v3 == Value(True))

        self.assertTrue(v1.valid())
        self.assertTrue(v2.valid())
        self.assertTrue(v3.valid())

    def testAbsoluteValue(self):
        v1 = Value(self.usd)

        with self.assertRaises(ArithmeticError):
            v1.abs()

        self.assertTrue(v1.valid())

    def testRounding(self):
        v1 = Value(self.usd)

        with self.assertRaises(ArithmeticError):
            v1.rounded()
        self.assertTrue(v1.__round__(2) == v1);
        with self.assertRaises(ArithmeticError):
            v1.truncated()
        with self.assertRaises(ArithmeticError):
            v1.floored()
        with self.assertRaises(ArithmeticError):
            v1.ceilinged()
        with self.assertRaises(ArithmeticError):
            v1.unrounded()
        self.assertTrue(v1.reduced() == v1)
        self.assertTrue(v1.unreduced() == v1)

        self.assertTrue(v1.valid())

    def testValuation(self):
        v1 = Value(self.usd)

        with self.assertRaises(ArithmeticError):
            v1.value(Amount("EUR1").commodity, datetime(2025, 1, 1))
        with self.assertRaises(ArithmeticError):
            v1.exchange_commodities("EUR")

        self.assertTrue(v1.valid())

    def testConversion(self):
        v1 = Value(self.usd)

        self.assertTrue(v1.to_commodity() == self.usd)
        self.assertTrue(Value(Amount("USD1").commodity).to_commodity() == self.usd)
        self.assertTrue(v1.to_string() == "$")
        self.assertTrue(Value(Amount("USD1").commodity).to_string() == "$")
        self.assertTrue(string_value("$").to_commodity() == self.usd)
        self.assertTrue(string_value("USD").to_commodity() == self.usd)
        with self.assertRaises(ArithmeticError):
            string_value("Z").to_commodity()

        self.assertTrue(v1.casted(ValueType.Commodity) == v1)
        self.assertTrue(Value(Amount("USD1").commodity).casted(ValueType.Commodity) == v1)
        self.assertTrue(v1.casted(ValueType.String) == string_value("$"))
        self.assertTrue(Value(Amount("USD1").commodity).casted(ValueType.String) == string_value("$"))
        self.assertTrue(string_value("$").casted(ValueType.Commodity) == v1)
        self.assertTrue(string_value("USD").casted(ValueType.Commodity) == v1)
        with self.assertRaises(ArithmeticError):
            string_value("Z").casted(ValueType.Commodity)

        self.assertTrue(v1.simplified() == v1)

        with self.assertRaises(ArithmeticError):
            v1.number()

        self.assertTrue(v1.valid())

    def testAnnotation(self):
        v1 = Value(self.usd)

        with self.assertRaises(ArithmeticError):
            v1.annotate(Annotation())
        with self.assertRaises(ArithmeticError):
            v1.has_annotation()
        with self.assertRaises(ArithmeticError):
            v1.annotation()
        self.assertTrue(v1.strip_annotations(KeepDetails()) == v1)

        self.assertTrue(v1.valid())

    def testLogging(self):
        v1 = Value(self.usd)

        self.assertTrue(v1.label() == "a commodity")
        self.assertTrue(v1.label(ValueType.Commodity) == "a commodity")
        self.assertTrue(v1.__repr__() == "$")
        self.assertTrue(Value(Amount("USD1").commodity).__repr__() == "$")
        self.assertTrue(value_context(v1) ==
            "                                      $")
        self.assertTrue(value_context(Value(Amount("USD1").commodity)) ==
            "                                      $")

        self.assertTrue(v1.valid())

def suite():
    return unittest.TestLoader().loadTestsFromTestCase(ValueTestCase)

if __name__ == '__main__':
    unittest.main()
