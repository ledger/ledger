#!/usr/bin/env python

from amounts import *

val = Value("$100.00")

print val

amt = Amount("$100.00")

print amt.commodity
amt.commodity = amt.commodity
print amt
amt.commodity = NullCommodity
print amt
print amt.quantity
