#!/usr/bin/env python

from amounts import *

amt_ncd = Amount("100")
print amt_ncd
amt_nc = Amount("100.00")
print amt_nc
amt = Amount("$100.00")
print amt
namt_ncd = Amount("-100")
print namt_ncd
namt_nc = Amount("-100.00")
print namt_nc
namt = Amount("$-100.00")
print namt
namt2 = Amount("-$100.00")
print namt2

val = Value("$100.00")

print val
