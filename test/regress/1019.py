#!/usr/bin/env python3
# Regression test for GitHub issue #1019:
# Setting an exchange rate via comms.exchange() using an Amount literal such
# as core.Amount('0.005 GBP') caused the GBP commodity to retain the
# 3-decimal precision implied by that literal.  When the journal was then read
# with GBP amounts having only 2 decimal places (e.g. 0.89 GBP), rounding
# would leave a non-zero remainder and the transaction would fail to balance.
#
# The fix tags the price commodity with COMMODITY_PRECISION_FROM_PRICE after
# pool.exchange() so that the textual parser can widen the precision to match
# the actual amounts it encounters in the journal.

from lpy import core

comms = core.commodities

# Set up an exchange rate that implies 3-decimal precision for GBP.
comms.exchange(comms.find_or_create('Nectar'), core.Amount('0.005 GBP'))

# Read a journal whose GBP amounts require only 2-decimal precision.
# With the bug: GBP precision stays at 3, so -1.40/1.578 rounds to -0.887 GBP
# giving a remainder of 0.003 GBP, which causes a balance error.
# With the fix: GBP precision is widened to 2 (from the 0.89 GBP posting),
# so -1.40/1.578 rounds to -0.89 GBP and the transaction balances exactly.
try:
    journal = core.read_journal_from_string("""
2014-01-01 Test
    A  -1.40 USD @ (1/1.578 GBP)
    B   0.89 GBP
""")
    print("OK")
except RuntimeError as e:
    print("FAIL: {}".format(e))
