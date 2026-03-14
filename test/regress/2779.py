#!/usr/bin/env python3
# Regression test for GitHub issue #2779:
# The backwards-compatible 'ledger' Python module (python/ledger/__init__.py)
# must re-export all native types from lpy.core via 'from lpy.core import *'.

import ledger

# Verify key native types are accessible directly from the ledger namespace
assert hasattr(ledger, 'Amount'),   "ledger.Amount not found"
assert hasattr(ledger, 'Balance'),  "ledger.Balance not found"
assert hasattr(ledger, 'Journal'),  "ledger.Journal not found"
assert hasattr(ledger, 'Account'),  "ledger.Account not found"

# Minimal functional test: create two Amounts and add them
a = ledger.Amount("$10.00")
b = ledger.Amount("$5.00")
c = a + b
assert str(c) == "$15.00", f"Expected $15.00, got {str(c)}"

print("ledger backwards compatibility ok")
