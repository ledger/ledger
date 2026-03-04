#!/usr/bin/env python3
# Regression test for GitHub issue #1014:
# session.read_journal(str) raised:
#   TypeError: Python argument types in Session.read_journal(Session, str)
#   did not match C++ signature:
#     read_journal(ledger::session_t {lvalue}, boost::filesystem::path)
# The fix accepts a plain string and converts it to a path internally.

import ledger
import os
import tempfile

data = """
2014-01-01 Payee
    Expenses:Food    $10.00
    Assets:Cash
"""

with tempfile.NamedTemporaryFile(mode='w', suffix='.ledger', delete=False) as f:
    f.write(data)
    tmppath = f.name

try:
    s = ledger.Session()
    journal = s.read_journal(tmppath)
    xacts = list(journal.xacts)
    assert len(xacts) == 1, "Expected 1 transaction"
    print("session.read_journal(str) works")
finally:
    os.unlink(tmppath)
