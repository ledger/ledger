#!/usr/bin/env python3
# Regression test for GitHub issue #2098:
# Calling session.read_journal_files() a second time (without an intervening
# close_journal_files()) raised an assertion failure:
#
#   Assertion failed in session.cc:
#     xact_count == journal->xacts.size()
#
# The assertion compared the count of transactions read from files in this
# call to the *total* journal xact count, which already included transactions
# from the previous read.  The fix records the initial xact count before
# reading files and checks the delta instead, allowing the journal to hold
# more transactions than were read in this particular call.
#
# The same assertion also prevented Python scripts from calling
# journal.add_xact() to programmatically add transactions and then invoking
# read_journal_files() afterwards.

from lpy import core
import os
import tempfile

data = """
2020-01-01 Payee
    Expenses:Food    $10.00
    Assets:Cash
"""

with tempfile.NamedTemporaryFile(mode='w', suffix='.ledger', delete=False) as f:
    f.write(data)
    tmppath = f.name

try:
    s = core.Session()

    # First read via read_journal() -- this resets the journal and reads the
    # file, so the assertion is satisfied: xact_count(1) == xacts.size()(1).
    j = s.read_journal(tmppath)
    assert len(list(j.xacts)) == 1, "Expected 1 xact after first read"

    # Second read via read_journal_files(): the journal is NOT reset between
    # calls, so it still holds the 1 xact from the first read.  Previously
    # this crashed with the assertion xact_count(1) != journal->xacts.size()(2).
    # With the fix the check becomes xact_count(1) == xacts.size()(2) - 1 == 1.
    j2 = s.read_journal_files()
    assert j2 is not None, "read_journal_files() should return a journal"

    print("read_journal_files() assertion is not triggered on repeated reads")
finally:
    os.unlink(tmppath)
