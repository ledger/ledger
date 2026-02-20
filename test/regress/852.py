#!/usr/bin/env python3
# Regression test for GitHub issue #852:
# Running "ledger python" and calling dir(ledger) used to crash with a
# segfault because Py_Main() loaded readline.so which conflicted with
# libedit already loaded by ledger.  The fix uses PyRun_InteractiveLoop
# for interactive mode instead of Py_Main, avoiding the readline conflict.

import ledger

attrs = dir(ledger)
assert 'Account' in attrs, "Expected 'Account' in dir(ledger)"
assert 'Amount' in attrs, "Expected 'Amount' in dir(ledger)"
assert 'Journal' in attrs, "Expected 'Journal' in dir(ledger)"
assert 'read_journal' in attrs, "Expected 'read_journal' in dir(ledger)"

print("dir(ledger) ok")
