#!/usr/bin/env python3
# Regression test for GitHub issue #852:
# Running "ledger python" and calling dir(ledger) used to crash with a
# segfault because Py_Main() loaded readline.so which conflicted with
# libedit already loaded by core.  The fix uses PyRun_InteractiveLoop
# for interactive mode instead of Py_Main, avoiding the readline conflict.

from lpy import core

attrs = dir(core)
assert 'Account' in attrs, "Expected 'Account' in dir(core)"
assert 'Amount' in attrs, "Expected 'Amount' in dir(core)"
assert 'Journal' in attrs, "Expected 'Journal' in dir(core)"
assert 'read_journal' in attrs, "Expected 'read_journal' in dir(core)"

print("dir(ledger) ok")
