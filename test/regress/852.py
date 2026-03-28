#!/usr/bin/env python3
# Regression test for GitHub issue #852:
# Running "ledger python" and calling dir(ledger) used to crash with a
# segfault because Py_Main() loaded readline.so which conflicted with
# libedit already loaded by lpy.core.  The fix uses PyRun_InteractiveLoop
# for interactive mode instead of Py_Main, avoiding the readline conflict.

import lpy

attrs = dir(lpy.core)
assert 'Account' in attrs, "Expected 'Account' in dir(lpy.core)"
assert 'Amount' in attrs, "Expected 'Amount' in dir(lpy.core)"
assert 'Journal' in attrs, "Expected 'Journal' in dir(lpy.core)"
assert 'read_journal' in attrs, "Expected 'read_journal' in dir(lpy.core)"

print("dir(lpy.core) ok")
