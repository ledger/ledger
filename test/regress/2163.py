import ledger

# Regression test for GitHub issue #2163.
#
# When `import ledger` is called from Python, pyledger.cc created a first
# python_interpreter_t (A), then initialize_for_python() called
# export_session() which registered A's raw pointer as the Python `session`
# attribute.  Immediately after, initialize_for_python() called
# python_session.reset(new python_interpreter_t) to create a second object
# (B) and destroyed A.  The Python `session` attribute now held a dangling
# pointer, causing segfaults when `session.close_journal_files()` was called.
#
# The fix moves the default_scope/empty_scope setup to pyledger.cc, before
# initialize_for_python() runs, so that python_session is stable when
# export_session() captures its raw pointer.
#
# This test verifies that session.close_journal_files() does not crash and
# that the session can be used correctly across multiple read/close cycles.

# 1. session.close_journal_files() must not crash (was segfaulting before fix)
ledger.session.close_journal_files()

# 2. Read a journal, close via session, read again -- must not crash
j1 = ledger.read_journal_from_string("""
2012-03-01 First
    Expenses:Food      $10.00
    Assets:Cash
""")
xacts1 = list(j1)
assert len(xacts1) == 1, "expected 1 transaction, got %d" % len(xacts1)
assert xacts1[0].payee == "First"

ledger.session.close_journal_files()

j2 = ledger.read_journal_from_string("""
2012-03-02 Second
    Expenses:Coffee    $5.00
    Assets:Cash
""")
xacts2 = list(j2)
assert len(xacts2) == 1, "expected 1 transaction, got %d" % len(xacts2)
assert xacts2[0].payee == "Second"

# 3. Repeated close_journal_files() calls via session must not crash
ledger.session.close_journal_files()
ledger.session.close_journal_files()

print("ok")
