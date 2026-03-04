import ledger

# Regression test for GitHub issue #2152:
# Segfault in journal_t::clear_xdata() when the PostCollectorWrapper
# (collector_wrapper) outlives the Python Journal object.
#
# The crash happened because collector_wrapper held a raw reference
# (journal_t&) to the journal. If the journal was freed before the
# collector_wrapper's destructor ran, clear_xdata() would access freed
# memory.
#
# The fix stores a boost::shared_ptr<journal_t> in collector_wrapper,
# keeping the journal alive for the full lifetime of the query result.

session = ledger.Session()
j = session.read_journal_from_string("""
2017-11-23 example
    acct1  1 USD
    acct2
""")

# Simulate the scenario where collector_wrapper outlives the Journal object:
# create the query result, then release all other references to the journal,
# then access the collector. Previously this caused a SIGSEGV in clear_xdata().
coll = j.query("")
del j
del session

# With the fix, the collector_wrapper holds a shared_ptr to the journal,
# so it remains valid for iteration and cleanup.
count = 0
for post in coll:
    count += 1
del coll  # destructor calls journal_sp->clear_xdata() - must not crash

print(f"Done: {count} post(s)")
