import ledger

# Regression test for GitHub issue #977:
# Segfault in account_t::fullname() when find_account_re() is called
# after close_journal_files() on a journal that was previously read.
#
# The crash happened in account_t::fullname() because close_journal_files()
# destroyed the journal while Python still held a reference to it.
journal = ledger.read_journal("test/regress/977.dat")
ledger.session.close_journal_files()

# These operations previously caused a SIGSEGV because the journal was freed.
# With the fix (shared_ptr ownership), the journal stays alive.
for xact in journal:
    print(xact.payee)
    for post in xact.posts():
        print(post.account.fullname())
