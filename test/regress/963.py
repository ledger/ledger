#!/usr/bin/env python3

import ledger

# Regression test for GitHub issue #963:
# find_account_re only finds the first matching account.
#
# The new find_accounts_re (plural) should return all accounts
# whose fullname matches the regex, not just the first one.

session = ledger.Session()
journal = session.read_journal_from_string("""
2024/01/01 Opening
    Expenses:Food:Groceries    $50.00
    Expenses:Food:Dining       $30.00
    Expenses:Transport         $20.00
    Assets:Cash
""")

# find_accounts_re on journal should return all matches
accounts = journal.find_accounts_re("Expenses:Food")
names = sorted(a.fullname() for a in accounts)
for n in names:
    print(n)
