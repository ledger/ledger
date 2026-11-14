#!/usr/bin/env python3

import ledger

# Account.children_with_flags keeps its one-argument Python form.  No
# report has run, so no account carries a display or visited flag.

session = ledger.Session()
journal = session.read_journal_from_string("""
2024/01/01 Opening
    Expenses:Food:Dairy      $20.00
    Expenses:Rent           $500.00
    Assets:Cash
""")

expenses = journal.find_account("Expenses")
print(expenses.children_with_flags(ledger.ACCOUNT_EXT_TO_DISPLAY))
print(journal.master.children_with_flags(ledger.ACCOUNT_EXT_VISITED))
