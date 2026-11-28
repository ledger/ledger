#!/usr/bin/env python3
# Accounts that a report builds from a path (account-rewrite, --account,
# --pivot-only, --inject) belong to the query and never join the
# journal's account tree, even when the path starts with an account the
# journal has (Expenses, Assets).  A posting moved from such an account
# onto one the journal has (Kitchen onto Expenses:Rent) leaves the
# journal's account as it was, not temporary.

import ledger

journal = ledger.read_journal_from_string("""
account-rewrite Food  Expenses:Groceries
account-rewrite Pantry  Expenses:Rent:Larder
account-rewrite Kitchen  Expenses:Rent

2024/01/01 Grocer
    Expenses:Food    $10.00
    ; Assets: Kitchen
    ; Brand: Kitchen
    Assets:Cash

2024/01/02 Landlord
    Expenses:Rent    $500.00
    ; Assets: Pantry
    Assets:Cash
""")


def journal_accounts():
    names = []

    def walk(account):
        for child in account.accounts():
            temporary = " (temporary)" if child.has_flags(ledger.ACCOUNT_TEMP) else ""
            names.append(child.fullname() + temporary)
            walk(child)

    walk(journal.master)
    return ", ".join(sorted(names))


print("journal: " + journal_accounts())

for query in ["Food",
              "--account='\"Assets\"' Food",
              "--pivot-only=Assets Food",
              "--inject=Expenses:Tip Food",
              "--pivot-only=Assets Rent Food",
              "--pivot-only=Brand Food"]:
    result = journal.query(query)
    print(query + " -> " + ", ".join(post.account.fullname() for post in result))
    del result
    print("journal: " + journal_accounts())
