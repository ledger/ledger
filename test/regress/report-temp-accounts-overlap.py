#!/usr/bin/env python3
# A query result keeps alive every account its postings, and the postings of
# other live results, reach through the journal's account tree.  Results are
# rebound (the old one is released after the new query returns), held side by
# side and released out of order, for a path under a journal account
# (Expenses:Groceries), for top-level paths (Brand:Home, X:Expenses:Groceries)
# and for a path that passes through an account another live result made
# (Brand:Expenses:Groceries).  Once the last result is released the journal's
# tree is as it was.

import ledger

journal = ledger.read_journal_from_string("""
account-rewrite Food  Expenses:Groceries
account-rewrite Rent  Brand:Home

2024/01/01 Grocer
    Expenses:Food    $10.00
    Assets:Cash

2024/01/02 Landlord
    Expenses:Rent    $500.00
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


def accounts(result):
    return ", ".join(post.account.fullname() for post in result)


print("journal: " + journal_accounts())

for query in ["Food", "Rent", "--account='\"X\"' Food"]:
    result = None
    for i in range(3):
        result = journal.query(query)
        print("%s #%d -> %s" % (query, i, accounts(result)))
    a = journal.query(query)
    b = journal.query(query)
    del a
    print("%s b -> %s" % (query, accounts(b)))
    del b
    del result
    print("journal: " + journal_accounts())

a = journal.query("Food")
b = journal.query("Rent")
c = journal.query("--account='\"Brand\"' Food")
del b
print("c -> " + accounts(c))
del a
print("c -> " + accounts(c))
del c
print("journal: " + journal_accounts())
