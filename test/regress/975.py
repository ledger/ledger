#!/usr/bin/env python3

import ledger

comms = ledger.commodities
eur = comms.find_or_create('EUR')

def balance_posts_subaccts(account):
    total = ledger.Balance()
    for post in account.posts():
        total += post.amount
    for subacct in account.accounts():
        total += balance_posts_subaccts(subacct)
    return total

def balance_for_account(journal, acctname):
    account = journal.find_account_re(acctname)
    return balance_posts_subaccts(account)

ledger.read_journal_from_string("""
2012-01-01 * Opening balance
    Assets:Cash                           100.00 EUR
    Equity:Opening balance
""")

ledger.session.close_journal_files()

journal = ledger.read_journal_from_string("""
2013-01-01 * foo
    Assets:A                                1.00 EUR
    Equity:Opening balance

2013-01-01 * Opening balance
    Assets:P                            100.00 GBP
    Assets:P                            10.00 "XX" @  1.00 GBP
    Equity:Opening balance

2013-01-04 * bar
    Assets:Receivable                       1.70 GBP
    Income:B
""")

result = balance_for_account(journal, "^Income")
print(result.value(eur))
