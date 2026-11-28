#!/usr/bin/env python3
# Transaction.raw_posts leaves out the postings Ledger flags as
# generated: those an automated transaction adds, and those finalize()
# adds for a null posting, its share that balances each further
# commodity.  The parts of a sale split across lots are copies of the
# written sale, not generated, and are kept.  Transaction.posts lists
# them all.

import ledger

journal = ledger.read_journal_from_string("""
= /Food/
    (Assets:Cash)    $100

2012-03-01 KFC
    Expenses:Food    $100
    Expenses:Travel  10 EUR
    Assets:Credit
""")

for xact in journal.xacts:
    print("posts:")
    for post in xact.posts:
        print("  %s %s" % (post.amount, post.account))
    print("raw_posts:")
    for post in xact.raw_posts:
        print("  %s %s" % (post.amount, post.account))
