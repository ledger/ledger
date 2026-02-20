#!/usr/bin/env python3
# Regression test for GitHub issue #682:
# xact.posts and journal.xacts should be iterable as properties,
# not require calling them as methods (i.e. xact.posts, not xact.posts()).

import ledger

journal = ledger.read_journal_from_string("""
2012-03-01 KFC
    Expenses:Food      $10.00
    Assets:Cash
""")

# Test that journal.xacts is iterable as a property (no parentheses)
for xact in journal.xacts:
    # Test that xact.posts is iterable as a property (no parentheses)
    for post in xact.posts:
        print("%s %s" % (post.amount, post.account))

# Test via query that post.xact.posts is iterable as a property
last_xact = None
for post in journal.query("food"):
    if post.xact != last_xact:
        for p in post.xact.posts:
            print("via xact: %s %s" % (p.amount, p.account))
        last_xact = post.xact
