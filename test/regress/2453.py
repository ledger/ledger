#!/usr/bin/env python3

import ledger

for xact in ledger.read_journal(__file__.replace(".py", "_py.test")).xacts():
  for post in xact.raw_posts():
    print(post.date, post.xact.payee, post.account, post.amount)
