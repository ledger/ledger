#!/usr/bin/env python3

import sys
import ledger

for post in ledger.read_journal(__file__.replace(".py", "_py.test")).query("income"):
  reference = post.tag("Reference")
  print(reference)
