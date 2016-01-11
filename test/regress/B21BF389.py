#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import print_function, unicode_literals

import ledger

for post in ledger.read_journal(__file__.replace(".py", "_py.test")).query("income"):
  print(unicode(post.tag("Reference")))
