#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import print_function, unicode_literals

import sys
import ledger

for post in ledger.read_journal(__file__.replace(".py", "_py.test")).query("income"):
  reference = post.tag("Reference")
  if sys.version_info.major == 2:
      reference = unicode(reference)
  print(reference)
