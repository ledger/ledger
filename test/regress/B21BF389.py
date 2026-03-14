#!/usr/bin/env python3

import sys
from lpy import core

for post in core.read_journal(__file__.replace(".py", "_py.test")).query("income"):
  reference = post.tag("Reference")
  print(reference)
