#!/usr/bin/python

# This script confirms what ledger tells you.

import sys
import os
import re

def clean(num):
    num = re.sub("(\s+|\$|,)","", num)
    m = re.search("([-0-9.]+)", num)
    if m:
        return float(m.group(1))
    else:
        return float(num)

running_total = 0.0
index = 1
last_line = ""
errors = 0

args = sys.argv[1]
for line in os.popen(re.sub('\$cmd', 'reg', args)):
    match = re.match("\\s*([-$,0-9.]+)\\s+([-$,0-9.]+)", line[55:])
    if not match:
        continue
    value = clean(match.group(1))
    total = clean(match.group(2))

    running_total += value
    diff = abs(running_total - total)
    if (re.search(' -V ', args) or re.search(' -G ', args)) and diff < 0.015:
        diff = 0.0
    if diff > 0.001:
        print "DISCREPANCY: %.3f (%.3f - %.3f) at line %d:" % \
              (running_total - total, running_total, total, index)
        print line,
        running_total = total
        errors += 1

    index += 1
    last_line = line

balance_total = 0.0

for line in os.popen(re.sub('\$cmd', 'bal', args)):
    if line[0] != '-':
        balance_total = clean(line[:20])

diff = abs(balance_total - running_total)
if (re.search(' -V ', args) or re.search(' -G ', args)) and diff < 0.015:
    diff = 0.0
if diff > 0.001:
    print
    print "DISCREPANCY: %.3f (%.3f - %.3f) between register and balance" % \
              (balance_total - running_total, balance_total, running_total)
    print last_line,
    errors += 1

sys.exit(errors)
