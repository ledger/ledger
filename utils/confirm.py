#!/usr/bin/python

# This script confirms what ledger tells you.

import sys
import os
import re

def clean(num):
    return float(re.sub("(\s+|\$|,)","",num))

running_total = 0.0
index = 1
last_line = ""

report = sys.argv[1]
for line in os.popen("./ledger -e 2004/4 %s reg %s" % (report, sys.argv[2])):
    value = clean(line[55:67])
    total = clean(line[68:])

    running_total += value
    diff = abs(running_total - total)
    if report == "-V" or report == "-G" and diff < 0.015:
	diff = 0.0
    if diff > 0.001:
	print "! discrepancy of %.2f (%.2f - %.2f) at line %d:" % \
	      (running_total - total, running_total, total, index)
	print line,
	running_total = total

    index += 1
    last_line = line

balance_total = 0.0

for line in os.popen("./ledger -e 2004/4 %s bal %s" % (report, sys.argv[2])):
    balance_total = clean(line[:20])

diff = abs(balance_total - running_total)
if report == "-V" or report == "-G" and diff < 0.015:
    diff = 0.0
if diff > 0.001:
    print
    print "! discrepancy of %.2f (%.2f - %.2f) between register and balance" % \
	      (balance_total - running_total, balance_total, running_total)
    print last_line,
    print line,
