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

for line in os.popen("./ledger %s reg %s" % (sys.argv[1], sys.argv[2])):
    value = clean(line[55:67])
    total = clean(line[68:80])

    running_total += value
    if abs(running_total - total) > 0.001:
	print "! discrepancy of %.2f (%.2f - %.2f) at line %d:" % \
	      (running_total - total, running_total, total, index)
	print line,
	running_total = total

    index += 1
    last_line = line

balance_total = 0.0

for line in os.popen("./ledger %s bal %s" % (sys.argv[1], sys.argv[2])):
    balance_total = clean(line[:20])

if abs(balance_total - running_total) > 0.001:
    print
    print "! discrepancy of %.2f (%.2f - %.2f) between register and balance" % \
	      (balance_total - running_total, balance_total, running_total)
    print last_line,
    print line,
