#!/usr/bin/python

# This script confirms both that the register report "adds up", and that its
# final balance is the same as what the balance report shows.

import sys
import os
import re

from subprocess import Popen, PIPE

commands = [
    "-f '$tests/standard.dat' -O 0ecbb1b15e2cf3e515cc0f8533e5bb0fb2326728",
    "-f '$tests/standard.dat' -B c56a21d23a6535184e7152ee138c28974f14280c",
    "-f '$tests/standard.dat' -V c56a21d23a6535184e7152ee138c28974f14280c",
    "-f '$tests/standard.dat' -G c56a21d23a6535184e7152ee138c28974f14280c",
    "-f '$tests/standard.dat' -B c0226fafdf9e6711ac9121cf263e2d50791859cb",
    "-f '$tests/standard.dat' -V c0226fafdf9e6711ac9121cf263e2d50791859cb",
    "-f '$tests/standard.dat' -G c0226fafdf9e6711ac9121cf263e2d50791859cb"
]

ledger    = sys.argv[1]
tests     = sys.argv[2]
succeeded = 0
failed    = 0

if not os.path.isfile(ledger):
    sys.exit(1)
if not os.path.isdir(tests) and not os.path.isfile(tests):
    sys.exit(1)

def clean(num):
    num = re.sub("(\s+|\$|,)","", num)
    m = re.search("([-0-9.]+)", num)
    if m:
        return float(m.group(1))
    else:
        return float(num)

def confirm_report(args):
    index         = 1
    last_line     = ""
    failure       = False
    running_total = 0.0

    p = Popen(re.sub('\$cmd', 'reg', args), shell=True,
              stdin=PIPE, stdout=PIPE, stderr=PIPE,
              close_fds=True)

    for line in p.stdout.readlines():
        match = re.match("\\s*([-$,0-9.]+)\\s+([-$,0-9.]+)", line[54:])
        if not match:
            continue
        value = clean(match.group(1))
        total = clean(match.group(2))

        running_total += value
        diff = abs(running_total - total)
        if re.search(' -[VGB] ', args) and diff < 0.015:
            diff = 0.0
        if diff > 0.001:
            print "DISCREPANCY: %.3f (%.3f - %.3f) at line %d:" % \
                  (running_total - total, running_total, total, index)
            print line,
            running_total = total
            failure = True

        index += 1
        last_line = line

    balance_total = 0.0

    p = Popen(re.sub('\$cmd', 'bal', args), shell=True,
              stdin=PIPE, stdout=PIPE, stderr=PIPE,
              close_fds=True)

    for line in p.stdout.readlines():
        if line[0] != '-':
            balance_total = clean(line[:20])

    diff = abs(balance_total - running_total)
    if re.search(' -[VGB] ', args) and diff < 0.015:
        diff = 0.0
    if diff > 0.001:
        print
        print "DISCREPANCY: %.3f (%.3f - %.3f) between register and balance" % \
                  (balance_total - running_total, balance_total, running_total)
        print last_line,
        failure = True

    return not failure

for cmd in commands:
    if confirm_report("%s --args-only --verify --columns=80 %s" %
                      (ledger, re.sub('\$tests', tests, cmd))):
        sys.stdout.write(".")
        succeeded += 1
    else:
        sys.stdout.write("E")
        failed += 1

print
if succeeded > 0:
    print "OK (%d) " % succeeded,
if failed > 0:
    print "FAILED (%d)" % failed,
print

sys.exit(failed)

