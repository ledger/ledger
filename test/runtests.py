#!/usr/bin/env python

import random
import string
import signal
import os
import sys

true, false = 1, 0

options = [
  "--account=TempAccount",
  "--actual",
  "--add-budget",
  "--amount-data",
  "--amount=a",
  "--ansi",
  "--ansi-invert",
  "--average",
  #"--balance-format",
  "--basis",
  "--begin=2004/01",
  "--budget",
  "--by-payee",
  #"--cache=/tmp/cache",
  "--cleared",
  "--collapse",
  "--comm-as-payee",
  #"--csv-register-format",
  "--current",
  "--date-format=%Y",
  "--descend='$100'",
  "--descend-if='t=={$100}'",
  "--deviation",
  "--display='a>10'",
  "--dow",
  "--download",
  "--effective",
  "--empty",
  "--end=2005/01",
  #"--equity-format",
  #"--file=/tmp/file",
  "--forecast='d<[2006]'",
  "--format=%Y",
  #"--full-help",
  "--gain",
  "--head=10",
  #"--help",
  #"--help-calc",
  #"--help-comm",
  #"--help-disp",
  #"--init-file=/tmp/init",
  #"--input-date-format",
  "--limit='a>10'",
  "--lots",
  "--lot-prices",
  "--lot-dates",
  "--lot-tags",
  "--market",
  "--monthly",
  "--no-cache",
  #"--output=/tmp/output",
  #"--pager",
  #"--percentage",
  "--performance",
  "--period-sort=A\\(t\\)",
  "--period=oct",
  #"--plot-amount-format",
  #"--plot-total-format",
  "--price",
  "--price-exp=1000",
  #"--price-db=/tmp/foo",
  #"--prices-format",
  #"--print-format",
  "--quantity",
  "--real",
  #"--reconcile",
  #"--reconcile-date",
  #"--register-format",
  "--related",
  "--sort=A\\(t\\)",
  "--subtotal",
  "--tail=5",
  "--total-data",
  "--total=O",
  "--totals",
  "--unbudgeted",
  "--uncleared",
  #"--version",
  "--weekly",
  "--wide",
  "--yearly",
]

commands = [
  "bal rent",
  "bal ira",
  "bal auto",
  "reg rent",
  "reg ira",
  "reg expenses:food",
  "print rent",
  "print irc",
  "xml rent",
  "xml irc",
  "equity rent",
  "equity ira",
  "prices AAPL",
]

random.seed ()

loop    = true
count   = 0
errors  = 0
if len(sys.argv) > 1:
    errors = int(sys.argv[1])
signals = 0

while loop:
    try:
        n = random.randint (0, len (options))
        opts = random.sample (options, n)
        for cmd in commands:
            if os.path.exists ("/tmp/out"):
                os.unlink ("/tmp/out")

            cmd = "./ledger -f tools/standard.dat " + string.join(opts, " ") + " " + cmd + \
                  " >> /tmp/out 2>&1"

            sys.stdout = open ("/tmp/out", "w")
            print "::", cmd
            sys.stdout.close ()

            ret = os.system (cmd)

            sys.stdout = open ("/tmp/out", "a")

            # Convert an odd UNIX return type into the appropriate
            # signal indication.
            if ret and ret % 256 == 0 and ret / 256 > 127:
                ret = 0x100 + (ret / 256 - 128)

            if ret and ret % 256 == 0:
                print "ERROR: Return value", ret / 256
                sys.stdout.close ()
                os.system ("cat /tmp/out >> errors.out")
                errors += 1
            elif ret:
                if ret % 256 == signal.SIGINT:
                    loop = false
                    break
                print "SIGNAL: Return value", ret % 256
                sys.stdout.close ()
                os.system ("cat /tmp/out >> signals.out")
                signals += 1
            else:
                sys.stdout.close ()
                os.system ("cat /tmp/out >> results.out")

            sys.stdout = sys.__stdout__
            count += 1
            if count < 10 or \
               (count < 100 and count % 10 == 0) or \
               (count < 1000 and count % 100 == 0) or \
               count % 1000 == 0:
                if signals > 0 and errors > 0:
                    print "%d tests ... (%d signals, %d errors)" % \
                          (count, signals, errors)
                elif signals > 0:
                    print "%d tests ... (%d signals)" % \
                          (count, signals)
                elif errors > 0:
                    print "%d tests ... (%d errors)" % \
                          (count, errors)
                else:
                    print "%d tests ..." % count

    except KeyboardInterrupt:
        loop = false
