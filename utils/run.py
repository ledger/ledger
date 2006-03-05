#!/usr/bin/python

import sys
import os

for line in open (sys.argv[1]):
    line = line[: -1]

    print line
    sys.stdout.flush ()

    ret = os.system (line)

    if ret and ret % 256 == 0:
	print "ERROR: Return value", ret / 256
	sys.stdout.flush ()
    elif ret:
	if ret % 256 == signal.SIGINT:
	    sys.exit (1)
	print "SIGNAL: Return value", ret % 256
	sys.stdout.flush ()
