#!/usr/bin/env python

import sys
import os
import re
import tempfile

from string import join
from difflib import unified_diff

from LedgerHarness import LedgerHarness

harness = LedgerHarness(sys.argv)
tests   = sys.argv[2]

if not os.path.isdir(tests) and not os.path.isfile(tests):
    sys.exit(1)

def test_regression(test_file):
    bug     = open(test_file)
    command = bug.readline()

    line = bug.readline()
    assert "<<<\n" == line
    line = bug.readline()

    data = []
    while line != ">>>1\n":
        data.append(line)
        line = bug.readline()
    line = bug.readline()

    use_stdin = False
    if command.startswith("-f - "):
        use_stdin = True

        command = '$ledger ' + command
    else:
        tempdata = tempfile.mkstemp()

        os.write(tempdata[0], join(data, ''))
        os.close(tempdata[0])

        command = ('$ledger -f "%s" ' % tempdata[1]) + command

    output = []
    while line != ">>>2\n":
        output.append(line)
        line = bug.readline()
    line = bug.readline()

    error = []
    while not line.startswith("==="):
        error.append(line)
        line = bug.readline()

    match = re.match('=== ([0-9]+)', line)
    assert match

    exitcode = int(match.group(1))

    p = harness.run(command, columns=(not re.search('--columns', command)))

    if use_stdin:
        p.stdin.write(join(data))
    p.stdin.close()

    success = True
    printed = False
    index   = 0
    for line in unified_diff(output, harness.readlines(p.stdout)):
        index += 1
        if index < 3:
            continue
        if not printed:
            if success: print
            print "Regression failure in output from %s:" % \
                os.path.basename(test_file)
            success = False
            printed = True
        print " ", line,

    printed = False
    index   = 0
    for line in unified_diff([re.sub('\$FILE', tempdata[1], line)
                              for line in error], harness.readlines(p.stderr)):
        index += 1
        if index < 3:
            continue
        if not printed:
            if success: print
            print "Regression failure in error output from %s:" % \
                os.path.basename(test_file)
            success = False
            printed = True
        print " ", line,

    if exitcode == p.wait():
        if success:
            harness.success()
        else:
            harness.failure()
    else:
        if success: print
        print "Regression failure in exitcode from %s: %d (expected) != %d" % \
            (os.path.basename(test_file), exitcode, p.returncode)
        harness.failure()

    if not use_stdin:
        os.remove(tempdata[1])

if os.path.isdir(tests):
    for test in os.listdir(tests):
        if re.search('\.test$', test):
            test_regression(os.path.join(tests, test))
else:
    test_regression(tests)

harness.exit()
