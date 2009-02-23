#!/usr/bin/env python

import sys
import os
import re
import string
import tempfile

from difflib import unified_diff
from subprocess import Popen, PIPE

ledger = sys.argv[1]
tests  = sys.argv[2]

if not os.path.isfile(ledger):
    sys.exit(1)
if not os.path.isdir(tests) and not os.path.isfile(tests):
    sys.exit(1)

succeeded = 0
failed    = 0

def test_regression(test_file):
    global succeeded, failed

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

        if re.search('--columns', command):
            command = ("%s" % ledger) + " --verify --args-only " + command
        else:
            command = (("%s" % ledger) +
                       " --verify --args-only --columns=80 " + command)
    else:
        tempdata = tempfile.mkstemp()

        os.write(tempdata[0], string.join(data, ''))
        os.close(tempdata[0])

        if re.search('--columns', command):
            command = (("%s -f \"%s\" " % (ledger, tempdata[1])) +
                       " --verify --args-only " + command)
        else:
            command = (("%s -f \"%s\" " % (ledger, tempdata[1])) +
                       " --verify --args-only --columns=80 " + command)

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

    p = Popen(command[:-1], shell=True,
              stdin=PIPE, stdout=PIPE, stderr=PIPE,
              close_fds=True)

    if use_stdin:
        p.stdin.write(string.join(data))
    p.stdin.close()

    success = True
    printed = False
    index   = 0
    for line in unified_diff(output, p.stdout.readlines()):
        index += 1
        if index < 3:
            continue
        if not printed:
            if success: print
            print "Regression failure in output from %s:" \
                % os.path.basename(test_file)
            if success: failed += 1
            success = False
            printed = True
        print " ", line,

    printed = False
    index   = 0
    for line in unified_diff([re.sub('\$FILE', tempdata[1], line)
                              for line in error],
                             p.stderr.readlines()):
        index += 1
        if index < 3:
            continue
        if not printed:
            if success: print
            print "Regression failure in error output from %s:" \
                % os.path.basename(test_file)
            if success: failed += 1
            success = False
            printed = True
        print " ", line,

    if exitcode != p.wait():
        if success: print
        if success: failed += 1
        success = False
        print "Regression failure in exitcode from %s: %d (expected) != %d" \
            % (os.path.basename(test_file), exitcode, p.returncode)

    if success:
        succeeded += 1
        print ".",

    if not use_stdin:
        os.remove(tempdata[1])

if os.path.isdir(tests):
    for test in os.listdir(tests):
        if re.search('\.test$', test):
            test_regression(os.path.join(tests, test))
else:
    test_regression(tests)

print
if succeeded > 0:
    print "OK (%d) " % succeeded,
if failed > 0:
    print "FAILED (%d)" % failed,
print

sys.exit(failed)
