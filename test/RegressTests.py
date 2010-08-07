#!/usr/bin/env python

import sys
import os
import re
import tempfile

multiproc = False
try:
    from multiprocessing import Pool
    multiproc = True
except:
    pass

from string import join
from difflib import unified_diff

from LedgerHarness import LedgerHarness

args = sys.argv
jobs = 1
match = re.match('-j([0-9]+)?', args[1])
if match:
    args = [args[0]] + args[2:]
    if match.group(1):
        jobs = int(match.group(1))
if jobs == 1:
    multiproc = False

harness = LedgerHarness(args)
tests   = args[3]

if not os.path.isdir(tests) and not os.path.isfile(tests):
    sys.exit(1)

class RegressFile(object):
    def __init__(self, filename):
        self.filename = filename
        self.fd = open(self.filename)

    def is_directive(self, line):
        return line == "<<<\n" or \
               line == ">>>\n" or \
               line == ">>>1\n" or \
               line == ">>>2\n" or \
               line.startswith("===")

    def transform_line(self, line):
        line = re.sub('\$sourcepath', harness.sourcepath, line)
        return line

    def read_section(self):
        lines = []
        line = self.fd.readline()
        while line and not self.is_directive(line):
            lines.append(self.transform_line(line))
            line = self.fd.readline()
        return (lines, line)

    def read_test(self, last_test = None):
        test = {
            'command':  None,
            'input':    "",
            'output':   "",
            'error':    "",
            'exitcode': 0
        }
        if last_test:
            test['input'] = last_test['input']

        line = self.fd.readline()
        while line:
            if line == "<<<\n":
                (test['input'], line) = self.read_section()
            elif line == ">>>\n" or line == ">>>1\n":
                (test['output'], line) = self.read_section()
            elif line == ">>>2\n":
                (test['error'], line) = self.read_section()
            elif line.startswith("==="):
                match = re.match('=== ([0-9]+)', line)
                assert match
                test['exitcode'] = int(match.group(1))
                return test
            else:
                test['command'] = self.transform_line(line)
                line = self.fd.readline()

        return None

    def run_test(self, test):
        use_stdin = False
        if test['command'].find("-f - ") != -1:
            use_stdin = True

            test['command'] = '$ledger ' + test['command']
        else:
            tempdata = tempfile.mkstemp()

            os.write(tempdata[0], join(test['input'], ''))
            os.close(tempdata[0])

            test['command'] = (('$ledger -f "%s" ' % tempdata[1]) +
                               test['command'])

        p = harness.run(test['command'],
                        columns=(not re.search('--columns', test['command'])))

        if use_stdin:
            p.stdin.write(join(test['input'], ''))
        p.stdin.close()

        success = True
        printed = False
        index   = 0
        for line in unified_diff(test['output'], harness.readlines(p.stdout)):
            index += 1
            if index < 3:
                continue
            if not printed:
                if success: print
                print "Regression failure in output from %s:" % \
                    os.path.basename(self.filename)
                success = False
                printed = True
            print " ", line,

        printed = False
        index   = 0
        for line in unified_diff([re.sub('\$FILE', tempdata[1], line)
                                  for line in test['error']],
                                 harness.readlines(p.stderr)):
            index += 1
            if index < 3:
                continue
            if not printed:
                if success: print
                print "Regression failure in error output from %s:" % \
                    os.path.basename(self.filename)
                success = False
                printed = True
            print " ", line,

        if test['exitcode'] == p.wait():
            if success:
                harness.success()
            else:
                harness.failure()
        else:
            if success: print
            print "Regression failure in exitcode from %s: %d (expected) != %d" % \
                (os.path.basename(self.filename), test['exitcode'], p.returncode)
            harness.failure()

        if not use_stdin:
            os.remove(tempdata[1])

    def run_tests(self):
        test = self.read_test()
        while test:
            self.run_test(test)
            test = self.read_test(test)
        return harness.failed

    def close(self):
        self.fd.close()

def do_test(path):
    entry = RegressFile(path)
    failed = entry.run_tests()
    entry.close()
    return failed

if __name__ == '__main__':
    if multiproc:
        pool = Pool(jobs*2)
    else:
        pool = None

    if os.path.isdir(tests):
        tests = [os.path.join(tests, x)
                 for x in os.listdir(tests) if x.endswith('.test')]
        if pool:
            harness.failed = sum(pool.map(do_test, tests, 1))
        else:
            harness.failed = sum(map(do_test, tests))
    else:
        entry = RegressFile(tests)
        entry.run_tests()
        entry.close()

    if pool:
        pool.close()
        pool.join()

    harness.exit()
