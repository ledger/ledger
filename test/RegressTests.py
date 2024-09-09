#!/usr/bin/env python3

from io import open

import argparse
import pathlib
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

from difflib import unified_diff

from LedgerHarness import LedgerHarness

parser = argparse.ArgumentParser(prog='RegressTests', parents=[LedgerHarness.parser()])
parser.add_argument('-j', '--jobs', type=int, default=1)
parser.add_argument('tests', type=pathlib.Path)
args = parser.parse_args()
multiproc &= (args.jobs >= 1)
harness = LedgerHarness(args.ledger, args.sourcepath, args.verify, args.gmalloc, args.python)

match = re.match(r'(Baseline|Regress|Manual)Test_(.*)', str(args.tests))
if match:
  args.tests = pathlib.Path('test') / match.group(1).lower() / (match.group(2) + '.test')

if not args.tests.is_dir() and not args.tests.is_file():
    print(f'{args.tests} is not a directory or file (cwd: {os.getcwd()})'
          , file=sys.stderr)
    sys.exit(1)

class RegressFile(object):
    def __init__(self, filename):
        self.filename = filename
        self.fd = open(self.filename, encoding='utf-8')

    def transform_line(self, line):
        return line\
            .replace('$sourcepath', str(harness.sourcepath))\
            .replace('$FILE', str(self.filename.resolve()))

    def read_test(self):
        class Test:
            command =  None
            output =   None
            error =    None
            exitcode = 0

        in_output = False
        in_error  = False

        line = self.fd.readline()
        test = Test()
        while line:
            if line.startswith("test "):
                command = line[5:]
                match = re.match(r'(.*) -> ([0-9]+)', command)
                if match:
                    test.command = self.transform_line(match.group(1))
                    test.exitcode = int(match.group(2))
                else:
                    test.command = command
                in_output = True

            elif in_output:
                if line.startswith("end test"):
                    in_output = in_error = False
                    break
                elif in_error:
                    if test.error is None:
                        test.error = []
                    test.error.append(self.transform_line(line))
                else:
                    if line.startswith("__ERROR__"):
                        in_error = True
                    else:
                        if test.output is None:
                            test.output = []
                        test.output.append(self.transform_line(line))
            line = self.fd.readline()
            #print("line =", line)

        return test.command and test

    def notify_user(self, msg, test):
        print(msg)
        print("--")
        print(self.transform_line(test.command),)
        print("--")

    def run_test(self, test):
        use_stdin = False
        if sys.platform == 'win32':
            test.command = test.command.replace('/dev/null', 'nul')
            # There is no equivalent to /dev/stdout, /dev/stderr, /dev/stdin
            # on Windows, so skip tests that require them.
            if '/dev/std' in test.command:
                harness.skipped()
                return
        if test.command.find('-f ') != -1:
            test.command = '$ledger ' + test.command
            if re.search(r'-f (-|/dev/stdin)(\s|$)', test.command):
                use_stdin = True
        else:
            test.command = f'$ledger -f "{str(self.filename.resolve())}" {test.command}'

        p = harness.run(test.command,
                        columns=(not re.search(r'--columns', test.command)))

        if use_stdin:
            fd = open(self.filename, encoding='utf-8')
            try:
                stdin = fd.read().encode('utf-8')
                p.stdin.write(stdin)
            finally:
                fd.close()
        p.stdin.close()

        success = True
        printed = False
        index   = 0
        if test.output is not None:
            process_output = harness.readlines(p.stdout)
            expected_output = test.output
            if sys.platform == 'win32':
                process_output = [l.replace('\r\n', '\n').replace('\\', '/')
                                  for l in process_output]
                # Replace \ with / in the expected output because the line above
                # makes it impossible for the process output to have a \.
                expected_output = [l.replace('\\', '/') for l in expected_output]

            for line in unified_diff(expected_output, process_output):
                index += 1
                if index < 3:
                    continue
                if not printed:
                    if success: print
                    self.notify_user("FAILURE in output from %s:" % self.filename, test)
                    success = False
                    printed = True
                print(' ', line,)

        printed = False
        index   = 0
        process_error = harness.readlines(p.stderr)
        if test.error is not None or process_error is not None:
            if test.error is None:
                test.error = []
            if sys.platform == 'win32':
                process_error = [l.replace('\r\n', '\n').replace('\\', '/')
                                 for l in process_error]
                test.error = [l.replace('\\', '/') for l in test.error]
            for line in unified_diff(test.error, process_error):
                index += 1
                if index < 3:
                    continue
                if not printed:
                    if success: print
                    self.notify_user("FAILURE in error output from %s:"
                                     % self.filename, test)
                    success = False
                    printed = True
                print(" ", line,)

        if test.exitcode == p.wait():
            if success:
                harness.success()
            else:
                harness.failure(self.filename.name)
                print("STDERR:")
                print(p.stderr.read())
        else:
            if success: print
            self.notify_user("FAILURE in exit code (%d != %d) from %s:"
                             % (test.exitcode, p.returncode, self.filename),
                             test)
            harness.failure(self.filename.name)

    def run_tests(self):
        if os.path.getsize(self.filename) == 0:
          print("WARNING: Empty testfile detected: %s" % (self.filename), file=sys.stderr)
          harness.failure(self.filename.name)
          return False
        test = self.read_test()
        while test:
            self.run_test(test)
            test = self.read_test()

    def close(self):
        self.fd.close()

def do_test(path):
    entry = RegressFile(path)
    entry.run_tests()
    entry.close()

if __name__ == '__main__':
    if multiproc:
        pool = Pool(args.jobs*2)
    else:
        pool = None

    if args.tests.is_dir():
        tests = [p for p in args.tests.iterdir()
                if (p.suffix == '.test' and
                    (not p.match('*_py.test') or (harness.python and
                                             not harness.verify)))]
        if pool:
            pool.map(do_test, tests, 1)
        else:
            map(do_test, tests)
    else:
        entry = RegressFile(args.tests)
        entry.run_tests()
        entry.close()

    if pool:
        pool.close()
        pool.join()

    harness.exit()
