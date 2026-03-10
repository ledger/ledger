#!/usr/bin/env python3

import argparse
import pathlib
import subprocess
import sys
import os
import re

from difflib import unified_diff

from LedgerHarness import LedgerHarness

harness = None

class RegressFile(object):
    def __init__(self, filename):
        self.filename = filename
        self.fd = open(self.filename, encoding='utf-8')
        self.line_num = 0

    def readline(self):
        self.line_num += 1
        return self.fd.readline()

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
        test_start_line = 0

        line = self.readline()
        test = Test()
        while line:
            if line.startswith("test "):
                if in_output:
                    print("ERROR: %s:%d: new 'test' directive without preceding "
                          "'end test' (block started at line %d)"
                          % (self.filename, self.line_num, test_start_line),
                          file=sys.stderr)
                    harness.failure(self.filename.name)
                    return None
                command = line[5:]
                if not command.strip():
                    print("ERROR: %s:%d: empty command after 'test' directive"
                          % (self.filename, self.line_num), file=sys.stderr)
                    harness.failure(self.filename.name)
                    return None
                match = re.match(r'(.*) -> ([0-9]+)', command)
                if match:
                    test.command = self.transform_line(match.group(1))
                    test.exitcode = int(match.group(2))
                else:
                    test.command = self.transform_line(command)
                in_output = True
                test_start_line = self.line_num

            elif in_output:
                if line.rstrip() == "end test":
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
            line = self.readline()

        if in_output:
            print("ERROR: %s:%d: unterminated test block "
                  "(missing 'end test' before end of file)"
                  % (self.filename, test_start_line),
                  file=sys.stderr)
            harness.failure(self.filename.name)
            return None

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
                harness.skip()
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
                    if success: print()
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
                    if success: print()
                    self.notify_user("FAILURE in error output from %s:"
                                     % self.filename, test)
                    success = False
                    printed = True
                print(" ", line,)

        try:
            exit_code = p.wait(timeout=60)
        except subprocess.TimeoutExpired:
            p.kill()
            p.wait()
            self.notify_user("TIMEOUT in %s:" % self.filename, test)
            harness.failure(self.filename.name)
            return
        if test.exitcode == exit_code:
            if success:
                harness.success()
            else:
                harness.failure(self.filename.name)
                print("STDERR:")
                print(''.join(process_error))
        else:
            if success: print()
            self.notify_user("FAILURE in exit code (%d != %d) from %s:"
                             % (test.exitcode, p.returncode, self.filename),
                             test)
            harness.failure(self.filename.name)

    def run_tests(self):
        if os.path.getsize(self.filename) == 0:
          print("WARNING: Empty testfile detected: %s" % (self.filename), file=sys.stderr)
          harness.failure(self.filename.name)
          return False
        tests_found = 0
        test = self.read_test()
        while test:
            tests_found += 1
            self.run_test(test)
            test = self.read_test()
        if tests_found == 0:
            print("WARNING: No test blocks found in: %s" % (self.filename), file=sys.stderr)
            harness.failure(self.filename.name)
            return False

    def close(self):
        self.fd.close()

    def __enter__(self):
        return self

    def __exit__(self, *args):
        self.close()

def do_test(path):
    with RegressFile(path) as entry:
        entry.run_tests()

if __name__ == '__main__':
    parser = argparse.ArgumentParser(prog='RegressTests',
                                     parents=[LedgerHarness.parser()])
    parser.add_argument('tests', type=pathlib.Path)
    args = parser.parse_args()
    harness = LedgerHarness(args.ledger, args.sourcepath, args.verify,
                            args.gmalloc, args.python)

    match = re.match(r'(Baseline|Regress|Manual)Test_(.*)', str(args.tests))
    if match:
        args.tests = pathlib.Path('test') / match.group(1).lower() / \
            (match.group(2) + '.test')

    if not args.tests.is_dir() and not args.tests.is_file():
        print(f'{args.tests} is not a directory or file (cwd: {os.getcwd()})',
              file=sys.stderr)
        sys.exit(1)

    if args.tests.is_dir():
        tests = sorted([p for p in args.tests.iterdir()
                if (p.suffix == '.test'
                    and p.resolve().is_relative_to(args.tests.resolve())
                    and (not p.match('*_py.test') or (harness.python and
                                                 not harness.verify)))])
        for t in tests:
            do_test(t)
    else:
        with RegressFile(args.tests) as entry:
            entry.run_tests()

    harness.exit()
