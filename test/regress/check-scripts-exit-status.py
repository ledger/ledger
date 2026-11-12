#!/usr/bin/env python3
# The check scripts (CheckBaselineTests, CheckManpage, CheckTexinfo, DocTests,
# CheckEnvVars) report only through their exit status, so that status must
# be 0 (pass), 1 (fail) or 77 (skip) and nothing else.  A failure count
# would wrap modulo 256, and 256 failures would read as success.  A ledger
# built without debug support lists no options under
# `--debug option.names`, so a check that needs that list must skip, not
# pass having checked nothing.  A ledger that fails while listing them must
# fail the check, not skip it.  Each expected failure must also print the
# script's own failure message, so a Python traceback (also exit 1) cannot
# satisfy this test.
#
# Usage: check-scripts-exit-status.py SOURCE_DIR

import os
import subprocess
import sys
import tempfile

SOURCE = os.path.realpath(sys.argv[1])
TEST_DIR = os.path.join(SOURCE, 'test')

# Two examples that name no command: DocTests counts each as a failure
# without running ledger.
TWO_FAILING_EXAMPLES = """\
@smallexample @c output:AAAAAAA
x
@end smallexample
@smallexample @c output:BBBBBBB
y
@end smallexample
"""

OPTION_CHECKS = ('CheckBaselineTests.py', 'CheckCompletion.py', 'CheckManpage.py', 'CheckTexinfo.py')

failures = []


def run(script, *args):
    result = subprocess.run([sys.executable, os.path.join(TEST_DIR, script), *args],
                            stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True)
    return result.returncode, result.stdout


def stub_ledger(directory, name, body):
    path = os.path.join(directory, name)
    with open(path, 'w') as f:
        f.write('#!/bin/sh\n' + body)
    os.chmod(path, 0o755)
    return path


def expect(label, outcome, wanted_status, wanted_text):
    status, output = outcome
    if status != wanted_status:
        failures.append('%s: exit status %d, expected %d' % (label, status, wanted_status))
    elif wanted_text not in output:
        failures.append('%s: output lacks %r:\n%s' % (label, wanted_text, output))


with tempfile.TemporaryDirectory() as tmp:
    silent = stub_ledger(tmp, 'ledger-without-debug', 'exit 0\n')
    broken = stub_ledger(tmp, 'ledger-that-fails', 'exit 3\n')
    # Fails as ledger does when the user's init file is broken, unless
    # --args-only tells it not to read that file.
    init_file_fails = stub_ledger(
        tmp, 'ledger-with-a-broken-init-file',
        'for arg in "$@"; do [ "$arg" = --args-only ] && exit 0; done\nexit 3\n')

    texi = os.path.join(tmp, 'two-failures.texi')
    with open(texi, 'w') as f:
        f.write(TWO_FAILING_EXAMPLES)
    expect('DocTests.py with two failing examples',
           run('DocTests.py', '--ledger', silent, '--file', texi),
           1, 'The following examples failed:')

    expect('CheckEnvVars.py with a ledger that prints nothing',
           run('CheckEnvVars.py', '--ledger', silent, '--sourcepath', SOURCE),
           1, 'FAILED (')

    for check in OPTION_CHECKS:
        expect('%s with a ledger that lists no options' % check,
               run(check, '--ledger', silent, '--source', SOURCE),
               77, 'lists no options')
        expect('%s with a ledger that exits 3' % check,
               run(check, '--ledger', broken, '--source', SOURCE),
               1, 'exited with status 3')
        expect('%s with a broken init file' % check,
               run(check, '--ledger', init_file_fails, '--source', SOURCE),
               77, 'lists no options')

for failure in failures:
    print(failure)
sys.exit(1 if failures else 0)
