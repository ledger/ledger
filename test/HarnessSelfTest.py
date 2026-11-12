#!/usr/bin/env python3

# Checks the rules of the test harnesses themselves, which no .test file can
# check.  Each section below states one harness's rule and fails when the
# harness breaks it.

import argparse
import os
import pathlib
import subprocess
import sys
import tempfile
import time

HERE = pathlib.Path(__file__).resolve().parent

parser = argparse.ArgumentParser(prog='HarnessSelfTest')
parser.add_argument('-l', '--ledger', type=pathlib.Path, required=True)
parser.add_argument('-s', '--sourcepath', type=pathlib.Path, required=True)
args = parser.parse_args()

JOURNAL = """2024/01/01 Payee
    Expenses:Food    $10.00
    Assets:Cash

"""

failures = []


def check(name, ok, detail=''):
    print(('PASS ' if ok else 'FAIL ') + name)
    if not ok:
        print(detail)
        failures.append(name)


def run_harness(script, argv, cwd):
    """Run SCRIPT with ARGV; return (exit status, stdout+stderr, seconds)."""
    start = time.monotonic()
    p = subprocess.run([sys.executable, '-B', str(HERE / script), *argv],
                       cwd=cwd, stdout=subprocess.PIPE,
                       stderr=subprocess.STDOUT, timeout=120)
    return (p.returncode, p.stdout.decode('utf-8', errors='replace'),
            time.monotonic() - start)


def regress(tmp, name, text, ledger=None, extra=()):
    path = tmp / (name + '.test')
    path.write_text(text, encoding='utf-8')
    return run_harness('RegressTests.py',
                       ['--ledger', str(ledger or args.ledger),
                        '--sourcepath', str(args.sourcepath), *extra,
                        str(path)], tmp)


def script(tmp, name, body):
    """A stand-in ledger: a shell script NAME with BODY."""
    path = tmp / name
    path.write_text('#!/bin/sh\n' + body)
    path.chmod(0o755)
    return path


def gone(pid):
    """True once PID no longer exists (allowing init a moment to reap it).
    os.kill(PID, 0) succeeds on a zombie, so this assumes that whatever
    adopts an orphan reaps it, as init, launchd, GitHub's runners and the
    nix sandbox do; a container started without an init would not."""
    for _ in range(50):
        try:
            os.kill(pid, 0)
        except ProcessLookupError:
            return True
        time.sleep(0.1)
    return False


def regress_empty_block(tmp):
    """RegressTests.py: a block with neither expected output nor an
    __ERROR__ section asserts empty stdout; an __ERROR__-only block keeps
    stdout a don't-care."""
    rc, out, _ = regress(tmp, 'empty_prints',
                         JOURNAL + 'test bal\nend test\n')
    check('an empty block fails when the command prints', rc == 1, out)

    rc, out, _ = regress(tmp, 'empty_silent',
                         JOURNAL + 'test bal Equity\nend test\n')
    check('an empty block passes when the command prints nothing',
          rc == 0, out)

    rc, out, _ = regress(tmp, 'error_only',
                         JOURNAL + 'test period "blah" -> 1\n__ERROR__\n'
                         "Error: Unexpected date period token 'blah'\n"
                         'end test\n')
    check('an __ERROR__-only block ignores stdout', rc == 0, out)

    # Output lines split at newlines only, as before: a form feed inside
    # a line stays inside it.
    if os.name == 'posix':
        printer = script(tmp, 'fake-printer', "printf 'a\\fb\\n'\n")
        rc, out, _ = regress(tmp, 'form_feed', 'test bal\na\fb\nend test\n',
                             ledger=printer)
        check('a form feed does not split an output line', rc == 0, out)


def regress_timeout(tmp):
    """RegressTests.py: a command that outlives --timeout is killed with
    every process of its pipeline and counted as a failure."""
    pidfile = tmp / 'pids'
    fake = script(tmp, 'fake-hung',
                  'echo $$ >> "%s"\necho start\nexec sleep 30\n' % pidfile)
    for name, command, expected in (
            ('plain', 'bal', 'start'),
            ('pipeline', 'bal | sed s/start/begun/', 'begun')):
        pidfile.write_text('')
        rc, out, secs = regress(tmp, 'hang_' + name,
                                'test %s\n%s\nend test\n'
                                % (command, expected),
                                ledger=fake, extra=('--timeout', '2'))
        check('a hung %s command times out' % name,
              rc == 1 and 'TIMEOUT' in out and secs < 20,
              'rc=%d after %.1fs\n%s' % (rc, secs, out))
        pids = [int(p) for p in pidfile.read_text().split()]
        check('a hung %s command leaves no process behind' % name,
              pids != [] and all(gone(pid) for pid in pids),
              'pids: %s' % pids)


def harness_wait(tmp):
    """LedgerHarness.wait(), which GenerateTests.py drives: it feeds its
    input, returns stdout on exit 0, and otherwise (a failure or a
    timeout, which kills the process) counts a failure and returns None."""
    from LedgerHarness import LedgerHarness

    def harness(ledger):
        return LedgerHarness(ledger, args.sourcepath, timeout=2)

    echo = harness(script(tmp, 'fake-cat', 'exec cat\n'))
    out = echo.wait(echo.run('$ledger'), input=b'fed\n')
    check('wait() feeds its input and returns stdout',
          out == b'fed\n' and echo.failed == 0, repr(out))

    failing = harness(script(tmp, 'fake-fail', 'echo oops >&2\nexit 3\n'))
    out = failing.wait(failing.run('$ledger'))
    check('wait() counts a failing command', out is None
          and failing.failed == 1, repr(out))

    pidfile = tmp / 'wait-pids'
    pidfile.write_text('')
    hung = harness(script(tmp, 'fake-wait-hung',
                          'echo $$ >> "%s"\nexec sleep 30\n' % pidfile))
    start = time.monotonic()
    out = hung.wait(hung.run('$ledger | cat'))
    secs = time.monotonic() - start
    pids = [int(p) for p in pidfile.read_text().split()]
    check('wait() times out a hung command and counts it',
          out is None and hung.failed == 1 and secs < 20,
          '%r after %.1fs' % (out, secs))
    check('wait() leaves no process behind',
          pids != [] and all(gone(pid) for pid in pids), 'pids: %s' % pids)


SECTIONS = [regress_empty_block, regress_timeout, harness_wait]
POSIX_ONLY = {regress_timeout, harness_wait}

with tempfile.TemporaryDirectory() as tmpdir:
    for section in SECTIONS:
        if section in POSIX_ONLY and os.name != 'posix':
            continue
        tmp = pathlib.Path(tmpdir) / section.__name__
        tmp.mkdir()
        try:
            section(tmp)
        except Exception as e:
            check(section.__name__ + ' ran', False,
                  '%s: %s' % (type(e).__name__, e))

print('%d failure(s)' % len(failures))
sys.exit(1 if failures else 0)
