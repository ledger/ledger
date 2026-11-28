#!/usr/bin/env python3
# The scripts that run ledger outside the .test harness must give it the
# environment LedgerHarness gives every .test file: the caller's sanitizer
# and coverage settings reach ledger, and nothing that configures ledger
# does (no LEDGER_* variable, no XDG_CONFIG_HOME, no ./.ledgerrc in the
# caller's directory).  The option list that the CheckOptions scripts and
# snapshot.py both read comes from one function, which reports what ledger
# lists and leaves the policy to its callers: the option checks skip when
# ledger lists no options, while snapshot.py then covers its own option
# matrix.
#
# Usage: harness-environment.py LEDGER SOURCE_DIR

import os
import shlex
import subprocess
import sys
import tempfile

LEDGER = os.path.realpath(sys.argv[1])
SOURCE = os.path.realpath(sys.argv[2])
TEST_DIR = os.path.join(SOURCE, 'test')
sys.path.insert(0, TEST_DIR)

failures = []


def check(label, ok, detail=''):
    if not ok:
        failures.append('%s\n%s' % (label, detail) if detail else label)


def stub_ledger(directory, name, body):
    path = os.path.join(directory, name)
    with open(path, 'w') as f:
        f.write('#!/bin/sh\n' + body)
    os.chmod(path, 0o755)
    return path


def run(argv, env=None, cwd=None):
    result = subprocess.run(argv, env=env, cwd=cwd, stdout=subprocess.PIPE,
                            stderr=subprocess.STDOUT, text=True)
    return result.returncode, result.stdout


def write(path, text):
    os.makedirs(os.path.dirname(path), exist_ok=True)
    with open(path, 'w') as f:
        f.write(text)


with tempfile.TemporaryDirectory() as tmp:
    silent = stub_ledger(tmp, 'ledger-without-debug', 'exit 0\n')
    broken = stub_ledger(tmp, 'ledger-that-fails', 'exit 3\n')

    # The shared environment.
    try:
        from LedgerHarness import sanitized_env
    except ImportError as error:
        check('LedgerHarness exports sanitized_env()', False, str(error))
    else:
        saved = dict(os.environ)
        os.environ.update({'LEDGER_DEPTH': '1', 'XDG_CONFIG_HOME': tmp,
                           'UBSAN_OPTIONS': 'print_stacktrace=1',
                           'GCOV_PREFIX': tmp, 'TZ': 'UTC'})
        try:
            env = sanitized_env()
        finally:
            os.environ.clear()
            os.environ.update(saved)
        check('sanitized_env() drops LEDGER_* and XDG_CONFIG_HOME',
              'LEDGER_DEPTH' not in env and 'XDG_CONFIG_HOME' not in env, repr(env))
        check('sanitized_env() keeps the sanitizer and coverage settings',
              env.get('UBSAN_OPTIONS') == 'print_stacktrace=1'
              and env.get('GCOV_PREFIX') == tmp, repr(env))
        check("sanitized_env() sets the suite's time zone",
              env.get('TZ') == 'America/Chicago', repr(env))
        env['TZ'] = 'UTC'
        check('sanitized_env() returns a new dict on each call',
              sanitized_env()['TZ'] == 'America/Chicago')

    # The shared option list: the set ledger lists, with no exit policy.
    try:
        from CheckOptions import ledger_option_names
    except ImportError as error:
        check('CheckOptions exports ledger_option_names()', False, str(error))
    else:
        try:
            names = ledger_option_names(silent)
        except SystemExit as error:
            check('ledger_option_names() returns an empty set when ledger lists no options',
                  False, 'it exited with status %s' % error.code)
        else:
            check('ledger_option_names() returns an empty set when ledger lists no options',
                  names == set(), repr(names))
        try:
            ledger_option_names(broken)
        except subprocess.CalledProcessError as error:
            check('ledger_option_names() reports the status of a failing ledger',
                  error.returncode == 3, str(error))
        except SystemExit as error:
            check('ledger_option_names() raises CalledProcessError when ledger fails',
                  False, 'it exited with status %s' % error.code)
        else:
            check('ledger_option_names() raises CalledProcessError when ledger fails', False)

    # snapshot.py covers its option matrix without a Debug build, and stops
    # when ledger cannot list its options.
    snapshot = os.path.join(TEST_DIR, 'snapshot.py')
    journal = os.path.join(TEST_DIR, 'input', 'sample.dat')
    status, output = run([sys.executable, '-B', snapshot, silent, journal,
                          os.path.join(tmp, 'snap'), '--only-curated', '--dry-run'])
    check('snapshot.py covers its option matrix when ledger lists no options',
          status == 0 and 'combinations' in output, 'exit %d\n%s' % (status, output[-1500:]))
    status, output = run([sys.executable, '-B', snapshot, broken, journal,
                          os.path.join(tmp, 'snap'), '--only-curated', '--dry-run'])
    check('snapshot.py stops when ledger fails while listing its options',
          status != 0 and 'non-zero exit status 3' in output,
          'exit %d\n%s' % (status, output[-1500:]))

    # CheckEnvVars runs ledger with the shared environment, from a scratch
    # directory.
    def check_env_vars(ledger, env=None, cwd=None):
        return run([sys.executable, '-B', os.path.join(TEST_DIR, 'CheckEnvVars.py'),
                    '--ledger', ledger, '--sourcepath', SOURCE], env=env, cwd=cwd)

    ubsan = os.environ.get('UBSAN_OPTIONS') or 'print_stacktrace=1'
    needs_ubsan = stub_ledger(
        tmp, 'ledger-that-needs-ubsan-options',
        '[ "$UBSAN_OPTIONS" = %s ] || { echo "UBSAN_OPTIONS did not reach ledger" >&2; exit 99; }\n'
        'exec %s "$@"\n' % (shlex.quote(ubsan), shlex.quote(LEDGER)))
    status, output = check_env_vars(needs_ubsan, env={**os.environ, 'UBSAN_OPTIONS': ubsan})
    check('CheckEnvVars passes the sanitizer settings on to ledger',
          status == 0 and 'FAILED' not in output, output)

    caller = os.path.join(tmp, 'caller')
    write(os.path.join(caller, '.ledgerrc'), '--depth 1\n')
    status, output = check_env_vars(LEDGER, cwd=caller)
    check("CheckEnvVars does not read the caller's ./.ledgerrc",
          status == 0 and 'FAILED' not in output, output)

    xdg = os.path.join(tmp, 'xdg')
    write(os.path.join(xdg, 'ledger', 'ledgerrc'), '--depth 1\n')
    status, output = check_env_vars(LEDGER, env={**os.environ, 'LEDGER_DEPTH': '1',
                                                 'LEDGER_FILE': '/nonexistent',
                                                 'XDG_CONFIG_HOME': xdg})
    check("CheckEnvVars keeps the caller's LEDGER_* variables and XDG_CONFIG_HOME from ledger",
          status == 0 and 'FAILED' not in output, output)

    # The valuation-footing driver gives ledger the shared environment too.
    # (The bisimulation drivers build theirs the same way, but need the Lean
    # oracle to run.)
    strict = stub_ledger(
        tmp, 'ledger-that-refuses-caller-settings',
        '[ -z "${LEDGER_DEPTH+set}" ] && [ "$TZ" = America/Chicago ] || '
        '{ echo "the caller\'s environment reached ledger" >&2; exit 99; }\n'
        'exec %s "$@"\n' % shlex.quote(LEDGER))
    corpus = os.path.join(tmp, 'corpus')
    write(os.path.join(corpus, 'priced.test'),
          '2024/01/01 Buy\n'
          '    Assets:Brokerage    10 AAPL @ $150\n'
          '    Assets:Cash\n'
          '\n'
          'P 2024/03/01 AAPL $170\n'
          '\n'
          'test bal Cash\n'
          '           $-1500.00  Assets:Cash\n'
          'end test\n')
    status, output = run([sys.executable, '-B',
                          os.path.join(TEST_DIR, 'semantic', 'footing.py'),
                          '--ledger', strict, '--source', SOURCE, corpus],
                         env={**os.environ, 'LEDGER_DEPTH': '1', 'TZ': 'UTC'})
    check("footing.py keeps the caller's LEDGER_* variables and time zone from ledger",
          status == 0 and 'FOOT:PASS' in output, 'exit %d\n%s' % (status, output[-1500:]))

for failure in failures:
    print('FAIL: ' + failure.split('\n')[0])
    for line in failure.split('\n')[1:]:
        print('  ' + line)
print('%d failure(s)' % len(failures))
sys.exit(1 if failures else 0)
