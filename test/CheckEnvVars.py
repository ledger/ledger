#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#
# CheckEnvVars.py - Test that LEDGER_* environment variables are honored.
#
# This test runs ledger directly (without --args-only) to verify that
# environment variables like LEDGER_INIT_FILE are properly processed.
# The standard test harness uses --args-only which skips env var processing,
# so this script is needed for env var testing.

import argparse
import os
import pathlib
import subprocess
import sys
import tempfile

def parser():
    p = argparse.ArgumentParser(add_help=False)
    p.add_argument('-l', '--ledger', type=pathlib.Path, required=True)
    p.add_argument('-s', '--sourcepath', type=pathlib.Path, required=True)
    return p

args = parser().parse_args()

ledger = str(args.ledger.resolve())

succeeded = 0
failed = 0

def run_test(description, env_vars, args_list, expected_output, expected_exit=0):
    global succeeded, failed

    env = {'TZ': 'America/Chicago', 'HOME': '/nonexistent'}
    env.update(env_vars)

    try:
        result = subprocess.run(
            [ledger] + args_list,
            env=env,
            capture_output=True,
            text=True,
            timeout=10
        )
        actual = result.stdout
        exit_code = result.returncode

        if actual.strip() == expected_output.strip() and exit_code == expected_exit:
            sys.stdout.write('.')
            sys.stdout.flush()
            succeeded += 1
        else:
            sys.stdout.write('E')
            sys.stdout.flush()
            failed += 1
            print(f'\nFAILURE: {description}')
            if actual.strip() != expected_output.strip():
                print(f'  Expected output: {repr(expected_output.strip())}')
                print(f'  Actual output:   {repr(actual.strip())}')
            if exit_code != expected_exit:
                print(f'  Expected exit: {expected_exit}, Actual exit: {exit_code}')
            if result.stderr:
                print(f'  Stderr: {result.stderr.strip()}')
    except subprocess.TimeoutExpired:
        sys.stdout.write('E')
        sys.stdout.flush()
        failed += 1
        print(f'\nFAILURE: {description} (timeout)')

# Test 1: LEDGER_INIT_FILE sets the init file path (issue #1189)
with tempfile.TemporaryDirectory() as tmpdir:
    journal_file = os.path.join(tmpdir, 'journal.dat')
    init_file = os.path.join(tmpdir, 'ledgerrc')

    with open(journal_file, 'w') as f:
        f.write('2024/01/01 Test payee\n')
        f.write('    Expenses:Food    $25.00\n')
        f.write('    Assets:Checking\n')

    with open(init_file, 'w') as f:
        f.write(f'--file {journal_file}\n')

    run_test(
        'LEDGER_INIT_FILE is honored (issue #1189)',
        {'LEDGER_INIT_FILE': init_file},
        ['bal', '--columns=80'],
            '             $-25.00  Assets:Checking\n'
            '              $25.00  Expenses:Food\n'
            '--------------------\n'
            '                   0',
        expected_exit=0
    )

# Test 2: Legacy LEDGER_INIT also sets the init file path
with tempfile.TemporaryDirectory() as tmpdir:
    journal_file = os.path.join(tmpdir, 'journal.dat')
    init_file = os.path.join(tmpdir, 'ledgerrc')

    with open(journal_file, 'w') as f:
        f.write('2024/06/01 Coffee shop\n')
        f.write('    Expenses:Coffee    $5.50\n')
        f.write('    Assets:Cash\n')

    with open(init_file, 'w') as f:
        f.write(f'--file {journal_file}\n')

    run_test(
        'Legacy LEDGER_INIT is honored',
        {'LEDGER_INIT': init_file},
        ['bal', '--columns=80'],
            '              $-5.50  Assets:Cash\n'
            '               $5.50  Expenses:Coffee\n'
            '--------------------\n'
            '                   0',
        expected_exit=0
    )

# Test 3: LEDGER_INIT_FILE takes priority over LEDGER_INIT
with tempfile.TemporaryDirectory() as tmpdir:
    journal_a = os.path.join(tmpdir, 'journal_a.dat')
    journal_b = os.path.join(tmpdir, 'journal_b.dat')
    init_a = os.path.join(tmpdir, 'init_a.ledgerrc')
    init_b = os.path.join(tmpdir, 'init_b.ledgerrc')

    with open(journal_a, 'w') as f:
        f.write('2024/01/01 From A\n')
        f.write('    Expenses:A    $10.00\n')
        f.write('    Assets:Cash\n')

    with open(journal_b, 'w') as f:
        f.write('2024/01/01 From B\n')
        f.write('    Expenses:B    $20.00\n')
        f.write('    Assets:Cash\n')

    with open(init_a, 'w') as f:
        f.write(f'--file {journal_a}\n')

    with open(init_b, 'w') as f:
        f.write(f'--file {journal_b}\n')

    run_test(
        'LEDGER_INIT_FILE takes priority over LEDGER_INIT',
        {'LEDGER_INIT_FILE': init_a, 'LEDGER_INIT': init_b},
        ['bal', 'Expenses:A', '--columns=80'],
            '              $10.00  Expenses:A',
        expected_exit=0
    )

print()
if succeeded > 0:
    print(f'OK ({succeeded})')
if failed > 0:
    print(f'FAILED ({failed})')
print()

sys.exit(failed)
