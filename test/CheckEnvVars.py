#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#
# CheckEnvVars.py - Test that LEDGER_* environment variables and init files
# are honored.
#
# The .test harness always passes --args-only, which skips the environment
# and every init file, so these cases run ledger themselves, without it, in a
# scratch directory: with HOME pointing nowhere unless a case says otherwise,
# and otherwise with the environment LedgerHarness gives every test.

import argparse
import pathlib
import subprocess
import tempfile

from LedgerHarness import LedgerHarness, sanitized_env

JOURNAL = """\
2024/01/01 Test payee
    Expenses:Food    $25.00
    Assets:Checking
"""

BALANCE = ('             $-25.00  Assets:Checking\n'
           '              $25.00  Expenses:Food\n')
TOTAL = ('--------------------\n'
         '                   0\n')
COLORED = ('             \033[38;5;196m$-25.00\033[0m'
           '  \033[38;2;95;135;255mAssets:Checking\033[0m\n'
           '              $25.00  \033[38;2;95;135;255mExpenses:Food\033[0m\n')

# A journal that defines a value, so that a case can tell whether it was read,
# and one whose two transactions do not balance (two parse errors).
MARKED = ('define journal_marker=7\n'
          '2024/01/01 Test payee\n'
          '    Expenses:Food    $25.00\n'
          '    Assets:Checking\n')
UNBALANCED = ('2024/01/01 X\n'
              '    A  $1\n'
              '    B  $2\n'
              '\n'
              '2024/01/02 Y\n'
              '    A  $1\n'
              '    B  $2\n')

# Each case: (description, environment, init files {name: text}, arguments,
# expected stdout, expected exit status).  In every string $DIR is the case's
# own scratch directory (which holds its init files) and $JOURNAL the journal.
CASES = [
    ('LEDGER_INIT_FILE is honored (issue #1189)',
     {'LEDGER_INIT_FILE': '$DIR/ledgerrc'},
     {'ledgerrc': '--file $JOURNAL\n'},
     ['bal'], BALANCE + TOTAL, 0),
    ('Legacy LEDGER_INIT is honored',
     {'LEDGER_INIT': '$DIR/ledgerrc'},
     {'ledgerrc': '--file $JOURNAL\n'},
     ['bal'], BALANCE + TOTAL, 0),
    ('LEDGER_INIT_FILE takes priority over LEDGER_INIT',
     {'LEDGER_INIT_FILE': '$DIR/init_a', 'LEDGER_INIT': '$DIR/init_b'},
     {'init_a': '--file $JOURNAL\n--no-total\n', 'init_b': '--file $JOURNAL\n'},
     ['bal'], BALANCE, 0),
    ('LEDGER_COLORS is honored (issue #589)',
     {'LEDGER_COLORS': 'blue=#5f87ff,red=196'}, {},
     ['-f', '$JOURNAL', 'bal', '--force-color'], COLORED + TOTAL, 0),
    # '#' does not start a comment in an init file, and --colors there adds
    # up with --colors on the command line, which wins for red.
    ('--colors in an init file is honored (issue #589)',
     {'LEDGER_INIT_FILE': '$DIR/ledgerrc'},
     {'ledgerrc': '--colors blue=#5f87ff\n--colors red=bold 33\n'},
     ['-f', '$JOURNAL', 'bal', '--force-color', '--colors=red=196'],
     COLORED + TOTAL, 0),
    # In a double-quoted format, \" writes the expression's empty string "".
    ('A quoted format in ~/.ledgerrc is honored (issue #1884)',
     {'HOME': '$DIR'},
     {'.ledgerrc':
      r'--balance-format "%(justify(scrub(display_total), 20, -1, true, color))'
      r'  %(!options.flat ? depth_spacer : \"\")'
      r'%-(ansify_if(partial_account(options.flat), yellow if color))'
      r'\n%/%$1\n%/--------------------\n"' '\n'},
     ['-f', '$JOURNAL', 'bal', '--force-color'],
     '             \033[31m$-25.00\033[0m  \033[33mAssets:Checking\033[0m\n'
     '              $25.00  \033[33mExpenses:Food\033[0m\n' + TOTAL, 0),
    # An account check is an automated check, which an init file must not
    # hold.
    ('An account check in the init file is rejected', {},
     {'ledgerrc': 'account Assets:Checking\n    check amount < 1000\n'},
     ['--init-file', '$DIR/ledgerrc', '-f', '$JOURNAL', 'bal'], '', 1),
    # In batch mode execute_command reads the journal only for a regular
    # command.  A precommand (eval) runs after the init file has been read
    # and without the journal; a precommand that needs the journal (source)
    # reads it itself, and journal parse errors (error_count) set the exit
    # status.  (Under --script and at the REPL main() reads the journal
    # before the first command; these five rows are all batch runs.)
    ('eval sees definitions from the init file', {},
     {'ledgerrc': 'define init_marker=42\n', 'journal.dat': MARKED},
     ['--init-file', '$DIR/ledgerrc', '-f', '$DIR/journal.dat', 'eval', 'init_marker'],
     '42\n', 0),
    ('eval runs without the journal being read', {},
     {'ledgerrc': 'define init_marker=42\n', 'journal.dat': MARKED},
     ['--init-file', '$DIR/ledgerrc', '-f', '$DIR/journal.dat', 'eval', 'journal_marker'],
     '', 1),
    ('eval ignores parse errors in an unread journal', {},
     {'ledgerrc': 'define init_marker=42\n', 'unbalanced.dat': UNBALANCED},
     ['--init-file', '$DIR/ledgerrc', '-f', '$DIR/unbalanced.dat', 'eval', 'init_marker'],
     '42\n', 0),
    ('source reads the journal itself; the exit status counts its errors', {},
     {'ledgerrc': 'define init_marker=42\n', 'unbalanced.dat': UNBALANCED},
     ['--init-file', '$DIR/ledgerrc', '-f', '$DIR/unbalanced.dat', 'source'],
     '', 2),
    ('A regular command reads the journal data files', {},
     {'ledgerrc': 'define init_marker=42\n', 'journal.dat': MARKED},
     ['--init-file', '$DIR/ledgerrc', '-f', '$DIR/journal.dat', 'bal',
      '--format', '%(journal_marker)\n', '--no-total', '--flat'],
     '7\n7\n', 0),
]
# Every documented way of naming an init file on the command line reads it,
# and a missing one is an error.
for form in (['--init-file', '$DIR/$NAME'], ['--init-file=$DIR/$NAME'],
             ['-i', '$DIR/$NAME']):
    flag = form[0].split('=')[0]
    CASES.append((f'{flag} names the init file ({" ".join(form)})', {},
                  {'ledgerrc': '--no-total\n'},
                  [a.replace('$NAME', 'ledgerrc') for a in form]
                  + ['-f', '$JOURNAL', 'bal'], BALANCE, 0))
    CASES.append((f'A missing init file is an error ({" ".join(form)})', {}, {},
                  [a.replace('$NAME', 'missing') for a in form]
                  + ['-f', '$JOURNAL', 'bal'], '', 1))

args = argparse.ArgumentParser(prog='CheckEnvVars',
                               parents=[LedgerHarness.parser()]).parse_args()
harness = LedgerHarness(args.ledger, args.sourcepath, timeout=args.timeout)

with tempfile.TemporaryDirectory() as tmpdir:
    journal = pathlib.Path(tmpdir) / 'journal.dat'
    journal.write_text(JOURNAL)
    for index, (description, env_vars, init_files, argv, expected,
                expected_exit) in enumerate(CASES):
        case_dir = pathlib.Path(tmpdir) / f'case{index}'
        case_dir.mkdir()

        def fill(text):
            return text.replace('$DIR', str(case_dir)).replace('$JOURNAL', str(journal))

        for name, text in init_files.items():
            (case_dir / name).write_text(fill(text))
        env = sanitized_env()
        env['HOME'] = '/nonexistent'
        env.update({k: fill(v) for k, v in env_vars.items()})
        try:
            # The shared scratch directory holds no .ledgerrc, so ./.ledgerrc,
            # ledger's last init-file fallback, is never read from the caller's.
            result = subprocess.run([str(harness.ledger), *map(fill, argv), '--columns=80'],
                                    env=env, cwd=tmpdir, capture_output=True, text=True,
                                    timeout=harness.timeout)
        except subprocess.TimeoutExpired:
            harness.failure(description)
            print(f'\n  timed out after {harness.timeout}s')
            continue
        if result.stdout == expected and result.returncode == expected_exit:
            harness.success()
            continue
        harness.failure(description)
        if result.stdout != expected:
            print(f'\n  Expected output: {expected!r}')
            print(f'  Actual output:   {result.stdout!r}')
        if result.returncode != expected_exit:
            print(f'  Expected exit: {expected_exit}, Actual exit: {result.returncode}')
        if result.stderr:
            print(f'  Stderr: {result.stderr.strip()}')

harness.exit()
