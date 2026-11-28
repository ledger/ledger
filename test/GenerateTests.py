#!/usr/bin/env python3

# Round-trip check: for each seed N in [beg_range, end_range), the balance of
# the journal `ledger --seed=N generate` writes must equal the balance of its
# `print`ed form.

import argparse
import pathlib
import sys
import os

from difflib import ndiff

from LedgerHarness import LedgerHarness

parser = argparse.ArgumentParser(prog='GenerateTests', parents=[LedgerHarness.parser()])
parser.add_argument('tests', type=pathlib.Path)
parser.add_argument('beg_range', nargs='?', type=int, default=1)
parser.add_argument('end_range', nargs='?', type=int, default=20)
args = parser.parse_args()
harness = LedgerHarness(args.ledger, args.sourcepath, args.verify, args.gmalloc, args.python,
                        args.timeout)

if not os.path.isdir(args.tests) and not os.path.isfile(args.tests):
    print(f'{args.tests} is not a directory or file (cwd: {os.getcwd()})'
          , file=sys.stderr)
    sys.exit(1)

def generation_test(seed):
    p_gen = harness.run('$ledger --seed=%d generate' % seed)
    cout = harness.wait(p_gen, msg=("Generation for seed %d failed:" % seed))
    if cout is None:
        return False

    p_print = harness.run('$ledger --actual -f - print')
    p_print_out = harness.wait(p_print, msg=("Print for seed %d failed:" % seed),
                               input=cout)
    if p_print_out is None:
        return False

    p_cout_bal = harness.run('$ledger -f - bal')
    cout_bal = harness.wait(p_cout_bal, msg=("Stdout balance for seed %d failed:" % seed),
                            input=cout)
    if cout_bal is None:
        return False
    cout_lines = harness.lines(cout_bal)
    if len(cout_lines) == 0:
      return False

    p_print_bal = harness.run('$ledger -f - bal')
    print_bal = harness.wait(p_print_bal, msg=("Print balance for seed %d failed:" % seed),
                             input=p_print_out)
    if print_bal is None:
        return False
    print_lines = harness.lines(print_bal)
    if len(print_lines) == 0:
      return False

    success = True
    printed = False
    for line in ndiff(cout_lines, print_lines, charjunk=None):
        if line[:2] == "  ":
            continue
        if not printed:
            if success: print()
            print("Generation failure in output from seed %d (cout vs. print):" % seed)
            success = False
            printed = True
        print(" ", line)

    return success

def run_gen_test(i):
    if generation_test(i):
        harness.success()
    else:
        harness.failure()
    return harness.failed

for i in range(args.beg_range, args.end_range):
    run_gen_test(i)

harness.exit()
