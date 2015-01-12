#!/usr/bin/python

# This script confirms both that the register report "adds up", and that its
# final balance is the same as what the balance report shows.

import sys
import re

from difflib import ndiff

multiproc = False
try:
    from multiprocessing import Pool
    multiproc = True
except:
    pass

args = sys.argv
jobs = 1
match = re.match('-j([0-9]+)?', args[1])
if match:
    args = [args[0]] + args[2:]
    if match.group(1):
        jobs = int(match.group(1))
if jobs == 1:
    multiproc = False

from LedgerHarness import LedgerHarness

harness = LedgerHarness(args)

#def normalize(line):
#    match = re.match("((\s*)([A-Za-z]+)?(\s*)([-0-9.]+)(\s*)([A-Za-z]+)?)(  (.+))?$", line)
#    if match:
#        if match.group(3):
#            prefix = match.group(3) + " " + match.group(5)
#            if match.group(8):
#                return prefix + match.group(8)
#            return prefix
#        elif match.group(7):
#            prefix = match.group(7) + " " + match.group(5)
#            if match.group(8):
#                return prefix + match.group(8)
#            return prefix
#    return line

def generation_test(seed):
    p_gen = harness.run('$ledger --seed=%d generate' % seed)

    cout = harness.read(p_gen.stdout)

    if not harness.wait(p_gen, msg=("Generation for seed %d failed:" % seed)):
        return False

    p_print = harness.run('$ledger --actual -f - print')
    p_print.stdin.write(cout)
    p_print.stdin.close()
    p_print_out = p_print.stdout.read()

    if not harness.wait(p_print, msg=("Print for seed %d failed:" % seed)):
        return False

    #p_cerr_bal = Popen("%s --args-only -f - bal" % ledger, shell=True,
    #                   stdin=PIPE, stdout=PIPE, stderr=PIPE, close_fds=True)
    #p_cerr_bal.stdin.write(cerr)
    #p_cerr_bal.stdin.close()
    #
    #cerr_lines = [normalize(line) for line in p_cerr_bal.stdout.readlines()]
    #
    #if p_cerr_bal.wait() != 0:
    #    print "Stderr balance for seed %d failed due to error:" % seed
    #    print p_cerr_bal.stderr.read()
    #    del p_cerr_bal
    #    return False
    #del p_cerr_bal

    p_cout_bal = harness.run('$ledger -f - bal')
    p_cout_bal.stdin.write(cout)
    p_cout_bal.stdin.close()

    cout_lines = harness.readlines(p_cout_bal.stdout)
    if len(cout_lines) == 0:
      return False
    #norm_cout_lines = [normalize(line) for line in cout_lines]

    if not harness.wait(p_cout_bal, msg=("Stdout balance for seed %d failed:" % seed)):
        return False

    p_print_bal = harness.run('$ledger -f - bal')
    p_print_bal.stdin.write(p_print_out)
    p_print_bal.stdin.close()

    print_lines = harness.readlines(p_print_bal.stdout)
    if len(print_lines) == 0:
      return False

    if not harness.wait(p_print_bal, msg=("Print balance for seed %d failed:" % seed)):
        return False

    success = True
    #printed = False
    #for line in ndiff(cerr_lines, norm_cout_lines, charjunk=None):
    #    if line[:2] == "  ":
    #        continue
    #    if not printed:
    #        if success: print
    #        print "Generation failure in output from seed %d (cerr vs. cout):" % seed
    #        if success: failed += 1
    #        success = False
    #        printed = True
    #    print " ", line

    printed = False
    for line in ndiff(cout_lines, print_lines, charjunk=None):
        if line[:2] == "  ":
            continue
        if not printed:
            if success: print
            print "Generation failure in output from seed %d (cout vs. print):" % seed
            success = False
            printed = True
        print " ", line

    return success

beg_range = 1
end_range = 20
if len(args) > 4:
    beg_range = int(args[3])
    end_range = int(args[4])

def run_gen_test(i):
    if generation_test(i):
        harness.success()
    else:
        harness.failure()
    return harness.failed

if multiproc:
    pool = Pool(jobs*2)
else:
    pool = None

if pool:
    pool.map(run_gen_test, range(beg_range, end_range))
else:
    for i in range(beg_range, end_range):
        run_gen_test(i)

if pool:
    pool.close()
    pool.join()

harness.exit()
