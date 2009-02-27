#!/usr/bin/python

# This script confirms both that the register report "adds up", and that its
# final balance is the same as what the balance report shows.

import sys
import os
import re

from subprocess import Popen, PIPE
from difflib import ndiff

ledger    = sys.argv[1]
succeeded = 0
failed    = 0

if not os.path.isfile(ledger):
    sys.exit(1)

def normalize(line):
    match = re.match("((\s*)([A-Za-z]+)?(\s*)([-0-9.]+)(\s*)([A-Za-z]+)?)(  (.+))?$", line)
    if match:
        if match.group(3):
            prefix = match.group(3) + " " + match.group(5)
            if match.group(8):
                return prefix + match.group(8)
            return prefix
        elif match.group(7):
            prefix = match.group(7) + " " + match.group(5)
            if match.group(8):
                return prefix + match.group(8)
            return prefix
    return line

def generation_test(seed):
    global succeeded, failed

    p_gen = Popen("%s --args-only --actual --seed=%d generate" % (ledger, seed),
                  shell=True, stdin=PIPE, stdout=PIPE, stderr=PIPE,
                  close_fds=True)
    cout = ""
    cout_data = os.read(p_gen.stdout.fileno(), 8192)
    while cout_data:
        if cout_data:
            cout += cout_data
        cout_data = os.read(p_gen.stdout.fileno(), 8192)
    if cout_data:
        cout += cout_data

    if p_gen.wait() != 0:
        print "Generation for seed %d failed due to error:" % seed
        print p_gen.stderr.read()
        del p_gen
        return False
    del p_gen

    p_print = Popen("%s --args-only --actual -f - print" % ledger, shell=True,
                    stdin=PIPE, stdout=PIPE, stderr=PIPE, close_fds=True)
    p_print.stdin.write(cout)
    p_print.stdin.close()
    p_print_out = p_print.stdout.read()

    if p_print.wait() != 0:
        print "Print for seed %d failed due to error:" % seed
        print p_print.stderr.read()
        del p_print
        return False
    del p_print

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

    p_cout_bal = Popen("%s --args-only -f - bal" % ledger, shell=True,
                       stdin=PIPE, stdout=PIPE, stderr=PIPE, close_fds=True)
    p_cout_bal.stdin.write(cout)
    p_cout_bal.stdin.close()

    cout_lines = p_cout_bal.stdout.readlines()
    norm_cout_lines = [normalize(line) for line in cout_lines]

    if p_cout_bal.wait() != 0:
        print "Stdout balance for seed %d failed due to error:" % seed
        print p_cout_bal.stderr.read()
        del p_cout_bal
        return False
    del p_cout_bal

    p_print_bal = Popen("%s --args-only -f - bal" % ledger, shell=True,
                        stdin=PIPE, stdout=PIPE, stderr=PIPE, close_fds=True)
    p_print_bal.stdin.write(p_print_out)
    p_print_bal.stdin.close()

    print_lines = p_print_bal.stdout.readlines()

    if p_print_bal.wait() != 0:
        print "Print balance for seed %d failed due to error:" % seed
        print p_print_bal.stderr.read()
        del p_print_bal
        return False
    del p_print_bal

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
            if success: failed += 1
            success = False
            printed = True
        print " ", line

    return success

beg_range = 1
end_range = 20
if len(sys.argv) > 3:
    beg_range = int(sys.argv[2])
    end_range = int(sys.argv[3])

for i in range(beg_range, end_range):
    if generation_test(i):
        sys.stdout.write(".")
        succeeded += 1
    else:
        sys.stdout.write("E")
        failed += 1

print
if succeeded > 0:
    print "OK (%d) " % succeeded,
if failed > 0:
    print "FAILED (%d)" % failed,
print

sys.exit(failed)

