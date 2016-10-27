#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import print_function

import sys
import re
import os
import argparse

from os.path import *
from subprocess import Popen, PIPE

from CheckOptions import CheckOptions

class CheckBaselineTests (CheckOptions):
  def __init__(self, args):
    CheckOptions.__init__(self, args)
    self.missing_baseline_tests = set()

    self.untested_options = [
        'anon',
        'args-only',
        'debug',
        'download',
        'force-pager',
        'generated',
        'help',
        'import',
        'no-color',
        'no-pager',
        'options',
        'price-exp',
        'revalued-total',
        'seed',
        'trace',
        'verbose',
        'verify',
        'verify-memory',
        'version'
    ]

  def main(self):
    for option in self.ledger_options():
      if option in self.untested_options: continue
      baseline_testpath = join(self.source, 'test', 'baseline', 'opt-%s.test' % option)
      if exists(baseline_testpath) and getsize(baseline_testpath) > 0: continue
      self.missing_baseline_tests.add(option)

    if len(self.missing_baseline_tests):
      print("Missing Baseline test for:%s%s\n" % (self.sep, self.sep.join(sorted(list(self.missing_baseline_tests)))))

    errors = len(self.missing_baseline_tests)
    return errors

if __name__ == "__main__":
  def getargs():
    parser = argparse.ArgumentParser(prog='CheckBaselineTests',
            description='Check that ledger options are tested')
    parser.add_argument('-l', '--ledger',
        dest='ledger',
        type=str,
        action='store',
        required=True,
        help='the path to the ledger executable to test with')
    parser.add_argument('-s', '--source',
        dest='source',
        type=str,
        action='store',
        required=True,
        help='the path to the top level ledger source directory')
    return parser.parse_args()

  args = getargs()
  script = CheckBaselineTests(args)
  status = script.main()
  sys.exit(status)
