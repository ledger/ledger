#!/usr/bin/env python3

import argparse
import sys
import re
import os

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
  args = argparse.ArgumentParser(prog='CheckBaselineTests',
                                 description='Check that ledger options are tested',
                                 parents=[CheckOptions.parser()]).parse_args()
  script = CheckBaselineTests(args)
  status = script.main()
  sys.exit(status)
