#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import print_function

import sys
import re
import os
import argparse

from os.path import *
from subprocess import Popen, PIPE

class CheckTests (object):
  def __init__(self, args):
    self.ledger     = os.path.abspath(args.ledger)
    self.source     = os.path.abspath(args.source)

    self.missing_baseline_tests = set()
    self.missing_texi_options = set()
    self.unknown_texi_options = set()
    self.missing_man_options = set()
    self.unknown_man_options = set()

    self.untested_options = [
        'anon',
        'args-only',
        'cache',
        'debug',
        'download',
        'file',
        'force-color',
        'force-pager',
        'full-help',
        'help',
        'help-calc',
        'help-comm',
        'help-disp',
        'import',
        'init-file',
        'no-color',
        'options',
        'price-db',
        'price-exp',
        'revalued-total',
        'script',
        'seed',
        'trace',
        'verbose',
        'verify',
        'version'
    ]

    self.known_alternates = [
        'cost',
        'first',
        'import',
        'last',
        'leeway',
        'period-sort'
    ]

  def find_options(self, pattern, filename):
    regex = re.compile(pattern)
    return {match.group(1) for match in {regex.match(line) for line in open(filename)} if match}

  def main(self):
    man_options = self.find_options('\.It Fl \\\\-([-A-Za-z]+)',
                                    join(self.source, 'doc', 'ledger.1'))

    texi_options = self.find_options('@item --([-A-Za-z]+).*@c option',
                                     join(self.source, 'doc', 'ledger3.texi'))

    pipe   = Popen('%s --debug option.names parse true' % self.ledger,
                   shell=True, stdout=PIPE, stderr=PIPE)
    regex = re.compile('\[DEBUG\] Option: (.*)')
    for line in filter(regex.search, [line.decode() for line in pipe.stderr]):
      match = regex.search(line)
      option = match.group(1)
      option = re.sub('_', '-', option)
      option = re.sub('-$', '', option)

      if option not in self.untested_options and \
         not exists(join(self.source, 'test', 'baseline',
                         'opt-%s.test' % option)):
          self.missing_baseline_tests.add(option)

      if option not in man_options:
          self.missing_man_options.add(option)
      else:
          man_options.remove(option)

      if option not in texi_options:
          self.missing_texi_options.add(option)
      else:
          texi_options.remove(option)

    self.unknown_man_options = [option for option in man_options if option not in self.known_alternates]
    self.unknown_texi_options = [option for option in texi_options if option not in self.known_alternates]

    sep = "\n  --"
    if len(self.missing_baseline_tests):
      print("Missing Baseline test for:%s%s\n" % (sep, sep.join(sorted(list(self.missing_baseline_tests)))))
    if len(self.missing_man_options):
      print("Missing man page entries for:%s%s\n" % (sep, sep.join(sorted(list(self.missing_man_options)))))
    if len(self.missing_texi_options):
      print("Missing texi entries for:%s%s\n" % (sep, sep.join(sorted(list(self.missing_texi_options)))))
    if len(self.unknown_man_options):
      print("Man page entry for unknown options:%s%s" % (sep, sep.join(sorted(list(self.unknown_man_options)))))
    if len(self.unknown_texi_options):
      print("Texi entry for unknown option:%s%s" % (sep, sep.join(sorted(list(self.unknown_texi_options)))))

    errors = len(self.missing_baseline_tests) + len(self.missing_man_options) + len(self.missing_baseline_tests)
    return errors

if __name__ == "__main__":
  def getargs():
    parser = argparse.ArgumentParser(prog='CheckTests', description='Check that ledger options are tested and documented', prefix_chars='-')
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
  script = CheckTests(args)
  status = script.main()
  sys.exit(status)
