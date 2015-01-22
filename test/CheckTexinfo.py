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

class CheckTexinfo (CheckOptions):
  def __init__(self, args):
    CheckOptions.__init__(self, args)
    self.option_pattern = '@item --([-A-Za-z]+).*@c option'
    self.source_file = join(self.source, 'doc', 'ledger3.texi')
    self.source_type = 'texinfo'

  def find_options(self, filename):
    options = set()
    state_normal = 0
    state_option_table = 1
    state = state_normal
    option = None
    opt_doc = str()
    item_regex = re.compile('^@item --([-A-Za-z]+)')
    itemx_regex = re.compile('^@itemx')
    fix_regex = re.compile('FIX')
    for line in open(filename):
      line = line.strip()
      if state == state_normal:
        if line == '@ftable @option':
            state = state_option_table
      elif state == state_option_table:
          if line == '@end ftable':
              if option and len(opt_doc) and not fix_regex.search(opt_doc):
                  options.add(option)
              state = state_normal
              option = None
              continue
          match = item_regex.match(line)
          if match:
              if option and len(opt_doc) and not fix_regex.search(opt_doc):
                  options.add(option)
              option = match.group(1)
              opt_doc = str()
          elif itemx_regex.match(line):
              continue
          else:
              opt_doc += line
    return options

if __name__ == "__main__":
  def getargs():
    parser = argparse.ArgumentParser(prog='CheckTexinfo',
            description='Check that ledger options are documented in the texinfo manual')
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
  script = CheckTexinfo(args)
  status = script.main()
  sys.exit(status)
