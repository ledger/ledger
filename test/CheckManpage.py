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

class CheckManpage (CheckOptions):
  def __init__(self, args):
    CheckOptions.__init__(self, args)
    self.option_pattern = '\.It Fl \\\\-([-A-Za-z]+)'
    self.function_pattern = '\.It Fn ([-A-Za-z_]+)'
    self.source_file = join(self.source, 'doc', 'ledger.1')
    self.source_type = 'manpage'

if __name__ == "__main__":
  def getargs():
    parser = argparse.ArgumentParser(prog='CheckManpage',
            description='Check that ledger options are documented in the manpage')
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
  script = CheckManpage(args)
  status = script.main()
  sys.exit(status)
