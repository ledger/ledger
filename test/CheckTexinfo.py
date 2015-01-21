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
